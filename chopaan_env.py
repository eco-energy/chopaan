"""
PufferLib environment for chopaan AC grid dispatch.

A 24-hour episode dispatch problem on the 4-node test grid:
- 1 slack (grid tie), 1 PV bus, 2 PQ loads
- Step = 1 hour, episode = 24 steps
- Observation: hour-of-day, solar availability, loads, last voltages
- Action: P,Q setpoints for the PV inverter (continuous, normalized to [-1, 1])
- Reward: -(grid import cost + voltage violation penalty + Kirchhoff residual)

Pakistan tariff context: K-Electric peak ~30 PKR/kWh, off-peak ~15 PKR/kWh.
PV is curtailment-free up to P_rated; over-Q draws from grid (penalized).
"""

from dataclasses import dataclass

import numpy as np
import pufferlib
import gymnasium as gym

from summing_functor_net import PowerGridGraph, create_test_grid


HOURS = 24
PV_BUS = 1
LOAD_BUSES = (2, 3)


@dataclass
class GridState:
    hour: int
    p_solar_avail: float  # kW available from PV at this hour
    p_load: np.ndarray    # (n_load,) kW demand
    q_load: np.ndarray    # (n_load,) kVAR demand
    v_last: np.ndarray    # (n_nodes,) per-unit voltage magnitudes


def solar_curve(hour: int, p_rated: float) -> float:
    """Bell-shaped solar profile peaking at noon."""
    if hour < 6 or hour > 18:
        return 0.0
    x = (hour - 12.0) / 6.0
    return p_rated * max(0.0, 1.0 - x * x)


def load_curve(hour: int, p_base: float) -> float:
    """Residential evening-peak load profile."""
    morning = 0.6 + 0.4 * np.exp(-((hour - 8) ** 2) / 4.0)
    evening = 0.7 + 0.5 * np.exp(-((hour - 19) ** 2) / 3.0)
    return p_base * max(morning, evening)


def tariff(hour: int) -> float:
    """PKR/kWh; peak hours 18-22."""
    return 30.0 if 18 <= hour <= 22 else 15.0


class ChopaanDispatchEnv(gym.Env):
    """Single-PV-inverter 24h dispatch env wrapped for PufferLib."""

    metadata = {"render_modes": []}

    def __init__(self, grid: PowerGridGraph = None, seed: int = 0):
        super().__init__()
        self.grid = grid if grid is not None else create_test_grid()
        self.n_nodes = self.grid.num_nodes
        self.p_rated = float(self.grid.node_features[PV_BUS, 4])
        self.q_rated = float(self.grid.node_features[PV_BUS, 5])
        self.p_load_base = self.grid.node_features[list(LOAD_BUSES), 1].numpy()
        self.q_load_base = 0.3 * self.p_load_base  # 0.3 power factor assumption

        # obs: [hour/24, p_solar/p_rated, p_load_2, p_load_3, q_load_2, q_load_3, v_0..v_3]
        self.observation_space = gym.spaces.Box(
            low=-1.0, high=2.0, shape=(2 + 2 * len(LOAD_BUSES) + self.n_nodes,),
            dtype=np.float32,
        )
        # action: [p_set_norm, q_set_norm] in [-1, 1] -> scaled to [0, p_rated] and [-q_rated, q_rated]
        self.action_space = gym.spaces.Box(low=-1.0, high=1.0, shape=(2,), dtype=np.float32)

        self.rng = np.random.default_rng(seed)
        self.state: GridState = None

    def reset(self, seed=None, options=None):
        if seed is not None:
            self.rng = np.random.default_rng(seed)
        noise = self.rng.normal(1.0, 0.05, size=len(LOAD_BUSES))
        self.state = GridState(
            hour=0,
            p_solar_avail=0.0,
            p_load=self.p_load_base * noise,
            q_load=self.q_load_base * noise,
            v_last=np.ones(self.n_nodes, dtype=np.float32),
        )
        return self._obs(), {}

    def step(self, action: np.ndarray):
        p_set = float(np.clip((action[0] + 1.0) * 0.5, 0.0, 1.0) * self.p_rated)
        q_set = float(np.clip(action[1], -1.0, 1.0) * self.q_rated)

        # curtailment: can't exceed available solar
        p_set = min(p_set, self.state.p_solar_avail)

        # power balance: load - PV = grid_import (slack)
        total_p_load = float(self.state.p_load.sum())
        total_q_load = float(self.state.q_load.sum())
        grid_p = total_p_load - p_set
        grid_q = total_q_load - q_set

        # Crude voltage drop proxy: |S| / S_base affects sag
        s_mag = np.hypot(grid_p, grid_q)
        v_drop = 0.02 * s_mag / 100.0
        self.state.v_last = np.clip(1.0 - v_drop, 0.85, 1.10) * np.ones(self.n_nodes)

        # Reward components
        cost = max(grid_p, 0.0) * tariff(self.state.hour) - max(-grid_p, 0.0) * 0.5 * tariff(self.state.hour)
        v_violation = float(np.sum(np.clip(0.95 - self.state.v_last, 0.0, None) +
                                    np.clip(self.state.v_last - 1.05, 0.0, None)))
        kirchhoff_resid = abs((p_set - total_p_load) - (-grid_p))  # always 0 by construction; kept for shape
        reward = -(cost / 100.0 + 10.0 * v_violation + kirchhoff_resid)

        # Advance hour
        self.state.hour += 1
        terminated = self.state.hour >= HOURS
        if not terminated:
            noise = self.rng.normal(1.0, 0.05, size=len(LOAD_BUSES))
            self.state.p_solar_avail = solar_curve(self.state.hour, self.p_rated)
            self.state.p_load = np.array(
                [load_curve(self.state.hour, self.p_load_base[i]) for i in range(len(LOAD_BUSES))]
            ) * noise
            self.state.q_load = 0.3 * self.state.p_load

        return self._obs(), float(reward), terminated, False, {"cost_pkr": cost}

    def _obs(self) -> np.ndarray:
        return np.concatenate([
            np.array([self.state.hour / HOURS, self.state.p_solar_avail / max(self.p_rated, 1e-6)],
                     dtype=np.float32),
            (self.state.p_load / np.maximum(self.p_load_base, 1e-6)).astype(np.float32),
            (self.state.q_load / np.maximum(self.q_load_base, 1e-6)).astype(np.float32),
            self.state.v_last.astype(np.float32),
        ])


def env_creator(seed: int = 0):
    """PufferLib factory."""
    return pufferlib.emulation.GymnasiumPufferEnv(env=ChopaanDispatchEnv(seed=seed))


if __name__ == "__main__":
    env = ChopaanDispatchEnv(seed=42)
    obs, _ = env.reset()
    total_reward = 0.0
    for _ in range(HOURS):
        action = env.action_space.sample()
        obs, r, term, _, info = env.step(action)
        total_reward += r
        if term:
            break
    print(f"Random policy 24h return: {total_reward:.2f} (cost ~{-total_reward * 100:.0f} PKR)")
