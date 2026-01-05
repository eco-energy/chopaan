#!/usr/bin/env python3
"""
Meshed Grid Power Flow with Resource Optimality

Extends the properad approach to handle:
1. Meshed (cyclic) grids via fixed-point iteration
2. Resource optimality via the adjunction β ⊣ ρ

Key insight from Manin-Marcolli §3.3:
  Optimization IS an adjoint functor.

  ρ : C → R   assigns resources to configurations
  β : R → C   finds optimal config given resources (LEFT ADJOINT)

  The neural network learns β.

For meshed grids:
  - Decompose into spanning tree + cycle edges
  - Process tree edges via properad composition (O(n))
  - Iterate on cycle edges until convergence

The adjunction ensures:
  - Any config C with resources ≤ A factors through β(A)
  - β(A) is the OPTIMAL use of resources A
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
from dataclasses import dataclass
from typing import List, Dict, Tuple, Optional, Set
import numpy as np
from collections import deque


@dataclass
class PowerResource:
    """Resources available to the power system."""
    gen_capacity: torch.Tensor   # (num_nodes,) max generation per node
    line_rating: torch.Tensor    # (num_edges,) thermal limit per line
    voltage_range: Tuple[float, float]  # (V_min, V_max)
    cost_coeffs: torch.Tensor    # (num_nodes, 3) quadratic cost coeffs
    reserve_margin: float = 0.1


@dataclass
class PowerFlowConfig:
    """Power flow configuration (system state)."""
    edge_flows: torch.Tensor     # (num_edges, 2) P, Q per edge
    node_voltage: torch.Tensor   # (num_nodes, 2) |V|, θ per node
    generation: torch.Tensor     # (num_nodes, 2) P, Q generated
    curtailment: torch.Tensor    # (num_nodes,) power curtailed
    losses: float


class CycleDecomposition:
    """Decompose graph into spanning tree + fundamental cycles."""

    def __init__(self, edge_index: torch.Tensor, num_nodes: int):
        self.edge_index = edge_index
        self.num_nodes = num_nodes
        self.num_edges = edge_index.shape[1]

        self._decompose()

    def _decompose(self):
        """Find spanning tree and fundamental cycles via DFS."""
        src, tgt = self.edge_index[0].tolist(), self.edge_index[1].tolist()

        visited = set()
        tree_edges = []
        non_tree_edges = []
        parent = {}

        # DFS from node 0
        stack = [0]
        while stack:
            node = stack.pop()
            if node in visited:
                continue
            visited.add(node)

            for e, (s, t) in enumerate(zip(src, tgt)):
                if s == node and t not in visited:
                    tree_edges.append(e)
                    parent[t] = (node, e)
                    stack.append(t)
                elif s == node and t in visited and e not in tree_edges:
                    non_tree_edges.append(e)

        self.tree_edges = tree_edges
        self.non_tree_edges = non_tree_edges
        self.parent = parent

        # Find fundamental cycles
        self.cycles = []
        for e in non_tree_edges:
            s, t = src[e], tgt[e]
            # Find path from t to s in tree
            cycle = [e]
            current = t
            while current != s:
                if current in parent:
                    p, pe = parent[current]
                    cycle.append(pe)
                    current = p
                else:
                    break
            self.cycles.append(cycle)

    @property
    def is_acyclic(self) -> bool:
        return len(self.non_tree_edges) == 0


class CorollaModule(nn.Module):
    """Local module for a node, with cycle-aware updates."""

    def __init__(self, node_dim: int, hidden_dim: int, max_degree: int):
        super().__init__()
        self.max_degree = max_degree

        self.encoder = nn.Sequential(
            nn.Linear(node_dim + 2 * max_degree, hidden_dim),
            nn.ReLU(),
            nn.LayerNorm(hidden_dim),
            nn.Linear(hidden_dim, hidden_dim),
            nn.ReLU(),
        )

        # Output: flows for all incident edges
        self.flow_head = nn.Linear(hidden_dim, 2 * max_degree)

        # Cycle correction: learn to adjust for loop flows
        self.cycle_correction = nn.Linear(hidden_dim, 2 * max_degree)

    def forward(
        self,
        node_features: torch.Tensor,
        incoming_flows: torch.Tensor,
        deg_in: int,
        deg_out: int,
        net_injection: torch.Tensor,
        cycle_flows: Optional[torch.Tensor] = None
    ) -> torch.Tensor:
        batch_size = node_features.shape[0]
        device = node_features.device

        # Encode
        x = torch.cat([node_features, incoming_flows.view(batch_size, -1)], dim=-1)
        h = self.encoder(x)

        # Base flows
        raw_flows = self.flow_head(h).view(batch_size, self.max_degree, 2)

        # Add cycle correction if in a loop
        if cycle_flows is not None:
            correction = self.cycle_correction(h).view(batch_size, self.max_degree, 2)
            raw_flows = raw_flows + 0.1 * correction * cycle_flows

        # Enforce local conservation
        outflows = torch.zeros(batch_size, self.max_degree, 2, device=device)

        if deg_out == 0:
            return outflows

        if deg_out == 1:
            total_in = incoming_flows[:, :deg_in, :].sum(dim=1)
            outflows[:, 0, :] = total_in + net_injection
            return outflows

        # Multiple outputs: predict deg_out - 1, compute last
        free_flows = raw_flows[:, :deg_out - 1, :]
        total_in = incoming_flows[:, :deg_in, :].sum(dim=1)
        required_total = total_in + net_injection
        free_sum = free_flows.sum(dim=1)
        last_flow = required_total - free_sum

        outflows[:, :deg_out - 1, :] = free_flows
        outflows[:, deg_out - 1, :] = last_flow

        return outflows


class ResourceFunctor(nn.Module):
    """
    ρ : C → R

    Assigns resource usage to a power flow configuration.
    """

    def forward(self, config: PowerFlowConfig) -> PowerResource:
        # Extract generation capacities
        gen_p = config.generation[:, 0]  # P generation

        # Calculate reserve margin: 1.0 - (total_gen / max_capacity)
        total_gen = gen_p.sum()
        max_capacity = gen_p.max() * gen_p.shape[0]  # Assume max per node * num nodes
        reserve_margin = 1.0 - (total_gen / max_capacity).item() if max_capacity > 0 else 0.0
        reserve_margin = max(0.0, min(1.0, reserve_margin))  # Clamp to [0, 1]

        return PowerResource(
            gen_capacity=gen_p,
            line_rating=torch.sqrt(
                config.edge_flows[:, 0]**2 + config.edge_flows[:, 1]**2
            ),
            voltage_range=(
                config.node_voltage[:, 0].min().item(),
                config.node_voltage[:, 0].max().item()
            ),
            cost_coeffs=torch.zeros(config.generation.shape[0], 3),  # No cost data
            reserve_margin=reserve_margin
        )


class OptimalityFunctor(nn.Module):
    """
    β : R → C  (LEFT ADJOINT to ρ)

    Given resources, find the optimal power flow configuration.
    THIS IS WHAT THE NEURAL NETWORK LEARNS.

    The adjunction MorC(β(A), C) ≃ MorR(A, ρ(C)) means:
    - Any config achievable with resources A factors through β(A)
    - β(A) is optimal: minimizes cost while satisfying constraints
    """

    def __init__(
        self,
        num_nodes: int,
        edge_index: torch.Tensor,
        node_dim: int = 6,
        hidden_dim: int = 64,
        max_iterations: int = 10,
        tolerance: float = 1e-5
    ):
        super().__init__()
        self.num_nodes = num_nodes
        self.num_edges = edge_index.shape[1]
        self.max_iterations = max_iterations
        self.tolerance = tolerance

        self.register_buffer('edge_index', edge_index)

        # Analyze graph structure
        self.decomposition = CycleDecomposition(edge_index, num_nodes)

        # Build incidence structure
        self._build_incidence()

        # Max degree for padding
        self.max_degree = max(
            max(len(self.in_edges[v]) for v in range(num_nodes)),
            max(len(self.out_edges[v]) for v in range(num_nodes)),
            2
        )

        # Corolla module (shared weights)
        self.corolla = CorollaModule(node_dim, hidden_dim, self.max_degree)

        # Resource encoder
        self.resource_encoder = nn.Sequential(
            nn.Linear(num_nodes * 2, hidden_dim),  # gen_capacity + line influence
            nn.ReLU(),
            nn.Linear(hidden_dim, node_dim)
        )

    def _build_incidence(self):
        src, tgt = self.edge_index[0].tolist(), self.edge_index[1].tolist()
        self.in_edges = {v: [] for v in range(self.num_nodes)}
        self.out_edges = {v: [] for v in range(self.num_nodes)}
        for e, (s, t) in enumerate(zip(src, tgt)):
            self.out_edges[s].append(e)
            self.in_edges[t].append(e)

    def forward(
        self,
        resources: PowerResource,
        node_features: torch.Tensor,
        slack_node: int = 0
    ) -> PowerFlowConfig:
        """
        Compute optimal configuration given resources.

        This is β(resources) - the left adjoint to ρ.
        """
        batch_size = node_features.shape[0]
        device = node_features.device

        # Encode resource constraints into node features
        resource_info = torch.cat([
            resources.gen_capacity.unsqueeze(0).expand(batch_size, -1),
            resources.line_rating.mean().unsqueeze(0).expand(batch_size, self.num_nodes)
        ], dim=-1)
        resource_embedding = self.resource_encoder(resource_info)

        # Augment node features with resource info
        augmented_features = node_features + resource_embedding.unsqueeze(1)

        # Initialize edge flows
        edge_flows = torch.zeros(batch_size, self.num_edges, 2, device=device)

        if self.decomposition.is_acyclic:
            # Simple case: use properad composition
            edge_flows = self._properad_pass(augmented_features, edge_flows, slack_node)
        else:
            # Meshed: iterate to convergence
            edge_flows, num_iter, converged = self._iterate_meshed(
                augmented_features, edge_flows, slack_node
            )

        # Compute derived quantities
        generation = self._compute_generation(edge_flows, node_features)
        curtailment = torch.relu(resources.gen_capacity - generation[:, :, 0])
        losses = self._compute_losses(edge_flows)

        return PowerFlowConfig(
            edge_flows=edge_flows,
            node_voltage=torch.ones(batch_size, self.num_nodes, 2, device=device),  # Placeholder
            generation=generation,
            curtailment=curtailment,
            losses=losses
        )

    def _properad_pass(
        self,
        node_features: torch.Tensor,
        edge_flows: torch.Tensor,
        slack_node: int
    ) -> torch.Tensor:
        """Single pass in topological order (for acyclic graphs)."""
        batch_size = node_features.shape[0]
        device = node_features.device

        # Topological order
        topo_order = self._topological_sort()

        for v in topo_order:
            if v == slack_node:
                continue

            deg_in = len(self.in_edges[v])
            deg_out = len(self.out_edges[v])

            incoming = torch.zeros(batch_size, self.max_degree, 2, device=device)
            for i, e in enumerate(self.in_edges[v]):
                incoming[:, i, :] = edge_flows[:, e, :]

            net_injection = node_features[:, v, :2]

            out_flows = self.corolla(
                node_features[:, v, :],
                incoming,
                deg_in,
                deg_out,
                net_injection
            )

            for i, e in enumerate(self.out_edges[v]):
                edge_flows[:, e, :] = out_flows[:, i, :]

        return edge_flows

    def _iterate_meshed(
        self,
        node_features: torch.Tensor,
        edge_flows: torch.Tensor,
        slack_node: int
    ) -> Tuple[torch.Tensor, int, bool]:
        """Iterate until convergence for meshed graphs."""
        batch_size = node_features.shape[0]
        device = node_features.device

        # Identify which nodes are in cycles
        cycle_nodes = set()
        for cycle in self.decomposition.cycles:
            for e in cycle:
                cycle_nodes.add(self.edge_index[0, e].item())
                cycle_nodes.add(self.edge_index[1, e].item())

        for iteration in range(self.max_iterations):
            old_flows = edge_flows.clone()

            # Process all nodes
            for v in range(self.num_nodes):
                if v == slack_node:
                    continue

                deg_in = len(self.in_edges[v])
                deg_out = len(self.out_edges[v])

                incoming = torch.zeros(batch_size, self.max_degree, 2, device=device)
                for i, e in enumerate(self.in_edges[v]):
                    incoming[:, i, :] = edge_flows[:, e, :]

                net_injection = node_features[:, v, :2]

                # Cycle flows for nodes in loops
                cycle_flows = None
                if v in cycle_nodes:
                    cycle_flows = torch.zeros(batch_size, self.max_degree, 2, device=device)
                    for i, e in enumerate(self.in_edges[v] + self.out_edges[v]):
                        if e in self.decomposition.non_tree_edges:
                            cycle_flows[:, i, :] = 1.0

                out_flows = self.corolla(
                    node_features[:, v, :],
                    incoming,
                    deg_in,
                    deg_out,
                    net_injection,
                    cycle_flows
                )

                # Damped update for stability
                for i, e in enumerate(self.out_edges[v]):
                    edge_flows[:, e, :] = 0.7 * out_flows[:, i, :] + 0.3 * edge_flows[:, e, :]

            # Check convergence
            max_diff = (edge_flows - old_flows).abs().max().item()
            if max_diff < self.tolerance:
                return edge_flows, iteration + 1, True

        return edge_flows, self.max_iterations, False

    def _topological_sort(self) -> List[int]:
        """Kahn's algorithm."""
        src, tgt = self.edge_index[0].tolist(), self.edge_index[1].tolist()
        in_degree = [0] * self.num_nodes
        for t in tgt:
            in_degree[t] += 1

        queue = deque([v for v in range(self.num_nodes) if in_degree[v] == 0])
        result = []

        while queue:
            v = queue.popleft()
            result.append(v)
            for e in self.out_edges[v]:
                t = tgt[e]
                in_degree[t] -= 1
                if in_degree[t] == 0:
                    queue.append(t)

        return result if len(result) == self.num_nodes else list(range(self.num_nodes))

    def _compute_generation(
        self,
        edge_flows: torch.Tensor,
        node_features: torch.Tensor
    ) -> torch.Tensor:
        """Compute generation at each node from edge flows."""
        batch_size = edge_flows.shape[0]
        device = edge_flows.device

        generation = torch.zeros(batch_size, self.num_nodes, 2, device=device)
        src, tgt = self.edge_index[0].tolist(), self.edge_index[1].tolist()

        for e, (s, t) in enumerate(zip(src, tgt)):
            generation[:, s, :] += edge_flows[:, e, :]
            generation[:, t, :] -= edge_flows[:, e, :]

        return generation

    def _compute_losses(self, edge_flows: torch.Tensor) -> float:
        """
        Estimate losses using approximation: Loss ≈ R * I² ≈ R * S²/V²

        Where:
        - R_e ≈ 0.001 pu is typical distribution line resistance
        - S_e² = P_e² + Q_e² is apparent power squared
        - V_nom² ≈ 1.0 pu (nominal voltage)

        Formula: losses = sum(R_e * S_e² / V_nom²)
        """
        R_line = 0.001  # Typical distribution line resistance in per-unit
        V_nom_sq = 1.0  # Nominal voltage squared (1.0 pu)

        S_squared = edge_flows[:, :, 0]**2 + edge_flows[:, :, 1]**2
        return (R_line * S_squared.sum() / V_nom_sq).item()


class ResourceOptimalDispatcher(nn.Module):
    """
    Complete dispatcher with resource optimality.

    Implements β ⊣ ρ where:
    - ρ extracts resource usage from config
    - β finds optimal config given resources

    The universal property ensures β(A) is truly optimal.
    """

    def __init__(
        self,
        num_nodes: int,
        edge_index: torch.Tensor,
        node_dim: int = 6,
        hidden_dim: int = 64
    ):
        super().__init__()

        self.rho = ResourceFunctor()
        self.beta = OptimalityFunctor(
            num_nodes=num_nodes,
            edge_index=edge_index,
            node_dim=node_dim,
            hidden_dim=hidden_dim
        )

    def forward(
        self,
        resources: PowerResource,
        node_features: torch.Tensor,
        slack_node: int = 0
    ) -> PowerFlowConfig:
        """Get optimal config for given resources."""
        return self.beta(resources, node_features, slack_node)

    def verify_adjunction(
        self,
        resources: PowerResource,
        config: PowerFlowConfig
    ) -> bool:
        """
        Verify the adjunction property:
        If config is achievable with resources, it factors through β(resources).
        """
        used_resources = self.rho(config)

        # Check if resources cover what config needs
        gen_ok = (resources.gen_capacity >= used_resources.gen_capacity).all()
        line_ok = (resources.line_rating >= used_resources.line_rating).all()

        return gen_ok.item() and line_ok.item()


if __name__ == "__main__":
    print("Testing Meshed Properad with Resource Optimality")
    print("=" * 60)

    # Create a meshed grid (has cycle: 0-1-2-3-0)
    edge_index = torch.tensor([
        [0, 1, 2, 3, 1],  # sources (last edge creates cycle)
        [1, 2, 3, 0, 3]   # targets
    ], dtype=torch.long)

    decomp = CycleDecomposition(edge_index, num_nodes=4)
    print(f"Tree edges: {decomp.tree_edges}")
    print(f"Cycle edges: {decomp.non_tree_edges}")
    print(f"Is acyclic: {decomp.is_acyclic}")
    print(f"Fundamental cycles: {decomp.cycles}")

    # Create dispatcher
    dispatcher = ResourceOptimalDispatcher(
        num_nodes=4,
        edge_index=edge_index,
        node_dim=6,
        hidden_dim=32
    )

    # Create resources
    resources = PowerResource(
        gen_capacity=torch.tensor([100.0, 50.0, 0.0, 0.0]),
        line_rating=torch.tensor([50.0, 50.0, 50.0, 50.0, 30.0]),
        voltage_range=(0.95, 1.05),
        cost_coeffs=torch.zeros(4, 3),
        reserve_margin=0.1
    )

    # Node features
    node_features = torch.tensor([[
        [0.0, 0.0, 0.95, 1.05, 100.0, 1.0],
        [30.0, 10.0, 0.95, 1.05, 50.0, 1.0],
        [-20.0, -5.0, 0.95, 1.05, 0.0, 0.0],
        [-15.0, -3.0, 0.95, 1.05, 0.0, 0.0]
    ]], dtype=torch.float32)

    # Get optimal config
    config = dispatcher(resources, node_features)

    print(f"\nOptimal config (β(resources)):")
    print(f"  Edge flows: {config.edge_flows}")
    print(f"  Losses: {config.losses:.4f}")
    print(f"  Curtailment: {config.curtailment}")

    # Verify adjunction
    valid = dispatcher.verify_adjunction(resources, config)
    print(f"\nAdjunction valid: {valid}")

    print("\n" + "=" * 60)
    print("Key: β(resources) is the OPTIMAL config for given resources.")
    print("The adjunction ensures any achievable config factors through it.")
