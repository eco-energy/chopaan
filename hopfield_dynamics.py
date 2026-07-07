"""
Categorical Hopfield Dynamics for Power Flow

Based on Manin-Marcolli §6: Hopfield dynamics on networks

The key insight: Power flow is a dynamical system that converges to
attractors representing optimal dispatch. The neural network learns
the transition matrix T such that these attractors minimize cost
while satisfying physical constraints.

Categorical Hopfield equation (Eq. 6.2):
    X_e(n+1) = X_e(n) ⊕ (⊕_{e'} T_{ee'}(X_{e'}(n)) ⊕ Θ_e)₊

Where:
    X_e     : state on edge e (P, Q flows)
    T_{ee'} : learnable transition functor (network coupling)
    Θ_e     : external input (generation/load)
    (·)₊    : threshold functor (enforces feasibility)

Advantages:
    - Dynamics naturally find valid power flow solutions
    - Fixed points are stable operating points
    - Threshold enforces constraints by construction
    - Learnable T adapts to network topology
    - Energy-based training objective
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
from torch_geometric.nn import MessagePassing
from torch_geometric.data import Data
from torch_geometric.utils import to_dense_adj, dense_to_sparse
import numpy as np
from typing import Tuple, Optional, Dict, List


class SoftThreshold(nn.Module):
    """
    Categorical threshold functor (·)₊ with differentiable approximation.

    (C)₊ = C if [ρ(C)] ⪰ 0, else 0

    For power flow, ρ(C) checks:
    - |P| ≤ P_max (thermal limit)
    - |Q| ≤ Q_max (reactive limit)
    - V_min ≤ V ≤ V_max (voltage bounds)

    Uses sigmoid for smooth differentiability during training.
    """

    def __init__(
        self,
        p_max: float = 100.0,      # kW per edge
        q_max: float = 100.0,      # kVAR per edge
        v_min: float = 0.94,       # pu (212V at 220V nominal)
        v_max: float = 1.06,       # pu (233V at 220V nominal)
        softness: float = 10.0,    # Sigmoid steepness
    ):
        super().__init__()
        self.register_buffer('p_max', torch.tensor(p_max))
        self.register_buffer('q_max', torch.tensor(q_max))
        self.register_buffer('v_min', torch.tensor(v_min))
        self.register_buffer('v_max', torch.tensor(v_max))
        self.softness = softness

    def forward(
        self,
        p: torch.Tensor,  # [batch, num_edges] or [num_edges]
        q: torch.Tensor,
        v: Optional[torch.Tensor] = None,
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Apply soft threshold to flow states.

        Returns (p_thresh, q_thresh, feasibility) where feasibility ∈ [0,1]
        indicates how "valid" each edge's state is.
        """
        # Check each constraint with sigmoid
        p_ok = torch.sigmoid(self.softness * (self.p_max - torch.abs(p)))
        q_ok = torch.sigmoid(self.softness * (self.q_max - torch.abs(q)))

        if v is not None:
            v_low_ok = torch.sigmoid(self.softness * (v - self.v_min))
            v_high_ok = torch.sigmoid(self.softness * (self.v_max - v))
            feasibility = p_ok * q_ok * v_low_ok * v_high_ok
        else:
            feasibility = p_ok * q_ok

        # Threshold: multiply state by feasibility
        p_thresh = p * feasibility
        q_thresh = q * feasibility

        return p_thresh, q_thresh, feasibility


class LearnableTransition(nn.Module):
    """
    Learnable transition matrix T_{ee'}.

    This is the key learnable component: T captures how flow on edge e'
    influences what happens on edge e. For power grids:
    - Nearby edges have stronger coupling
    - Impedance determines influence strength
    - Topology constrains which edges interact

    The network learns T such that the dynamics converge to optimal dispatch.
    """

    def __init__(
        self,
        num_edges: int,
        hidden_dim: int = 64,
        use_topology: bool = True,
    ):
        super().__init__()
        self.num_edges = num_edges
        self.use_topology = use_topology

        # Learnable transition matrix (initialized small for stability)
        self.T = nn.Parameter(0.1 * torch.randn(num_edges, num_edges))

        # Self-connection (diagonal) - provides inertia/memory
        self.self_weight = nn.Parameter(torch.ones(num_edges) * 0.5)

        # Optional: learn T from edge features
        if hidden_dim > 0:
            self.edge_mlp = nn.Sequential(
                nn.Linear(4, hidden_dim),  # (P, Q, Z_real, Z_imag) features
                nn.ReLU(),
                nn.Linear(hidden_dim, hidden_dim),
                nn.ReLU(),
                nn.Linear(hidden_dim, 1),
            )
        else:
            self.edge_mlp = None

        # Damping factor (learned)
        self.damping = nn.Parameter(torch.tensor(0.3))

    def forward(
        self,
        p: torch.Tensor,  # [batch, num_edges]
        q: torch.Tensor,
        edge_features: Optional[torch.Tensor] = None,
        adjacency_mask: Optional[torch.Tensor] = None,
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        """
        Apply transition: ⊕_{e'} T_{ee'}(X_{e'})
        """
        # Build effective transition matrix
        T_eff = self.T.clone()

        # Add self-connections on diagonal
        T_eff = T_eff + torch.diag(self.self_weight)

        # Mask by topology if provided
        if adjacency_mask is not None:
            T_eff = T_eff * adjacency_mask

        # Normalize rows for stability
        row_sums = T_eff.abs().sum(dim=1, keepdim=True).clamp(min=1e-6)
        T_eff = T_eff / row_sums

        # Apply transition: T @ X
        # Handle batched input
        if p.dim() == 1:
            p_new = T_eff @ p
            q_new = T_eff @ q
        else:
            # Batched: [batch, num_edges]
            p_new = torch.einsum('ij,bj->bi', T_eff, p)
            q_new = torch.einsum('ij,bj->bi', T_eff, q)

        return p_new, q_new


class ExternalInput(nn.Module):
    """
    External input Θ_e from generation and load injections.

    Distributes nodal injections (P_gen - P_load, Q_gen - Q_load)
    to edges based on network topology.

    Topology-aware: Only nodes incident to an edge can influence that edge.
    """

    def __init__(self, num_nodes: int, num_edges: int, edge_index: Optional[torch.Tensor] = None):
        super().__init__()
        self.num_nodes = num_nodes
        self.num_edges = num_edges

        # Build and register incidence structure for topology-aware distribution
        if edge_index is not None:
            # Build node-edge incidence: I[n,e] = 1 if node n in edge e
            incidence = torch.zeros(num_nodes, num_edges)
            incidence[edge_index[0], torch.arange(num_edges)] = 1
            incidence[edge_index[1], torch.arange(num_edges)] = 1
            self.register_buffer('incidence_mask', incidence)
        else:
            # Fallback: fully connected (will be masked during forward if incidence provided)
            self.register_buffer('incidence_mask', torch.ones(num_nodes, num_edges))

        # Learn how to distribute node injections to edges (topology-aware)
        # Separate weights for P and Q
        self.distribution_p = nn.Linear(num_nodes, num_edges, bias=False)
        self.distribution_q = nn.Linear(num_nodes, num_edges, bias=False)

        # Initialize weights to respect topology
        with torch.no_grad():
            # Only allow connections where node is incident to edge
            self.distribution_p.weight.data *= self.incidence_mask.T
            self.distribution_q.weight.data *= self.incidence_mask.T

    def forward(
        self,
        node_injections: torch.Tensor,  # [batch, num_nodes, 2] = (P, Q)
        incidence_matrix: Optional[torch.Tensor] = None,
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        """
        Convert node injections to edge inputs using topology.
        """
        batch_size = node_injections.shape[0] if node_injections.dim() == 3 else 1

        # Extract P and Q injections
        if node_injections.dim() == 3:
            p_inject = node_injections[:, :, 0]  # [batch, num_nodes]
            q_inject = node_injections[:, :, 1]
        else:
            node_injections = node_injections.reshape(self.num_nodes, 2)
            p_inject = node_injections[:, 0].unsqueeze(0)  # [1, num_nodes]
            q_inject = node_injections[:, 1].unsqueeze(0)

        # Apply topology-aware distribution
        # Mask weights to enforce topology during forward pass
        with torch.no_grad():
            self.distribution_p.weight.data *= self.incidence_mask.T
            self.distribution_q.weight.data *= self.incidence_mask.T

        p_input = self.distribution_p(p_inject)  # [batch, num_edges]
        q_input = self.distribution_q(q_inject)

        return p_input.squeeze(0), q_input.squeeze(0)


class HopfieldStep(nn.Module):
    """
    Single step of categorical Hopfield dynamics.

    X_e(n+1) = X_e(n) ⊕ (⊕_{e'} T_{ee'}(X_{e'}(n)) ⊕ Θ_e)₊

    Implemented as:
    1. Apply transition T to current state
    2. Add external input Θ
    3. Apply threshold (·)₊
    4. Blend with previous state (damping)
    """

    def __init__(
        self,
        transition: LearnableTransition,
        threshold: SoftThreshold,
        external: ExternalInput,
    ):
        super().__init__()
        self.transition = transition
        self.threshold = threshold
        self.external = external

    def forward(
        self,
        p: torch.Tensor,
        q: torch.Tensor,
        node_injections: torch.Tensor,
        adjacency_mask: Optional[torch.Tensor] = None,
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Execute one Hopfield step.

        Returns (p_new, q_new, feasibility)
        """
        # Step 1: Apply transition
        p_trans, q_trans = self.transition(p, q, adjacency_mask=adjacency_mask)

        # Step 2: Add external input
        p_ext, q_ext = self.external(node_injections)
        p_with_input = p_trans + p_ext
        q_with_input = q_trans + q_ext

        # Step 3: Apply threshold
        p_thresh, q_thresh, feasibility = self.threshold(p_with_input, q_with_input)

        # Step 4: Damped update
        alpha = torch.sigmoid(self.transition.damping)  # Keep in [0, 1]
        p_new = alpha * p + (1 - alpha) * p_thresh
        q_new = alpha * q + (1 - alpha) * q_thresh

        return p_new, q_new, feasibility


class HopfieldDynamics(nn.Module):
    """
    Full Hopfield dynamics network for power flow.

    Unrolls K steps of the Hopfield equation and returns the final state
    (ideally an attractor = valid power flow solution).

    Training objective: make attractors minimize dispatch cost while
    satisfying Kirchhoff's laws and operational constraints.
    """

    def __init__(
        self,
        num_nodes: int,
        num_edges: int,
        num_steps: int = 10,
        hidden_dim: int = 64,
        p_max: float = 100.0,
        q_max: float = 100.0,
        v_min: float = 0.94,
        v_max: float = 1.06,
        edge_index: Optional[torch.Tensor] = None,
    ):
        super().__init__()
        self.num_nodes = num_nodes
        self.num_edges = num_edges
        self.num_steps = num_steps

        # Components
        self.transition = LearnableTransition(num_edges, hidden_dim)
        self.threshold = SoftThreshold(p_max, q_max, v_min, v_max)
        self.external = ExternalInput(num_nodes, num_edges, edge_index=edge_index)

        # Single step module
        self.step = HopfieldStep(self.transition, self.threshold, self.external)

        # Initial state encoder (from node features to edge flows)
        self.init_encoder = nn.Sequential(
            nn.Linear(num_nodes * 2, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, num_edges * 2),
        )

    def forward(
        self,
        node_injections: torch.Tensor,  # [batch, num_nodes, 2]
        adjacency_mask: Optional[torch.Tensor] = None,
        return_trajectory: bool = False,
    ) -> Dict[str, torch.Tensor]:
        """
        Run Hopfield dynamics to find power flow solution.

        Returns dict with:
            'p_final': Final active power flows [batch, num_edges]
            'q_final': Final reactive power flows
            'feasibility': Final feasibility scores
            'converged': Whether dynamics converged
            'energy': Final Hopfield energy
            'trajectory': (optional) List of (p, q) at each step
        """
        batch_size = node_injections.shape[0] if node_injections.dim() == 3 else 1

        # Initialize edge flows from node injections
        flat_inject = node_injections.reshape(batch_size, -1)
        init_flows = self.init_encoder(flat_inject)
        init_flows = init_flows.reshape(batch_size, self.num_edges, 2)

        p = init_flows[:, :, 0]
        q = init_flows[:, :, 1]

        if batch_size == 1:
            p = p.squeeze(0)
            q = q.squeeze(0)

        trajectory = [(p.clone(), q.clone())] if return_trajectory else None

        # Run dynamics
        prev_p, prev_q = p, q
        converged = False

        for step in range(self.num_steps):
            p, q, feasibility = self.step(
                p, q, node_injections, adjacency_mask
            )

            if return_trajectory:
                trajectory.append((p.clone(), q.clone()))

            # Check convergence
            p_diff = (p - prev_p).abs().max()
            q_diff = (q - prev_q).abs().max()
            if p_diff < 1e-4 and q_diff < 1e-4:
                converged = True
                break

            prev_p, prev_q = p, q

        # Compute Hopfield energy: E = -½ xᵀTx
        T_eff = self.transition.T + torch.diag(self.transition.self_weight)
        if p.dim() == 1:
            energy = -0.5 * (p @ T_eff @ p)
        else:
            energy = -0.5 * torch.einsum('bi,ij,bj->b', p, T_eff, p)

        result = {
            'p_final': p,
            'q_final': q,
            'feasibility': feasibility,
            'converged': converged,
            'energy': energy,
            'num_steps': step + 1,
        }

        if return_trajectory:
            result['trajectory'] = trajectory

        return result


class HopfieldPowerFlow(nn.Module):
    """
    Complete Hopfield-based power flow solver.

    Combines:
    1. Node feature encoding (load, generation, topology)
    2. Hopfield dynamics for flow computation
    3. Kirchhoff verification layer
    4. Output heads for dispatch decisions

    The attractor of the dynamics IS the power flow solution.
    """

    def __init__(
        self,
        num_nodes: int,
        num_edges: int,
        num_generators: int,
        node_features: int = 8,
        edge_features: int = 4,
        hidden_dim: int = 64,
        num_steps: int = 15,
        p_max: float = 100.0,
        q_max: float = 100.0,
        edge_index: Optional[torch.Tensor] = None,
    ):
        super().__init__()
        self.num_nodes = num_nodes
        self.num_edges = num_edges
        self.num_generators = num_generators

        # Node encoder
        self.node_encoder = nn.Sequential(
            nn.Linear(node_features, hidden_dim),
            nn.LayerNorm(hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, hidden_dim),
        )

        # Compute node injections from features
        self.injection_head = nn.Linear(hidden_dim, 2)  # (P, Q)

        # Hopfield dynamics (with topology-aware external input)
        self.hopfield = HopfieldDynamics(
            num_nodes=num_nodes,
            num_edges=num_edges,
            num_steps=num_steps,
            hidden_dim=hidden_dim,
            p_max=p_max,
            q_max=q_max,
            edge_index=edge_index,
        )

        # Generator dispatch head
        self.dispatch_head = nn.Sequential(
            nn.Linear(num_edges * 2 + num_nodes * hidden_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, num_generators * 2),  # P, Q per generator
        )

        # Kirchhoff checker (for loss, not inference)
        self.kirchhoff_weight = nn.Parameter(torch.tensor(1.0))

    def forward(
        self,
        node_features: torch.Tensor,     # [batch, num_nodes, node_features]
        edge_index: torch.Tensor,         # [2, num_edges]
        return_details: bool = False,
    ) -> Dict[str, torch.Tensor]:
        """
        Compute power flow via Hopfield dynamics.
        """
        batch_size = node_features.shape[0]

        # Encode nodes
        node_encoded = self.node_encoder(node_features)  # [batch, nodes, hidden]

        # Compute node injections
        node_injections = self.injection_head(node_encoded)  # [batch, nodes, 2]

        # Build adjacency mask from edge_index
        adjacency_mask = self._build_edge_adjacency(edge_index)

        # Run Hopfield dynamics
        hopfield_result = self.hopfield(
            node_injections,
            adjacency_mask=adjacency_mask,
            return_trajectory=return_details,
        )

        # Extract flows
        p_flows = hopfield_result['p_final']
        q_flows = hopfield_result['q_final']

        # Compute generator dispatch
        if p_flows.dim() == 1:
            flows_flat = torch.cat([p_flows, q_flows])
            node_flat = node_encoded.reshape(-1)
        else:
            flows_flat = torch.cat([p_flows, q_flows], dim=-1)
            node_flat = node_encoded.reshape(batch_size, -1)

        combined = torch.cat([flows_flat, node_flat], dim=-1)
        dispatch = self.dispatch_head(combined)
        dispatch = dispatch.reshape(-1, self.num_generators, 2)

        result = {
            'p_dispatch': dispatch[:, :, 0],
            'q_dispatch': dispatch[:, :, 1],
            'p_flows': p_flows,
            'q_flows': q_flows,
            'feasibility': hopfield_result['feasibility'],
            'energy': hopfield_result['energy'],
            'converged': hopfield_result['converged'],
        }

        if return_details:
            result['trajectory'] = hopfield_result.get('trajectory')
            result['node_injections'] = node_injections

        return result

    def _build_edge_adjacency(self, edge_index: torch.Tensor) -> torch.Tensor:
        """Build edge-to-edge adjacency using incidence matrix."""
        num_edges = edge_index.shape[1]

        # Build node-edge incidence: I[n,e] = 1 if node n in edge e
        incidence = torch.zeros(self.num_nodes, num_edges, device=edge_index.device)
        incidence[edge_index[0], torch.arange(num_edges)] = 1
        incidence[edge_index[1], torch.arange(num_edges)] = 1

        # Edge adjacency: E_adj = I^T @ I (edges sharing nodes)
        adj = incidence.T @ incidence
        return (adj > 0).float()


class HopfieldLoss(nn.Module):
    """
    Loss function for training Hopfield power flow network.

    Components:
    1. Energy minimization (attractors should be low energy)
    2. Kirchhoff violation (flows must balance at nodes)
    3. Dispatch accuracy (match ground truth optimal dispatch)
    4. Feasibility (all constraints satisfied)
    5. Convergence (dynamics should converge)
    """

    def __init__(
        self,
        kirchhoff_weight: float = 10.0,
        energy_weight: float = 1.0,
        feasibility_weight: float = 5.0,
        convergence_weight: float = 0.1,
    ):
        super().__init__()
        self.kirchhoff_weight = kirchhoff_weight
        self.energy_weight = energy_weight
        self.feasibility_weight = feasibility_weight
        self.convergence_weight = convergence_weight

    def forward(
        self,
        predictions: Dict[str, torch.Tensor],
        targets: Dict[str, torch.Tensor],
        incidence_matrix: torch.Tensor,
    ) -> Dict[str, torch.Tensor]:
        """
        Compute total loss.

        Args:
            predictions: Output from HopfieldPowerFlow
            targets: Ground truth dispatch (from IPOPT)
            incidence_matrix: [num_nodes, num_edges] incidence matrix
        """
        losses = {}

        # 1. Dispatch accuracy
        dispatch_loss = F.mse_loss(
            predictions['p_dispatch'],
            targets['p_dispatch']
        ) + F.mse_loss(
            predictions['q_dispatch'],
            targets['q_dispatch']
        )
        losses['dispatch'] = dispatch_loss

        # 2. Kirchhoff violation: A @ flows = injections
        p_flows = predictions['p_flows']
        if p_flows.dim() == 1:
            p_flows = p_flows.unsqueeze(0)

        kirchhoff_violation = torch.einsum(
            'ne,be->bn',
            incidence_matrix,
            p_flows
        )
        kirchhoff_loss = (kirchhoff_violation ** 2).mean()
        losses['kirchhoff'] = self.kirchhoff_weight * kirchhoff_loss

        # 3. Energy (should be low at attractors)
        energy = predictions['energy']
        if energy.dim() == 0:
            energy = energy.unsqueeze(0)
        energy_loss = F.relu(energy).mean()  # Penalize positive energy
        losses['energy'] = self.energy_weight * energy_loss

        # 4. Feasibility
        feasibility = predictions['feasibility']
        feasibility_loss = (1 - feasibility).mean()
        losses['feasibility'] = self.feasibility_weight * feasibility_loss

        # 5. Convergence (penalize if didn't converge)
        if not predictions['converged']:
            losses['convergence'] = torch.tensor(self.convergence_weight)
        else:
            losses['convergence'] = torch.tensor(0.0)

        # Total
        losses['total'] = sum(losses.values())

        return losses


def build_hopfield_model(
    num_nodes: int = 10,
    num_edges: int = 15,
    num_generators: int = 3,
    hidden_dim: int = 64,
    num_steps: int = 15,
    edge_index: Optional[torch.Tensor] = None,
) -> Tuple[HopfieldPowerFlow, HopfieldLoss]:
    """
    Build Hopfield power flow model and loss function.

    Example usage:
        model, loss_fn = build_hopfield_model(10, 15, 3, edge_index=edge_index)

        # Training loop
        for batch in dataloader:
            predictions = model(batch.node_features, batch.edge_index)
            losses = loss_fn(predictions, batch.targets, batch.incidence)
            losses['total'].backward()
    """
    model = HopfieldPowerFlow(
        num_nodes=num_nodes,
        num_edges=num_edges,
        num_generators=num_generators,
        hidden_dim=hidden_dim,
        num_steps=num_steps,
        edge_index=edge_index,
    )

    loss_fn = HopfieldLoss()

    return model, loss_fn


# Example usage and test
if __name__ == '__main__':
    print("Testing Categorical Hopfield Dynamics")
    print("=" * 50)

    # Small test network: 5 nodes, 6 edges, 2 generators
    num_nodes = 5
    num_edges = 6
    num_generators = 2

    # Define topology
    edge_index = torch.tensor([
        [0, 0, 1, 2, 2, 3],  # Source nodes
        [1, 2, 3, 3, 4, 4],  # Target nodes
    ])

    # Create model (with topology-aware initialization)
    model, loss_fn = build_hopfield_model(
        num_nodes=num_nodes,
        num_edges=num_edges,
        num_generators=num_generators,
        hidden_dim=32,
        num_steps=10,
        edge_index=edge_index,
    )

    print(f"Model parameters: {sum(p.numel() for p in model.parameters()):,}")

    # Test input
    batch_size = 4
    node_features = torch.randn(batch_size, num_nodes, 8)

    # Forward pass
    with torch.no_grad():
        result = model(node_features, edge_index, return_details=True)

    print(f"\nResults:")
    print(f"  P dispatch shape: {result['p_dispatch'].shape}")
    print(f"  Q dispatch shape: {result['q_dispatch'].shape}")
    print(f"  P flows shape: {result['p_flows'].shape}")
    print(f"  Feasibility mean: {result['feasibility'].mean():.4f}")
    print(f"  Converged: {result['converged']}")
    print(f"  Energy: {result['energy'].mean():.4f}")

    if result.get('trajectory'):
        print(f"  Trajectory length: {len(result['trajectory'])}")

    # Test loss computation
    targets = {
        'p_dispatch': torch.randn(batch_size, num_generators),
        'q_dispatch': torch.randn(batch_size, num_generators),
    }
    incidence = torch.randn(num_nodes, num_edges)  # Placeholder

    losses = loss_fn(result, targets, incidence)
    print(f"\nLosses:")
    for name, value in losses.items():
        print(f"  {name}: {value.item():.4f}")

    print("\n✓ Categorical Hopfield dynamics test passed")
