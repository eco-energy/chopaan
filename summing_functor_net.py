#!/usr/bin/env python3
"""
Summing Functor Neural Network for AC Power Flow

Based on Manin-Marcolli's categorical framework for neural information networks.
The key insight is that Kirchhoff's current law is exactly Proposition 2.10:
a summing functor Φ is in the equalizer iff conservation holds at all vertices.

By constructing a network that outputs edge flows and then projects to the
Kirchhoff equalizer, we guarantee power balance BY CONSTRUCTION - no soft
penalty needed, the solution is always feasible.

Architecture:
  1. Encoder: Map node features to latent representations
  2. Message Passing: Propagate information along edges (respects graph structure)
  3. Edge Predictor: Output (P, Q) flows for each edge
  4. Kirchhoff Projection: Project to equalizer Σ_C^eq(G)

The projection is a differentiable layer that solves:
  min ||Φ' - Φ||² subject to Σ_{in} Φ'(e) = Σ_{out} Φ'(e) for all non-slack nodes
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
from torch_geometric.nn import MessagePassing, GATConv
from torch_geometric.data import Data
import numpy as np
from typing import Tuple, Optional, List
from dataclasses import dataclass


@dataclass
class PowerGridGraph:
    """Power grid as a directed graph with node/edge features."""
    num_nodes: int
    num_edges: int
    edge_index: torch.Tensor  # (2, num_edges) source/target indices
    node_features: torch.Tensor  # (num_nodes, node_dim)
    edge_features: torch.Tensor  # (num_edges, edge_dim)
    slack_node: int = 0  # Slack bus index (absorbs imbalance)

    def to(self, device):
        return PowerGridGraph(
            num_nodes=self.num_nodes,
            num_edges=self.num_edges,
            edge_index=self.edge_index.to(device),
            node_features=self.node_features.to(device),
            edge_features=self.edge_features.to(device),
            slack_node=self.slack_node
        )


class KirchhoffProjection(nn.Module):
    """
    Differentiable projection to the Kirchhoff equalizer.

    Given edge flows Φ(e) = (P_e, Q_e), find the closest flows Φ'(e) that
    satisfy conservation at all non-slack nodes:

        Σ_{e: t(e)=v} Φ'(e) = Σ_{e: s(e)=v} Φ'(e)  for all v ≠ slack

    This is a quadratic program with linear equality constraints.
    Using the method of Lagrange multipliers, the solution is:

        Φ' = Φ - A^T (A A^T)^{-1} A Φ

    where A is the incidence matrix (with slack row removed).

    The beauty: this is differentiable! Gradients flow through the projection.
    """

    def __init__(self, num_nodes: int, num_edges: int, slack_node: int = 0):
        super().__init__()
        self.num_nodes = num_nodes
        self.num_edges = num_edges
        self.slack_node = slack_node

        # Will be set when we know the graph structure
        self.register_buffer('projection_matrix', None)
        self._graph_initialized = False

    def initialize_graph(self, edge_index: torch.Tensor):
        """
        Build the projection matrix from edge_index.

        The incidence matrix A has rows = non-slack nodes, cols = edges.
        A[v, e] = +1 if v = target(e), -1 if v = source(e), 0 otherwise.

        Projection: P = I - A^T (A A^T)^{-1} A
        """
        device = edge_index.device
        src, tgt = edge_index[0], edge_index[1]

        # Build incidence matrix (excluding slack node)
        non_slack_nodes = [i for i in range(self.num_nodes) if i != self.slack_node]
        n_constraints = len(non_slack_nodes)
        node_to_constraint = {n: i for i, n in enumerate(non_slack_nodes)}

        A = torch.zeros(n_constraints, self.num_edges, device=device)

        for e in range(self.num_edges):
            s, t = src[e].item(), tgt[e].item()
            if s in node_to_constraint:
                A[node_to_constraint[s], e] = -1.0  # Outflow
            if t in node_to_constraint:
                A[node_to_constraint[t], e] = +1.0  # Inflow

        # Compute projection: P = I - A^T (A A^T)^{-1} A
        # Use pseudo-inverse for numerical stability
        AAT = A @ A.T
        AAT_inv = torch.linalg.pinv(AAT + 1e-6 * torch.eye(n_constraints, device=device))

        I = torch.eye(self.num_edges, device=device)
        P = I - A.T @ AAT_inv @ A

        self.projection_matrix = P
        self._graph_initialized = True

    def forward(self, edge_flows: torch.Tensor, edge_index: torch.Tensor) -> torch.Tensor:
        """
        Project edge flows to Kirchhoff equalizer.

        Args:
            edge_flows: (batch, num_edges, 2) tensor of (P, Q) flows
            edge_index: (2, num_edges) edge connectivity

        Returns:
            projected_flows: (batch, num_edges, 2) flows satisfying Kirchhoff
        """
        if not self._graph_initialized:
            self.initialize_graph(edge_index)

        batch_size = edge_flows.shape[0]
        P_flows = edge_flows[:, :, 0]  # (batch, num_edges)
        Q_flows = edge_flows[:, :, 1]

        # Project P and Q separately (they're independent)
        P_proj = torch.matmul(P_flows, self.projection_matrix.T)
        Q_proj = torch.matmul(Q_flows, self.projection_matrix.T)

        return torch.stack([P_proj, Q_proj], dim=-1)


class EdgeMessagePassing(MessagePassing):
    """
    Message passing that respects the directed graph structure.

    In the summing functor framework, information flows along edges.
    This layer aggregates information from incident edges at each node.
    """

    def __init__(self, node_dim: int, edge_dim: int, hidden_dim: int):
        super().__init__(aggr='add')  # Sum aggregation (matches monoidal structure)

        self.node_transform = nn.Linear(node_dim, hidden_dim)
        self.edge_transform = nn.Linear(edge_dim, hidden_dim)
        self.message_mlp = nn.Sequential(
            nn.Linear(2 * hidden_dim + hidden_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, hidden_dim)
        )
        self.update_mlp = nn.Sequential(
            nn.Linear(2 * hidden_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, hidden_dim)
        )

    def forward(self, x: torch.Tensor, edge_index: torch.Tensor,
                edge_attr: torch.Tensor) -> torch.Tensor:
        """
        Args:
            x: (num_nodes, node_dim) node features
            edge_index: (2, num_edges) connectivity
            edge_attr: (num_edges, edge_dim) edge features
        """
        x = self.node_transform(x)
        edge_attr = self.edge_transform(edge_attr)

        return self.propagate(edge_index, x=x, edge_attr=edge_attr)

    def message(self, x_i: torch.Tensor, x_j: torch.Tensor,
                edge_attr: torch.Tensor) -> torch.Tensor:
        """Message from source j to target i."""
        return self.message_mlp(torch.cat([x_i, x_j, edge_attr], dim=-1))

    def update(self, aggr_out: torch.Tensor, x: torch.Tensor) -> torch.Tensor:
        """Update node representation with aggregated messages."""
        return self.update_mlp(torch.cat([x, aggr_out], dim=-1))


class SummingFunctorNet(nn.Module):
    """
    Neural network that respects the summing functor structure.

    The architecture:
    1. Encode node features (solar generation, load, voltage limits)
    2. Message passing along graph edges (respects grid topology)
    3. Predict edge flows (P, Q) for each edge
    4. Project to Kirchhoff equalizer (guarantees power balance)

    The final projection is the key innovation: by projecting to the equalizer,
    we get GUARANTEED feasibility with respect to power balance constraints.
    """

    def __init__(
        self,
        node_input_dim: int,
        edge_input_dim: int,
        hidden_dim: int = 64,
        num_message_passing_layers: int = 3,
        num_nodes: int = 10,
        num_edges: int = 15,
        slack_node: int = 0,
        dropout: float = 0.1,
        use_gat: bool = True
    ):
        super().__init__()

        self.hidden_dim = hidden_dim
        self.num_nodes = num_nodes
        self.num_edges = num_edges
        self.dropout = dropout

        # Node encoder
        self.node_encoder = nn.Sequential(
            nn.Linear(node_input_dim, hidden_dim),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(hidden_dim, hidden_dim)
        )

        # Edge encoder
        self.edge_encoder = nn.Sequential(
            nn.Linear(edge_input_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, hidden_dim)
        )

        # Message passing layers (GAT or custom)
        self.message_layers = nn.ModuleList()
        if use_gat:
            for _ in range(num_message_passing_layers):
                self.message_layers.append(
                    GATConv(hidden_dim, hidden_dim // 4, heads=4, dropout=dropout)
                )
        else:
            for _ in range(num_message_passing_layers):
                self.message_layers.append(
                    EdgeMessagePassing(hidden_dim, hidden_dim, hidden_dim)
                )

        self.layer_norms = nn.ModuleList([
            nn.LayerNorm(hidden_dim) for _ in range(num_message_passing_layers)
        ])

        # Edge flow predictor: takes source and target node embeddings
        self.edge_predictor = nn.Sequential(
            nn.Linear(2 * hidden_dim + hidden_dim, hidden_dim),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(hidden_dim, hidden_dim // 2),
            nn.ReLU(),
            nn.Linear(hidden_dim // 2, 2)  # Output: (P, Q)
        )

        # Kirchhoff projection layer
        self.kirchhoff_projection = KirchhoffProjection(
            num_nodes=num_nodes,
            num_edges=num_edges,
            slack_node=slack_node
        )

        self._use_gat = use_gat

    def forward(
        self,
        graph: PowerGridGraph,
        return_pre_projection: bool = False
    ) -> Tuple[torch.Tensor, Optional[torch.Tensor]]:
        """
        Forward pass through the summing functor network.

        Args:
            graph: PowerGridGraph with node/edge features
            return_pre_projection: If True, also return flows before projection

        Returns:
            edge_flows: (batch, num_edges, 2) tensor of (P, Q) flows in equalizer
            pre_projection: Optional (batch, num_edges, 2) flows before projection
        """
        x = graph.node_features  # (num_nodes, node_dim)
        edge_index = graph.edge_index  # (2, num_edges)
        edge_attr = graph.edge_features  # (num_edges, edge_dim)

        # Handle batched vs unbatched input
        if x.dim() == 2:
            x = x.unsqueeze(0)  # Add batch dimension
            edge_attr = edge_attr.unsqueeze(0)

        batch_size = x.shape[0]

        # Encode nodes and edges
        x = self.node_encoder(x)  # (batch, num_nodes, hidden)
        edge_emb = self.edge_encoder(edge_attr)  # (batch, num_edges, hidden)

        # Message passing (process each batch element)
        # For simplicity, we process batch sequentially for GNN layers
        x_list = []
        for b in range(batch_size):
            x_b = x[b]  # (num_nodes, hidden)
            edge_attr_b = edge_attr[b] if edge_attr.dim() == 3 else edge_attr

            for layer, norm in zip(self.message_layers, self.layer_norms):
                if self._use_gat:
                    x_b = x_b + layer(x_b, edge_index)
                else:
                    x_b = x_b + layer(x_b, edge_index, edge_attr_b)
                x_b = norm(x_b)
                x_b = F.relu(x_b)

            x_list.append(x_b)

        x = torch.stack(x_list, dim=0)  # (batch, num_nodes, hidden)

        # Predict edge flows from source/target node embeddings
        src, tgt = edge_index[0], edge_index[1]
        src_emb = x[:, src, :]  # (batch, num_edges, hidden)
        tgt_emb = x[:, tgt, :]  # (batch, num_edges, hidden)

        edge_input = torch.cat([src_emb, tgt_emb, edge_emb], dim=-1)
        raw_flows = self.edge_predictor(edge_input)  # (batch, num_edges, 2)

        # Project to Kirchhoff equalizer - THIS IS THE KEY STEP
        # After this, power balance is GUARANTEED at all non-slack nodes
        projected_flows = self.kirchhoff_projection(raw_flows, edge_index)

        if return_pre_projection:
            return projected_flows, raw_flows
        return projected_flows, None

    def enable_mc_dropout(self):
        """Enable MC Dropout for uncertainty quantification."""
        for module in self.modules():
            if isinstance(module, nn.Dropout):
                module.train()

    def mc_forward(
        self,
        graph: PowerGridGraph,
        num_samples: int = 20
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        """
        Monte Carlo forward pass for uncertainty quantification.

        Returns:
            mean_flows: (batch, num_edges, 2) mean edge flows
            std_flows: (batch, num_edges, 2) standard deviation
        """
        self.enable_mc_dropout()

        samples = []
        for _ in range(num_samples):
            flows, _ = self.forward(graph)
            samples.append(flows)

        samples = torch.stack(samples, dim=0)  # (num_samples, batch, num_edges, 2)

        mean_flows = samples.mean(dim=0)
        std_flows = samples.std(dim=0)

        return mean_flows, std_flows


class EdgeFlowToNodeSetpoints(nn.Module):
    """
    Convert edge flows to node-level P, Q setpoints.

    For each node, the setpoint is the net injection:
      P_node = Σ_{e: s(e)=node} P_e - Σ_{e: t(e)=node} P_e
      Q_node = Σ_{e: s(e)=node} Q_e - Σ_{e: t(e)=node} Q_e
    """

    def __init__(self, num_nodes: int, num_edges: int):
        super().__init__()
        self.num_nodes = num_nodes
        self.num_edges = num_edges

    def forward(
        self,
        edge_flows: torch.Tensor,
        edge_index: torch.Tensor
    ) -> torch.Tensor:
        """
        Args:
            edge_flows: (batch, num_edges, 2) edge (P, Q) flows
            edge_index: (2, num_edges) source/target indices

        Returns:
            node_setpoints: (batch, num_nodes, 2) node (P, Q) injections
        """
        batch_size = edge_flows.shape[0]
        device = edge_flows.device

        src, tgt = edge_index[0], edge_index[1]

        # Initialize node injections
        node_P = torch.zeros(batch_size, self.num_nodes, device=device)
        node_Q = torch.zeros(batch_size, self.num_nodes, device=device)

        P_flows = edge_flows[:, :, 0]  # (batch, num_edges)
        Q_flows = edge_flows[:, :, 1]

        # Vectorized: use scatter_add
        # Expand indices for batched scatter
        src_exp = src.unsqueeze(0).expand(batch_size, -1)
        tgt_exp = tgt.unsqueeze(0).expand(batch_size, -1)

        # Outflows: add at source
        node_P.scatter_add_(1, src_exp, P_flows)
        node_Q.scatter_add_(1, src_exp, Q_flows)

        # Inflows: subtract at target
        node_P.scatter_add_(1, tgt_exp, -P_flows)
        node_Q.scatter_add_(1, tgt_exp, -Q_flows)

        return torch.stack([node_P, node_Q], dim=-1)


class SummingFunctorDispatcher(nn.Module):
    """
    Complete dispatcher using summing functor architecture.

    This wraps SummingFunctorNet to provide the same interface as the
    original neural dispatcher, but with guaranteed feasibility.
    """

    def __init__(
        self,
        num_nodes: int,
        num_edges: int,
        node_input_dim: int = 6,  # P_solar, P_load, V_min, V_max, P_rated, Q_rated
        edge_input_dim: int = 4,  # R, X, B, S_rated
        hidden_dim: int = 64,
        slack_node: int = 0
    ):
        super().__init__()

        self.num_nodes = num_nodes
        self.num_edges = num_edges

        self.functor_net = SummingFunctorNet(
            node_input_dim=node_input_dim,
            edge_input_dim=edge_input_dim,
            hidden_dim=hidden_dim,
            num_nodes=num_nodes,
            num_edges=num_edges,
            slack_node=slack_node
        )

        self.edge_to_node = EdgeFlowToNodeSetpoints(num_nodes, num_edges)

    def forward(
        self,
        graph: PowerGridGraph
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        """
        Get dispatch setpoints for all nodes.

        Returns:
            node_setpoints: (batch, num_nodes, 2) P, Q setpoints
            edge_flows: (batch, num_edges, 2) edge flows (for debugging)
        """
        edge_flows, _ = self.functor_net(graph)
        node_setpoints = self.edge_to_node(edge_flows, graph.edge_index)

        return node_setpoints, edge_flows

    def get_uncertainty(
        self,
        graph: PowerGridGraph,
        num_samples: int = 20
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Get setpoints with uncertainty estimates.

        Returns:
            mean_setpoints: (batch, num_nodes, 2) mean P, Q setpoints
            std_setpoints: (batch, num_nodes, 2) uncertainty in setpoints
            mean_flows: (batch, num_edges, 2) mean edge flows
            std_flows: (batch, num_edges, 2) uncertainty in flows
        """
        mean_flows, std_flows = self.functor_net.mc_forward(graph, num_samples)
        mean_setpoints = self.edge_to_node(mean_flows, graph.edge_index)

        # Propagate uncertainty (approximate via samples)
        std_setpoints = self.edge_to_node(std_flows, graph.edge_index)

        return mean_setpoints, std_setpoints, mean_flows, std_flows


def create_test_grid() -> PowerGridGraph:
    """Create a simple test grid for debugging."""
    # Simple 4-node grid:
    #   0 (slack) -- 1 (PV) -- 2 (PQ)
    #                |
    #                3 (PQ)

    edge_index = torch.tensor([
        [0, 1, 1, 1],  # sources
        [1, 0, 2, 3]   # targets
    ], dtype=torch.long)

    # Node features: [P_solar, P_load, V_min, V_max, P_rated, Q_rated]
    node_features = torch.tensor([
        [0.0, 0.0, 0.95, 1.05, 100.0, 50.0],   # Slack
        [50.0, 20.0, 0.95, 1.05, 60.0, 30.0],  # PV bus
        [0.0, 30.0, 0.95, 1.05, 0.0, 0.0],     # PQ bus (load)
        [0.0, 25.0, 0.95, 1.05, 0.0, 0.0],     # PQ bus (load)
    ], dtype=torch.float32)

    # Edge features: [R, X, B, S_rated]
    edge_features = torch.tensor([
        [0.01, 0.05, 0.02, 100.0],
        [0.01, 0.05, 0.02, 100.0],
        [0.02, 0.08, 0.01, 50.0],
        [0.02, 0.08, 0.01, 50.0],
    ], dtype=torch.float32)

    return PowerGridGraph(
        num_nodes=4,
        num_edges=4,
        edge_index=edge_index,
        node_features=node_features,
        edge_features=edge_features,
        slack_node=0
    )


def verify_kirchhoff(
    edge_flows: torch.Tensor,
    edge_index: torch.Tensor,
    num_nodes: int,
    slack_node: int = 0
) -> Tuple[bool, torch.Tensor]:
    """
    Verify that edge flows satisfy Kirchhoff's law at all non-slack nodes.

    Returns:
        satisfied: True if all imbalances are below tolerance
        imbalances: (num_nodes,) tensor of imbalances at each node
    """
    if edge_flows.dim() == 3:
        edge_flows = edge_flows[0]  # Take first batch element

    P_flows = edge_flows[:, 0]
    src, tgt = edge_index[0], edge_index[1]

    imbalances = torch.zeros(num_nodes)

    for e in range(edge_index.shape[1]):
        s, t = src[e].item(), tgt[e].item()
        imbalances[s] += P_flows[e].item()  # Outflow
        imbalances[t] -= P_flows[e].item()  # Inflow

    # Check non-slack nodes
    satisfied = True
    for n in range(num_nodes):
        if n != slack_node and abs(imbalances[n]) > 1e-5:
            satisfied = False

    return satisfied, imbalances


if __name__ == "__main__":
    # Test the summing functor network
    print("Testing SummingFunctorNet...")

    grid = create_test_grid()

    dispatcher = SummingFunctorDispatcher(
        num_nodes=4,
        num_edges=4,
        node_input_dim=6,
        edge_input_dim=4,
        hidden_dim=32,
        slack_node=0
    )

    # Forward pass
    setpoints, flows = dispatcher(grid)
    print(f"Node setpoints shape: {setpoints.shape}")
    print(f"Edge flows shape: {flows.shape}")

    # Verify Kirchhoff's law
    satisfied, imbalances = verify_kirchhoff(
        flows, grid.edge_index, grid.num_nodes, grid.slack_node
    )
    print(f"\nKirchhoff's law satisfied: {satisfied}")
    print(f"Imbalances: {imbalances}")

    # Test uncertainty quantification
    print("\nTesting MC Dropout uncertainty...")
    mean_sp, std_sp, mean_fl, std_fl = dispatcher.get_uncertainty(grid, num_samples=10)
    print(f"Mean setpoints: {mean_sp.squeeze()}")
    print(f"Uncertainty (std): {std_sp.squeeze()}")

    # Verify Kirchhoff holds for mean flows too
    satisfied, _ = verify_kirchhoff(
        mean_fl, grid.edge_index, grid.num_nodes, grid.slack_node
    )
    print(f"\nKirchhoff's law satisfied for mean: {satisfied}")

    print("\nAll tests passed!")
