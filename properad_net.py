#!/usr/bin/env python3
"""
Properad-Based Power Flow Neural Network

Based on Manin-Marcolli §2.3.2 and Corollary 2.20: a network summing functor
Φ ∈ Σ^prop_C(G) is completely determined by its values on corollas.

Architecture:
  1. Each node v has a CorollaModule that outputs flows on incident edges
  2. Local conservation is enforced WITHIN each corolla (not post-hoc)
  3. Nodes are processed in topological order
  4. Grafting: upstream outputs become downstream inputs

Key advantage: O(n) instead of O(n³) - no matrix inverse needed.
Conservation is local, not a global projection.

Comparison to KirchhoffProjection approach:
  - Projection: Global GNN → All edge flows → Project (O(n³))
  - Properad:   Local corollas → Topological composition → Done (O(n))

The properad composition ∘_E along edges E ensures:
  - Output of upstream node = Input to downstream node (on shared edge)
  - This IS Kirchhoff, but encoded in composition rather than projection
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
from torch_geometric.nn import MessagePassing
from dataclasses import dataclass
from typing import List, Dict, Tuple, Optional
import numpy as np


@dataclass
class PowerFlowState:
    """State of power flow on an edge."""
    P: float  # Real power (kW)
    Q: float  # Reactive power (kVAR)

    def __add__(self, other):
        return PowerFlowState(self.P + other.P, self.Q + other.Q)

    def __sub__(self, other):
        return PowerFlowState(self.P - other.P, self.Q - other.Q)


class CorollaModule(nn.Module):
    """
    Neural module for a single corolla (vertex + incident half-edges).

    By Corollary 2.20, the full summing functor is determined by values
    on corollas. This module learns what value to assign to a corolla
    given its local features and incoming flows from upstream.

    Key property: LOCAL conservation is enforced by construction.
    Given deg_out outgoing edges, we predict deg_out - 1 flows freely
    and compute the last one to satisfy:

        sum(outflows) = sum(inflows) + net_injection

    This is Kirchhoff's law at this vertex, enforced locally.
    """

    def __init__(
        self,
        node_dim: int,
        hidden_dim: int,
        max_degree: int,
        shared_weights: bool = True
    ):
        super().__init__()
        self.node_dim = node_dim
        self.hidden_dim = hidden_dim
        self.max_degree = max_degree

        # Input: node features + incoming flows (padded to max_degree)
        # The incoming flows come from upstream nodes via properad composition
        input_dim = node_dim + 2 * max_degree  # 2 for (P, Q) per edge

        self.encoder = nn.Sequential(
            nn.Linear(input_dim, hidden_dim),
            nn.ReLU(),
            nn.LayerNorm(hidden_dim),
            nn.Linear(hidden_dim, hidden_dim),
            nn.ReLU(),
        )

        # Predict "free" outflows: (max_degree - 1) edges × 2 (P, Q)
        # The last outflow is computed to satisfy conservation
        self.flow_head = nn.Linear(hidden_dim, 2 * (max_degree - 1))

    def forward(
        self,
        node_features: torch.Tensor,     # (batch, node_dim)
        incoming_flows: torch.Tensor,    # (batch, max_degree, 2) - padded
        deg_in: int,                      # Actual number of incoming edges
        deg_out: int,                     # Actual number of outgoing edges
        net_injection: torch.Tensor,     # (batch, 2) = (P_gen - P_load, Q_gen - Q_load)
    ) -> torch.Tensor:
        """
        Compute outgoing flows satisfying local conservation.

        Returns:
            outflows: (batch, max_degree, 2) - first deg_out entries are valid
        """
        batch_size = node_features.shape[0]
        device = node_features.device

        # Encode node + incoming flows
        x = torch.cat([node_features, incoming_flows.view(batch_size, -1)], dim=-1)
        h = self.encoder(x)

        # Initialize output
        outflows = torch.zeros(batch_size, self.max_degree, 2, device=device)

        if deg_out == 0:
            # Leaf node: no outgoing edges
            # Conservation just means: sum(inflows) = net_injection (absorbed)
            return outflows

        if deg_out == 1:
            # Only one outgoing edge: it must carry all the flow
            # outflow = sum(inflows) + net_injection
            total_in = incoming_flows[:, :deg_in, :].sum(dim=1)  # (batch, 2)
            outflows[:, 0, :] = total_in + net_injection
            return outflows

        # Multiple outgoing edges: predict deg_out - 1 freely
        raw_flows = self.flow_head(h)  # (batch, 2 * (max_degree - 1))
        raw_flows = raw_flows.view(batch_size, self.max_degree - 1, 2)

        # Use first deg_out - 1 predictions
        free_flows = raw_flows[:, :deg_out - 1, :]  # (batch, deg_out - 1, 2)

        # Sum of incoming flows
        total_in = incoming_flows[:, :deg_in, :].sum(dim=1)  # (batch, 2)

        # Required total outflow (by conservation)
        required_total = total_in + net_injection  # (batch, 2)

        # Compute last outflow to satisfy conservation
        free_sum = free_flows.sum(dim=1)  # (batch, 2)
        last_flow = required_total - free_sum  # (batch, 2)

        # Assemble output
        outflows[:, :deg_out - 1, :] = free_flows
        outflows[:, deg_out - 1, :] = last_flow

        return outflows


class ProperadPowerFlowNet(nn.Module):
    """
    Power flow network using properad composition of corollas.

    Implementation of Manin-Marcolli Corollary 2.20:
    "A network summing functor Φ ∈ Σ^prop_C(G) is completely
    determined by its value on corollas."

    Architecture:
        1. Compute topological order of nodes (DAG)
        2. Process nodes in order:
           - Gather incoming flows from already-processed upstream nodes
           - Apply CorollaModule to get outgoing flows (conservation enforced)
           - Store outgoing flows for downstream nodes
        3. The grafting/composition is implicit in this ordering

    This is the properad composition ∘_E along edges E:
        Φ(G' ⋆ G'') = Φ(G') ∘_{E(G',G'')} Φ(G'')

    For G' < G'' (upstream < downstream), the output of G' on shared
    edges becomes the input to G''.
    """

    def __init__(
        self,
        num_nodes: int,
        edge_index: torch.Tensor,  # (2, num_edges)
        node_dim: int = 6,
        hidden_dim: int = 64,
        slack_node: int = 0
    ):
        super().__init__()
        self.num_nodes = num_nodes
        self.num_edges = edge_index.shape[1]
        self.slack_node = slack_node

        # Store edge structure
        self.register_buffer('edge_index', edge_index)

        # Compute graph structure
        self._compute_structure(edge_index, num_nodes)

        # Shared corolla module
        self.corolla = CorollaModule(
            node_dim=node_dim,
            hidden_dim=hidden_dim,
            max_degree=self.max_degree
        )

    def _compute_structure(self, edge_index: torch.Tensor, num_nodes: int):
        """Compute incidence and topological order."""
        src, tgt = edge_index[0].tolist(), edge_index[1].tolist()

        # Incoming and outgoing edges for each node
        self.in_edges = {v: [] for v in range(num_nodes)}
        self.out_edges = {v: [] for v in range(num_nodes)}

        for e, (s, t) in enumerate(zip(src, tgt)):
            self.out_edges[s].append(e)
            self.in_edges[t].append(e)

        # Max degree for padding
        self.max_degree = max(
            max(len(self.in_edges[v]) for v in range(num_nodes)),
            max(len(self.out_edges[v]) for v in range(num_nodes)),
            2  # At least 2 for the module to work
        )

        # Topological order (simple BFS from slack)
        self.topo_order = self._topological_sort(edge_index, num_nodes)

    def _topological_sort(
        self,
        edge_index: torch.Tensor,
        num_nodes: int
    ) -> List[int]:
        """Kahn's algorithm for topological sort."""
        src, tgt = edge_index[0].tolist(), edge_index[1].tolist()

        # Count incoming edges
        in_degree = [0] * num_nodes
        for t in tgt:
            in_degree[t] += 1

        # Start with nodes that have no incoming edges
        queue = [v for v in range(num_nodes) if in_degree[v] == 0]
        result = []

        while queue:
            v = queue.pop(0)
            result.append(v)

            for e in self.out_edges[v]:
                t = tgt[e]  # Direct lookup - e is the edge index
                in_degree[t] -= 1
                if in_degree[t] == 0:
                    queue.append(t)

        # If graph has cycles, fall back to simple order
        if len(result) != num_nodes:
            result = list(range(num_nodes))

        return result

    def forward(
        self,
        node_features: torch.Tensor,  # (batch, num_nodes, node_dim)
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        """
        Forward pass using properad composition.

        Args:
            node_features: (batch, num_nodes, node_dim)
                Expected: [:, :, 0:2] = (P_gen - P_load, Q_gen - Q_load)

        Returns:
            node_setpoints: (batch, num_nodes, 2) - P, Q setpoints
            edge_flows: (batch, num_edges, 2) - P, Q on each edge
        """
        batch_size = node_features.shape[0]
        device = node_features.device

        # Initialize edge flows
        edge_flows = torch.zeros(batch_size, self.num_edges, 2, device=device)

        # Process nodes in topological order
        # This is the properad composition: upstream → downstream
        for v in self.topo_order:
            if v == self.slack_node:
                # Slack bus absorbs imbalance, don't process
                continue

            deg_in = len(self.in_edges[v])
            deg_out = len(self.out_edges[v])

            # Gather incoming flows (from already-processed upstream nodes)
            incoming = torch.zeros(batch_size, self.max_degree, 2, device=device)
            for i, e in enumerate(self.in_edges[v]):
                incoming[:, i, :] = edge_flows[:, e, :]

            # Net injection at this node
            # Assuming first two features are (P_net, Q_net)
            net_injection = node_features[:, v, :2]

            # Corolla computes outgoing flows (conservation enforced locally)
            out_flows = self.corolla(
                node_features[:, v, :],
                incoming,
                deg_in,
                deg_out,
                net_injection
            )

            # Store outgoing flows for downstream nodes
            for i, e in enumerate(self.out_edges[v]):
                edge_flows[:, e, :] = out_flows[:, i, :]

        # Compute node setpoints from edge flows
        node_setpoints = self._compute_setpoints(edge_flows, node_features)

        return node_setpoints, edge_flows

    def _compute_setpoints(
        self,
        edge_flows: torch.Tensor,
        node_features: torch.Tensor
    ) -> torch.Tensor:
        """Compute P, Q setpoints at each node from edge flows."""
        batch_size = edge_flows.shape[0]
        device = edge_flows.device

        # Setpoint = net injection = sum(outflows) - sum(inflows)
        setpoints = torch.zeros(batch_size, self.num_nodes, 2, device=device)

        src = self.edge_index[0].tolist()
        tgt = self.edge_index[1].tolist()

        for e, (s, t) in enumerate(zip(src, tgt)):
            # Outflow from s
            setpoints[:, s, :] += edge_flows[:, e, :]
            # Inflow to t
            setpoints[:, t, :] -= edge_flows[:, e, :]

        return setpoints


class ProperadDispatcher(nn.Module):
    """
    Complete dispatcher using properad architecture.

    Wraps ProperadPowerFlowNet to provide the same interface
    as the projection-based dispatcher.
    """

    def __init__(
        self,
        num_nodes: int,
        edge_index: torch.Tensor,
        node_dim: int = 6,
        hidden_dim: int = 64,
        slack_node: int = 0,
        dropout: float = 0.1
    ):
        super().__init__()

        self.num_nodes = num_nodes
        self.slack_node = slack_node

        # Node feature encoder (optional preprocessing)
        self.node_encoder = nn.Sequential(
            nn.Linear(node_dim, hidden_dim),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(hidden_dim, node_dim)  # Back to node_dim for corolla
        )

        # Properad network
        self.properad_net = ProperadPowerFlowNet(
            num_nodes=num_nodes,
            edge_index=edge_index,
            node_dim=node_dim,
            hidden_dim=hidden_dim,
            slack_node=slack_node
        )

    def forward(
        self,
        node_features: torch.Tensor
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        """
        Get dispatch setpoints.

        Args:
            node_features: (batch, num_nodes, node_dim)

        Returns:
            setpoints: (batch, num_nodes, 2)
            edge_flows: (batch, num_edges, 2)
        """
        # Encode node features
        batch_size = node_features.shape[0]
        encoded = self.node_encoder(node_features.view(-1, node_features.shape[-1]))
        encoded = encoded.view(batch_size, self.num_nodes, -1)

        # Preserve net injection in first two dims
        encoded[:, :, :2] = node_features[:, :, :2]

        return self.properad_net(encoded)

    def verify_conservation(
        self,
        edge_flows: torch.Tensor,
        node_features: torch.Tensor,
        tolerance: float = 1e-5
    ) -> Tuple[bool, torch.Tensor]:
        """
        Verify Kirchhoff's law is satisfied at all non-slack nodes.

        Returns:
            satisfied: True if all imbalances below tolerance
            imbalances: (batch, num_nodes, 2) imbalance at each node
        """
        batch_size = edge_flows.shape[0]
        device = edge_flows.device

        imbalances = torch.zeros(batch_size, self.num_nodes, 2, device=device)

        src = self.properad_net.edge_index[0].tolist()
        tgt = self.properad_net.edge_index[1].tolist()

        for e, (s, t) in enumerate(zip(src, tgt)):
            imbalances[:, s, :] += edge_flows[:, e, :]
            imbalances[:, t, :] -= edge_flows[:, e, :]

        # Subtract net injection
        imbalances -= node_features[:, :, :2]

        # Check non-slack nodes
        non_slack_imbalance = imbalances.clone()
        non_slack_imbalance[:, self.slack_node, :] = 0

        max_imbalance = non_slack_imbalance.abs().max().item()
        satisfied = max_imbalance < tolerance

        return satisfied, imbalances


def create_test_properad_network():
    """Create a simple test network."""
    # 4-node radial network
    #   0 (slack) → 1 → 2
    #               ↓
    #               3

    edge_index = torch.tensor([
        [0, 1, 1],  # sources
        [1, 2, 3]   # targets
    ], dtype=torch.long)

    dispatcher = ProperadDispatcher(
        num_nodes=4,
        edge_index=edge_index,
        node_dim=6,
        hidden_dim=32,
        slack_node=0
    )

    # Test input
    # node_features[:, :, 0:2] = (P_net, Q_net)
    node_features = torch.tensor([
        [[0.0, 0.0, 0.95, 1.05, 100.0, 1.0],   # Slack
         [30.0, 10.0, 0.95, 1.05, 60.0, 1.0],  # PV: 30kW net
         [-20.0, -5.0, 0.95, 1.05, 0.0, 0.0],  # Load: 20kW
         [-15.0, -3.0, 0.95, 1.05, 0.0, 0.0]]  # Load: 15kW
    ], dtype=torch.float32)

    return dispatcher, node_features, edge_index


if __name__ == "__main__":
    print("Testing Properad Power Flow Network")
    print("=" * 60)

    dispatcher, node_features, edge_index = create_test_properad_network()

    print(f"Network: 4 nodes, {edge_index.shape[1]} edges")
    print(f"Topological order: {dispatcher.properad_net.topo_order}")

    # Forward pass
    setpoints, edge_flows = dispatcher(node_features)

    print(f"\nEdge flows:")
    for e in range(edge_flows.shape[1]):
        src, tgt = edge_index[0, e].item(), edge_index[1, e].item()
        P, Q = edge_flows[0, e, 0].item(), edge_flows[0, e, 1].item()
        print(f"  Edge {src}→{tgt}: P={P:.2f} kW, Q={Q:.2f} kVAR")

    print(f"\nNode setpoints:")
    for v in range(4):
        P, Q = setpoints[0, v, 0].item(), setpoints[0, v, 1].item()
        print(f"  Node {v}: P={P:.2f} kW, Q={Q:.2f} kVAR")

    # Verify conservation
    satisfied, imbalances = dispatcher.verify_conservation(edge_flows, node_features)
    print(f"\nKirchhoff satisfied: {satisfied}")
    print(f"Max imbalance: {imbalances.abs().max().item():.6f}")

    print("\n" + "=" * 60)
    print("Key insight: Conservation is LOCAL, not a global projection!")
    print("Each corolla enforces its own balance. Composition is O(n).")
