// an init script that returns a Map allows explicit setting of global bindings.
def globals = [:]

// defines a sample LifeCycleHook that prints some output to the Gremlin Server console.
// note that the name of the key in the "global" map is unimportant.
globals << [hook : [
        onStartUp: { ctx ->
            ctx.logger.info("Executed once at startup of Gremlin Server.")
        },
        onShutDown: { ctx ->
            ctx.logger.info("Executed once at shutdown of Gremlin Server.")
        }
] as LifeCycleHook]
 

println("====================== Start making index");

graph.tx().rollback();
mg = graph.openManagement();
if(mg.getGraphIndex("byNodeId") == null) {

    println("--- get node");
    node = mg.getVertexLabel("node");
    if(node == null) {
        node = mg.makeVertexLabel("node");
    }
    
    println("--- get fnode");
    fnode = mg.getVertexLabel("found_node");
    if(fnode == null) {
        fnode = mg.makeVertexLabel("found_node");
    }

    println("--- get kbtz");
    kbtz = mg.getVertexLabel("kbtz");
    if(kbtz == null) {
        kbtz = mg.makeVertexLabel("kbtz");
    }

    println("--- get hh");
    hh = mg.getVertexLabel("hh");
    if(hh == null) {
        hh = mg.makeVertexLabel("hh");
    }

    println("--- get @timestamp");
    ts = mg.getPropertyKey("@timestamp");
    if(ts == null) {
        ts = mg.makePropertyKey("@timestamp").dataType(Long.class).make();
    }

    println("--- get kbtz_id");
    kbtz_id = mg.getPropertyKey("@kbtz_id");
    if(kbtz_id == null) {
        kbtz_id = mg.makePropertyKey("@kbtz_id").dataType(String.class).make();
    }
    ntype = mg.getPropertyKey("@node_type");
    if(ntype == null) {
        ntype = mg.makePropertyKey("@node_type").dataType(String.class).make();
    }

    println("--- get hh_id");
    hh_id = mg.getPropertyKey("@hh_id");
    if(hh_id == null) {
        hh_id = mg.makePropertyKey("@hh_id").dataType(String.class).make();
    }

    println("--- get node_id");
    node_id = mg.getPropertyKey("@node_id");
    if(node_id == null) {
        node_id = mg.makePropertyKey("@node_id").dataType(String.class).make();
    }

    println("--- get mesh_node");
    mesh_node = mg.getPropertyKey("@mesh_node");
    if(mesh_node == null) {
        mesh_node = mg.makePropertyKey("@mesh_node").dataType(String.class).make();
    }

    println("--- get tx_node");
    tx_node = mg.getPropertyKey("@transactor_node");
    if(tx_node == null) {
        tx_node = mg.makePropertyKey("@transactor_node").dataType(String.class).make();
    }

    println("--- get status_node");
    status_node = mg.getPropertyKey("@status_node");
    if(status_node == null) {
        status_node = mg.makePropertyKey("@status_node").dataType(String.class).make();
    }

    println("--- get flow_node");
    flow_node = mg.getPropertyKey("@flow_node");
    if(flow_node == null) {
        flow_node = mg.makePropertyKey("@flow_node").dataType(String.class).make();
    }

    println("--- make index");
    mg.buildIndex("byNodeId", Vertex.class).addKey(node_id).buildCompositeIndex();
    mg.buildIndex("byMeshNode", Vertex.class).addKey(mesh_node).buildCompositeIndex();
    mg.buildIndex("byFlowNode", Vertex.class).addKey(flow_node).buildCompositeIndex();  
    mg.buildIndex("byTxNode", Vertex.class).addKey(tx_node).buildCompositeIndex();
    mg.buildIndex("byStatusNode", Vertex.class).addKey(status_node).buildCompositeIndex();
    mg.buildIndex("byKbtzId", Vertex.class).addKey(kbtz_id).buildCompositeIndex();
    mg.buildIndex("byHHId", Vertex.class).addKey(hh_id).buildCompositeIndex();
    mg.buildIndex("byType", Vertex.class).addKey(ntype).buildCompositeIndex();


    println("--- commit");
    mg.commit();


}


println("====================== Finish making index");


// define the default TraversalSource to bind queries to - this one will be named "g".
globals << [g : graph.traversal()]
