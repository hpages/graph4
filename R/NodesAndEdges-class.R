### =========================================================================
### NodesAndEdges objects
### -------------------------------------------------------------------------
###
### Just a bundle of nodes and edges. With concrete subclasses DGraph
### and DGraphNodes:
###   1. The backbone of a DGraph object are its **edges** i.e. the object
###      is viewed as a vector of edges. More precisely, the length of the
###      object is its number of edges and subsetting the object means
###      selecting a particular subset of edges (without touching the set
###      of nodes).
###   2. The backbone of a DGraphNodes object are its **nodes** i.e. the
###      object is viewed as a vector of nodes. More precisely, the length
###      of the object is its number of nodes and subsetting the object
###      means selecting a particular subset of nodes. This also means
###      that subsetting a DGraphNodes object may drop some of its edges!
###

setClass("NodesAndEdges",
    contains=c("Graph", "Vector"),
    representation(
        #"VIRTUAL",
        nodes="Vector",
        edges="SelfHits"
    ),
    prototype(
        nodes=AnnotatedIDs()
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.validate_NodesAndEdges <- function(x)
{
    ## The 'nodes' slot.
    if (!is(x@nodes, "Vector"))
        return("'nodes' slot must be a Vector derivative")
    if (is(x@nodes, "DGraphNodes"))
        return("'nodes' slot cannot be a DGraphNodes object")

    ## The 'edges' slot.
    if (class(x@edges)[[1L]] != "SelfHits")
        return("'edges' slot must be of class SelfHits")

    ## Compatibility between 'nodes' and 'edges' slots.
    N1 <- NROW(x@nodes)
    N2 <- nnode(x@edges)
    if (N1 != N2)
        return(paste0("'NROW(x@nodes)' [", N1, "] and ",
                      "'nnode(x@edges)' [", N2, "] must be equal"))

    ## The 'elementMetadata' slot.
    ## We inherit this slot from the Vector class but NodesAndEdges
    ## derivatives must never use it (i.e. the slot must be NULL at
    ## all time) because the metadata columns of a DGraph or DGraphNodes
    ## object are the metadata columns of its edges or nodes, respectively.
    if (!is.null(x@elementMetadata))
        return("'elementMetadata' slot must be NULL")

    TRUE
}

setValidity2("NodesAndEdges", .validate_NodesAndEdges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Low-level constructor
###

.new_NodesAndEdges <- function(nodes, edges)
{
    ## 'nodes' and 'edges' are trusted.
    new2("NodesAndEdges", nodes=nodes, edges=edges, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### make_annotated_nodes()
###
### Make sure the user-supplied nodes are represented as a Vector derivative.
### Wrap them in an AnnotatedIDs object if necessary e.g. if they are supplied
### as an ordinary vector.
###

.normarg_nodes <- function(nodes)
{
    if (is(nodes, "Vector"))
        return(nodes)
    if (is.numeric(nodes) && length(nodes) == 1L) {
        if (is.na(nodes))
            stop(wmsg("'nodes' cannot be NA"))
        nodes <- seq_len(nodes)
    }
    AnnotatedIDs(nodes)
}

make_annotated_nodes <- function(nodes, ...)
{
    ans <- .normarg_nodes(nodes)
    mcols <- DataFrame(..., check.names=FALSE)
    if (length(mcols) != 0L) {
        ans_mcols <- mcols(ans)
        if (length(ans_mcols) != 0L)
            mcols <- cbind(ans_mcols, mcols)
        mcols(ans) <- mcols
    }
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("nodes", "NodesAndEdges", function(object) object@nodes)
setMethod("edges", "NodesAndEdges", function(object) object@edges)

### "nodes<-" is an S4 generic defined in the graph package.
setReplaceMethod("nodes", "NodesAndEdges",
    function(object, value)
    {
        N2 <- nnode(object@edges)
        if (is.null(value)) {
            value <- make_annotated_nodes(N2)
        } else {
            value <- make_annotated_nodes(value)
            if (NROW(value) != N2)
                stop(wmsg("this ", class(object), " object ",
                          "expects ", N2, " node",
                          if (N2 == 1L) "", "s"))
        }
        object@nodes <- value
        object
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Core SelfHits API
###
### See R/Graph-class.R for the details.
###

setAs("NodesAndEdges", "SelfHits", function(from) from@edges)

setMethod("t", "NodesAndEdges", function(x) {x@edges <- t(x@edges); x})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("SelfHits", "NodesAndEdges",
    function(from)
    {
        nodes <- make_annotated_nodes(nnode(from))
        edges <- as(from, "SelfHits")
        .new_NodesAndEdges(nodes, edges)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Adjacency matrix
###

### We provide a coercion method from ngCMatrix (defined in the Matrix
### package) to NodesAndEdges. Note that if 'x' is a square ngCMatrix
### object, 'adjacencyMatrix(as(x, "NodesAndEdges"))' is guaranted to
### be identical to 'x' (modulo the dimnames).
.from_ngCMatrix_to_NodesAndEdges <- function(from)
{
    edges <- as(from, "SelfHits")
    nodes <- make_annotated_nodes(nnode(edges))
    if (!is.null(rownames(from))) {
        names(nodes) <- rownames(from)
    } else if (!is.null(colnames(from))) {
        names(nodes) <- colnames(from)
    }
    .new_NodesAndEdges(nodes, edges)
}
setAs("ngCMatrix", "NodesAndEdges", .from_ngCMatrix_to_NodesAndEdges)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Compatibility with graphNEL objects
###

### Uses 'edgeDataDefaults()' and 'edgeData()'.
### NOTE: 'edgeData()' is quite slow AND is broken on graphNEL objects
### with "repeated" edges (i.e. with more than 1 edge between the same
### 2 nodes). So do NOT use!
.edgeData_as_DataFrame_or_NULL <- function(from)
{
    ans_colnames <- names(edgeDataDefaults(from))
    if (length(ans_colnames) == 0L)
        return(NULL)
    S4Vectors:::new_DataFrame(
        lapply(setNames(ans_colnames, ans_colnames),
               function(colname) unname(edgeData(from, attr=colname))))
}

### Same problem as described above.
.nodeData_as_DataFrame_or_NULL <- function(from)
{
    ans_colnames <- names(nodeDataDefaults(from))
    if (length(ans_colnames) == 0L)
        return(NULL)
    S4Vectors:::new_DataFrame(
        lapply(setNames(ans_colnames, ans_colnames),
               function(colname) unname(nodeData(from, attr=colname))))
}

### '.attrData_as_DataFrame_or_NULL(from@edgeData)' is equivalent but **much**
### faster than '.edgeData_as_DataFrame_or_NULL(from)'.
.attrData_as_DataFrame_or_NULL <- function(attrData)
{
    ans_colnames <- names(attrData@defaults)
    if (length(ans_colnames) == 0L)
        return(NULL)
    data <- unname(attrData@data)
    S4Vectors:::new_DataFrame(
        lapply(setNames(ans_colnames, ans_colnames),
               function(colname) lapply(data, `[[`, colname)),
        nrows=length(data))
}

.from_graphNEL_to_NodesAndEdges <- function(from)
{
    if (!isDirected(from))
        stop(wmsg("Coercing an **undirected** ", class(from), " object ",
                  "to DGraph or DGraphNodes is not supported yet. ",
                  "Please set the edgemode of the object to \"directed\" ",
                  "(with 'edgemode(x) <- \"directed\"') before trying ",
                  "to coerce again."))
    m <- edgeMatrix(from)
    ans_from <- m[1L, ]
    ans_to <- m[2L, ]
    edges_mcols <- .attrData_as_DataFrame_or_NULL(from@edgeData)
    nodes_mcols <- .attrData_as_DataFrame_or_NULL(from@nodeData)
    ans_nodes <- make_annotated_nodes(from@nodes, nodes_mcols)
    ans_edges <- SelfHits(ans_from, ans_to, nnode=NROW(ans_nodes), edges_mcols)
    ans <- .new_NodesAndEdges(ans_nodes, ans_edges)

    metadata(ans) <- list(edgeData_defaults=from@edgeData@defaults,
                          nodeData_defaults=from@nodeData@defaults,
                          graphData=from@graphData,
                          renderInfo=from@renderInfo)
    ans
}
setAs("graphNEL", "NodesAndEdges", .from_graphNEL_to_NodesAndEdges)

.make_graphNEL_nodes <- function(from)
{
    ans_nodes <- make_node_labels(from@nodes)
    ## The graphNEL() constructor function will fail (with error "Node names
    ## may not be duplicated") if the character vector supplied to its 'nodes'
    ## argument contains duplicates. By checking this upfront, we can provide
    ## a more useful error message.
    if (anyDuplicated(ans_nodes))
        stop(c(wmsg("graphNEL objects don't support duplicated node names. ",
                    "Please set unique node names on the ",
                    class(from), " object to coerce to graphNEL."),
                    "\n  For example: names(nodes(x)) <- seq_along(nodes(x))"))
    ans_nodes
}

.prepare_attrData <- function(mcols, defaults)
{
    data <- lapply(seq_len(nrow(mcols)),
        function(i) {
            attrs <- as.list(mcols[i, , drop=FALSE])
            lapply(attrs,
                function(attr) {
                    if (!is.list(attr))
                        return(attr)
                    stopifnot(length(attr) == 1L)  # should never happen
                    attr[[1L]]
                })
        })

    if (!is.list(defaults) || is.null(names(defaults))) {
        defaults <- vector("list", ncol(mcols))
    } else {
        defaults <- defaults[colnames(mcols)]
    }
    names(defaults) <- colnames(mcols)

    list(data, defaults)
}

.from_NodesAndEdges_to_graphNEL <- function(from)
{
    ans_nodes <- .make_graphNEL_nodes(from)

    edges <- as(from@edges, "SortedByQuerySelfHits")
    ans_edgeL <- as.list(setNames(as(edges, "IntegerList"), ans_nodes))
    ans_edgeL <- lapply(ans_edgeL, function(edges) list(edges=edges))

    ans <- graphNEL(ans_nodes, ans_edgeL, edgemode="directed")

    ## 'edgeData' slot
    edge_mcols <- mcols(edges)
    if (!is.null(edge_mcols)) {
        edge_mcols0 <- .attrData_as_DataFrame_or_NULL(ans@edgeData)
        col_to_preprend <- setdiff(colnames(edge_mcols0), colnames(edge_mcols))
        edge_mcols <- cbind(edge_mcols0[col_to_preprend], edge_mcols)
        edgeData <- .prepare_attrData(edge_mcols,
                                      metadata(from)$edgeData_defaults)
        ans@edgeData@data[] <- edgeData[[1L]]
        ans@edgeData@defaults <- edgeData[[2L]]
    }

    ## 'nodeData' slot
    node_mcols <- mcols(from@nodes)
    if (!is.null(node_mcols)) {
        nodeData <- .prepare_attrData(node_mcols,
                                      metadata(from)$nodeData_defaults)
        ans@nodeData@data <- setNames(nodeData[[1L]], ans_nodes)
        ans@nodeData@defaults <- nodeData[[2L]]
    }

    ## 'renderInfo' slot
    renderInfo <- metadata(from)$renderInfo
    if (is(renderInfo, "renderInfo"))
        ans@renderInfo <- renderInfo

    ## 'graphData' slot
    graphData <- metadata(from)$graphData
    if (is.list(graphData) && graphData$edgemode == ans@graphData$edgemode)
        ans@graphData <- graphData

    ans
}
setAs("NodesAndEdges", "graphNEL", .from_NodesAndEdges_to_graphNEL)

