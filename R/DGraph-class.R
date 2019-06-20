### =========================================================================
### DGraph objects (directed graphs)
### -------------------------------------------------------------------------


setClass("DGraph",
    contains="SelfHits",
    representation(
        nodes="Vector"
    ),
    prototype(
        nodes=AnnotatedIDs()
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.validate_DGraph <- function(x)
{
    ## 'nodes' slot
    if (!is(x@nodes, "Vector"))
        return("'nodes' slot must be a Vector derivative")
    if (length(x@nodes) != nnode(x))
        return("'nodes' slot must have one ROW per node")

    TRUE
}

setValidity2("DGraph", .validate_DGraph)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### Returns a Vector derivative (wrap ordinary vectors in an AnnotatedIDs
### object).
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

DGraph <- function(from=integer(0), to=integer(0), nodes=0, ...)
{
    nodes <- .normarg_nodes(nodes)
    sh <- SelfHits(from, to, nnode=length(nodes), ...)
    new2("DGraph", sh, nodes=nodes, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

### Clash with generics of same name defined in the graph package.
### FIXME: Define these generics in BiocGenerics.
setGeneric("nodes", function(object, ...) standardGeneric("nodes"))
setGeneric("nodes<-", function(object, value) standardGeneric("nodes<-"))

setMethod("nodes", "DGraph", function(object) object@nodes)

setReplaceMethod("nodes", "DGraph",
    function(object, value)
    {
        object_nnode <- nnode(object)
        if (is.null(value)) {
            value <- AnnotatedIDs(seq_len(object_nnode))
        } else {
            value <- .normarg_nodes(value)
            if (length(value) != object_nnode)
                stop(wmsg("this ", class(object), " object ",
                          "expects ", object_nnode, " node",
                          if (object_nnode == 1L) "", "s"))
        }
        object@nodes <- value
        object
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

### Uses 'graph::edgeDataDefaults()' and 'graph::edgeData()'.
### NOTE: 'graph::edgeData()' is quite slow AND is broken on graphNEL objects
### with "repeated" edges (i.e. with more than 1 edge between the same
### 2 nodes). So do NOT use!
.edgeData_as_DataFrame_or_NULL <- function(from)
{
    ans_colnames <- names(graph::edgeDataDefaults(from))
    if (length(ans_colnames) == 0L)
        return(NULL)
    S4Vectors:::new_DataFrame(
        lapply(setNames(ans_colnames, ans_colnames),
               function(colname) unname(graph::edgeData(from, attr=colname))))
}

### Same problem as described above.
.nodeData_as_DataFrame_or_NULL <- function(from)
{
    ans_colnames <- names(graph::nodeDataDefaults(from))
    if (length(ans_colnames) == 0L)
        return(NULL)
    S4Vectors:::new_DataFrame(
        lapply(setNames(ans_colnames, ans_colnames),
               function(colname) unname(graph::nodeData(from, attr=colname))))
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

.from_graphNEL_to_DGraph <- function(from)
{
    if (!requireNamespace("graph", quietly=TRUE))
        stop(wmsg("Couldn't load the graph package. Please install ",
                  "the graph package before you try to coerce ",
                  "a ", class(from), " object to DGraph."))

    if (graph::edgemode(from) == "undirected")
        stop(wmsg("Coercing an **undirected** ", class(from), " object ",
                  "to DGraph is not supported. ",
                  "Please set the edgemode of the object to \"directed\" ",
                  "(with 'edgemode(x) <- \"directed\"') before trying ",
                  "to coerce again."))
    m <- graph::edgeMatrix(from)
    ans_from <- m[1L, ]
    ans_to <- m[2L, ]
    edges_mcols <- .attrData_as_DataFrame_or_NULL(from@edgeData)
    nodes_mcols <- .attrData_as_DataFrame_or_NULL(from@nodeData)
    ans_nodes <- AnnotatedIDs(from@nodes, nodes_mcols)
    ans <- DGraph(ans_from, ans_to, ans_nodes, edges_mcols)

    metadata(ans) <- list(edgeData_defaults=from@edgeData@defaults,
                          nodeData_defaults=from@nodeData@defaults,
                          graphData=from@graphData,
                          renderInfo=from@renderInfo)
    ans
}
setAs("graphNEL", "DGraph", .from_graphNEL_to_DGraph)

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

.from_DGraph_to_graphNEL <- function(from)
{
    if (!requireNamespace("graph", quietly=TRUE))
        stop(wmsg("Couldn't load the graph package. Please install ",
                  "the graph package before you try to coerce ",
                  "a ", class(from), " object to graphNEL."))

    from_nodes <- nodes(from)
    ans_nodes <- as.character(from_nodes)

    sh <- as(from, "SortedByQuerySelfHits")
    ans_edgeL <- as.list(setNames(as(sh, "IntegerList"), ans_nodes))
    ans_edgeL <- lapply(ans_edgeL, function(edges) list(edges=edges))

    ans <- graph::graphNEL(ans_nodes, ans_edgeL, edgemode="directed")

    ## 'edgeData' slot
    edge_mcols <- mcols(sh)
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
    node_mcols <- mcols(from_nodes)
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
setAs("DGraph", "graphNEL", .from_DGraph_to_graphNEL)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

.DGraph_summary <- function(object)
{
    object_len <- length(object)
    object_mcols <- mcols(object, use.names=FALSE)
    object_nmc <- if (is.null(object_mcols)) 0L else ncol(object_mcols)
    paste0(classNameForDisplay(object), " object with ", object_len, " ",
           ifelse(object_len == 1L, "edge", "edges"),
           " and ", object_nmc, " metadata ",
           ifelse(object_nmc == 1L, "column", "columns"))
}
### S3/S4 combo for summary.DGraph
summary.DGraph <- function(object, ...)
    .DGraph_summary(object, ...)
setMethod("summary", "DGraph", summary.DGraph)

### TODO: Print a bottom line like for Factor objects e.g. something like:
###   Nodes: IRanges object of length 10


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### adjacencyMatrix()
###

### Clash with generic of same name defined in the graph package.
### FIXME: Define this generic in BiocGenerics.
setGeneric("adjacencyMatrix",
    function(object) standardGeneric("adjacencyMatrix")
)

setMethod("adjacencyMatrix", "DGraph",
    function(object)
    {
        object_nodes <- nodes(object)
        object_nnode <- length(object_nodes)
        object_nodenames <- names(object_nodes)
        ans_dim <- c(object_nnode, object_nnode)
        ans_dimnames <- list(object_nodenames, object_nodenames)
        sparseMatrix(from(object), to(object), dims=ans_dim,
                     dimnames=ans_dimnames)
    }
)

