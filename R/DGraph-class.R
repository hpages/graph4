### =========================================================================
### DGraph objects (directed graphs)
### -------------------------------------------------------------------------


### A bundle of nodes and edges. Viewed as a vector of edges i.e. the length
### of the object is its number of edges and subsetting the object means
### selecting a particular subset of edges (without touching the set of
### nodes).
setClass("DGraph",
    ## Graph must precede SelfHits so dispatch will pick up methods
    ## defined for Graph objects over methods defined for SelfHits
    ## objects (e.g. adjacencyMatrix()).
    contains=c("Graph", "SelfHits"),
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
    if (is(x@nodes, "DGraphNodes"))
        return("'nodes' slot cannot be a DGraphNodes object")
    if (NROW(x@nodes) != nnode(x))
        return("'nodes' slot must have one ROW per node")

    TRUE
}

setValidity2("DGraph", .validate_DGraph)


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
### Constructor
###

### Low-level constructor.
new_DGraph <- function(nodes, edges)
{
    ## 'nodes' and 'edges' are trusted.
    new2("DGraph", edges, nodes=nodes, check=FALSE)
}

### High-level constructor.
### Accept a DGraphNodes object and turn it into a DGraph object.
### Using DGraph() and DGraphNodes() is the standard way to switch back and
### forth between DGraph and DGraphNodes. The transformation is very fast and
### lossless.
### Arguments passed thru the ellipsis (...) are metadata columns to set on
### the **edges** of the object. Note that they will be considered to be the
### metadata columns of the DGraphNodes object itself.
DGraph <- function(nodes, from=integer(0), to=integer(0), ...)
{
    if (is(nodes, "DGraphNodes")) {
        if (!(identical(from, integer(0)) &&
              identical(to, integer(0)) &&
              length(list(...)) == 0L))
            stop(wmsg("additional arguments are not allowed ",
                      "when 'nodes' is a DGraphNodes object"))
        return(from_DGraphNodes_to_DGraph(nodes))
    }
    nodes <- make_annotated_nodes(nodes)
    edges <- SelfHits(from, to, nnode=NROW(nodes), ...)
    new_DGraph(nodes, edges)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setMethod("nodes", "DGraph", function(object) object@nodes)

### "nodes<-" is an S4 generic defined in the graph package.
setReplaceMethod("nodes", "DGraph",
    function(object, value)
    {
        object_nnode <- nnode(object)
        if (is.null(value)) {
            value <- make_annotated_nodes(object_nnode)
        } else {
            value <- make_annotated_nodes(value)
            if (NROW(value) != object_nnode)
                stop(wmsg("this ", class(object), " object ",
                          "expects ", object_nnode, " node",
                          if (object_nnode == 1L) "", "s"))
        }
        object@nodes <- value
        object
    }
)

setMethod("isDirected", "DGraph",
    function(object) !is(object, "UGraph")
)

### "edgemode<-" is an S4 generic defined in the graph package.
setReplaceMethod("edgemode", c("DGraph", "ANY"),
    function(object, value)
    {
        if (!(isSingleString(value) && value %in% c("directed", "undirected")))
            stop(wmsg("edgemode must be \"directed\" or \"undirected\""))
        if (edgemode(object) == value)
            return(object)
        to_class <- if (value == "directed") "DGraph" else "UGraph"
        as(object, to_class)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("SelfHits", "DGraph",
    function(from)
    {
        edges <- as(from, "SelfHits")
        nodes <- make_annotated_nodes(nnode(edges))
        new_DGraph(nodes, edges)
    }
)

setAs("DGraph", "DFrame",
    function(from)
    {
        listData <- list(fromNode=fromNode(from),
                         from=from(from),
                         to=to(from),
                         toNode=toNode(from))
        ans <- S4Vectors:::new_DataFrame(listData)
        from_mcols <- mcols(from, use.names=FALSE)
        if (!is.null(from_mcols))
            ans <- cbind(ans, from_mcols)
        ans
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

.DGraph_summary <- function(object)
{
    object_nnode <- nnode(object)
    object_nedge <- numEdges(object)
    object_mcols <- mcols(object, use.names=FALSE)
    object_nmc <- if (is.null(object_mcols)) 0L else ncol(object_mcols)
    paste0(classNameForDisplay(object), " object with ",
           object_nnode, " node", ifelse(object_nnode == 1L, "", "s"), ", ",
           object_nedge, " edge", ifelse(object_nedge == 1L, "", "s"), ", and ",
           object_nmc, " metadata column", ifelse(object_nmc == 1L, "", "s"),
           " on the edges")
}
### S3/S4 combo for summary.DGraph
summary.DGraph <- function(object, ...)
    .DGraph_summary(object, ...)
setMethod("summary", "DGraph", summary.DGraph)

.from_DGraph_to_naked_character_matrix_for_display <- function(x)
{
    m <- cbind(fromNode=showAsCell(fromNode(x)),
               from=showAsCell(from(x)),
               rep.int(ifelse(is(x, "UGraph"), "<->", "->"), length(x)),
               to=showAsCell(to(x)),
               toNode=showAsCell(toNode(x)))
    cbind_mcols_for_display(m, x)
}
setMethod("makeNakedCharacterMatrixForDisplay", "DGraph",
    .from_DGraph_to_naked_character_matrix_for_display
)

.show_DGraph <- function(x, margin="", print.classinfo=FALSE)
{
    cat(margin, summary(x), ":\n", sep="")
    ## makePrettyMatrixForCompactPrinting() assumes that head() and tail()
    ## work on 'x'.
    out <- makePrettyMatrixForCompactPrinting(x)
    if (print.classinfo) {
        x_nodes <- nodes(x)
        COL2CLASS <- c(
            fromNode=class(x_nodes)[[1L]],
            from="integer",
            "",
            to="integer",
            toNode=class(x_nodes)[[1L]]
        )
        classinfo <- makeClassinfoRowForCompactPrinting(x, COL2CLASS)
        ## A sanity check, but this should never happen!
        stopifnot(identical(colnames(classinfo), colnames(out)))
        out <- rbind(classinfo, out)
    }
    if (nrow(out) != 0L)
        rownames(out) <- paste0(margin, "  ", rownames(out))
    ## We set 'max' to 'length(out)' to avoid the getOption("max.print")
    ## limit that would typically be reached when 'showHeadLines' global
    ## option is set to Inf.
    print(out, quote=FALSE, right=TRUE, max=length(out))
}

setMethod("show", "DGraph",
    function(object)
        .show_DGraph(object, print.classinfo=TRUE)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Adjacency matrix
###

### We provide a coercion method to go from ngCMatrix (defined in
### Matrix package) to DGraph. Note that if 'x' is a square ngCMatrix
### object, 'adjacencyMatrix(as(x, "DGraph"))' is guaranted to be
### identical to 'x' (modulo the dimnames).
.from_ngCMatrix_to_DGraph <- function(from)
{
    edges <- as(from, "SelfHits")
    nodes <- make_annotated_nodes(nnode(edges))
    if (!is.null(rownames(from))) {
        names(nodes) <- rownames(from)
    } else if (!is.null(colnames(from))) {
        names(nodes) <- colnames(from)
    }
    new_DGraph(nodes, edges)
}
setAs("ngCMatrix", "DGraph", .from_ngCMatrix_to_DGraph)


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

.from_graphNEL_to_DGraph <- function(from)
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
    ans <- DGraph(ans_nodes, ans_from, ans_to, edges_mcols)

    metadata(ans) <- list(edgeData_defaults=from@edgeData@defaults,
                          nodeData_defaults=from@nodeData@defaults,
                          graphData=from@graphData,
                          renderInfo=from@renderInfo)
    ans
}
setAs("graphNEL", "DGraph", .from_graphNEL_to_DGraph)

.make_graphNEL_nodes <- function(from)
{
    from_nodes <- nodes(from)
    ans_nodes <- make_node_labels(from_nodes)
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

.from_DGraph_to_graphNEL <- function(from)
{
    ans_nodes <- .make_graphNEL_nodes(from)

    edges <- as(from, "SortedByQuerySelfHits")
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
    node_mcols <- mcols(nodes(from))
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

