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

DGraph <- function(nodes, from=integer(0), to=integer(0), ...)
{
    nodes <- .normarg_nodes(nodes)
    sh <- SelfHits(from, to, nnode=length(nodes), ...)
    new2("DGraph", sh, nodes=nodes, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

### Generic defined in the graph package.
### Equivalent to 'nnode(x)'.
setMethod("numNodes", "DGraph", function(object) length(object@nodes))

### Generic defined in the graph package.
setMethod("nodes", "DGraph", function(object) object@nodes)

### Generic defined in the graph package.
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

setGeneric("fromNode", function(x) standardGeneric("fromNode"))
setMethod("fromNode", "DGraph", function(x) extractROWS(nodes(x), from(x)))

setGeneric("toNode", function(x) standardGeneric("toNode"))
setMethod("toNode", "DGraph", function(x) extractROWS(nodes(x), to(x)))


### Generic defined in the graph package.
setMethod("isDirected", "DGraph",
    function(object) !is(object, "UGraph")
)

### Generic defined in the graph package.
setMethod("edgemode", "DGraph",
    function(object) if (isDirected(object)) "directed" else "undirected"
)

### Generic defined in the graph package.
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

.drop_duplicated_edges_from_undirected_graph <- function(x)
{
    flip_idx <- which(from(x) > to(x))
    if (length(flip_idx) != 0L) {
        tmp <- x@from[flip_idx]
        x@from[flip_idx] <- x@to[flip_idx]
        x@to[flip_idx] <- tmp
    }
    unique(x)
}

### Generic defined in the graph package.
setMethod("edgeMatrix", "DGraph",
    function(object, duplicates=FALSE)
    {
        if (!isTRUEorFALSE(duplicates))
            stop(wmsg("'duplicates' must be TRUE or FALSE"))
        if (!(duplicates || isDirected(object)))
            object <- .drop_duplicated_edges_from_undirected_graph(object)
        rbind(from=from(object), to=to(object))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### adjacencyMatrix()
###

### Generic defined in the graph package.
### Returns an ngCMatrix object (defined in Matrix package).
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

### Note that the 'from' argument below is the standard argument for
### coercion methods. It should not be confused with the 'from()' accessor
### for DGraph objects!
setAs("SelfHits", "DGraph",
    function(from)
    {
        nodes <- AnnotatedIDs(seq_len(nnode(from)))
        new2("DGraph", from, nodes=nodes, check=FALSE)
    }
)

### We provide a coercion method to go from ngCMatrix (defined in
### Matrix package) to DGraph. Note that if 'x' is a square ngCMatrix
### object, 'adjacencyMatrix(as(x, "DGraph"))' is guaranted to be
### identical to 'x' (modulo the dimnames).
setAs("ngCMatrix", "DGraph",
    function(from)
    {
        N <- nrow(from)
        if (ncol(from) != N)
            stop(wmsg(class(from), " object to coerce to DGraph ",
                      "must be square"))
        ans <- DGraph(N, from@i + 1L, rep.int(seq_len(N), diff(from@p)))
        if (!is.null(rownames(from))) {
            names(nodes(ans)) <- rownames(from)
            return(ans)
        }
        if (!is.null(colnames(from))) {
            names(nodes(ans)) <- colnames(from)
            return(ans)
        }
        ans
    }
)

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
                  "to DGraph is not supported yet. ",
                  "Please set the edgemode of the object to \"directed\" ",
                  "(with 'edgemode(x) <- \"directed\"') before trying ",
                  "to coerce again."))
    m <- edgeMatrix(from)
    ans_from <- m[1L, ]
    ans_to <- m[2L, ]
    edges_mcols <- .attrData_as_DataFrame_or_NULL(from@edgeData)
    nodes_mcols <- .attrData_as_DataFrame_or_NULL(from@nodeData)
    ans_nodes <- AnnotatedIDs(from@nodes, nodes_mcols)
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
    ans_nodes <- names(from_nodes)
    if (is.null(ans_nodes)) {
        ## Unlike as.character(), showAsCell() is guaranted to produce a
        ## character vector **parallel** to its argument (i.e. with one
        ## string per ROW). For example as.character() won't do the right
        ## thing on a DNAString object.
        ans_nodes <- showAsCell(from_nodes)
    }
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

    sh <- as(from, "SortedByQuerySelfHits")
    ans_edgeL <- as.list(setNames(as(sh, "IntegerList"), ans_nodes))
    ans_edgeL <- lapply(ans_edgeL, function(edges) list(edges=edges))

    ans <- graphNEL(ans_nodes, ans_edgeL, edgemode="directed")

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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

.DGraph_summary <- function(object)
{
    object_nnode <- nnode(object)
    object_len <- length(object)
    object_mcols <- mcols(object, use.names=FALSE)
    object_nmc <- if (is.null(object_mcols)) 0L else ncol(object_mcols)
    paste0(classNameForDisplay(object), " object with ",
           object_nnode, " node", ifelse(object_nnode == 1L, "", "s"), ", ",
           object_len, " edge", ifelse(object_len == 1L, "", "s"), ", and ",
           object_nmc, " metadata column", ifelse(object_nmc == 1L, "", "s"))
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
        .COL2CLASS <- c(
            fromNode=class(x@nodes),
            from="integer",
            "",
            to="integer",
            toNode=class(x@nodes)
        )
        classinfo <- makeClassinfoRowForCompactPrinting(x, .COL2CLASS)
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
### connComp()
###

### Always treats 'x' as an **undirected** graph.
### Returns the connected components in an IntegerList object
### where each list element is strictly sorted.
.connComp_DGraph <- function(x)
{
    x <- union(x, t(x))
    x_from <- from(x)
    x_to <- to(x)
    N <- nnode(x)
    cid <- cid0 <- seq_len(N)  # cluster ids
    repeat {
        cid2 <- pmin(cid, selectHits(x, "first"))
        if (identical(cid2, cid))
            break
        cid <- cid2
        x <- Hits(x_from, cid[x_to], N, N)
    }
    unname(splitAsList(cid0, cid))
}

### Generic defined in the graph package.
setMethod("connComp", "DGraph",
    function(object) .connComp_DGraph(object)
)

setMethod("isConnected", "DGraph",
    function(object) { length(connComp(object)) == 1L }
)

