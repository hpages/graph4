### =========================================================================
### DGraph objects (directed graphs)
### -------------------------------------------------------------------------


### See R/NodesAndEdges-class.R for a quick overview of the semantic of
### DGraph objects.
setClass("DGraph", contains="NodesAndEdges")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### vertical_slot_names()
###

### Combine the new "vertical slots" with those of the parent class. Make
### sure to put the new vertical slots **first**. See R/bindROWS.R file in
### the S4Vectors package for what slots should or should not be considered
### "vertical".
setMethod("vertical_slot_names", "DGraph",
    function(x) c("edges", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### .from_NodesAndEdges_to_DGraph()
###

.from_NodesAndEdges_to_DGraph <- function(from)
{
    class(from) <- "DGraph"
    from
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### Low-level constructor.
.new_DGraph <- function(nodes, edges)
{
    ## 'nodes' and 'edges' are trusted.
    new2("DGraph", nodes=nodes, edges=edges, check=FALSE)
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
    if (class(nodes)[[1L]] == "NodesAndEdges" || is(nodes, "DGraphNodes")) {
        if (!(identical(from, integer(0)) &&
              identical(to, integer(0)) &&
              length(list(...)) == 0L))
            stop(wmsg("additional arguments are not allowed when ",
                      "calling DGraph() on a ", class(nodes), " object"))
        return(.from_NodesAndEdges_to_DGraph(nodes))
    }
    nodes <- make_annotated_nodes(nodes)
    edges <- SelfHits(from, to, nnode=NROW(nodes), ...)
    .new_DGraph(nodes, edges)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

### The names and metadata columns of a DGraph object are the names and
### metadata columns of its **edges**.
### IMPORTANT: The edges (SelfHits object) cannot hold names at the moment.

setMethod("elementMetadata", "DGraph",
    function(x, use.names=TRUE, ...)
        elementMetadata(x@edges, use.names=use.names, ...)
)

setReplaceMethod("elementMetadata", "DGraph",
    function (x, ..., value)
    {
        elementMetadata(x@edges, ...) <- value
        x
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

setAs("ANY", "DGraph",
    function(from) DGraph(as(from, "NodesAndEdges"))
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
    function(object) .show_DGraph(object, print.classinfo=TRUE)
)

