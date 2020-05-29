### =========================================================================
### DGraphNodes objects
### -------------------------------------------------------------------------


### See R/NodesAndEdges-class.R for a quick overview of the semantic of
### DGraphNodes objects.
setClass("DGraphNodes", contains="NodesAndEdges")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### vertical_slot_names()
###

### Combine the new "vertical slots" with those of the parent class. Make
### sure to put the new vertical slots **first**. See R/bindROWS.R file in
### the S4Vectors package for what slots should or should not be considered
### "vertical".
setMethod("vertical_slot_names", "DGraphNodes",
    function(x) c("nodes", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### .from_NodesAndEdges_to_DGraphNodes()
###

.from_NodesAndEdges_to_DGraphNodes <- function(from)
{
    class(from) <- "DGraphNodes"
    from
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### Low-level constructor.
.new_DGraphNodes <- function(nodes, edges)
{
    ## 'nodes' and 'edges' are trusted.
    new2("DGraphNodes", nodes=nodes, edges=edges, check=FALSE)
}

### High-level constructor.
### Accept a DGraph object and turn it into a DGraphNodes object.
### Using DGraph() and DGraphNodes() is the standard way to switch back and
### forth between DGraph and DGraphNodes. The transformation is very fast and
### lossless.
### Arguments passed thru the ellipsis (...) are metadata columns to set on
### the **nodes** of the object. Note that they will be considered to be the
### metadata columns of the DGraphNodes object itself.
DGraphNodes <- function(nodes, from=integer(0), to=integer(0), ...)
{
    if (class(nodes)[[1L]] == "NodesAndEdges" || is(nodes, "DGraph")) {
        if (!(identical(from, integer(0)) &&
              identical(to, integer(0)) &&
              length(list(...)) == 0L))
            stop(wmsg("additional arguments are not allowed when ",
                      "calling DGraphNodes() on a ", class(nodes), " object"))
        return(.from_NodesAndEdges_to_DGraphNodes(nodes))
    }
    nodes <- make_annotated_nodes(nodes, ...)
    edges <- SelfHits(from, to, nnode=NROW(nodes))
    .new_DGraphNodes(nodes, edges)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

### The names and metadata columns of a DGraphNodes object are the names
### and metadata columns of its **nodes**.

setMethod("names", "DGraphNodes", function(x) ROWNAMES(x@nodes))

setReplaceMethod("names", "DGraphNodes",
    function(x, value)
    {
        ## TODO: Add `ROWNAMES<-` generic to S4Vectors (we already have the
        ## ROWNAMES() getter but no setter) and use it here.
        if (length(dim(x@nodes)) < 2L) {
            names(x@nodes) <- value
        } else {
            rownames(x@nodes) <- value
        }
        x
    }
)

setMethod("elementMetadata", "DGraphNodes",
    function(x, use.names=TRUE, ...)
        elementMetadata(x@nodes, use.names=use.names, ...)
)

setReplaceMethod("elementMetadata", "DGraphNodes",
    function (x, ..., value)
    {
        elementMetadata(x@nodes, ...) <- value
        x
    }
)

setMethod("isDirected", "DGraphNodes",
    function(object) !is(object, "UGraphNodes")
)

### "edgemode<-" is an S4 generic defined in the graph package.
setReplaceMethod("edgemode", c("DGraphNodes", "ANY"),
    function(object, value)
    {
        if (!(isSingleString(value) && value %in% c("directed", "undirected")))
            stop(wmsg("edgemode must be \"directed\" or \"undirected\""))
        if (edgemode(object) == value)
            return(object)
        to_class <- if (value == "directed") "DGraphNodes" else "UGraphNodes"
        as(object, to_class)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("ANY", "DGraphNodes",
    function(from) DGraphNodes(as(from, "NodesAndEdges"))
)

setAs("DGraphNodes", "DFrame",
    function(from)
    {
        listData <- list(nodes=nodes(from),
                         outDegree=outDegree(from),
                         inDegree=inDegree(from))
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

.DGraphNodes_summary <- function(object)
{
    object_nnode <- nnode(object)
    object_nedge <- numEdges(object)
    object_mcols <- mcols(object, use.names=FALSE)
    object_nmc <- if (is.null(object_mcols)) 0L else ncol(object_mcols)
    paste0(classNameForDisplay(object), " object with ",
           object_nnode, " node", ifelse(object_nnode == 1L, "", "s"), ", ",
           object_nedge, " edge", ifelse(object_nedge == 1L, "", "s"), ", and ",
           object_nmc, " metadata column", ifelse(object_nmc == 1L, "", "s"),
           " on the nodes")
}
### S3/S4 combo for summary.DGraphNodes
summary.DGraphNodes <- function(object, ...)
    .DGraphNodes_summary(object, ...)
setMethod("summary", "DGraphNodes", summary.DGraphNodes)

.from_DGraphNodes_to_naked_character_matrix_for_display <- function(x)
{
    m <- cbind(nodes=showAsCell(nodes(x)),
               outDegree=showAsCell(outDegree(x)),
               inDegree=showAsCell(inDegree(x)))
    cbind_mcols_for_display(m, x)
}

.show_DGraphNodes <- function(x, margin="", print.classinfo=FALSE)
{
    cat(margin, summary(x), ":\n", sep="")
    ## makePrettyMatrixForCompactPrinting(x) would call head() and tail()
    ## on 'x' and this would alter the outDegree and inDegree of the nodes
    ## that get displayed. The work around is to turn the **full** object
    ## into a "naked matrix for display" and to pass that matrix to
    ## makePrettyMatrixForCompactPrinting().
    m <- .from_DGraphNodes_to_naked_character_matrix_for_display(x)
    out <- makePrettyMatrixForCompactPrinting(m)
    if (print.classinfo) {
        COL2CLASS <- c(
            nodes=class(nodes(x))[[1L]],
            outDegree="integer",
            inDegree="integer"
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

setMethod("show", "DGraphNodes",
    function(object) .show_DGraphNodes(object, print.classinfo=TRUE)
)

