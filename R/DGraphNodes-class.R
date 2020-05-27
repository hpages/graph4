### =========================================================================
### DGraphNodes objects
### -------------------------------------------------------------------------


### The same bundle of nodes and edges as DGraph.
### HOWEVER, now viewed as a **vector of nodes** i.e. the length of
### the object is its number of nodes and subsetting the object means
### selecting a particular subset of nodes. This means subsetting a
### DGraphNodes may drop some of its edges!
setClass("DGraphNodes",
    contains="AnnotatedIDs",
    representation(
        edges="SelfHits"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.validate_DGraphNodes <- function(x)
{
    ## 'edges' slot
    if (class(x@edges) != "SelfHits")
        return("'edges' slot must be of class SelfHits")
    if (nnode(x@edges) != length(x))
        return("'edges' slot must have one node per element in 'x'")

    TRUE
}

setValidity2("DGraphNodes", .validate_DGraphNodes)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### Low-level constructor.
.new_DGraphNodes <- function(nodes, edges)
{
    ## 'nodes' and 'edges' are trusted.
    new2("DGraphNodes", nodes, edges=edges, check=FALSE)
}

.from_DGraph_to_DGraphNodes <- function(from)
{
    edges <- as(from, "SelfHits")
    .new_DGraphNodes(from@nodes, edges)
}

from_DGraphNodes_to_DGraph <- function(from)
{
    nodes <- as(from, "AnnotatedIDs")
    new_DGraph(nodes, from@edges)
}

### High-level constructor.
### Accept a DGraph object and turn it into a DGraphNodes object.
### Using DGraph() and DGraphNodes() is the standard way to switch back and
### forth between DGraph and DGraphNodes. The transformation is very fast and
### lossless.
DGraphNodes <- function(nodes, from=integer(0), to=integer(0), ...)
{
    if (is(nodes, "DGraph")) {
        if (!(identical(from, integer(0)) &&
              identical(to, integer(0)) &&
              length(list(...)) == 0L))
            stop(wmsg("additional arguments are not allowed ",
                      "when 'nodes' is a DGraph object"))
        return(.from_DGraph_to_DGraphNodes(nodes))
    }
    nodes <- normarg_nodes(nodes)
    edges <- SelfHits(from, to, nnode=NROW(nodes), ...)
    .new_DGraphNodes(nodes, edges)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SelfHits API
###
### Unlike DGraph objects, DGraphNodes don't inherit the SelfHits API so we
### implement it.
###

setAs("DGraphNodes", "SelfHits", function(from) from@edges)
setAs("DGraphNodes", "SortedByQuerySelfHits",
    function(from) as(from@edges, "SortedByQuerySelfHits")
)

setMethod("from", "DGraphNodes", function(x) from(x@edges))
setMethod("to", "DGraphNodes", function(x) to(x@edges))
setMethod("nLnode", "DGraphNodes", function(x) nLnode(x@edges))
setMethod("nRnode", "DGraphNodes", function(x) nRnode(x@edges))
setMethod("nnode", "DGraphNodes", function(x) nnode(x@edges))
setMethod("countLnodeHits", "DGraphNodes", function(x) countLnodeHits(x@edges))
setMethod("countRnodeHits", "DGraphNodes", function(x) countRnodeHits(x@edges))
setMethod("t", "DGraphNodes", function(x) {x@edges <- t(x@edges); x})
setMethod("connComp", "DGraphNodes", function(object) connComp(object@edges))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

### Generic defined in the graph package.
setMethod("numNodes", "DGraphNodes", function(object) length(object))

### Generic defined in the graph package.
setMethod("numEdges", "DGraphNodes", function(object) length(object@edges))

### Generic defined in the graph package.
setMethod("nodes", "DGraphNodes", function(object) as(object, "AnnotatedIDs"))

### Generic defined in the graph package.
setReplaceMethod("nodes", "DGraphNodes",
    function(object, value)
    {
        stop("IMPLEMENT ME!")
    }
)

setMethod("fromNode", "DGraphNodes", function(x) extractROWS(nodes(x), from(x)))

setMethod("toNode", "DGraphNodes", function(x) extractROWS(nodes(x), to(x)))

### TODO: Current method for DGraph objects would work out-of-the-box on
### a DGraphNodes object! So maybe just replace current method for DGraph
### with method for DGraph_OR_DGraphNodes and get rid of the method below.
setMethod("outDegree", "DGraphNodes",
    function(object) outDegree(DGraph(object))
)

### TODO: Current method for DGraph objects would work out-of-the-box on
### a DGraphNodes object! So maybe just replace current method for DGraph
### with method for DGraph_OR_DGraphNodes and get rid of the method below.
setMethod("inDegree", "DGraphNodes",
    function(object) inDegree(DGraph(object))
)

### Generic defined in the graph package.
setMethod("isDirected", "DGraphNodes",
    function(object) !is(object, "UGraphNodes")
)

### Generic defined in the graph package.
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

### Generic defined in the graph package.
### TODO: Current method for DGraph objects would work out-of-the-box on
### a DGraphNodes object! So maybe just replace current method for DGraph
### with method for DGraph_OR_DGraphNodes and get rid of the method below.
setMethod("edgeMatrix", "DGraphNodes",
    function(object, duplicates=FALSE)
        edgeMatrix(DGraph(object), duplicates=duplicates)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### adjacencyMatrix()
###

### Generic defined in the graph package.
### Returns an ngCMatrix object (defined in Matrix package).
### TODO: Current method for DGraph objects would work out-of-the-box on
### a DGraphNodes object! So maybe just replace current method for DGraph
### with method for DGraph_OR_DGraphNodes and get rid of the method below.
setMethod("adjacencyMatrix", "DGraphNodes",
    function(object) adjacencyMatrix(DGraph(object))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setAs("ANY", "DGraphNodes", function(from) DGraphNodes(as(from, "DGraph")))

setAs("SelfHits", "DGraphNodes",
    function(from)
    {
        nodes <- AnnotatedIDs(seq_len(nnode(from)))
        edges <- as(from, "SelfHits")
        .new_DGraphNodes(nodes, edges)
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
setMethod("makeNakedCharacterMatrixForDisplay", "DGraphNodes",
    .from_DGraphNodes_to_naked_character_matrix_for_display
)

.show_DGraphNodes <- function(x, margin="", print.classinfo=FALSE)
{
    cat(margin, summary(x), ":\n", sep="")
    ## makePrettyMatrixForCompactPrinting() assumes that head() and tail()
    ## work on 'x'.
    out <- makePrettyMatrixForCompactPrinting(x)
    if (print.classinfo) {
        COL2CLASS <- c(
            nodes="AnnotatedIDs",
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
    function(object)
        .show_DGraphNodes(object, print.classinfo=TRUE)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Compatibility with graphNEL objects
###

### TODO: Current method for DGraph objects would work out-of-the-box on
### a DGraphNodes object! So maybe just replace current method for DGraph
### with method for DGraph_OR_DGraphNodes and get rid of the method below.
setAs("DGraphNodes", "graphNEL", function(from) as(DGraph(from), "graphNEL"))

