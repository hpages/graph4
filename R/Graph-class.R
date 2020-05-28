### =========================================================================
### Graph objects
### -------------------------------------------------------------------------
###
### Graph is a virtual class with no slots to be extended by classes that
### aim at representing objects with a graph-like semantic.
### Graph derivatives are expected to support at least nnode(), nodes()
### and edges(), but also typically from(), to(), and isDirected().
### nnode(), from(), and to() are S4 generics defined in the S4Vectors package.
### nodes(), edges(), and isDirected() are S4 generics defined in the graph
### package.
###

setClass("Graph", representation("VIRTUAL"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###
### S4 generic defined in the S4Vectors package: nnode().
### S4 generics defined in the graph package: numNodes(), numEdges(),
### degree(), edgemode(), edgeMatrix().
### New S4 generics introduced by the graph4 package: fromNode(), toNode(),
### outDegree(), inDegree().
###

### Default numNodes() method (alias for nnode()).
setMethod("numNodes", "ANY", function(object) nnode(object))

### numEdges() method for SelfHits objects.
setMethod("numEdges", "SelfHits", function(object) length(object))

### Default numEdges() method for Graph objects.
setMethod("numEdges", "Graph", function(object) NROW(edges(object)))

setGeneric("fromNode", function(x) standardGeneric("fromNode"))
### Default fromNode() method for Graph objects.
setMethod("fromNode", "Graph", function(x) extractROWS(nodes(x), from(x)))

setGeneric("toNode", function(x) standardGeneric("toNode"))
### Default toNode() method for Graph objects.
setMethod("toNode", "Graph", function(x) extractROWS(nodes(x), to(x)))

setGeneric("outDegree", function(object) standardGeneric("outDegree"))
setGeneric("inDegree", function(object) standardGeneric("inDegree"))

.make_node_labels <- function(nodes)
{
    labels <- ROWNAMES(nodes)
    if (!is.null(labels))
        return(labels)
    ## Unlike as.character(), showAsCell() is guaranted to produce a
    ## character vector **parallel** to its argument (i.e. with one
    ## string per ROW). For example as.character() won't do the right
    ## thing on a DataFrame or DNAString object.
    showAsCell(nodes)
}

### Default outDegree() and inDegree() methods for Graph objects.
setMethod("outDegree", "Graph",
    function(object)
    {
        ans <- countLnodeHits(object)
        names(ans) <- .make_node_labels(nodes(object))
        ans
    }
)
setMethod("inDegree", "Graph",
    function(object)
    {
        ans <- countRnodeHits(object)
        names(ans) <- .make_node_labels(nodes(object))
        ans
    }
)

### Default degree() method for Graph objects.
setMethod("degree", "Graph",
    function(object) outDegree(object) + inDegree(object)
)

### Default edgemode() method for Graph objects.
setMethod("edgemode", "Graph",
    function(object) if (isDirected(object)) "directed" else "undirected"
)

.normalize_undirected_edges <- function(x)
{
    x_from <- from(x)
    x_to <- to(x)
    flip_idx <- which(x_from > x_to)
    if (length(flip_idx) != 0L) {
        tmp <- x_from[flip_idx]
        x_from[flip_idx] <- x_to[flip_idx]
        x_to[flip_idx] <- tmp
    }
    SelfHits(x_from, x_to, nnode=nnode(x))
}

### Default edgeMatrix() method for Graph objects.
### Return a 2-row integer matrix.
setMethod("edgeMatrix", "Graph",
    function(object, duplicates=FALSE)
    {
        if (!isTRUEorFALSE(duplicates))
            stop(wmsg("'duplicates' must be TRUE or FALSE"))
        if (!(duplicates || isDirected(object))) {
            object <- .normalize_undirected_edges(object)
            object <- unique(object)
        }
        rbind(from=from(object), to=to(object))
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Adjacency matrix
###
### adjacencyMatrix() is an S4 generic defined in the graph package.
###

.make_adjacencyMatrix_from_SelfHits <- function(x, nodenames=NULL)
{
    x_nnode <- nnode(x)
    ans_dim <- c(x_nnode, x_nnode)
    ans_dimnames <- list(nodenames, nodenames)
    sparseMatrix(from(x), to(x), dims=ans_dim, dimnames=ans_dimnames)
}

### Method for SelfHits objects.
### Return an ngCMatrix object (defined in the Matrix package).
setMethod("adjacencyMatrix", "SelfHits",
    function(object) .make_adjacencyMatrix_from_SelfHits(object)
)

setAs("SelfHits", "ngCMatrix", function(from) adjacencyMatrix(from))

### Default adjacencyMatrix() method for Graph objects.
### Return an ngCMatrix object (defined in the Matrix package).
setMethod("adjacencyMatrix", "Graph",
    function(object)
    {
        nodenames <- names(nodes(object))
        .make_adjacencyMatrix_from_SelfHits(object, nodenames)
    }
)

setAs("Graph", "ngCMatrix", function(from) adjacencyMatrix(from))

### We provide a coercion method from ngCMatrix to SelfHits. Note that
### if 'x' is a square ngCMatrix object, adjacencyMatrix(as(x, "SelfHits"))
### is guaranted to be identical to 'x' (modulo the dimnames).
.from_ngCMatrix_to_SelfHits <- function(from)
{
    N <- nrow(from)
    if (ncol(from) != N)
        stop(wmsg(class(from), " object to coerce to a SelfHits ",
                  "or Graph derivative must be square"))
    SelfHits(from@i + 1L, rep.int(seq_len(N), diff(from@p)), nnode=N)
}
setAs("ngCMatrix", "SelfHits", .from_ngCMatrix_to_SelfHits)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Connected components
###
### connComp() and isConnected() are S4 generics defined in the graph package.
### We define methods for SelfHits objects.
###

### Always treats 'x' as an **undirected** graph.
### Return the connected components in an IntegerList object where each
### list element is strictly sorted.
.connComp_SelfHits <- function(x)
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

setMethod("connComp", "SelfHits",
    function(object) .connComp_SelfHits(object)
)

setMethod("isConnected", "ANY",
    function(object) length(connComp(object)) == 1L
)

