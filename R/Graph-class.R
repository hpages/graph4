### =========================================================================
### Graph objects
### -------------------------------------------------------------------------
###
### Graph is a virtual class with no slots to be extended by classes that
### aim at representing objects with a graph-like semantic.
### Graph derivatives are expected to support at least coercion to SelfHits
### and core Graph accessors nodes(), edges(), and isDirected().
###

setClass("Graph", representation("VIRTUAL"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Core Graph accessors
###   - nodes()
###   - edges()
###   - isDirected()
###
### nodes(), edges(), and isDirected() are S4 generics defined in the
### graph package.
###

### We only provide a default edges() method.
setMethod("edges", "Graph", function(object) as(object, "SelfHits"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Core SelfHits API
###   - nnode()
###   - from()
###   - to()
###   - countLnodeHits()
###   - countRnodeHits()
###   - nLnode()
###   - nRnode()
###   - t()
###   - coercion from Graph to SortedByQuerySelfHits
###
### The SelfHits class and core API are implemented in the S4Vectors package.
### We provide default methods for the full core SelfHits API except for t().
###

### In order to be as performant as possible, we avoid coercion to SelfHits
### if 'x' derives from SelfHits (e.g. 'x' is a DGraph object).
.as_SelfHits <- function(x) if (is(x, "SelfHits")) x else as(x, "SelfHits")

if (FALSE) {
  ## BROKEN!!!
  .SelfHits_CORE_GENERICS <- c("nnode", "from", "to",
                               "countLnodeHits", "countRnodeHits",
                               "nLnode", "nRnode")

  for (f in .SelfHits_CORE_GENERICS) {
    METHOD <- selectMethod(f, "SelfHits")
    setMethod(f, "Graph", function(x) METHOD(.as_SelfHits(x)))
  }
}

.METHOD1 <- selectMethod("nnode", "SelfHits")
setMethod("nnode", "Graph", function(x) .METHOD1(.as_SelfHits(x)))

.METHOD2 <- selectMethod("from", "SelfHits")
setMethod("from", "Graph", function(x) .METHOD2(.as_SelfHits(x)))

.METHOD3 <- selectMethod("to", "SelfHits")
setMethod("to", "Graph", function(x) .METHOD3(.as_SelfHits(x)))

.METHOD4 <- selectMethod("countLnodeHits", "SelfHits")
setMethod("countLnodeHits", "Graph", function(x) .METHOD4(.as_SelfHits(x)))

.METHOD5 <- selectMethod("countRnodeHits", "SelfHits")
setMethod("countRnodeHits", "Graph", function(x) .METHOD5(.as_SelfHits(x)))

.METHOD6 <- selectMethod("nLnode", "SelfHits")
setMethod("nLnode", "Graph", function(x) .METHOD6(.as_SelfHits(x)))

.METHOD7 <- selectMethod("nRnode", "SelfHits")
setMethod("nRnode", "Graph", function(x) .METHOD7(.as_SelfHits(x)))

.METHOD8 <- selectMethod("coerce", c("SelfHits", "SortedByQuerySelfHits"))
setAs("Graph", "SortedByQuerySelfHits",
    function(from) .METHOD8(.as_SelfHits(from))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Extended SelfHits API
###   - numNodes()
###   - numEdges()
###   - outDegree()
###   - inDegree()
###   - degree()
###   - adjacencyMatrix()
###   - coercion from SelfHits to ngCMatrix
###   - coercion from Graph to ngCMatrix
###   - coercion from ngCMatrix to SelfHits
###   - connComp()
###   - isConnected()
###
### numNodes(), numEdges(), degree(), adjacencyMatrix(), connComp(),
### and isConnected() are S4 generics defined in the graph package.
### outDegree() and inDegree() are new S4 generics introduced by the
### graph4 package.
###

setMethod("numNodes", "ANY", function(object) nnode(object))

setMethod("numEdges", "SelfHits", function(object) length(object))
setMethod("numEdges", "Graph", function(object) length(.as_SelfHits(object)))

setGeneric("outDegree", function(object) standardGeneric("outDegree"))
setGeneric("inDegree", function(object) standardGeneric("inDegree"))

setMethod("outDegree", "SelfHits", function(object) countLnodeHits(object))
setMethod("inDegree", "SelfHits", function(object) countRnodeHits(object))

make_node_labels <- function(nodes)
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

setMethod("outDegree", "Graph",
    function(object)
        setNames(countLnodeHits(object), make_node_labels(nodes(object)))
)
setMethod("inDegree", "Graph",
    function(object)
        setNames(countRnodeHits(object), make_node_labels(nodes(object)))
)
setMethod("degree", "ANY",
    function(object) outDegree(object) + inDegree(object)
)

.make_adjacencyMatrix_from_SelfHits <- function(x, nodenames=NULL)
{
    x_nnode <- nnode(x)
    ans_dim <- c(x_nnode, x_nnode)
    ans_dimnames <- list(nodenames, nodenames)
    sparseMatrix(from(x), to(x), dims=ans_dim, dimnames=ans_dimnames)
}

### Return an ngCMatrix object (defined in the Matrix package).
setMethod("adjacencyMatrix", "SelfHits",
    function(object) .make_adjacencyMatrix_from_SelfHits(object)
)

setAs("SelfHits", "ngCMatrix", function(from) adjacencyMatrix(from))

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

setMethod("connComp", "SelfHits", function(object) .connComp_SelfHits(object))
setMethod("connComp", "Graph",
    function(object) connComp(as(object, "SelfHits"))
)

setMethod("isConnected", "ANY",
    function(object) length(connComp(object)) == 1L
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other Graph accessors
###   - fromNode()
###   - toNode()
###   - edgemode()
###   - edgeMatrix()
###
### fromNode() and toNode() are new S4 generics introduced by the graph4
### package.
### edgemode() and edgeMatrix() are S4 generics defined in the graph package.
###

setGeneric("fromNode", function(x) standardGeneric("fromNode"))
### Default fromNode() method for Graph objects.
setMethod("fromNode", "Graph", function(x) extractROWS(nodes(x), from(x)))

setGeneric("toNode", function(x) standardGeneric("toNode"))
### Default toNode() method for Graph objects.
setMethod("toNode", "Graph", function(x) extractROWS(nodes(x), to(x)))

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

