\name{DGraph-class}
\docType{class}

\alias{class:DGraph}
\alias{DGraph-class}
\alias{DGraph}

\alias{numNodes}
\alias{numNodes,DGraph-method}
\alias{nodes}
\alias{nodes,DGraph-method}
\alias{nodes<-}
\alias{nodes<-,DGraph,ANY-method}
\alias{isDirected}
\alias{isDirected,DGraph-method}
\alias{edgemode}
\alias{edgemode,DGraph-method}
\alias{edgemode<-}
\alias{edgemode<-,DGraph,ANY-method}
\alias{edgeMatrix}
\alias{edgeMatrix,DGraph-method}

\alias{adjacencyMatrix}
\alias{adjacencyMatrix,DGraph-method}

\alias{coerce,SelfHits,DGraph-method}
\alias{coerce,ngCMatrix,DGraph-method}
\alias{coerce,graphNEL,DGraph-method}
\alias{coerce,DGraph,graphNEL-method}

\alias{summary.DGraph}
\alias{summary,DGraph-method}

\alias{connComp}
\alias{connComp,DGraph-method}

\title{DGraph objects}

\description{
  The DGraph class is a container for representing and manipulating a
  \emph{directed graph}. A DGraph object is a vector of edges, that is,
  the length of the object is the number of edges and the elements in
  the object are the edges.
}

\usage{
## Constructor function
DGraph(nodes, from=integer(0), to=integer(0), ...)
}

\arguments{
  \item{nodes}{
    A \emph{vector-like object} containing the nodes.
    This can be any \link[S4Vectors]{Vector} derivative.
    This can also be an ordinary vector (e.g. a character vector),
    in which case it will be wrapped in an \link{AnnotatedIDs}
    object (with \code{AnnotatedIDs(nodes)}).

    A special case is when \code{nodes} is a single number: in that case
    the nodes will be generated with \code{AnnotatedIDs(seq_len(nodes))}.
  }
  \item{from, to}{
    Two integer vectors of the same length describing the edges.
    The values in each vector must be valid node indices.
    The pair formed by \code{from[i]} and \code{to[i]} represents
    an edge that goes from node \code{nodes[from[i]]} to node
    \code{nodes[to[i]]}.
  }
  \item{...}{
    Optional metadata columns to attach to the returned DGraph object.
  }
}

\details{
  TODO
}

\value{
  A DGraph object.
}

\section{Accessors}{
  Because DGraph objects are \link[S4Vectors]{SelfHits} derivatives (i.e.
  DGraph is a subclass of the \link[S4Vectors]{SelfHits} class defined in
  the \pkg{S4Vectors} package), they support the same set of getters and
  setters as \link[S4Vectors]{SelfHits} objects.

  In addition they support the following graph-specific getters:
  \itemize{
    \item \code{numNodes(x)}: same as \code{nnode{x}} (provided for
          compatibility with the \pkg{graph} package)
    \item \code{nodes(x)}: get the nodes
    \item \code{isDirected(x)}: whether the object is \emph{directed}
          or not
    \item \code{edgemode(x)}: returns \code{"directed"} or
          \code{"undirected"}
  }
  and setters:
  \itemize{
    \item \code{nodes(x) <- value}: replace the current nodes
    \item \code{edgemode(x) <- value}: set the \emph{edgemode}
  }
}

\section{Subsetting}{
  A DGraph object can be subsetted with \code{[}, like any vector-like
  object. Note that this only subsets the set of edges i.e. all the nodes
  are preserved.
}

\section{Concatenation}{
  TODO (not supported yet)
}

\section{Comparing & Ordering}{
  Comparing two DGraph objects (e.g. with \code{==}, \code{!=}, \code{<=},
  \code{<}, \code{match()}) is only supported between objects with the same
  set of nodes in the same order.

  An edge in one object is considered to be \emph{equal} (\code{==}) to an
  edge in the other object if the 2 edges go from the same two nodes. More
  formally, the i-th edge in DGraph object \code{x} is considered to be equal
  to the j-th edge in DGraph object \code{y} if \code{from(x)[i] == from(y)[j]}
  and \code{to(x)[i] == to(y)[j]}.

  To decide whether the i-th edge is \emph{lower} (\code{<}) or \emph{greater}
  (\code{>}) than the j-th edge, first \code{from(x)[i]} and \code{from(y)[j]}
  are compared and, if they are equal, then \code{to(x)[i]} and
  \code{to(y)[j]} are also compared to break the tie.

  The same rules are used for comparing the edges within a DGraph object e.g.
  when computing their order (with \code{order()}), or sorting them (with
  \code{sort()}), or ranking them (with \code{rank()}).
  Note that for a DGraph object \code{x}, \code{order(x)} is equivalent to
  \code{order(from(x), to(x))}.
}

\author{Hervé Pagès}

\seealso{
  \itemize{
    \item \link{AnnotatedIDs} objects.

    \item \link[S4Vectors]{SelfHits} objects in the \pkg{S4Vectors} package
          for the parent class.

    \item \link[graph]{graphNEL} objects in the \pkg{graph} package.

    \item \code{\link[Rgraphviz]{agopen}} in the \pkg{Rgraphviz} package.
  }
}

\examples{
showClass("DGraph")  # DGraph extends SelfHits

## ---------------------------------------------------------------------
## CONSTRUCTOR & ACCESSORS
## ---------------------------------------------------------------------

## A directed graph with 10 nodes and no edges:
dg0 <- DGraph(nodes=letters[1:10])
dg0

## A directed graph with 8 nodes and 9 edges:
dg1 <- DGraph(nodes=8, c(1, 1:5, 6, 6, 6), c(6, 8:4, 1:2, 7),
                       weights=0.11*(1:9))
dg1

length(dg1)  # number of edges
mcols(dg1)   # metadata columns on the edges (1 row per edge)

nnode(dg1)   # number of nodes (same as 'numNodes(dg1)')
nodes(dg1)   # the nodes

## Set some names and metadata columns on the nodes:
names(nodes(dg1)) <- LETTERS[1:8]
mcols(nodes(dg1))$score <- runif(8)
nodes(dg1)

## ---------------------------------------------------------------------
## COERCION
## ---------------------------------------------------------------------

## Coercing to data.frame or DataFrame looses the node information:
as.data.frame(dg1)
as(dg1, "DataFrame")

adjm <- adjacencyMatrix(dg1)
adjm

as(adjm, "DGraph")

## Note that if 'x' is a square ngCMatrix object,
## 'adjacencyMatrix(as(x, "DGraph"))' is guaranted to be
## identical to 'x' (modulo the dimnames).
stopifnot(identical(adjacencyMatrix(as(adjm, "DGraph")), adjm))

## Coerce to a graphNEL instance from the graph package:
gnel <- as(dg1, "graphNEL")
gnel

## The graphNEL instance can be used to plot the graph with the
## Rgraphviz package:
library(Rgraphviz)
plot(agopen(gnel, "dg1"))
plot(agopen(gnel, "dg1", recipEdges="distinct"))

## TODO: More coercions to come soon (to DGraph<->graphAM,
## DGraph<->graphBAM, DGraph<->igraph, etc...)

## ---------------------------------------------------------------------
## CONCATENATION
## ---------------------------------------------------------------------

## TODO

## ---------------------------------------------------------------------
## COMPARING & ORDERING
## ---------------------------------------------------------------------

## TODO

## ---------------------------------------------------------------------
## CONNECTED COMPONENTS
## ---------------------------------------------------------------------

connComp(dg1)
}
\keyword{methods}
\keyword{classes}
