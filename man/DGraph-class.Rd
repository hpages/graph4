\name{DGraph-class}
\docType{class}

\alias{class:DGraph}
\alias{DGraph-class}
\alias{DGraph}

\alias{nodes}
\alias{nodes,DGraph-method}
\alias{nodes<-}
\alias{nodes<-,DGraph-method}

\alias{coerce,graphNEL,DGraph-method}
\alias{coerce,DGraph,graphNEL-method}

\alias{summary.DGraph}
\alias{summary,DGraph-method}

\alias{adjacencyMatrix}
\alias{adjacencyMatrix,DGraph-method}

\title{DGraph objects}

\description{
  The DGraph class is a container for representing and manipulating a
  \emph{directed graph}. A DGraph object is a vector of edges, that is,
  the length of the object is the number of edges and the elements in
  the object are the edges.
}

\usage{
## Constructor function
DGraph(from=integer(0), to=integer(0), nodes=0, ...)
}

\arguments{
  \item{from, to}{
    Two integer vectors of the same length describing the edges.
    The values in each vector must be valid node indices.
    The pair formed by \code{from[i]} and \code{to[i]} represents
    an edge that goes from node \code{nodes[from[i]]} to node
    \code{nodes[to[i]]}.
  }
  \item{nodes}{
    A \emph{vector-like object} containing the nodes.
  }
  \item{...}{
    Optional metadata columns to be attached to the edges.
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
  the \pkg{S4Vectors} package), they support all the same set of accessors
  as \link[S4Vectors]{SelfHits} objects.

  In addition they support the \code{nodes()} accessor to get or set the
  nodes.
}

\section{Subsetting}{
  A DGraph object can be subsetted with \code{[}, like any vector-like
  object. Note that this subsets the set of edges. It doesn't modify the
  set of nodes.
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
    \item \link[S4Vectors]{SelfHits} objects in the \pkg{S4Vectors} package
          for the parent class.

    \item \link[graph]{graphNEL} objects in the \pkg{graph} package.
  }
}

\examples{
showClass("DGraph")  # DGraph extends SelfHits

## ---------------------------------------------------------------------
## CONSTRUCTOR & ACCESSORS
## ---------------------------------------------------------------------

g1 <- DGraph(1:4, 4:1, nodes=4, weights=runif(4))
g1

length(g1)  # number of edges
nnode(g1)   # number of nodes

names(g1)   # names of the edges
mcols(g1)   # metadata columns on the edges (1 row per edge)

nodes(g1)   # the nodes
names(nodes(g1))  # names of the nodes
mcols(nodes(g1))  # metadata columns on the nodes (1 row per node)

## ---------------------------------------------------------------------
## COERCION
## ---------------------------------------------------------------------

as.data.frame(g1)

## ---------------------------------------------------------------------
## CONCATENATION
## ---------------------------------------------------------------------

## ---------------------------------------------------------------------
## COMPARING & ORDERING
## ---------------------------------------------------------------------

## ---------------------------------------------------------------------
## ADJACENCY MATRIX
## ---------------------------------------------------------------------

adjacencyMatrix(g1)
}
\keyword{methods}
\keyword{classes}
