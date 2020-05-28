\name{DGraph-class}
\docType{class}

\alias{class:DGraph}
\alias{DGraph-class}
\alias{DGraph}

\alias{numNodes}
\alias{numNodes,DGraph-method}
\alias{numEdges}
\alias{numEdges,DGraph-method}
\alias{nodes}
\alias{nodes,DGraph-method}
\alias{nodes<-}
\alias{nodes<-,DGraph,ANY-method}
\alias{fromNode}
\alias{fromNode,DGraph-method}
\alias{toNode}
\alias{toNode,DGraph-method}
\alias{outDegree}
\alias{outDegree,DGraph-method}
\alias{inDegree}
\alias{inDegree,DGraph-method}
\alias{degree}
\alias{degree,ANY-method}
\alias{isDirected}
\alias{isDirected,DGraph-method}
\alias{edgemode}
\alias{edgemode,ANY-method}
\alias{edgemode<-}
\alias{edgemode<-,DGraph,ANY-method}
\alias{edgeMatrix}
\alias{edgeMatrix,DGraph-method}

\alias{coerce,SelfHits,DGraph-method}
\alias{coerce,DGraph,DFrame-method}

\alias{summary.DGraph}
\alias{summary,DGraph-method}
\alias{show,DGraph-method}

\alias{adjacencyMatrix}
\alias{adjacencyMatrix,DGraph-method}
\alias{coerce,DGraph,ngCMatrix-method}
\alias{coerce,ngCMatrix,DGraph-method}

\alias{connComp}
\alias{connComp,SelfHits-method}
\alias{isConnected}
\alias{isConnected,ANY-method}

\alias{coerce,graphNEL,DGraph-method}
\alias{coerce,DGraph,graphNEL-method}

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
    in which case it will get automatically wrapped in an
    \link{AnnotatedIDs} object (with \code{AnnotatedIDs(nodes)}).

    Passing a single number to \code{nodes} is treated as a special
    case: in that case the nodes get automatically generated with
    \code{AnnotatedIDs(seq_len(nodes))}.
  }
  \item{from, to}{
    Two \emph{parallel} integer vectors describing the edges. By
    \emph{parallel} we mean that the two vectors must have the same
    length. The values in each vector must be valid node indices.
    For each \code{i} between 1 and N (where N is the common length
    of the \code{from} and \code{to} vectors), the pair formed by
    \code{from[i]} and \code{to[i]} represents the i-th edge, which
    is an edge that goes from node \code{nodes[from[i]]} to node
    \code{nodes[to[i]]}.

    Note that there can be an arbitrary number of edges going in
    both directions between two given nodes.
  }
  \item{...}{
    Optional \emph{edge attributes}. These must be vector-like objects
    \emph{parallel} to the \code{from} and \code{to} vectors. They will
    be stored as the metadata columns of the DGraph object.
  }
}

\details{
  TODO
}

\value{
  A DGraph object.
}

\section{Accessors}{

  \subsection{Getters}{
    Because DGraph objects are \link[S4Vectors]{Vector} derivatives,
    they support the \code{length()}, \code{mcols()}, and \code{metadata()}
    getters.
    Note that the length of the object is its number of edges and
    the \code{mcols()} getter returns the \emph{edge attributes}
    in a \link[S4Vectors]{DataFrame} object with 1 row per edge
    and 1 column per attribute.

    In addition DGraph objects support the following graph-specific getters:
    \itemize{
      \item \code{nodes(x)}: Get the nodes of \code{x}.

      \item \code{nnode(x)} and \code{numNodes(x)}: Both return the number
            of nodes in \code{x} (i.e. \code{length(nodes(x))}).
            The \code{numNodes} getter is provided for compatibility with
            the \pkg{graph} package.

      \item \code{from(x)} and \code{to(x)}: Get the \emph{from index} and
            \emph{to index} for each edge.
            More precisely, both getters return an integer vector
            that is \emph{parallel} to \code{x} i.e. with one element per edge
            in \code{x}. The values in the 2 vectors are \emph{node indices}.
            \code{from(x)[i]} and \code{to(x)[i]} are the indices of the
            \emph{from} and \emph{to} nodes of the i-th edge, respectively.
            Said otherwise: the i-th edge goes from node
            \code{nodes(x)[from(x)[i]]} to \code{nodes(x)[to(x)[i]]}.

      \item \code{edgeMatrix(x)}: Returns a 2-row matrix with 1 column per
            edge. The 1st row of the matrix contains the \emph{from indices}
            and its 2nd row contains the \emph{to indices}.

      \item \code{fromNode(x)} and \code{toNode(x)}: Get the \emph{from
            node} and \emph{to node} for each edge.
            More precisely, both getters return a vector of nodes
            that is \emph{parallel} to \code{x}. \code{fromNode(x)[i]}
            and \code{toNode(x)[i]} are the \emph{from} and \emph{to}
            nodes of the i-th edge, respectively.
            More formally, \code{fromNode(x)} and \code{toNode(x)} are
            just doing \code{nodes(x)[from(x)]} and \code{nodes(x)[to(x)]}.

      \item \code{isDirected(x)}: Indicates whether the object is
            \emph{directed} or not.

      \item \code{edgemode(x)}: Returns \code{"directed"} or
            \code{"undirected"}.
    }
  }

  \subsection{Setters}{
    Because DGraph objects are \link[S4Vectors]{Vector} derivatives,
    they support the \code{mcols()} and \code{metadata()} setters.

    In addition they support the following setters:
    \itemize{
      \item \code{nodes(x) <- value}: Replace the current nodes.

      \item \code{edgemode(x) <- value}: Set the \emph{edgemode}.
    }
  }
}

\section{Subsetting}{
  A DGraph object can be subsetted with \code{[}, like any vector-like
  object. Note that this only subsets the set of edges i.e. all the nodes
  are preserved.
}

\section{Coercion}{
  TODO
}

\section{Adjacency matrix}{
  TODO
}

\section{Connected components}{
  TODO
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

\section{Compatibility with graphNEL objects}{
  \link[graph]{graphNEL} objects are defined in the \pkg{graph} package.
  The \pkg{graph4} package provides coercion methods for coercing a
  DGraph object to \link[graph]{graphNEL} and vice-versa.
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

length(dg0)     # number of edges
nodes(dg0)
nnode(dg0)
outDegree(dg0)
inDegree(dg0)

## A directed graph with 8 nodes and 9 edges:
dg1 <- DGraph(nodes=8, c(1, 1:5, 6, 6, 6), c(6, 8:4, 1:2, 7),
                       weights=0.11*(1:9))
dg1

length(dg1)     # number of edges
mcols(dg1)      # metadata columns on the edges (1 row per edge)

nodes(dg1)      # the nodes
nnode(dg1)      # number of nodes (same as 'numNodes(dg1)')
from(dg1)
to(dg1)
outDegree(dg1)  # nb of edges starting from each node
inDegree(dg1)   # nb of edges ending on each node
degree(dg1)     # outDegree(dg1) + inDegree(dg1)

edgeMatrix(dg1)
fromNode(dg1)
toNode(dg1)

## Set some names and metadata columns on the nodes:
names(nodes(dg1)) <- LETTERS[1:8]
mcols(nodes(dg1))$score <- runif(8)
nodes(dg1)
degree(dg1)

## ---------------------------------------------------------------------
## COERCION
## ---------------------------------------------------------------------

## Coercing to data.frame or DataFrame looses the node information:
as.data.frame(dg1)
as(dg1, "DataFrame")

## ---------------------------------------------------------------------
## ADJACENCY MATRIX
## ---------------------------------------------------------------------

adjm <- adjacencyMatrix(dg1)
adjm

as(adjm, "DGraph")

## Note that if 'x' is a square ngCMatrix object,
## 'adjacencyMatrix(as(x, "DGraph"))' is guaranted to be
## identical to 'x' (modulo the dimnames).
stopifnot(identical(adjacencyMatrix(as(adjm, "DGraph")), adjm))

## ---------------------------------------------------------------------
## CONNECTED COMPONENTS
## ---------------------------------------------------------------------

connComp(dg1)

## ---------------------------------------------------------------------
## CONCATENATION
## ---------------------------------------------------------------------

## TODO

## ---------------------------------------------------------------------
## COMPARING & ORDERING
## ---------------------------------------------------------------------

## TODO

## ---------------------------------------------------------------------
## COMPATIBILITY WITH graphNEL OBJECTS
## ---------------------------------------------------------------------

## Coerce to a graphNEL instance from the graph package:
gnel1 <- as(dg1, "graphNEL")
gnel1

## The graphNEL instance can be used to plot the graph with the
## Rgraphviz package:
library(Rgraphviz)
plot(agopen(gnel1, "dg1"))
plot(agopen(gnel1, "dg1", recipEdges="distinct"))

## TODO: More coercions to come soon (to DGraph<->graphAM,
## DGraph<->graphBAM, DGraph<->igraph, etc...)
}
\keyword{methods}
\keyword{classes}
