\name{makeDGraphFromGO}

\alias{makeDGraphFromGO}

\title{Import the GO graph as a DGraph object}

\description{
  \code{makeDGraphFromGO} imports the graph of GO terms as
  a \link{DGraph} object.
}

\usage{
makeDGraphFromGO(godb)
}

\arguments{
  \item{godb}{
    Typically the \code{GO.db} object from the \pkg{GO.db} package.

    Can be missing, in which case the \pkg{GO.db} package must be
    installed and the \code{GO.db::GO.db} object is used.
  }
}

\value{
  A \link{DGraph} object representing the GO graph. The nodes are the GO
  terms and the edges are directed from child to parent.
}

\author{Hervé Pagès}

\references{
  \url{http://geneontology.org/}
}

\seealso{
  \itemize{
    \item \link{DGraph} objects.

    \item The \pkg{\link[GO.db]{GO.db}} package.
  }
}

\examples{
gograph <- makeDGraphFromGO()
gograph

nodes(gograph)  # all the GO terms

## The nodes are annotated with the name, ontology, and definition
## of each term:
mcols(nodes(gograph))

## Number of terms in each sub-ontology:
table(mcols(nodes(gograph))$ontology)

## Number of parents of each node:
out_deg <- outDegree(gograph)
table(out_deg)  # 1 node has no parents

## The GO.db package introduces an artifical term called "all"
## that is the common parent of the three sub-ontologies:
nodes(gograph)[out_deg == 0]

## To find the direct children of the "all" term, we select the edges
## that go **to** that node. This shows us the top-level term of each
## sub-ontology:
gograph[toNode(gograph) == "all"]
fromNode(gograph[toNode(gograph) == "all"])

## Note that the "all" term has no parent:
gograph[fromNode(gograph) == "all"]

## Thanks to the "all" term the graph is connected:
isConnected(gograph)

## After removing the 3 edges that go to the "all" term, the graph
## is no longer connected (it has 3 connected components):
gograph2 <- gograph[toNode(gograph) != "all"]
connComp(gograph2)

## Comparison with a graphNEL-based solution:
gognel <- as(gograph, "graphNEL")  # takes about 80 sec.
gognel

head(nodes(gognel))
head(nodes(gograph))           # nodes are annotated

object.size(gognel)            # 185.5 MB
object.size(gograph)           # 18.5 MB

deg0 <- degree(gognel)         # takes about 0.2 sec.
out_deg <- outDegree(gograph)  # takes about 0.01 sec.
in_deg <- inDegree(gograph)    # takes about 0.01 sec.
stopifnot(all.equal(deg0$outDegree, out_deg))
stopifnot(all.equal(deg0$inDegree, in_deg))

\donttest{
  isConnected(gognel)          # takes about 1 h
  isConnected(gograph)         # takes < 0.2 sec
}
}
\keyword{manip}
