### =========================================================================
### UGraph objects (undirected graphs)
### -------------------------------------------------------------------------


setClass("UGraph", contains="DGraph")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

UGraph <- function(from=integer(0), to=integer(0), nodes=0, ...)
{
    new2("UGraph", DGraph(from, to, nodes, ...), check=FALSE)
}

