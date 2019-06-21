### =========================================================================
### UGraph objects (undirected graphs)
### -------------------------------------------------------------------------


setClass("UGraph", contains="DGraph")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

UGraph <- function(nodes, from=integer(0), to=integer(0), ...)
{
    new2("UGraph", DGraph(nodes, from, to, ...), check=FALSE)
}

