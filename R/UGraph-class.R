### =========================================================================
### UGraph objects
### -------------------------------------------------------------------------


setClass("UGraph", contains="Graph")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

UGraph <- function(from=integer(0), to=integer(0), nodes=0, ...)
{
    new2("UGraph", Graph(from, to, nodes, ...), check=FALSE)
}

