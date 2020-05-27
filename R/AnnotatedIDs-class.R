### =========================================================================
### AnnotatedIDs objects
### -------------------------------------------------------------------------


### A simple container to wrap an ordinary vector in a Vector object.
### This allows attaching metadata columns to it.
setClass("AnnotatedIDs",
    contains="Vector",
    representation(
        ## Using "vector_OR_Vector" instead of just "vector" allows us to
        ## wrap any Vector derivative at no extra cost.
        ## Note that wrapping data-frame-like objects (e.g. data.frame or
        ## DataFrame) turns it into an AnnotatedIDs object of length its
        ## number of **rows**! As a consequence, the metadata columns of
        ## the AnnotatedIDs object are annotating the **rows** of the
        ## data-frame-like object.
        ID="vector_OR_Vector"
    ),
    prototype(
        ID=integer(0)
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### vertical_slot_names()
###

### Combine the new "vertical slots" with those of the parent class. Make
### sure to put the new vertical slots **first**. See R/bindROWS.R file in
### the S4Vectors package for what slots should or should not be considered
### "vertical".
setMethod("vertical_slot_names", "AnnotatedIDs",
    function(x) c("ID", callNextMethod())
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

### FIXME: Currently broken if 'IDs' is a DataFrame object!
AnnotatedIDs <- function(IDs=integer(0), ...)
{
    mcols <- DataFrame(..., check.names=FALSE)
    if (is(IDs, "AnnotatedIDs")) {
        if (length(mcols) != 0L) {
            IDs_mcols <- mcols(IDs)
            if (length(IDs_mcols) != 0L)
                mcols <- cbind(IDs_mcols, mcols)
            mcols(IDs) <- mcols
        }
        return(IDs)
    }
    ans <- new2("AnnotatedIDs", ID=IDs, check=FALSE)
    if (length(mcols) != 0L)
        mcols(ans) <- mcols
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###

setGeneric("ID", function(x) standardGeneric("ID"))
setMethod("ID", "AnnotatedIDs", function(x) x@ID)

setMethod("names", "AnnotatedIDs", function(x) ROWNAMES(x@ID))

setReplaceMethod("names", "AnnotatedIDs",
    function(x, value)
    {
        if (length(dim(x@ID)) < 2L) {
            names(x@ID) <- value
        } else {
            rownames(x@ID) <- value
        }
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

setMethod("as.vector", "AnnotatedIDs",
    function(x, mode="any") as.vector(x@ID, mode=mode)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

.from_AnnotatedIDs_to_naked_character_matrix_for_display <- function(x)
{
    m <- cbind(ID=showAsCell(x@ID))
    cbind_mcols_for_display(m, x)
}
setMethod("makeNakedCharacterMatrixForDisplay", "AnnotatedIDs",
    .from_AnnotatedIDs_to_naked_character_matrix_for_display
)

.show_AnnotatedIDs <- function(x, margin="", print.classinfo=FALSE)
{
    x_len <- length(x)
    x_mcols <- mcols(x, use.names=FALSE)
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    cat(classNameForDisplay(x), " object of length ", x_len, " with ",
        x_nmc, " metadata ", ifelse(x_nmc == 1L, "column", "columns"),
        ":\n", sep="")
    ## makePrettyMatrixForCompactPrinting() assumes that head() and tail()
    ## work on 'x'.
    out <- makePrettyMatrixForCompactPrinting(x)
    if (print.classinfo) {
        .COL2CLASS <- c(
            ID=classNameForDisplay(x@ID)
        )
        classinfo <- makeClassinfoRowForCompactPrinting(x, .COL2CLASS)
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

setMethod("show", "AnnotatedIDs",
    function(object)
        .show_AnnotatedIDs(object, print.classinfo=TRUE)
)

setMethod("showAsCell", "AnnotatedIDs", function(object) showAsCell(object@ID))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Comparing and ordering
###

setMethod("pcompare", c("AnnotatedIDs", "ANY"),
    function(x, y) pcompare(x@ID, y)
)
setMethod("pcompare", c("ANY", "AnnotatedIDs"),
    function(x, y) pcompare(x, y@ID)
)
setMethod("pcompare", c("AnnotatedIDs", "AnnotatedIDs"),
    function(x, y) pcompare(x@ID, y@ID)
)

setMethod("match", c("AnnotatedIDs", "AnnotatedIDs"),
    function(x, table, nomatch=NA_integer_, incomparables=NULL, ...)
    {
        x <- x@ID
        table <- table@ID
        callGeneric()
    }
)

setMethod("selfmatch", "AnnotatedIDs", function(x, ...) selfmatch(x@ID, ...))

setMethod("xtfrm", "AnnotatedIDs", function(x) xtfrm(x@ID))

