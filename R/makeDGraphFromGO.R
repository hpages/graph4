### =========================================================================
### makeDGraphFromGO()
### -------------------------------------------------------------------------

.dbselect <- function(dbconn, columns, from)
{
    SQL <- sprintf("SELECT %s FROM %s", paste0(columns, collapse=","), from)
    DBI::dbGetQuery(dbconn, SQL)
}

.fetch_GO_terms <- function(godb)
{
    ## Expected columns are "_id", "go_id", "term", "ontology",
    ## and "definition". Column "term" actually contains the **name**
    ## of the term.
    EXPECTED_COLS <- c("_id", "go_id", "term", "ontology", "definition")
    terms <- .dbselect(godb$conn, "*", "go_term")
    if (!identical(colnames(terms), EXPECTED_COLS))
        stop(wmsg("SQL table 'go_term' does not have the expected columns"))
    colnames(terms)[3L] <- "name"
    terms
}

.fetch_GO_parents <- function(godb, table)
{
    ## Expected columns are "_id", "_parent_id", and "relationship_type".
    EXPECTED_COLS <- c("_id", "_parent_id", "relationship_type")
    parents <- .dbselect(godb$conn, "*", table)
    if (!identical(colnames(parents), EXPECTED_COLS))
        stop(wmsg("SQL table '", table, "' does not have the expected columns"))
    parents
}

### Return a GOdb object.
.normarg_godb <- function(godb)
{
    if (!requireNamespace("AnnotationDbi", quietly=TRUE))
            stop(wmsg("Couldn't load the AnnotationDbi package. ",
                      "The package is needed by makeDGraphFromGO(). ",
                      "Please install it and try again."))
    if (missing(godb)) {
        pkgenvir <- try(loadNamespace("GO.db"), silent=TRUE)
        if (inherits(pkgenvir, "try-error"))
            stop(wmsg("Couldn't load the GO.db package. Please install ",
                      "the GO.db package and try again."))
        godb <- pkgenvir[["GO.db"]]
        if (!is(godb, "GODb"))
            stop(wmsg("invalid GO.db package (GO.db object ",
                      "missing or is not a GODb object)"))
    } else {
        if (!is(godb, "GODb"))
            stop(wmsg("'godb' must be a GODb object"))
    }
    godb
}

### makeDGraphFromGO() requires packages DBI and AnnotationDbi (both are
### suggested by graph4). In addition, if 'godb' is not specified, it also
### requires the GO.db package (also suggested by graph4).
### When specified, 'godb' must be a GOdb object (the GOdb class is defined
### in the AnnotationDbi package).
makeDGraphFromGO <- function(godb)
{
    godb <- .normarg_godb(godb)  # GOdb object

    terms <- .fetch_GO_terms(godb)
    bp_parents <- .fetch_GO_parents(godb, "go_bp_parents")
    mf_parents <- .fetch_GO_parents(godb, "go_mf_parents")
    cc_parents <- .fetch_GO_parents(godb, "go_cc_parents")
    parents <- rbind(bp_parents, mf_parents, cc_parents)

    from <- match(parents[ , "_id"], terms[ , "_id"])
    to <- match(parents[ , "_parent_id"], terms[ , "_id"])
    relation_levels <- c("is_a", "part_of", "regulates",
                         "negatively_regulates", "positively_regulates")
    relation <- factor(parents[ , "relationship_type"], levels=relation_levels)

    nodes <- AnnotatedIDs(terms[ , 2], terms[ , -(1:2)])
    DGraph(nodes, from=from, to=to, relation=relation)
}

