\name{AnnotatedIDs-class}
\docType{class}

\alias{class:AnnotatedIDs}
\alias{AnnotatedIDs-class}
\alias{AnnotatedIDs}

\alias{vertical_slot_names,AnnotatedIDs-method}

\alias{ID}
\alias{ID,AnnotatedIDs-method}
\alias{names,AnnotatedIDs-method}
\alias{names<-,AnnotatedIDs,ANY-method}

\alias{as.vector,AnnotatedIDs-method}

\alias{show,AnnotatedIDs-method}
\alias{showAsCell,AnnotatedIDs-method}

\alias{pcompare,AnnotatedIDs,ANY-method}
\alias{pcompare,ANY,AnnotatedIDs-method}
\alias{pcompare,AnnotatedIDs,AnnotatedIDs-method}

\alias{match,AnnotatedIDs,AnnotatedIDs-method}
\alias{selfmatch,AnnotatedIDs-method}
\alias{xtfrm,AnnotatedIDs-method}

\title{AnnotatedIDs}

\description{
  The AnnotatedIDs class is a simple container that wraps an ordinary vector
  in a Vector object. This allows attaching metadata columns to it.
}

\usage{
## Constructor function
AnnotatedIDs(IDs=integer(0), ...)
}

\arguments{
  \item{IDs}{
    An ordinary vector.
  }
  \item{...}{
    Optional metadata columns to attach to the returned AnnotatedIDs object.
  }
}

\details{
  TODO
}

\value{
  An AnnotatedIDs object.
}

\section{Accessors}{
  Because AnnotatedIDs objects are \link[S4Vectors]{Vector} derivatives
  (i.e. AnnotatedIDs is a subclass of the \link[S4Vectors]{Vector} class
  defined in the \pkg{S4Vectors} package), they support the same set of
  accessors as \link[S4Vectors]{Vector} objects e.g. the \code{length()},
  \code{names()}, and \code{mcols()} getters, or the \code{names()} and
  \code{mcols()} setters.
}

\section{Subsetting}{
  An AnnotatedIDs object can be subsetted with \code{[}, like any vector-like
  object.
}

\section{Concatenation}{
  Two or more AnnotatedIDs objects can be concatenated with \code{c()}.
  Note that this returns an AnnotatedIDs object of length the sum of the
  objects to concatenate.
}

\section{Comparing & Ordering}{
  Comparison operations between AnnotatedIDs objects (e.g. with \code{==},
  \code{!=}, \code{<=}, \code{<}, \code{match()}) behave consistently with
  comparison of the \emph{ID} objects that are wrapped in the AnnotatedIDs
  objects to compare (and that can be obtained with \code{ID()}).
  More precisely, comparing AnnotatedIDs objects \code{x} and \code{y} is
  equivalent to comparing \code{ID(x)} and \code{ID(y)}.

  \code{order()}), \code{sort()}), and \code{rank()} do the same thing.
}

\author{Hervé Pagès}

\seealso{
  \itemize{
    \item \link[S4Vectors]{Vector} objects in the \pkg{S4Vectors} package
          for the parent class.

    \item \link{DGraph} objects for examples of using an AnnotatedIDs
          object to represent the nodes of a \link{DGraph} object.
  }
}

\examples{
showClass("AnnotatedIDs")  # AnnotatedIDs extends Vector

aids <- AnnotatedIDs(IDs=letters, score=runif(26))
aids

length(aids)
mcols(aids)

ID(aids)
}
\keyword{methods}
\keyword{classes}
