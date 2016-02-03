

# https://github.com/gimoya/theBioBucket-Archives/blob/master/R/txtmining_pdf.R
# http://stackoverflow.com/questions/17563896/readpdf-tm-package-in-r/19926301#19926301
# https://gist.github.com/benmarwick/11333467 downloads
# the file

.onAttach <- function(libname,pkgname)
{
required <<- 0
actualData <<- NULL
}
# http://www.inside-r.org/packages/cran/tm/docs/readPDF
#Required function for every methods
#' Data extrapolation
#'
#' \code{RequiredFunction} returns the data set actualData.
#'
#' @return Returns the average of \code{actualData}.
#'
#' @export
RequiredFunction <- function() {

    #' @import utils
    # devtools::use_package("utils")


    # convert from pdf to txt -----------------
    #' @import tm
    # devtools::use_package("tm")

    # data text
    uri2 <- system.file("extdata", "bulletin2013_14.txt", package = "AverageAge")

    # two cases for having pdftotext and not having pdftotext
      if (all(file.exists(Sys.which(c("pdfinfo", "pdftotext"))))) {
        #download the file
        url <- "http://web.williams.edu/admin/registrar/catalog/bulletin2013_14.pdf"
        dest <- tempfile(fileext = ".pdf")
        download.file(url, dest, mode = "wb")
        #temp file uri
        uri1 <- sprintf("file://%s", dest)
        #convert file
        pdf <- (tm::readPDF(control = list(text = "-layout")))(elem = list(uri = uri1),
              language = "en", id = "id1")
          txt <- toString(pdf[1])
      } else {
       txt <- paste(readLines(uri2), sep="\n", collapse="\n")

     }


    # string manipulation and data extraction --------
    #' @import stringr

    # devtools::use_package("stringr")

    # getting string sets of BA and BS(seperated)
    BaTxt <- stringr::str_match_all(txt, "[:digit:]{4}, (BA|AB|BS)")
    # converting to (lists of strings with just number)
    BaTxt <- stringr::str_match_all(BaTxt, "[:digit:]{4}")

    # converting to numeric

    BaData <- lapply(BaTxt, as.numeric)

    # converting to matrix

    BaMat <- as.matrix(data.frame(BaData))

    # data of prof's age matrix

    actualData <<- 2015 - BaMat + 22
    required <<- 1
}


#calculating the average age

#' Average of faculty ages
#'
#' \code{Average} returns the average of the data set actualData.
#'
#' @return Returns the average of \code{actualData}.
#'
#' @export
Average  <- function() {
  if(required!=1){RequiredFunction()}
 AveAge <- mean(actualData)
 print(AveAge)
}
#calculates the range of faculty ages
#' Range of faculty ages
#'
#' \code{Range} returns the range of the data set actualData.
#'
#' @return Returns the range of \code{actualData}.
#'
#' @export
Range <- function() {
  if(required!=1){RequiredFunction()}
  AgeRange <- max(actualData)-min(actualData)
  print(AgeRange)
}
#calculates the maximum value of faculty ages
#' Maximum value of faculty ages
#'
#' \code{Max} returns the maximum value of the data set actualData.
#'
#' @return Returns the maximum of \code{actualData}.
#'
#' @export
Max <- function() {
  if(required!=1){RequiredFunction()}
  AgeMax <- max(actualData)
  print(AgeMax)
}

#calculates the minimum value of faculty ages
#' Minimum value of faculty ages
#'
#' \code{Min} returns the minimum value of the data set actualData.
#'
#' @return Returns the minimum of \code{actualData}.
#'
#' @export
Min <- function() {
  if(required!=1){RequiredFunction()}
  AgeMin <- min(actualData)
  print(AgeMin)
}

#plots the histogram of faculty ages
#' Histogram  of faculty ages
#'
#' \code{AgeHist} returns the histogram of the data set actualData.
#'
#' @return Returns the histogram of \code{actualData}.
#'
#' @export
PlotHist <- function(){
  if(required!=1){RequiredFunction()}
  AgeHist <- hist(actualData,main="Distribution of Age",xlab="Age")
}

#prints all the statistics of faculty ages
#' Information  of faculty ages
#'
#' \code{AgeHist} returns all the above information of actualData.
#'
#' @return Returns the information of \code{actualData}.
#'
#' @export
PrintAll <- function(){
  if(required!=1){RequiredFunction()}
  print('Average:')
  Average()
  print('Range:')
  Range()
  print('Maximum:')
  Max()
  print('Minimum:')
  Min()

  PlotHist()

}



# data frame?probably matrix BaData<-data.frame(BaTxt)
# BsData<-data.frame(BsTxt)

# good stuff:
# http://stackoverflow.com/questions/9424311/how-to-get-mean-median-and-other-statistics-over-entire-matrix-array-or-dataf

