#' Describe Data
#'
#' This function adds to the describe function in the psych pacakage
#' @param dta Data being loaded
#' @return A data frame
#' @export
p.summary <- function(dta) {
  dta[dta==""] <- NA
  tmp <- describe(dta)
  ncol <- ncol(tmp)
  tmp$nrow <- dim(dat)[1]
  tmp$ncol <- dim(dat)[2]
  tmp$distn <- apply(dta, 2, function(x) length(unique(x)))
  tmp$distperc <- apply(dta, 2, function(x) round(length(unique(x))/length(x),3))
  tmp$nulln <- apply(dta, 2, function(x) sum(I(is.na(x))*1))
  cbind(tmp[1:2], distn = tmp$distn, distperc = tmp$distperc, nulln = tmp$nulln, tmp[3:ncol], nrow = tmp$nrow, ncol = tmp$ncol)
}


#' Detailed Data Structure
#'
#' Shows a more custom view of the data
#' @param dta Data
#' @param max_levels Limits factors that are compared.
#' @export
p.structure <- function (dta, max_levels = .1) {
  cat("All Variables\n")
  print(colnames(dta))

  cat("Unique Variables\n")
  print(p.unique(dta))

  cat("Numeric\n")
  print(p.numeric(dta))

  cat("Numeric Continuous\n")
  print(p.continuous(dta, max_levels))

  cat("Numeric Discrete\n")
  print(p.discrete(dta, max_levels))

  cat("Factors\n")
  print(p.factor(dta))

  cat("Integer\n")
  print(p.integer(dta))

  cat("Character\n")
  print(p.character(dta))
}

#' Convert to Factor
#'
#' @param dta Data
#' @param columns Fields
#' @export
p.asFactor <- function(dta, columns = p.discrete(dta)) {
  dta[columns] <- lapply(dta[columns], factor)
  dta
}

#' Table Summary
#'
#' Generates Table Summaries of Variables
#'
#' @param dta Data
#' @param max_levels Limits data
#' @export
p.table <- function(dta, max_levels = 100) {
  tmp <- p.asFactor(dta)
  tmp <- cbind(sapply(tmp,is.factor), apply(tmp, 2, function(x) length(unique(x))))
  nrows <- sum(I(tmp[,1] == 1 & tmp[,2] <= max_levels))

  for(i in 1:nrows){
    cat(rownames(tmp[tmp[,1] == 1 & tmp[,2] <= max_levels,])[i])
    print(table(dta[rownames(tmp[tmp[,1] == 1 & tmp[,2] <= max_levels,])[i]]))
    cat("\n")
  }
}

#' Replace Null Values
#'
#' Function generates plots to describe null values
#'
#' @param dta Data
#' @param columns Fields
#' @param null_value Value to populate for null fields
#' @export
p.replaceNull <- function (dta, columns, null_value = "Missing") {
  dta[columns] <-
    lapply(dta[columns], function(xx){
      if (sum(I(is.na(xx))) > 0) {
      levels(xx) <- c(levels(xx), null_value)
      xx[is.na(xx)] <- null_value
      }
      xx
    }
    )
  dta[columns]
}

#' Remove Variables
#'
#' @param dta Data
#' @param columns Fields
#' @export
p.removeVariables <- function (dta, columns) {
  within(dta, rm(list = columns))
}

#' Numeric Variables
#'
#' @param dta Data
#' @export
p.numeric <- function (dta) {
  tmp <- as.matrix(sapply(dta,is.numeric))
  names(tmp[tmp,])
}

#' Numeric Continuous Variables
#'
#' @param dta Data
#' @param max_levels Ratio of distinct values
#' @export
p.continuous <- function (dta, max_levels = .1) {
  tmp <- apply(dta, 2, function(x) round(length(unique(x))/length(x),3))
  tmp <- as.matrix(sapply(dta[names(tmp[tmp>=max_levels])], is.numeric))
  names(tmp[tmp,])
}

#' Numeric Discrete Variables
#'
#' @param dta Data
#' @param max_levels Ratio of distinct values
#' @export
p.discrete <- function (dta, max_levels = .1) {
  tmp <- apply(dta, 2, function(x) round(length(unique(x))/length(x),3))
  tmp <- as.matrix(sapply(dta[names(tmp[tmp<max_levels])], is.numeric))
  names(tmp[tmp,])
}

#' Integer Variables
#'
#' @param dta Data
#' @export
p.integer <- function (dta) {
  tmp <- as.matrix(sapply(dta,is.integer))
  names(tmp[tmp,])
}

#' Character Variables
#'
#' @param dta Data
#' @export
p.character <- function (dta) {
  tmp <- as.matrix(sapply(dta,is.character))
  names(tmp[tmp,])
}

#' Unique Variables
#'
#' @param dta Data
#' @export
p.unique <- function (dta) {
  tmp <- apply(dta, 2, function(x) round(length(unique(x))/length(x),3))
  names(tmp[tmp==1])
}

#' Factor Variables
#'
#' @param dta Data
#' @export
p.factor <- function (dta) {
  tmp <- as.matrix(sapply(dta,is.factor))
  names(tmp[tmp,])
}



#' Generate Prep Report
#'
#' This function generates a report to help prepare the data
#'
#' @param input_data Data
#' @param output_file Name for output file
#' @param output_format Format for file, PDF or HTML
#' @param output_dir Directory to save the output file
#' @export
prepIt <- function(input_data, output_file = "prepIt.html", output_format = "html", output_dir = getwd(), ...) {
  ## Get argument list
  args <- as.list(match.call())
  ## Get directory of report markdown template
  if (output_format == "pdf") {
    report_dir <- system.file("rmd_template/prepItpdf.rmd", package = "dataFun")
    output_file <- "prepIt.pdf"
  } else {
    report_dir <- system.file("rmd_template/prepIt.rmd", package = "dataFun")
  }

  ## Render report into html
  render(
    input = report_dir,
    output_file = output_file,
    output_dir = output_dir,
    intermediates_dir = output_dir,
    params = list(data = input_data, fun_options = list()),
    ...
  )
  ## Open report
  report_path <- file.path(output_dir, output_file)
  browseURL(report_path)
  ## Print report directory
  if (ifelse(is.null(args[["quiet"]]), TRUE, !args[["quiet"]])) message(paste0("\n\nReport is generated at \"", report_path, "\"."))
}

