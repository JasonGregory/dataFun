#' Generate Exploration Report
#'
#' This function generates a report to explore the data
#'
#' @param input_data Data
#' @param output_file Name for output file
#' @param output_format Format for file, PDF or HTML
#' @param output_dir Directory to save the output file
#' @export
exploreIt <- function(input_data, output_file = "exploreIt.html", output_format = "html", output_dir = getwd(), ...) {
  ## Get argument list
  args <- as.list(match.call())
  ## Get directory of report markdown template
  if (output_format == "pdf") {
    report_dir <- system.file("rmd_template/exploreItpdf.rmd", package = "dataFun")
    output_file <- "exploreIt.pdf"
  } else {
    report_dir <- system.file("rmd_template/exploreIt.rmd", package = "dataFun")
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

#' Generate a Correlation Matrix
#'
#' Produces correlation matrix
#'
#' @param dta Data
#' @param pch Pch
#' @export
e.cormat <- function(dta, pch='.') pairs.panels(dta, gap=0, ellipses=F, pch=pch)
