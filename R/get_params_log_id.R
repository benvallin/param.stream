#' Get parameters log ID
#'
#' @param params character vector of object names. All objects should be defined in the global environment and bound to atomic vectors.
#' @param in_dir_path character vector of length 1 representing the input directory the params.log file should be read from.
#'
#' @return the log ID matching <params> in the params.log file at <in_dir_path>.
#' @export
#'
#' @examples
#' # Define analysis parameters
#' input_count <- "tpm_lengthScaledTPM"
#' model_formula <- "~ n_gene_on + exposure + (1 | line_name)"
#' cell_type <- "neuron"
#' lines <- c("B856", "B156", "B067")
#' protein_coding_only <- TRUE
#' min_cnt_excl <- 0
#' min_freq_incl <- 0.2
#' padj <- 0.05
#'
#' params <- c("input_count", "model_formula", "cell_type", "lines",
#'             "protein_coding_only", "min_cnt_excl", "min_freq_incl", "padj")
#'
#' # Write parameters log file
#' write_params_log(params = params, out_dir_path = "~")
#'
#' # Get parameters log ID
#' get_params_log_id(params = params, in_dir_path = "~")
#'
#' # Delete example parameters log file
#' file.remove("~/params.log")
#'
get_params_log_id <- function(params, in_dir_path) {

  # Check that in_dir_path is a valid and existing path
  if(!is.character(in_dir_path) ||
     length(in_dir_path) != 1L) {

    stop("Invalid in_dir_path argument.",
         call. = FALSE)

  }

  if(!dir.exists(in_dir_path)) {

    stop("in_dir_path does not exist.",
         call. = FALSE)

  }

  # Append trailing "/" to in_dir_path if missing
  in_dir_path <- ifelse(grepl(pattern = "^.*/$", x = in_dir_path),
                        in_dir_path,
                        paste0(in_dir_path, "/"))

  # Check that params.log file exists in in_dir_path
  if(!file.exists(paste0(in_dir_path, "params.log"))) {

    stop("params.log file not found in in_dir_path.",
         call. = FALSE)

  }

  # Build params log
  uneval_params <- substitute(params)

  if(is.call(uneval_params)) {

    params <- substitute(params)
  }

  current_log <- do.call(what = make_params_log, args = list(params))

  current_log_params <- current_log[, c("nm", "val")]

  # Check if current log is identical to one of previous logs
  previous_log <- utils::read.table(file = paste0(in_dir_path, "params.log"),
                                    header = TRUE,
                                    colClasses = "character",
                                    stringsAsFactors = FALSE)

  previous_log <- split(x = previous_log,
                        f = previous_log$log_id)

  previous_log_params <- lapply(X = previous_log,
                                FUN = function(x) {

                                  rownames(x) <- NULL
                                  x[, c("nm", "val")]

                                })

  previous_is_current <- vapply(X = previous_log_params,
                                FUN = function(x) {

                                  identical(x, current_log_params)

                                },
                                FUN.VALUE = NA)

  if(!any(previous_is_current)) {

    stop("No record for input params in params.log file",
         call. = FALSE)

  }

  # Return log ID for input params
  names(previous_is_current[previous_is_current])

}
