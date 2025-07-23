#' Get parameters log table from log ID
#'
#' @param log_id character vector of length 1 representing the log ID for which parameters values should be retrieved.
#' @param in_dir_path character vector of length 1 representing the input directory the params.log file should be read from.
#'
#' @return the params log table matching <log_id> in the params.log file at <in_dir_path>.
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
#' # Get log ID from parameters values
#' params_log_id <- get_params_log_id(params = params, in_dir_path = "~")
#'
#' # Get parameters log table from log ID
#' get_params_log_table(log_id = params_log_id, in_dir_path = "~")
#'
#' # Delete example parameters log file
#' file.remove("~/params.log")
#'
get_params_log_table <- function(log_id, in_dir_path) {

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

  # Check that log_id is valid
  if(!is.character(log_id) ||
     is.na(log_id) ||
     length(log_id) != 1L) {

    stop("Invalid log_id argument.",
         call. = FALSE)

  }

  # Read previous logs
  previous_log <- utils::read.table(file = paste0(in_dir_path, "params.log"),
                                    header = TRUE,
                                    colClasses = "character",
                                    stringsAsFactors = FALSE)

  # Check that there exists a record for the input log_id in the params.log file
  if(!log_id %in% previous_log$log_id) {

    stop("No record for input log_id in params.log file",
         call. = FALSE)

  }

  # Return requested params log table
  previous_log[previous_log$log_id == log_id,]

}
