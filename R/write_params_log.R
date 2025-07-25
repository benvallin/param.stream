#' Write parameters log file
#'
#' @param params character vector of object names. All objects should be defined in the global environment and bound to atomic vectors.
#' @param out_dir_path character vector of length 1 representing the output directory where the params.log file should be stored.
#' @param params_log_table logical vector of length 1 indicating if the params log table should be returned.
#'
#' @return a params.log file written in <out_dir_path> and optionally the corresponding params log table.
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
#' # Delete example parameters log file
#' file.remove("~/params.log")
#'
write_params_log <- function(params, out_dir_path, params_log_table = TRUE) {

  # Check that out_dir_path is a valid and existing path
  if(!is.character(out_dir_path) ||
     length(out_dir_path) != 1L) {

    stop("Invalid out_dir_path argument.",
         call. = FALSE)

  }

  if(!dir.exists(out_dir_path)) {

    stop("out_dir_path does not exist.",
         call. = FALSE)

  }

  # Append trailing "/" to out_dir_path if missing
  out_dir_path <- ifelse(grepl(pattern = "^.*/$", x = out_dir_path),
                         out_dir_path,
                         paste0(out_dir_path, "/"))

  # Check that params_log_table is valid
  if(!is.logical(params_log_table) ||
     is.na(params_log_table) ||
     length(params_log_table) != 1L) {

    stop("Invalid params_log_table argument.",
         call. = FALSE)

  }

  # Build params log
  uneval_params <- substitute(params)

  if(is.call(uneval_params)) {

    params <- substitute(params)
  }

  current_log <- do.call(what = make_params_log, args = list(params))

  current_log_params <- current_log[, c("nm", "val")]

  # Check if params.log file exists already
  params.log_exists <- file.exists(paste0(out_dir_path, "params.log"))

  # If params.log file does not exist, assign log ID to current log and write it to params.log file
  if(!params.log_exists) {

    current_log$log_id <- make_log_id()

    message("Creating params.log with first log.")

    utils::write.table(x = current_log,
                       file = paste0(out_dir_path, "params.log"),
                       row.names = FALSE,
                       col.names = TRUE)

    if(params_log_table) {

      return(current_log_params)

    } else {

      return(invisible(NULL))

    }

  }

  # If params.log file exists, first check if current log is identical to one of previous logs
  if(params.log_exists) {

    previous_log <- utils::read.table(file = paste0(out_dir_path, "params.log"),
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

  }

  # If current log is identical to one of previous logs, update params.log file with a new timestamp for the previous log
  if(any(previous_is_current)) {

    log_id <- names(previous_is_current[previous_is_current])

    previous_log[[log_id]]$timestamp <- current_log$timestamp

    updated_log <- do.call(what = rbind,
                           args = c(previous_log, list(make.row.names = FALSE)))

    updated_log <- updated_log[order(updated_log$timestamp),]

    message("Updating params.log with new timestamp.")

    utils::write.table(x = updated_log,
                       file = paste0(out_dir_path, "params.log"),
                       row.names = FALSE,
                       col.names = TRUE,
                       append = FALSE)

    if(params_log_table) {

      return(current_log_params)

    } else {

      return(invisible(NULL))

    }

  }

  # Otherwise, assign log ID to current log and append it to params.log file
  previous_log_ids <- vapply(X = previous_log,
                             FUN = function(x) unique(x$log_id),
                             FUN.VALUE = NA_character_)

  log_id <- make_log_id()

  while(log_id %in% previous_log_ids) {

    log_id <- make_log_id()

  }

  current_log$log_id <- log_id

  message("Appending new log to params.log.")

  utils::write.table(x = current_log,
                     file = paste0(out_dir_path, "params.log"),
                     row.names = FALSE,
                     col.names = FALSE,
                     append = TRUE)

  if(params_log_table) {

    return(current_log_params)

  } else {

    return(invisible(NULL))

  }

}
