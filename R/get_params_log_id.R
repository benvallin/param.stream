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
  params <- substitute(params)

  current_log <- do.call(what = make_params_log, args = list(params))

  current_log_params <- current_log[, c("nm", "val")]

  # Check if current log is identical to one of previous logs
  previous_log <- utils::read.table(file = paste0(in_dir_path, "params.log"),
                                    header = TRUE)

  previous_log <- split(x = previous_log,
                        f = previous_log$log_id)

  previous_log_params <- lapply(X = previous_log,
                                FUN = function(x) x[, c("nm", "val")])

  previous_is_current <- vapply(X = previous_log_params,
                                FUN = function(x) {

                                  rownames(x) <- NULL
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
