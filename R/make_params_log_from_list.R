make_params_log_from_list <- function(params_list) {

  # Check that params_list is a named list of atomic vectors
  if(!is.list(params_list) ||
     is.null(names(params_list)) ||
     !all(vapply(X = params_list,
                 FUN = function(x) is.atomic(x) | is.null(x),
                 FUN.VALUE = NA))) {

    stop("Invalid params_list argument.",
         "\nInput params_list is not a named list of atomic or NULL vectors.",
         call. = FALSE)

  }

  # Build params log
  log_params <- lapply(X = params_list,
                       FUN = function(x) {

                         if(is.null(x)) {

                           x <- "NULL"

                         }

                         paste0(x, collapse = "_")

                       })

  log_params <- log_params[order(names(log_params))]

  timestamp <- format(Sys.time(), "%Y.%m.%d_%H:%M:%S")

  data.frame(timestamp = timestamp,
             nm = names(log_params),
             val = unlist(log_params),
             row.names = NULL)

}
