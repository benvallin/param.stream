make_param_log <- function(param_list) {

  # Check that param_list is a named list of atomic vectors
  if(!is.list(param_list) ||
     is.null(names(param_list)) ||
     !all(vapply(X = param_list,
                 FUN = function(x) is.atomic(x) | is.null(x),
                 FUN.VALUE = NA))) {

    stop("Invalid param_list argument.",
         "\nInput param_list is not a named list of atomic or NULL vectors.",
         call. = FALSE)

  }

  # Build params log
  log_params <- lapply(X = param_list,
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
