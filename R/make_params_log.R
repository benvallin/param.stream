make_params_log <- function(params) {

  # If params is a call, check the validity of params names
  uneval_params <- substitute(params)

  if(is.call(uneval_params)) {

    params_nm_not_valid <- vapply(X = uneval_params[2:length(uneval_params)],
                                  FUN = function(x) !check_valid_name(x),
                                  FUN.VALUE = NA)

    if(sum(params_nm_not_valid) > 0L) {

      stop("Invalid params argument.",
           "\nInput params contains element(s) which are not of type character or are not syntactically valid name(s).",
           call. = FALSE)

    }

  }

  # Check that params is of type character
  if(!is.character(params)) {

    stop("Invalid params argument.",
         "\nInput params is not of type character.",
         call. = FALSE)

  }

  # If params is not a call, check the validity of params names
  params_nm_not_valid <- vapply(X = params,
                                FUN = function(x) !check_valid_name(x),
                                FUN.VALUE = NA)

  if(sum(params_nm_not_valid) > 0L) {

    stop("Invalid params argument.",
         "\nInput params contains element(s) which are not syntactically valid name(s).",
         call. = FALSE)

  }

  # Check that params names are defined in global environment
  params_nm_undefined <- vapply(X = params,
                                FUN = function(x) !exists(x, envir = .GlobalEnv),
                                FUN.VALUE = NA)

  params_nm_undefined <- params[params_nm_undefined]

  if(length(params_nm_undefined) > 0L) {

    stop("Params ", paste0(params_nm_undefined, collapse = ", "), " not defined.",
         call. = FALSE)

  }

  # Check that params names are bound to atomic vectors
  params_val_not_atomic_not_null <- vapply(X = params,
                                           FUN = function(x) {

                                             p <- eval(as.symbol(x))

                                             !is.atomic(p) & !is.null(p)

                                           },
                                           FUN.VALUE = NA)

  params_val_not_atomic_not_null <- params[params_val_not_atomic_not_null]

  if(length(params_val_not_atomic_not_null) > 0L) {

    stop("Params ", paste0(params_val_not_atomic_not_null, collapse = ", "), " not of atomic type or NULL.",
         call. = FALSE)

  }

  # Build params log
  params <- params[order(params)]

  output <- lapply(X = params,
                   FUN = function(x) {

                     p <- eval(as.symbol(x))

                     if(is.null(p)) {

                       "NULL"

                     } else {

                       as.character(eval(as.symbol(x),
                                         envir = .GlobalEnv))

                     }
                   })

  output <- lapply(X = output,
                   FUN = function(x) {

                     ifelse(length(x) > 1L,
                            paste0(x, collapse = "_"),
                            x)

                   })

  output <- unlist(output)

  timestamp <- format(Sys.time(), "%Y.%m.%d_%H:%M:%S")

  data.frame(timestamp = timestamp,
             nm = params,
             val = output)

}
