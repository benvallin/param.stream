make_log_id <- function(size = 10) {

  paste0(sample(c(letters, LETTERS, 0:9),
                size = size,
                replace = TRUE),
         collapse = "")

}
