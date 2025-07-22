check_valid_name <- function(nm) {

  (is.character(nm) && !is.na(nm)) &&
    make.names(names = nm) == nm &&
    !grepl(pattern = "^\\.", x = nm)

}
