
named <- function(x) {
  set_names(x, names2(x))
}
named_list <- function(...) {
  named(list(...))
}
