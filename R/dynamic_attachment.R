
dynamic_attachment_env <- new.env()

dynamic_attachment_env$locate_section <- function(f, search_str) {
  locate_section(body(f), search_str)
}

dynamic_attachment_env$get_section <- function(f, tloc) {
  get_section(body(f), tloc)
}

dynamic_attachment_control_env <- new.env()

dynamic_attachment_name <- "patch:dynamic_attachment"

# this is to check the dynamic attachment
is_dynamic_attachment_done <- function() {
  !is.na(match(dynamic_attachment_name, search()))
}

# this is to clear the dynamic attachment
clear_dynamic_attachment <- function() {
  if (is_dynamic_attachment_done()) {
    try(detach(dynamic_attachment_name, pos = 2L, character.only = T), silent = TRUE)
  }
}

# these items should be removed once the package is unloaded/detached
# what_to_add can be configured in different case.
# here it is only two functions to add
dynamic_attachment <- function(what_to_add = NULL, toggle = TRUE) {
  if (!is_dynamic_attachment_done()) {
    attach(dynamic_attachment_env, pos = 2L, name = dynamic_attachment_name)
    cat(
      "\nNote:",
      "locate_section and get_section added to the search path (visible to",
      "you) \n(modified to work directly on function instead of the body)",
      "\nIt will be automatically cleared when you unload or dettach main ",
      "package. \nYou can manually clear it also by calling <me> again.",
      "\n"
    )
  } else {
    if (toggle) clear_dynamic_attachment()
  }
}
