
#' Locate a section of a function
#'
#' @param fbody Body of a function / (function directly if attached dynamically)
#' @param search_str The string(s) to find in the body
#'
#' @return Recursive list path (if match found) for function body
#' @keywords internal
#' @seealso [`get_section`][get_section()]
#' @examples
#' # this only happens if user attached optional functions dynamically
#' if (exists("locate_section") & exists("get_section")) {
#'   get_section(library, locate_section(library, "logical.return"))
#' }
locate_section <- function(fbody, search_str) {
  fbody_n <- fbody
  tloc <- integer(0)
  repeat{
    fbc <- as.character(fbody_n)
    si <- grepl(search_str, fbc)
    if (any(si)) {
      sit <- which(si)[1]
      tloc <- c(tloc, sit)
      fbody_n <- fbody_n[[sit]]
      if (length(as.list(fbody_n)) == 1) break()
    } else {
      break()
    }
  }
  tloc
}

#' See a section of a function (for development of patch)
#'
#' @param fbody Body of a function / (function directly if attached dynamically)
#' @param tloc Recursive list path as returned by
#' [`locate_section`][locate_section()] or hand typed
#'
#' @return Language object
#' @keywords internal
#' @seealso [`locate_section`][locate_section()]
#'
#' @examples
#' # this only happens if user attached optional functions dynamically
#' if (exists("locate_section") & exists("get_section")) {
#'   get_section(library, locate_section(library, "logical.return"))
#' }
get_section <- function(fbody, tloc) {
  obj <- NULL
  if (length(tloc) > 0) {
    obj <- eval(parse(text = paste0("fbody", paste0("[[", tloc, "]]", collapse = ""))))
  }
  obj
}

replace_in_section <- function(fbody, tloc, expr, env = NULL) {
  fbody_mod <- fbody
  if (length(tloc) > 0) {
    if (is.null(env)) {
      env_str <- ")"
    } else {
      env_str <- ", env = env)"
    }
    eval(parse(text = paste0(
      paste0("fbody_mod", paste0("[[", tloc, "]]", collapse = "")), " <- ", "substitute(expr", env_str
    )))
  }
  fbody_mod
}

append_in_section <- function(fbody, tloc, expr, env = NULL, after = TRUE) {
  fbody_mod <- fbody

  if (length(tloc) > 0) {
    if (is.null(env)) {
      env_str <- ")"
    } else {
      env_str <- ", env = env)"
    }

    if (length(tloc) > 1) {
      taget_brackets <- paste0("[[", tloc[-length(tloc)], "]]", collapse = "")
    } else {
      # length(tloc)==1 case
      taget_brackets <- ""
    }

    eval(
      parse(
        text = paste0(
          "fbm_part <- ", paste0("fbody_mod", taget_brackets), "\n",
          "fbm_part <- fbm_part[c(", paste0(c(1:tloc[length(tloc)], tloc[length(tloc)]), collapse = ","), ":length(fbm_part))]", "\n",
          paste0("fbm_part[[", ifelse(after, tloc[length(tloc)] + 1, tloc[length(tloc)]), "]]"), " <- ", "substitute(expr", env_str, "\n",
          "fbm_part -> ", paste0("fbody_mod", taget_brackets)
        )
      )
    )
  }
  fbody_mod
}

# env to store functions
pre_patch_function_store <- new.env()

get_f_well_name <- function(f, env = environment()){
  fn <- as.character(substitute(f, env))
  if(length(fn)>1){
    fn[1] <- gsub("::|:::", "from_p", fn[1])
    fn[1] <- gsub("@", "S4_slot", fn[1])
    fn[1] <- gsub("\\$", "dollar", fn[1])
    fn <- paste0(fn, collapse = "_")
    fn <- gsub("[^\\.0-9a-zA-Z_]","_", fn)
    fn <- gsub("^_","",fn)
  }
  fn
}

store_original_function <- function(f, env = NULL) {

  fn <- get_f_well_name(f, env)

  if (is.null(pre_patch_function_store[[fn]])) {
    assign(fn, f, envir = pre_patch_function_store)
  } else {
    return(get(fn, envir = pre_patch_function_store))
  }
  return(f)
}
