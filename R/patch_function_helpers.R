
#' Locate a section of a function
#'
#' @param fbody Body of a function / (function directly if attached dynamically)
#' @param search_str The string(s) to find in the body
#'
#' @details There is additional argument `all` which is available only if
#'   attached dynamically). If set to `TRUE` it will list all matches in a list
#'   (otherwise only first match)
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


# recursive version with full scan (not only first match)
locate_section_rec <- function(fbody, search_str, pre = integer(0)) {
  tloc <- list()
  rlocs <- grepl(search_str, as.character(fbody))
  if(any(rlocs)){
    if(length(fbody)>1){
      tl0 <- seq_along(rlocs)[rlocs]
      tl1 <- lapply(tl0, function(x) c(pre, x))
      tl2 <- lapply(
        seq_along(tl0),
        function(i){
          locate_section_rec(fbody[[tl0[i]]], search_str, pre = tl1[[i]])
        })
      tl3 <- Reduce(c, tl2)
      tloc <- tl3
    }else{
      tloc <- list(c(pre, which(rlocs)))
    }
  }
  tloc
}


prune_line_location_locator <- function(fbody, tloc){
  tf <-tloc[-length(tloc)]
  tf_pre <- tloc
  repeat{
    if(identical(get_section(fbody, tf),get_section(fbody, tloc))){
      tf_pre <- tf
      tf <- tf[-length(tf)]
    }else{
      break()
    }
  }
  tf_pre
}



#' See a section of a function (for development of patch)
#'
#' @param fbody Body of a function / (function directly if attached dynamically)
#' @param tloc Recursive list path as returned by
#' [`locate_section`][locate_section()] or hand typed
#'
#' @details There is additional argument `search_str` which is available only if
#'   attached dynamically). This can be given directly omitting `tloc`.
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
    obj <- eval(
      parse(
        text =
          paste0("fbody", paste0("[[", tloc, "]]", collapse = ""))
      )
    )
  }
  obj
}

replace_in_section <- function(fbody, tloc, expr, env = NULL,
                               add_browser = FALSE, replace_safely = FALSE) {
  fbody_mod <- fbody
  if (length(tloc) > 0) {
    if (is.null(env)) {
      env_str <- ")"
    } else {
      env_str <- ", env = env)"
    }
    if(add_browser){

      # kept for testing
      fbody_mod_loc <- fbody_mod

      code_n_chk <- function(test = FALSE){
        fbn <- "fbody_mod"
        if(test){
          fbn <- "fbody_mod_loc"
        }
        code <-
          paste0(".tmpNode <- ",
                 paste0(fbn, paste0("[[", tloc, "]]", collapse = "")),
                 "\n",

                 paste0(fbn, paste0("[[", tloc, "]]", collapse = "")),
                 " <- substitute(tryCatch('', error = function(e) browser()))\n",

                 paste0(fbn, paste0("[[", tloc, "]]", collapse = "")),
                 "[[2]] <- .tmpNode")

        if(test){
          echk <- tryCatch(eval(parse(text = code)), error = function(e) {e})
        }else{
          echk <- TRUE
        }

        list(code = code, echk = echk)
      }

      raw <- code_n_chk(test = TRUE)

      if(inherits(raw$echk,"error")){
        # try by pruning locator
        tloc <- prune_line_location_locator(fbody,tloc)
      }
      # possible pruning

      prn <- code_n_chk()
      code <- prn$code

    }else{
      if(replace_safely){
        code <-
          paste0(
            paste0("tmp <- substitute(suppressMessages(suppressWarnings(",
                   "tryCatch('this', error = function(e) invisible(NULL)))))"),
            paste0("\ntmp[[2]][[2]][[2]] <- ", "substitute(expr", env_str, "\n"),
            paste0("fbody_mod", paste0("[[", tloc, "]]", collapse = "")),
            " <- tmp", "\n")

      }else{
        code <- paste0(
          paste0("fbody_mod", paste0("[[", tloc, "]]", collapse = "")),
          " <- ", "substitute(expr", env_str
        )
      }

    }

    eval(parse(text = code))
  }
  fbody_mod
}

append_in_section <- function(fbody, tloc, expr, env = NULL, after = TRUE,
                              add_safely = FALSE) {
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


    code <- paste0(
      "fbm_part <- ", paste0("fbody_mod", taget_brackets), "\n")
    code <-
      paste0(
        code,
        "fbm_part <- fbm_part[c(",
        paste0(c(1:tloc[length(tloc)], tloc[length(tloc)]), collapse = ","),
        ":length(fbm_part))]", "\n")

    if(add_safely){
      code <-
        paste0(
          code,
          # #--->> simple `tryCatch` (kept just for ref)
          # paste0("tmp <- substitute(tryCatch('this', ",
          #        "error = function(e) invisible(NULL)))"),
          paste0("tmp <- substitute(suppressMessages(suppressWarnings(",
                 "tryCatch('this', error = function(e) invisible(NULL)))))"),
          # #--->> this is for simple `tryCatch` (kept just for ref)
          # paste0("\ntmp[[2]] <- ", "substitute(expr", env_str, "\n"),
          paste0("\ntmp[[2]][[2]][[2]] <- ", "substitute(expr", env_str, "\n"),
          paste0("fbm_part[[",
                 ifelse(after, tloc[length(tloc)] + 1, tloc[length(tloc)]), "]]"),
          " <- tmp", "\n")

    }else{
      code <-
        paste0(
          code,
          paste0("fbm_part[[",
                 ifelse(after, tloc[length(tloc)] + 1, tloc[length(tloc)]), "]]"),
          " <- ", "substitute(expr", env_str, "\n")
    }
    code <-
      paste0(
        code,
        "fbm_part -> ", paste0("fbody_mod", taget_brackets))

    eval(parse(text = code))
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

store_original_function <- function(f, env = NULL, check_only = FALSE) {

  fn <- get_f_well_name(f, env)

  chk<- is.null(pre_patch_function_store[[fn]])
  if(check_only){
    return(!chk)
  }

  if (chk) {
    assign(fn, f, envir = pre_patch_function_store)
  } else {
    return(get(fn, envir = pre_patch_function_store))
  }
  return(f)
}


# Argument manipulation

check_args_in_body <- function(fn){
  args_def <- formals(fn)
  bd <- as.character(body(fn))
  chk <- unlist(lapply(names(args_def), function(an) any(grepl(an, bd))))
  args_def[chk]
}

integrate_formals <- function(new_arg, old_arg){
  final_new_arg <- new_arg
  old_names <- setdiff(names(old_arg), names(new_arg))
  if(length(old_names)>0){
   for(on in old_names){
     final_new_arg[[on]] <- old_arg[[on]]
   }
  }
  final_new_arg
}

modify_args <- function(fn, new_arg = NULL,
                        integrate_with_old_formals = TRUE,
                        use_in_body_arg_for_integration = TRUE){

  # this can be used to track unintentional argument changes
  old_formals <- formals(fn)
  old_body_formals <- check_args_in_body(fn)

  if(integrate_with_old_formals){
    old_arg <- old_formals
    if(use_in_body_arg_for_integration){
      old_arg <- old_body_formals
    }
    new_arg_put <- integrate_formals(new_arg,old_arg)
  }else{
    new_arg_put <- new_arg
  }


  tryCatch(
    formals(fn) <- new_arg_put,
    error = function(e){
      stop(
        paste0(
          "Unable to modify arguments of the function.\n",
          "Make sure you have used <alist> correctly ",
          "(e.g. alist(a=, b=), `=` is important).\n",
          "For quick reference check <formals> help.\n"),
        call. = FALSE)
    }
  )

  if(length(setdiff(names(old_formals), names(formals(fn))))>0){
    body_arg_missed <- FALSE
    if(length(setdiff(names(old_body_formals), names(formals(fn))))>0){
      body_arg_missed <- TRUE
    }
    warning(
      paste0(
        "At least one ",
        ifelse(body_arg_missed,
               "'used in body argument (old argument)'","'old argument'"),
        " is missed. Make sure that is intentional."
      ),
      call. = FALSE
    )
  }
  fn
}
