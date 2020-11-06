

#' @title Patch a function in current R Session
#'
#' @description Any function can be modified programmatically within a session.
#' It also keeps a backup of original function which can be retried by calling
#' `patch_function` and `f` argument only (without specifying anything else).
#'
#'
#' @param f Function to modify / patch
#' @param search_str String (or strings; in case multiple strings is given only
#' first match will be considered) to find in the function body which should act
#' as location of modification.
#' @param expr Expression or code to puch in
#' @param replace_it Whether replace it (the match by `search_str`) (if `TRUE`
#' the argument `append_after` will be omitted)
#' @param append_after Whether add after it (`replace_it` should be `FALSE`)
#' @param chop_locator_to Fine-tuning the locator (should be an index within the
#' length of locator)
#' @param env In case `expr` is passed from a different location apart from
#' global environment. It may be required if `patch_function` is called inside a
#' function or in different environment from `.GlobalEnv`
#'
#' @details Backup of the original function is kept in an environment for
#' restoration purpose (if required). That can be obtained by calling
#' `patch_function` and `f` argument only (without specifying anything else).
#'
#' Note that a function can be patched unlimited times (backup of original
#' function, as available in single session of the package, will be kept only
#' once)
#'
#' In case there is a high chance that the target function `f` will undergo
#' several modifications, then `search_str` should be given in a way that match
#' happens even if the code changes (to certain degree). The first match will be
#' considered for locating purposes.
#'
#' It is sometimes difficult to understand how `chop_locator_to` should be
#' constructed. In such cases, users may attach optional functions to help them
#' understand the locator. Which can be done via simple `patch_function()` call.
#' Meaning omitting all arguments.
#'
#' Note that few sample use-cases are supplied with the package. This may be
#' accessed by
#'
#' *`source(system.file("embedded","usecases.R",package = "patch"))`*
#'
#' However, note that these may cause problem (though it is meant for R-Studio
#' actually) with R-Studio. If such things happen kindly restart the session.
#' This feature is not checked as a part of package evaluations.
#' So **use it at your own risk**.
#'
#' @return Patched `function` (if the `search_str` is found)
#' @export
#'
#' @examples
#'
#' # test function
#' ftest <- function(x, y){
#'   u <- x+y
#'   v <- u^2
#'   z <- v+x
#'   if(x%%2==0){
#'     z0 <- z+1
#'   }else{
#'     if(y%%2 == 0){
#'       v <- 11
#'       z1 <- 14
#'       u <- 10
#'     }else{
#'       z1 <- 10
#'     }
#'     z0 <- z^2+z1
#'   }
#'
#'   if(z0%%5 == 1){
#'     z2 <- z0*2
#'   }else{
#'     z2 <- z0*3
#'   }
#'
#'   z2
#'
#' }
#'
#'
#' # find z1 <- 14 and add z1 <- z1 + length(letters) after that
#' patch_function(ftest, "z1 <- 14", z1 <- z1 + length(letters))
#'
#' # multiple candidate for matching locator (first match will be considered)
#' patch_function(ftest, c("v <- 11","z1 <- 14","u <- 10"),
#' z1 <- z1 + length(letters), replace_it = TRUE)
patch_function <- function(f, search_str, expr,
                           replace_it = FALSE, append_after = TRUE,
                           chop_locator_to = NULL, env = NULL){


  if(missing(f)) {
    dynamic_attachment()
    return(invisible(0))
  }

  if(is.null(env)){
    env <- environment()
  }

  fs <- store_original_function(f, env = env)
  if(missing(search_str)){
    return(fs)
  }


  fbody <- body(f)
  # match first occurrence
  if(length(search_str)>1){
    for(ss in search_str){
      tloc <- locate_section(fbody, ss)
      if(length(tloc)>0) break()
    }
  }else{
    tloc <- locate_section(fbody, search_str)
  }

  if(!is.null(chop_locator_to)){
    chop_locator_to <- min(chop_locator_to, length(tloc))
    tloc <- tloc[seq(chop_locator_to)]
  }

  if(replace_it){
    fbody_new <- replace_in_section(fbody, tloc, expr, env = env)
  }else{
    fbody_new <- append_in_section(fbody, tloc, expr, env = env,
                                   after = append_after)
  }


  body(f) <- fbody_new
  f
}

