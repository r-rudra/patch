

#' @title Patch a function in current R Session
#'
#' @description Any function can be modified programmatically within a session.
#'   It also keeps a backup of original function which can be retried by calling
#'   `patch_function` and `f` argument only (without specifying anything else).
#'
#'
#' @param f Function to modify / patch
#' @param search_str String (or strings; in case multiple strings is given only
#'   first match will be considered. Even for multiple matches of single
#'   supplied string **only first match** will be considered unless
#'   `which_match_to_target` is not supplied or more than `1`.) to find in the
#'   function body which should act as location of modification.
#' @param expr Expression or code to punch in. If user don't supply this it will
#'   be assumed that (replace_it = TRUE , append_after = FALSE) and the browser
#'   on error will be used. This option is good for debugging the functions.
#' @param modification_placement This is to control the placment of modification
#'   (supplied by `expr`) relative to found match (by `search_str` or
#'   `line_location`). It can be ignored if user want to control modification
#'   placement (using `replace_it` and `append_after`). If not missed it has to
#'   be one of `c("after","replace","before")` _(Easy to understand what each
#'   options meant)_. If supplied further inputs to arguments `append_after` and
#'   `replace_it` will be ignored.
#' @param which_match_to_target If there is multiple match by `search_str` then
#'   which one to target (number should be sequential appearance of `search_str`
#'   in function body). If missed or `1` only first match will be considered.
#'   _Note: `which_match_to_target` is supposed to be single integer and is
#'   designed for single entry of `search_str`. For multiple entry in
#'   `search_str` it's better not to use this feature._
#' @param line_location Exact line location (a vector similar to return of
#'   [`locate_section`][locate_section()]. Note this function `locate_section`
#'   is available only in **dynamic attachment**). This is ignored if
#'   `search_str` is present. _(This is non-robust way of editing)_
#' @param replace_it Whether replace it (the match by `search_str`) (if `TRUE`
#'   the argument `append_after` will be omitted)
#' @param append_after Whether add after it (`replace_it` should be `FALSE`). If
#'   set to `FALSE`, the modification will be added before the found match.
#' @param safely If set to `TRUE` the modification expression (`expr`) will be
#'   wrapped inside `suppressMessages(suppressWarnings(tryCatch(..)))`. This
#'   maybe useful if one is not sure how the modification will behave in unknown
#'   situations.
#' @param move_up_to Fine-tuning the locator (should be an index within the
#'   length of locator). Moving up from found match.
#' @param chop_locator_to Fine-tuning the locator (should be an index within the
#'   length of locator). Moving from beginning of found match. `move_up_to` is
#'   better option. It is exactly opposite of `move_up_to` (in a sense).
#' @param further_locator (Optional) In case you need to further tune _locator_
#'   by adding few extra locations (or depth) after found match (by
#'   `search_str`) or supplied location (by `line_location`). A vector of
#'   integers (specifying location in nested list form of function body).
#'   `further_locator` is applied before fine tuning by other arguments
#'   (`move_up_to` and `chop_locator_to`).
#' @param new_arguments (Optional) In case one need to change the arguments of
#'   the function. Note that it has to be full list of argument. (It should not
#'   be considered as additional argument). You can use [`alist`][base::alist()]
#'   for this (_Remember to give `=` otherwise it will not work_). Leave it to
#'   `NULL` (default) in case argument alteration is not required.
#' @param new_arguments_integrate If set to `FALSE` (by default it is `TRUE`)
#'   then new argument will be passed as it is (replacement of complete
#'   argument). Otherwise, it will be integrated (or merged) with arguments
#'   which are used in the body of function directly after the modifications.
#'   Note: In case few argument is missed warning will be issued accordingly.
#' @param env In case `expr` is passed from a different location apart from
#'   global environment. It may be required if `patch_function` is called inside
#'   a function or in different environment from `.GlobalEnv`
#' @param auto_assign_patched_function Whether the patched function to be auto
#'   patched into it's source. By default `FALSE`. Usethis with care.
#' @param no_store If set to `TRUE`. It will not try to backup the function in
#'   current session. (Maybe useful for understanding edit locations before
#'   actual patch work.). By default is `TRUE`. It can be configured like
#'   `options(store_function_before_patch = TRUE)`. **Note: The storing and
#'   restoring mechanism is simply achieved by function names (with minor
#'   modifications if required).**
#' @param auto_add_curly_brackets If `TRUE` adds `{}` to single line functions.
#'   This enables further editing of single functions. _Note: after edit it will
#'   be no more single line._
#'
#' @details Backup of the original function is kept in an environment for
#'   restoration purpose (if required). That can be obtained by calling
#'   `patch_function` and `f` argument only (without specifying anything else).
#'
#'   Note that a function can be patched unlimited times (backup of original
#'   function, as available in single session of the package, will be kept only
#'   once)
#'
#'   In case there is a high chance that the target function `f` will undergo
#'   several modifications, then `search_str` should be given in a way that
#'   match happens even if the code changes (to certain degree). The first match
#'   will be considered for locating purposes.
#'
#'   It is sometimes difficult to understand how `move_up_to` or
#'   `chop_locator_to` should be constructed. In such cases, users may attach
#'   optional functions to help them understand the locator. Which can be done
#'   via simple `patch_function()` call. Meaning omitting all arguments. It is
#'   referred as **dynamic attachment**.
#'
#'   Note that few sample use-cases are supplied with the package. This may be
#'   accessed by
#'
#'   *`source(system.file("embedded","usecases.R",package = "patch"))`*
#'
#'   However, note that these may cause problem (though it is meant for R-Studio
#'   actually) with R-Studio. If such things happen kindly restart the session.
#'   This feature is not checked as a part of package evaluations. So **use it
#'   at your own risk**.
#'
#' @section Types of _patching_:
#'
#'   `{patch}` is designed for quick in-session modification to certain type of
#'   functions. Sometimes it is useful if some experimental feature (or yet to
#'   implement feature) is required quickly.
#'
#'   There are many ways you can patch a function. But most importantly there is
#'   a concept of robust patching. The underlying source of the target function
#'   changes as and when developer releases new version, patches etc. The idea
#'   is to generate a robust _(so that the patch will mostly work even if there
#'   is a code change in the function)_ patch.
#'
#'   ### Robust way:
#'
#'   Ideally one should use a `search_str` and possibly `move_up_to` to fine
#'   tune the patch. Make sure `search_str` is a core item (part of the body of
#'   target function) which usually will not change (unless there is major
#'   changes in the underlying code-base) One can use **dynamic attachment** to
#'   fine tune the `move_up_to` numbers (or directly `patch_function`).
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
patch_function <- function(
  f,
  search_str, expr,
  modification_placement,
  which_match_to_target,
  line_location,
  replace_it = FALSE, append_after = TRUE, safely = FALSE,
  move_up_to = NULL, chop_locator_to = NULL, further_locator = integer(0),
  new_arguments = NULL,
  new_arguments_integrate = TRUE,
  env = NULL,
  auto_assign_patched_function = FALSE,
  no_store = !isTRUE(getOption("store_function_before_patch")),
  auto_add_curly_brackets = TRUE
){
  mc <- match.call()
  mc[[1]] <- patch_function_raw
  tryCatch({
    eval(mc, envir = parent.frame())
  }, error = function(e){
    stop("Some error occurred during patching.", call. = FALSE)
  })
}


patch_function_raw <- function(
  f,
  search_str, expr,
  modification_placement,
  which_match_to_target,
  line_location,
  replace_it = FALSE, append_after = TRUE, safely = FALSE,
  move_up_to = NULL, chop_locator_to = NULL, further_locator = integer(0),
  new_arguments = NULL,
  new_arguments_integrate = TRUE,
  env = NULL,
  auto_assign_patched_function = FALSE,
  no_store = !isTRUE(getOption("store_function_before_patch")),
  auto_add_curly_brackets = TRUE
){

  # modification_placement is either of c("after","replace","before") (if
  # supplied)
  if(!missing(modification_placement)){
    modification_placement <-
      match.arg(modification_placement,
                choices = c("after","replace","before"))

    if(modification_placement == "after") {
      append_after <- TRUE
      replace_it <- FALSE
    }

    if(modification_placement == "replace") {
      append_after <- FALSE
      replace_it <- TRUE
    }

    if(modification_placement == "before") {
      append_after <- FALSE
      replace_it <- FALSE
    }

  }

  if(missing(expr)){
    add_browser_here <- TRUE
    replace_it <- TRUE
    append_after <- FALSE
  }else{
    add_browser_here <- FALSE
  }

  if(missing(f)) {
    dynamic_attachment()
    return(invisible(0))
  }

  if(is.null(env)){
    env <- environment()
  }

  if(missing(search_str) & missing(line_location) & missing(new_arguments)){
    # no modification requested
    if(!no_store){
      fs <- store_original_function(f, env = env)

      # return original function (since session start)
      if(auto_assign_patched_function){
        auto_assign(f,
                    new_f_body = body(fs),
                    new_f_arg = formals(fs),
                    env)
        # exit without printing
        return(invisible(fs))
      }else{
        # exit with printing
        return(fs)
      }
    }else{
      cat("\nNothing to do here!\n")
      return(invisible(f))
    }
  }


  fbody <- body(f)

  if(as.character(as.list(fbody)[[1]])!="{"){
    if(auto_add_curly_brackets){
      empty.bd <- body(function(){})
      empty.bd[[2]] <- fbody
      fbody <- empty.bd
    }
  }

  if(is.null(fbody)){
    if(is.primitive(f)){
      cat("\nSupplied function is a primitive.\n")
    }
    stop("This function can not be modified directly.")
  }

  body_change_required <- TRUE
  if(missing(search_str) & missing(line_location) & !missing(new_arguments)){
    # only argument change requested
    body_change_required <- FALSE
  }

  if(missing(new_arguments)){
    new_arguments <- formals(f)
  }

  if(body_change_required){
    # default option
    locate_section_fn <- locate_section

    if(!missing(which_match_to_target)){
      which_match_to_target <- as.integer(which_match_to_target)
      if(which_match_to_target>1){
        locate_section_fn <- function(fbody, str){
          rawl <- locate_section_rec(fbody, str)
          rawl[[min(which_match_to_target, length(rawl))]]
        }
      }
    }

    if(!missing(search_str)){
      if(length(search_str)>1){
        # match first occurrence
        for(ss in search_str){
          tloc <- locate_section_fn(fbody, ss)
          if(length(tloc)>0) break()
        }
      }else{
        tloc <- locate_section_fn(fbody, search_str)
      }
    }else{
      tloc <- line_location
    }

    # add further locator at the end
    tloc <- c(tloc, further_locator)

    if(length(tloc)==0){
      stop(
        paste0(
          "Unable to find supplied string(s) ",
          "in the body of the target function (or invalid locator)."),
        call. = FALSE)
    }

    # move_up_to = length(tloc)-chop_locator_to+1
    if(!is.null(move_up_to)){
      move_up_to <- min(move_up_to, length(tloc))
      chop_locator_to <- length(tloc)-move_up_to+1
    }

    if(!is.null(chop_locator_to)){
      chop_locator_to <- min(chop_locator_to, length(tloc))
      tloc <- tloc[seq(chop_locator_to)]
    }


    if(replace_it){
      fbody_new <- replace_in_section(fbody, tloc, expr, env = env,
                                      add_browser = add_browser_here,
                                      replace_safely = safely)
    }else{
      fbody_new <- append_in_section(fbody, tloc, expr, env = env,
                                     after = append_after,
                                     add_safely = safely)
    }

  }else{
    fbody_new <- fbody
  }


  if(auto_assign_patched_function){
    auto_assign(f,
                new_f_body = fbody_new,
                new_f_arg = new_arguments,
                new_f_arg_integrate = new_arguments_integrate,
                env)
    # early exit without printing or modifying
    return(invisible(f))
  }

  body(f) <- fbody_new
  f <- modify_args(f, new_arguments,
                   integrate_with_old_formals = new_arguments_integrate)
  f
}
