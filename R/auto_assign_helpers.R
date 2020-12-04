

# support for @ and $ may be given

details_of_function <- function(f, env = environment()){

  f_mod <- f

  fc <- substitute(f, env = env)
  fc <- as.list(fc)
  fs <- as.character(fc[[1]])

  fd <- list()
  fd$auto_assign_possible <- FALSE

  # class
  cls <- switch (fs,
                 `function` = "anonymous_function",
                 `::` = "package_exported",
                 `:::` = "package_internal",
                 `$` = "dollar",
                 `@` = "S4_slot",
                 "unknown"
  )

  # @ and $ not implemented
  # TODO
  # if(cls %in% c("S4_slot","dollar")) stop("Not implemented yet!",call. = FALSE)


  if(cls == "unknown"){
    if(!grepl("[^0-9a-zA-Z_\\.]+",fs)){
      if(exists(fs, mode = "function")){
        cls <- "function_in_search_path"
      }
    }
  }

  # if it is a package function (and attached)
  # change to correct form
  fl_info <- list()
  if(cls == "function_in_search_path"){
    fl_info <- locate_object(fs)
    where_this <- fl_info$name[1]
    if(grepl("package:",where_this)){
      fc[[2]] <- gsub("package:","", where_this)
      fc[[3]] <- fs
      cls <- "package_exported"
    }
  }


  fd$class <- cls

  # assign_back options
  # TODO
  fd$assign_back <- function(new_body){
    stop("Not implemented yet!", call. = FALSE)
  }

  if(cls %in% c("package_exported","package_internal")){
    package_name <- as.character(fc[[2]])
    package_fn <- as.character(fc[[3]])

    fd$assign_back <- function(new_body){
      body(f_mod) <- new_body
      redefine_package_object(package_name, package_fn, f_mod)
    }

    fd$auto_assign_possible <- TRUE

  }

  if(cls == "function_in_search_path"){
    # fl_info and where_this should be already defined
    fd$assign_back <- function(new_body){
      if(where_this %in% search()){
        body(f_mod) <- new_body
        asNamespace("base")[["assign"]](fs, f_mod, pos = where_this)
      }
    }

    fd$auto_assign_possible <- TRUE
  }

  fd

}



redefine_package_object <- function(package_name, object_name, object_new_val){

  ._set_this <- function(x, val){
    utils::assignInMyNamespace(x, val)
  }

  p_env <- asNamespace(package_name)

  environment(._set_this) <- p_env

  all_names <- ls(envir = p_env, all.names = TRUE)

  if(object_name %in% all_names){
    ._set_this(object_name,object_new_val)
  }else{
    stop(paste0("Function/object named ", object_name,""), call. = FALSE)
  }

  rm(._set_this)

  invisible(0)
}



locate_object <- function(obj_name){

  sp <- search()

  locs <- unlist(lapply(seq_along(sp), function(sn) exists(obj_name, where = sn, inherits = F)))

  res <- list()

  res$found <- any(locs)
  res$pos <- which(locs)
  res$name <- sp[res$pos]

  res

}
