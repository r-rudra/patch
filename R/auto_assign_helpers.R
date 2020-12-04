

# support for @ and $ may be given

details_of_function <- function(f, env = environment()){
  fc <- substitute(f, env = env)
  fc <- as.list(fc)
  fs <- as.character(fc[[1]])

  fd <- list()

  # class
  cls <- switch (fs,
    `function` = "anonymous_function",
    `::` = "package_exported",
    `:::` = "package_internal",
    `$` = "dollar",
    `@` = "S4_slot",
    "unknown"
  )

  # @ not implemented
  # TODO
  if(cls %in% c("S4_slot","dollar")) stop("Not implemented yet!",call. = FALSE)



  if(cls == "unknown"){
    if(!grepl("[^0-9a-zA-Z_\\.]+",fs)){
      if(exists(fs, mode = "function")){
        cls <- "function_in_search_path"
      }
    }
  }

  fd$class <- cls

  # assign_back options
  fd$assign_back <- function(new_body){
    stop("Not implemented yet!", call. = FALSE)
  }

  if(cls %in% c("package_exported","package_internal")){
    package_name <- as.character(fc[[2]])
    package_fn <- as.character(fc[[3]])

    fd$assign_back <- function(new_body){
      redefine_package_object(package_name, package_fn, new_body)
    }

  }


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


# essentially bad approach

list_objects_store <- new.env()

list_objects <- function(cache_session = Sys.time()){
  if(!identical(list_objects_store$cache_session, cache_session)){
    sp <- search()
    obj_list <- lapply(seq_along(sp), function(sn) ls(pos = sn))
    list_objects_store$sp <<- sp
    list_objects_store$obj_list <<- obj_list
    list_objects_store$cache_session <<- cache_session
  }
  invisible(list_objects_store)
}

locate_object <- function(obj_name, cache_session = Sys.time()){

  lo <- list_objects(cache_session)

  locs <- unlist(lapply(lo$obj_list, function(objs) (obj_name %in% objs)))

  res <- list()

  res$found <- any(locs)
  res$pos <- which(locs)
  res$name <- lo$sp[res$pos]

  res

}
