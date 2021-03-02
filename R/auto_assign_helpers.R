

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


  sys_fr_target <- new.env()

  if(cls == "unknown"){
    if(!grepl("[^0-9a-zA-Z_\\.]+",fs)){
      if(exists(fs, mode = "function")){
        cls <- "function_in_search_path"
      }else{
        # check in sys.frames
        schk <- locate_object(fs, envs = sys.frames())
        if(schk$found & isTRUE(schk$pos_rev>0)){
          sys_fr_target <- schk$env
          cls <- "function_in_sys.frames"
        }
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
  fd$assign_back <- function(new_body, new_arg, new_arg_integrate){
    stop("Not implemented yet!", call. = FALSE)
  }

  if(cls %in% c("package_exported","package_internal")){
    package_name <- as.character(fc[[2]])
    package_fn <- as.character(fc[[3]])

    fd$assign_back <- function(new_body, new_arg, new_arg_integrate){
      body(f_mod) <- new_body
      f_mod <- modify_args(f_mod, new_arg, new_arg_integrate)
      redefine_package_object(package_name, package_fn, f_mod)
    }

    fd$auto_assign_possible <- TRUE

  }

  if(cls == "function_in_search_path"){
    # fl_info and where_this should be already defined
    fd$assign_back <- function(new_body, new_arg, new_arg_integrate){
      if(where_this %in% search()){
        body(f_mod) <- new_body
        f_mod <- modify_args(f_mod, new_arg, new_arg_integrate)
        tryCatch(
          assign(fs, f_mod, pos = where_this),
          error = function(e){
            # mostly it is locked
            base::unlockBinding(fs, as.environment(where_this))
            assign(fs, f_mod, pos = where_this)
            lockBinding(fs, as.environment(where_this))
          }
        )
      }
    }

    fd$auto_assign_possible <- TRUE
  }

  if(cls == "function_in_sys.frames"){
    # sys_fr_target should be already defined
    fd$assign_back <- function(new_body, new_arg, new_arg_integrate){
      body(f_mod) <- new_body
      f_mod <- modify_args(f_mod, new_arg, new_arg_integrate)
      tryCatch(
        assign(fs, f_mod, envir = sys_fr_target),
        error = function(e){
          # mostly it is locked
          base::unlockBinding(fs, sys_fr_target)
          assign(fs, f_mod, envir = sys_fr_target)
          lockBinding(fs, sys_fr_target)
        }
      )
    }

    fd$auto_assign_possible <- TRUE
  }

  fd

}



redefine_package_object <- function(package_name, object_name, object_new_val){


  p_env <- asNamespace(package_name)

  all_names <- ls(envir = p_env, all.names = TRUE)

  if(object_name %in% all_names){
    ._set_this <- function(x, val){
      utils::assignInMyNamespace(x, val)
    }

    environment(._set_this) <- p_env

    # this will solve package_name::object_name calls (or ::: calls)
    ._set_this(object_name,object_new_val)
    rm(._set_this)

    # for attached packages one more step is required
    psn <- paste0("package:", package_name)

    if(psn %in% search()){
      pos <- which(search()==psn)
      tryCatch({
        detach(psn, character.only = TRUE, force = TRUE)
        base::attach(p_env, name = psn, pos = pos, warn.conflicts = FALSE)
      }, error = function(e) NULL)
    }

  }else{
    stop(paste0("Function/object named ", object_name,
                " not present in {",package_name,"}"),
         call. = FALSE)
  }

  invisible(0)
}



locate_object <- function(obj_name, envs = search()){

  if(missing(envs)){
    sp <- search()

    locs <- unlist(
      lapply(seq_along(sp),
             function(sn) exists(obj_name, where = sn, inherits = F)))


  }else{
    locs <- unlist(
      lapply(envs,
             function(en) exists(obj_name, where = en, inherits = F)))
  }

  res <- list()

  res$found <- any(locs)
  res$pos <- which(locs)
  if(missing(envs)){
    res$name <- sp[res$pos]
  }else{
    if(any(locs)){
      res$env <- envs[[res$pos[1]]]
      res$pos_rev <- length(envs)-res$pos[1]
    }
  }


  res

}
