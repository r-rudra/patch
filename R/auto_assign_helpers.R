

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
    "unknown"
  )
  if(cls == "unknown"){
    if(!grepl("[^0-9a-zA-Z_\\.]+",fs)){
      cls <- "function_in_search_path"
    }
  }

  fd$class <- cls

}
