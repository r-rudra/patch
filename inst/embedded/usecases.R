

# this shows few sample use-cases
# library(patch)

# credit goes to creators of RStudio
# Check this https://github.com/rstudio/rstudio/blob/efc873ed38a738be9dc0612d70f21f59bfd58410/src/cpp/session/modules/SessionRCompletions.R
# Above is AGPL-3.0
# Check this also https://support.rstudio.com/hc/en-us/articles/205273297-Code-Completion
# this code is having the same license as main package {patch}


################################################################################
# to put this in start-up use usethis::edit_r_profile() or simply edit .Rprofile
# paste either of the options (code after and before #######>>>>>>> indicator)
#
# { # <- this bracket is not required
#
#   #######>>>>>>> option 1
#
#   #### Load {patch} auto-complete use-cases at start-up
#   # Note : it has to be using later otherwise .rs* functions will not be loaded in
#   # R Studio
#   later::later(
#     function(){
#       ef <- system.file("embedded","usecases.R",package = "patch")
#       if(file.exists(ef)){
#         source(ef)
#         cat("{patch} autocomplete use-cases loaded\n")
#       }
#     },
#     # change it appropriately
#     delay = 5
#   )
#
#   #######>>>>>>>
#
#   #######################################################################
#
#   #######>>>>>>> option 2
#
#   #### Load {patch} auto-complete use-cases at start-up
#   # Note : it has to be using later otherwise .rs* functions will not be loaded in
#   # R Studio
#   later::later(
#     function(){
#       ef <- system.file("embedded","usecases.R",package = "patch")
#       if(file.exists(ef)){
#         source(ef)
#         cat("{patch} autocomplete use-cases loaded\n")
#       }
#
#       ##### {later} unloading
#       # This part can be avoided if you "require" or "don't mind" {later} loaded
#       # Note this will unload any other packages loaded during start-up so decide
#       # accordingly.
#       on.exit({
#         tryCatch(
#           {
#             si <- sessionInfo()
#
#             base <- names(which(lapply(si$loadedOnly,`[[`, "Priority") == "base"))
#             base <- unique(c(base, si$basePkgs))
#
#             to_unload <- setdiff(loadedNamespaces(),
#                                  # as patch is required
#                                  # retain other packages as required
#                                  # or you may completely delete this section
#                                  c(base, "patch"))
#
#             for(ln in to_unload){
#               tryCatch(
#                 unloadNamespace(ln),
#                 error = function(e) NULL
#               )
#             }
#           },
#           error = function(e) NULL
#         )
#       })
#     },
#     # change it appropriately
#     delay = 5
#   )
#
#   #######>>>>>>>
#
# } # <- this bracket is not required
################################################################################

enable_all <- function() {

  auto_complete_future_plan <- function() {
    .rs.addJsonRpcHandler(
      "get_completions",
      patch::patch_function(.rs.rpc.get_completions,
                     ".rs.getCompletionsEnvironmentVariables",
        # addition portion
        if (length(string) &&
          ("future" %in% .packages()) &&
          string[[1]] == "plan" &&
          numCommas[[1]] == 0) {
          candidates <- c(
            "sequential",
            "transparent",
            "multisession",
            "multicore",
            "multiprocess",
            "cluster",
            "remote"
          )
          results <- .rs.selectFuzzyMatches(candidates, token)

          return(.rs.makeCompletions(
            token = token,
            results = results,
            quote = FALSE,
            type = .rs.acCompletionTypes$VECTOR
          ))
        },
        chop_locator_to = 1,
        safely = TRUE
      )
    )
  }

  auto_complete_dplyr_filter <- function() {
    .rs.addJsonRpcHandler(
      "get_completions",
      patch::patch_function(.rs.rpc.get_completions,
                     ".rs.getCompletionsEnvironmentVariables",

        # addition portion
        if (length(string) &&
          ("dplyr" %in% .packages()) &&
          string[[1]] == "filter" &&
          any(grepl("==", line))) {
          chainObject <- .rs.getAnywhere(chainObjectName, envir)

          if (!is.null(chainObject)) {
            if (is.data.frame(chainObject)) {
              pline <- gsub(" +", "", line)
              cname_attempt <- tryCatch(
                rev(unlist(strsplit(
                  rev(strsplit(rev(unlist(strsplit(pline, "==")))[1],
                               "&|\\|")[[1]])[1],
                  "\\(|[ ]+|,")))[[1]],
                error = function(e) ""
              )
              if (cname_attempt %in% colnames(chainObject)) {
                # safe limit
                safe_lim <- getOption("filter_auto_complete_row_limit")
                safe_lim <- ifelse(is.null(safe_lim), 10^6, safe_lim)
                if (nrow(chainObject) < safe_lim) {
                  this_col <- chainObject[[cname_attempt]]
                  if (is.character(this_col) | is.factor(this_col)) {
                    if (is.factor(this_col)) {
                      choices <- levels(this_col)
                    } else {
                      choices <- unique(this_col)
                    }

                    # safe limit
                    if (length(choices) < safe_lim / 2) {
                      results <- .rs.selectFuzzyMatches(choices, token)

                      # show only first 200 (max)
                      results <- results[seq(min(200, length(results)))]

                      return(.rs.makeCompletions(
                        token = token,
                        results = results,
                        quote = TRUE,
                        type = .rs.acCompletionTypes$STRING
                      ))
                    }
                  }
                }
              }
            }
          }
        },
        chop_locator_to = 1,
        safely = TRUE
      )
    )
  }

  auto_complete_matchArg_functionArgs <- function() {
    .rs.addFunction(
      "getCompletionsArgument",
      patch::patch_function(.rs.getCompletionsArgument,
        '"knitr"',
        {
          # one has to add {} in such cases

          list_match_args <- function(fun) {
            out <- list()
            if (is.function(fun)) {
              fbody <- body(fun)
              fbodyl <- as.character(fbody)
              fargs <- formals(fun)
              chk_strs1 <- paste0("arg_match[[:space:]]*\\([[:space:]]*",
                                  names(fargs), "[[:space:]]*\\)")
              chk_strs2 <- paste0("arg_match0[[:space:]]*\\([[:space:]]*",
                                  names(fargs), "[[:space:]]*\\)")
              chk_strs3 <- paste0("match.arg[[:space:]]*\\([[:space:]]*",
                                  names(fargs), "[[:space:]]*\\)")
              is_marg <- logical(length(chk_strs1))
              for (i in seq_along(chk_strs1)) {
                is_marg[i] <- any(grepl(chk_strs1[i], fbodyl)) |
                  any(grepl(chk_strs2[i], fbodyl)) |
                  any(grepl(chk_strs3[i], fbodyl))
              }
              if (any(is_marg)) {
                out <- fargs[is_marg]
              }
            }
            out
          }

          fun <- .rs.getAnywhere(as.character(functionCall)[1], envir)
          m_args <- list_match_args(fun)

          if (activeArg %in% names(m_args)) {
            choices <- eval(m_args[[activeArg]])
            results <- .rs.selectFuzzyMatches(choices, token)

            return(.rs.makeCompletions(
              token = token,
              results = results,
              quote = TRUE,
              type = .rs.acCompletionTypes$STRING
            ))
          }
        },
        chop_locator_to = 1,
        safely = TRUE
      )
    )
  }

  # not called yet
  auto_complete_reset <- function() {
    # reset
    .rs.addJsonRpcHandler(
      "get_completions",
      patch::patch_function(.rs.rpc.get_completions)
    )

    .rs.addFunction(
      "getCompletionsArgument",
      patch::patch_function(.rs.getCompletionsArgument)
    )
  }


  # implement
  options(store_function_before_patch = TRUE)
  auto_complete_future_plan()
  auto_complete_dplyr_filter()
  auto_complete_matchArg_functionArgs()
  options(store_function_before_patch = NULL)
}

if (exists(".rs.addFunction")) {
  enable_all()
} else {
  cat("\nThese are designed for R-Studio\n")
}

rm(enable_all)
