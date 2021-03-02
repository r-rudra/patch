test_that("load unload works", {

  # https://github.com/r-lib/covr/issues/40
  skip_on_cran()
  skip_if_not_installed("callr")
  rs <- asNamespace("callr")$r_session$new()
  rs$run(function() {
    tryCatch({
      library("patch", lib.loc = "../../")},
      error = function(e){
        if(requireNamespace("devtools", quietly = TRUE)){
          tryCatch(
            devtools::load_all("../../"),
            error = function(e) NULL
          )
        }
      })
  })

  if(isTRUE(rs$run(function() "patch" %in% .packages()))){
    rs$run(function() patch_function())

    expect_true(rs$run(function() exists("locate_section")))
    expect_equal(rs$run(function() formals(locate_section)),
                 as.pairlist(alist(f = , search_str = , all = FALSE)))
    rs$run(function() patch_function())
    expect_failure(
      expect_equal(rs$run(function() formals(locate_section)),
                   as.pairlist(alist(f = , search_str = , all = FALSE))))
    rs$run(function(){
      detach("package:patch",
             unload = TRUE, character.only = TRUE, force = TRUE)
    })

    expect_false(rs$run(function() exists("locate_section")))

  }

  rs$kill_tree()
})
