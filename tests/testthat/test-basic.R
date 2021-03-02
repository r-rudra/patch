test_that("basic things works", {

  options(store_function_before_patch = TRUE)

  f0 <- function(x){
    if(TRUE){
      l <- list()
      l$a <- function(y){
        x+y
      }
      l$a(x^2)
    }
  }

  f1 <- patch_function(f0, "x \\+ y", x*y)
  expect_equal(get_section(body(f0), locate_section(body(f0),"x \\+ y")),
               get_section(body(f1), locate_section(body(f0),"x \\+ y")))
  rn <- rnorm(10)
  expect_equal(f1(rn), rn^3)

  f2 <- patch_function(f0, "x \\+ y", x*y, modification_placement = "before")

  expect_equal(get_section(body(f2), locate_section(body(f0),"x \\+ y")),
               substitute(x * y))

  expect_equal(f0(rn), f2(rn))

  f3 <- patch_function(f0, "x \\+ y", x*y, modification_placement = "replace")
  expect_equal(get_section(body(f3), locate_section(body(f0),"x \\+ y")),
               substitute(x * y))
  expect_equal(f1(rn), f3(rn))

  expect_error(f3(),  'argument "x" is missing')

  f4 <- patch_function(f0, "x \\+ y", x*y, replace_it = TRUE,
                       new_arguments = list(x = 5))

  expect_failure(expect_error(f4(),  'argument "x" is missing'))
  expect_equal(f4(), sqrt(f3(5)*f4(5)))

  patch_function(f4, "x \\* y", x^y, modification_placement = "after",
                 auto_assign_patched_function = TRUE)
  expect_equal(f4(2),2^(2^2))

})


test_that("intermediate features works", {

  options(store_function_before_patch = TRUE)

  f02 <- function() {
    "here"
    if (TRUE) {
      "here"
    }
  }

  f002 <- patch_function(f02, "here", which_match_to_target = 2)

  expect_equal(as.character(body(f002)[[3]][[3]][[2]]),
               c("tryCatch", "here", "function(e) browser()"))

  expect_equal(
    patch_function(f02, "here","there",
                   which_match_to_target = 2,
                   modification_placement = "after")(),
    c("here","there")
  )

  body(f02)[[3]][[4]] <- substitute({
    wrong
  })

  patch_function(f02, "TRUE", FALSE, replace_it = TRUE,
                 auto_assign_patched_function = TRUE)


  expect_error(f02(), "object 'wrong' not found")


  f1 <- patch_function(f02, "wrong")

  patch_function(f1, "browser", function(er) {"hi"},
                 replace_it = T, move_up_to = 2,
                 auto_assign_patched_function = TRUE)
  expect_equal(f1(), "hi")

  patch_function(f02, auto_assign_patched_function = TRUE)

  f2 <- patch_function(f02, "here", "there",
                       which_match_to_target = 2, safely = TRUE,
                       modification_placement = "replace", move_up_to = 2)

  expect_equal(f2(), "there")

  f2 <- patch_function(f02, "here", {
    message("there")
    warning("bad")
  },
  which_match_to_target = 2, safely = TRUE,
  modification_placement = "replace", move_up_to = 2)

  expect_failure(expect_message(f2()))

  f3 <- patch_function(f02, "here",log("hi"),
                       which_match_to_target = 2,
                       safely = TRUE, move_up_to = 2)

  expect_failure(expect_error(f3()))

  expect_error(patch_function(f02, "here",log("hi"),
                              which_match_to_target = 2),
               "Some error occurred during patching")


})


test_that("pkg replacement works", {

  options(store_function_before_patch = TRUE)
  expect_warning(nr <- patch_function(rbinom, "size", 10,
                                      new_arguments = alist(n=10, prob = 0.5),
                                      modification_placement = "replace"),
                 "is missed")

  rs <- round(runif(1)*100)
  set.seed(rs)
  res1 <- nr()

  set.seed(rs)
  res2 <- rbinom(10, 10, 0.5)

  expect_equal(res1, res2)

  # expect_error(rbinom(),"is missing, with no default")

  patch_function(rbinom,
                 new_arguments = alist(n=10, prob = 0.5),
                 auto_assign_patched_function = TRUE)
  expect_failure(expect_error(rbinom(size = 10),"is missing, with no default"))

  # reset
  patch_function(rbinom,
                 auto_assign_patched_function = TRUE)

  expect_error(rbinom(size = 10),"is missing, with no default")


  expect_equal(get_f_well_name(utils::choose.files),
               "from_p_utils_choose.files")

})


