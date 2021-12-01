test_that("length(x) != 1 throws error in pedalfast_factor",
          {
            expect_error(pedalfast_factor(x = 1, variable = c("a", "b")))
            expect_error(pedalfast_factor(x = 1, variable = character(0)))
            expect_error(pedalfast_factor(x = 1, variable = 19))
          })

test_that("bad name in pedalfast_factor returns error",
          {
            expect_error(pedalfast_factor(x = 1, variable = "gcsmotore"))
          })
