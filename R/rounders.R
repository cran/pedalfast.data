#' Round Age
#'
#' Round age per FITBIR guidelines
#'
#' FITBIR Definition for the "AgeYrs" data element: Value for participant's
#' subject age, calculated as elapsed time since the birth of the
#' participant/subject in years. The subjects age is typically recorded to the
#' nearest full year completed, e.g. 11 years and 6 months should be recorded as
#' 11 years.
#'
#' Guidelines & Instructions:
#' The subject's age is typically recorded to the nearest full year completed,
#' e.g. 11 years and 6 months should be recorded as 11 years. For subject's
#' which are under 1 year old, use decimal points and use the following
#' convention- record 1 month as 0.083 (1/12), 2 months as 0.166 (2/12), 3
#' months as 0.25 (3/12), 4 months as 0.333 (4/12), 5 months as 0.416 (5/12), 6
#' months as 0.5 (6/12), 7 months as 0.583 (7/12), 8 months as 0.666 (8/12), 9
#' months as 0.75 (9/12), 10 months as 0.833 (10/12), 11 months as 0.916 (11/12)
#' and 12 months as 1 year. For the individuals 90 or older, in order to
#' preserve PII, please submit "150" and make a note this in the "general notes"
#' column.
#'
#' @param x a numeric vector
#' @param type defaults to \code{character} (default), also accepts
#' \code{numeric}.
#'
#' @return a character or numeric vector depending on the value of \code{type}.
#'
#' @examples
#' ages <- c(92, 12.12, 89 + 10/12, 9.12, 9.73, 1.1, 1.75, ( 1:11 + 0.05 ) / 12,
#' 2, 90)
#'
#' round_age(ages)
#' round_age(ages, "numeric")
#'
#' @export
round_age <- function(x, type = "character") {

  brks <- c(0, seq(1, 12, by = 1) / 12, 2:90, Inf)
  names(brks) <- c("0", "0.083", "0.166", "0.25", "0.333", "0.416", "0.5",
                   "0.583", "0.666", "0.75", "0.833", "0.916", "1",
                   as.character(2:89), "150", "Older than dirt")

  rtn <- cut(x,
             breaks = brks,
             labels = names(brks)[-length(brks)],
             include.lowest = TRUE,
             right = FALSE)

  switch(type,
         character = as.character(rtn),
         numeric   = as.numeric(as.character(rtn)),
         )
}

