#' PEDALFAST factor
#'
#' A flavor of the base function factor but aimed to use specific default values
#' for levels and labels based on the information in the
#' \code{\link{pedalfast_metadata}} object.
#'
#' @param x a vector of data
#' @param variable character string identifying the variable name in
#' \code{pedalfast_metadata} defining the levels and labels for the factor.
#' @param label_with_level (default to FALSE) labels will include the integer
#' value.  See examples.
#' @param ... not currently used.
#'
#' @references
#' Bennett TD, Dixon RR, Kartchner C, DeWitt PE, Sierra Y, Ladell D, Kempe A,
#' Runyan DK, Dean JM, Keenan HT. Functional Status Scale in Children With
#' Traumatic Brain Injury: A Prospective Cohort Study. Pediatr Crit Care Med.
#' 2016 Dec;17(12):1147-1156. doi: 10.1097/PCC.0000000000000934. PMID: 27753754;
#' PMCID: PMC5138132.
#'
#' Pollack MM, Holubkov R, Funai T, et al. Pediatric intensive care outcomes:
#' development of new morbidities during pediatric critical care. Pediatr Crit
#' Care Med. 2014;15(9):821-827. doi:10.1097/PCC.0000000000000250
#'
#' @return An object of class \code{factor}
#'
#' @examples
#' data(pedalfast, pedalfast_metadata, package = "pedalfast.data")
#'
#' # The Motor GCS in the emergency department is reported as an integer value.
#' str(pedalfast$gcsmotored)
#'
#' # Each integer value a specific meaning
#' pedalfast_metadata[grepl("gcsmotored", pedalfast_metadata$variable), ]
#'
#' # Creating the factor in base R
#'
#' pedalfast_factor(x = c(1, 3, 2), variable = "gcsmotored")
#' pedalfast_factor(x = c(1, 3, 2), variable = "gcsmotored", label_with_level = TRUE)
#'
#'
#' @export
pedalfast_factor <- function(x, variable, label_with_level = FALSE, ...) {
  if ( !is.character(variable) | length(variable) != 1L) {
    stop("variable needs to be a character vector of length 1.")
  }

  e <- new.env()
  utils::data(list = "pedalfast_metadata", envir = e)

  rw <- e$pedalfast_metadata$variable == variable

  if (sum(rw) != 1L) {
    stop(paste(deparse(substitute(variable)), "does not uniquely match a variable in pedalfast_metadata"))
  }

  rw <- e$pedalfast_metadata[rw, ]
  v <- strsplit(rw$values, "\\ \\|\\ ")[[1]]
  lvls <- as.integer(sub("^(\\d+),.+$", "\\1", v))
  if (label_with_level) {
    lbls <- sub("^(\\d+),\\ (.+)$", "\\1 - \\2", v)
  } else {
    lbls <- sub("^\\d+,\\ (.+)$", "\\1", v)
  }

  do.call(factor, list(x = x, levels = lvls, labels = lbls))
}

#' GCS Factor/Integer
#'
#' Functions for mapping integer values to GCS labeled factor and visa versa
#'
#' @param x a integer, factor, or character vector.
#' @param scale a character string to denote eye, motor, or verbal GCS scale.
#' @param long_label logical to prepend the numeric value to the label of a
#' factor.
#' @param highest_first logical if the factor levels should be ordered with the
#' highest GCS score as the reference level, else the lowest GCS score as the
#' reference level.
#' @param ... not currently used.
#'
#' @return \code{gcs_as_factor} returns a factor of equal length to \code{x}.
#'
#' \code{gcs_as_integer} returns an integer vector of equal length to \code{x}.
#'
#' @examples
#'
#' ########################################
#' # Mapping from numeric values to factor:
#' nums <- c(0:7, 2.3)
#'
#' # with short labels
#' data.frame(nums   = nums,
#'            eye    = gcs_as_factor(nums, scale = "eye"),
#'            motor  = gcs_as_factor(nums, scale = "motor"),
#'            verbal = gcs_as_factor(nums, scale = "verbal"))
#'
#' # with long labels
#' data.frame(nums   = nums,
#'            eye    = gcs_as_factor(nums, scale = "eye", long_label = TRUE),
#'            motor  = gcs_as_factor(nums, scale = "motor", long_label = TRUE),
#'            verbal = gcs_as_factor(nums, scale = "verbal", long_label = TRUE))
#'
#' ###################################################
#' # Mapping from factors/characters to numeric values
#'
#' # A quick way to access the labels and numeric values
#' pedalfast.data::gcs_ll
#'
#' all_levels <- do.call(c, lapply(pedalfast.data::gcs_ll, names))
#'
#' data.frame(lvls   = all_levels,
#'            eye    = gcs_as_integer(all_levels, scale = "eye"),
#'            motor  = gcs_as_integer(all_levels, scale = "motor"),
#'            verbal = gcs_as_integer(all_levels, scale = "verbal")
#'            )
#'
#' ##################################################
#' # Order of the levels:
#' # The data values are the same, but the order of the levels differs.
#' gcs_as_factor(1:4, "eye", highest_first = FALSE)
#' gcs_as_factor(1:4, "eye", highest_first = TRUE)
#'
#' @name gcs
NULL

#' @rdname gcs
#' @export
gcs_as_integer <- function(x, scale, ...) {
  UseMethod("gcs_as_integer")
}

#' @rdname gcs
#' @export
gcs_as_integer.factor <- function(x, scale, ...) {
  gcs_as_integer(as.character(x), scale = scale, ...)
}

#' @rdname gcs
#' @export
gcs_as_integer.character <- function(x, scale, ...) {
  if (length(scale) != 1L | !any(scale %in% c("eye", "motor", "verbal"))) {
    stop("scale needs to be one of 'eye', 'motor', or 'verbal'")
  }

  rtn <- rep(NA_integer_, length(x))
  for (i in seq_along(gcs_ll[[scale]])) {
    idx <- grep(paste0("^", names(gcs_ll[[scale]])[i], "$"), x)
    rtn[idx] <- gcs_ll[[scale]][i]
  }
  rtn
}

#' @rdname gcs
#' @export
gcs_as_factor <- function(x, scale, long_label = FALSE, highest_first = FALSE, ...) {
  UseMethod("gcs_as_factor")
}

#' @rdname gcs
#' @export
gcs_as_factor.character <- function(x, scale, long_label = FALSE, highest_first = FALSE, ...) {
  gcs_as_factor(gcs_as_integer(x, scale = scale), scale = scale, long_label = long_label, ...)
}

#' @rdname gcs
#' @export
gcs_as_factor.numeric <- function(x, scale, long_label = FALSE, highest_first = FALSE, ...) {
  if (length(scale) != 1L | !any(scale %in% c("eye", "motor", "verbal"))) {
    stop("scale needs to be one of 'eye', 'motor', or 'verbal'")
  }

  lvls <- gcs_ll[[scale]]
  lbls <- names(gcs_ll[[scale]])

  if (long_label) {
    lbls <- paste(lvls, lbls, sep = " - ")
  }

  if (highest_first) {
    lvls <- rev(lvls)
    lbls <- rev(lbls)
  }

  factor(x, levels = lvls, labels = lbls)
}

#' @rdname gcs
#' @export
gcs_ll <-
  list(eye =
       c(
         "No response"  = 1,
         "To pain only" = 2,
         "To speech"    = 3,
         "Spontaneous"  = 4
         ),
       motor =
         c(
           "No response/flaccid"                  = 1,
           "Abnormal extension to pain"           = 2,
           "Abnormal flexion to pain"             = 3,
           "Withdraws from painful stimuli"       = 4,
           "Localizes pain or withdraws to touch" = 5,
           "Obeys commands"                       = 6
           ),
       verbal =
         c(
           "No response"                               = 1,
           "Incomprehensible sounds or moans to pain"  = 2,
           "Inappropriate words or cries to pain"      = 3,
           "Confused or irritable cries"               = 4,
           "Oriented, appropriate or coos and babbles" = 5
         )
  )

#' Functional Status Scale Categories
#'
#' Mapping FSS Total scores (integer values) to categorical values.
#'
#' FSS scores are integer values from 6 to 30.  
#'
#' The a mapping of ranges of integer values to categories is
#' \itemize{
#'   \item FSS 6, 7: Good
#'   \item FSS 8, 9: Mildly abnormal
#'   \item FSS 10, 11, 12, 13, 14, 15: Moderately abnormal
#'   \item FSS 16, 17, 18, 19, 20, 21: Severe abnormal
#'   \item FSS 22, 23, 24, 25, 26, 27, 28, 29, 30: Very severely abnormal
#' }
#'
#'
#' @param x an integer vector
#' @param long_label logical if the score range should be prepended to the
#' label.
#' @param ... not currently used.
#'
#' @return A factor of equal length to the input \code{x} with labels for the
#' categorical ranges of FSS.
#'
#' @examples
#' x <- seq(5, 32)
#' data.frame(x           = x,
#'            short_label = fss_as_factor(x),
#'            long_label  = fss_as_factor(x, long_label = TRUE))
#'
#' @export
fss_as_factor <- function(x, long_label = FALSE, ...) {
  UseMethod("fss_as_factor")
}

#' @export
fss_as_factor.numeric <- function(x, long_label = FALSE, ...) {
  lbs <- c("Good", "Mildly abnormal", "Moderately abnormal", "Severe abnormal", "Very severely abnormal")

  if (long_label) {
    lbs <- paste0(c("6-7 (", "8-9 (", "10-15 (", "16-21 (", "22-30 ("), lbs, ")")
  }

  rtn <- character(length(x))
  rtn[which(x %in% seq( 6,  7, by = 1))] <- "GOOD"
  rtn[which(x %in% seq( 8,  9, by = 1))] <- "MILD"
  rtn[which(x %in% seq(10, 15, by = 1))] <- "MODR"
  rtn[which(x %in% seq(16, 21, by = 1))] <- "SEVR"
  rtn[which(x %in% seq(21, 30, by = 1))] <- "VSVR"

  factor(rtn, levels = c("GOOD", "MILD", "MODR", "SEVR", "VSVR"), labels = lbs)
}


