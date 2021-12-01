## ----label = "editing_note", eval = FALSE, include = TRUE, echo = FALSE, results = "hide"----
#  # ############################################################################ #
#  #                              **IMPORTANT NOTE**
#  #
#  # If you are reading this comment in a .Rmd file **DO NOT EDIT THE FILE**  The
#  # vignette was written in the source package under the
#  # vignette-spinners/datasets.R file.  Any changes to this vignette need to be
#  # made in that file.
#  #
#  # Edits made to the vignettes/datasets.Rmd file will not be preserved during the
#  # build process for the package.
#  #
#  #                              **IMPORTANT NOTE**
#  # ############################################################################ #

## ----label = "setup", include = FALSE-----------------------------------------
options(qwraps2_markup = "markdown")
knitr::opts_chunk$set(collapse = TRUE)

## ----label = "list_exported_datasets"-----------------------------------------
data(package = "pedalfast.data")$results[, c("Item", "Title")]

## ----label = "pedalfast_data"-------------------------------------------------
library(pedalfast.data)

## -----------------------------------------------------------------------------
data(pedalfast,          package = "pedalfast.data")
data(pedalfast_metadata, package = "pedalfast.data")

str(pedalfast,          max.level = 0)
str(pedalfast_metadata, max.level = 0)

## -----------------------------------------------------------------------------
head(pedalfast[, 1:3])
pedalfast_metadata[1:3, ]

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable == "studyid"))
str(pedalfast$studyid)

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable == "age"))
summary(pedalfast$age)          # in days
summary(pedalfast$age / 365.25) # in years

## -----------------------------------------------------------------------------
fitbir_ages <-
  data.frame(age  = pedalfast$age / 365.25,
             char = round_age(pedalfast$age / 365.25),
             num  = round_age(pedalfast$age / 365.25, type = "numeric"))

plot(fitbir_ages$age, fitbir_ages$num, xlab = "Age (years)", ylab = "FITBIR Age (Years)")

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable == "female"))
sum(pedalfast$female)
mean(pedalfast$female)

## -----------------------------------------------------------------------------
inj_vars <- c("sourceinj", "injurytoadmit", "injurymech")
knitr::kable(subset(pedalfast_metadata, variable %in% inj_vars))
summary(pedalfast[, inj_vars])

## -----------------------------------------------------------------------------
table(pedalfast$injurymech, useNA = "always")

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, grepl("^gcs.*ed$", variable)))
summary(pedalfast[, grep("^gcs.*ed$", names(pedalfast))])

## -----------------------------------------------------------------------------
knitr::kable(
             data.frame(integers = 1:6,
                        eye      = gcs_as_factor(1:6, scale = "eye"),
                        motor    = gcs_as_factor(1:6, scale = "motor"),
                        verbal   = gcs_as_factor(1:6, scale = "verbal"))
)

## -----------------------------------------------------------------------------
gcs_example_data <-
  data.frame(los = pedalfast$hosplos,
             motor_int = pedalfast$gcsmotored,
             motor_f1  = gcs_as_factor(pedalfast$gcseyeed, scale = "eye"),
             motor_f2  = gcs_as_factor(pedalfast$gcseyeed, scale = "eye", highest_first = TRUE))

head(gcs_example_data)

## -----------------------------------------------------------------------------
summary(gcs_example_data)

## -----------------------------------------------------------------------------
summary(lm(los ~ motor_int, data = gcs_example_data))$coef
summary(lm(los ~ motor_f1,  data = gcs_example_data))$coef
summary(lm(los ~ motor_f2,  data = gcs_example_data))$coef

## -----------------------------------------------------------------------------
summary(pedalfast$gcsed)

## -----------------------------------------------------------------------------
identical(pedalfast$gcsed,
          pedalfast$gcseyeed + pedalfast$gcsmotored + pedalfast$gcsverbaled)

identical(pedalfast$gcsed,
          as.integer(gcs_as_factor(pedalfast$gcseyeed, "eye")) +
          as.integer(gcs_as_factor(pedalfast$gcsmotored, "motor")) +
          as.integer(gcs_as_factor(pedalfast$gcsverbaled, "verbal")))

identical(pedalfast$gcsed,
          as.integer(gcs_as_factor(pedalfast$gcseyeed, "eye", highest_first = TRUE)) +
          as.integer(gcs_as_factor(pedalfast$gcsmotored, "motor", highest_first = TRUE)) +
          as.integer(gcs_as_factor(pedalfast$gcsverbaled, "verbal", highest_first = TRUE)))

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable == "eddisposition"))
table(pedalfast$eddisposition, useNA = "always")

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, grepl("^(admitto)*ct", variable)))
head(pedalfast[, grepl("^(admitto)*ct", names(pedalfast))])
summary(pedalfast[, grepl("^(admitto)*ct", names(pedalfast))])

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable == "sourceicu"))
table(pedalfast$sourceicu, useNA = "always")

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable == "puplrcticu"))
table(pedalfast$puplrcticu, useNA = "always")

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, grepl("^gcs.*icu$", variable)))
summary(pedalfast[, grepl("^gcs.*icu$", names(pedalfast))])

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, grepl("^admittoicu", variable)))
summary(pedalfast[, grepl("^admittoicu", names(pedalfast))])

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable %in% c("ventyn", "admittoint", "admittoext")))
summary(pedalfast[, c("ventyn", "admittoint", "admittoext")])

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, grepl("^(admitto)*icp.+\\d", variable)))
summary(pedalfast[pedalfast$icpyn1 == 1, grepl("^(admitto)*icp.+\\d", names(pedalfast))])

## -----------------------------------------------------------------------------
table(pedalfast$icptype1)

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, grepl("^(admitto)*cath.+\\d", variable)))

## -----------------------------------------------------------------------------
summary(
  subset(pedalfast,
         !is.na(cathtype1),
         select = grep("cath.*1$", names(pedalfast)))
)
table(pedalfast$cathtype1)

## -----------------------------------------------------------------------------
summary(
  subset(pedalfast,
         !is.na(cathtype2),
         select = grep("cath.*2$", names(pedalfast)))
)
table(pedalfast$cathtype2)

## -----------------------------------------------------------------------------
summary(
  subset(pedalfast,
         !is.na(cathtype3),
         select = grep("cath.*3$", names(pedalfast)))
)
table(pedalfast$cathtype3)

## -----------------------------------------------------------------------------
summary(
  subset(pedalfast,
         !is.na(cathtype4),
         select = grep("cath.*4$", names(pedalfast)))
)
table(pedalfast$cathtype4)

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable %in% c("newtrachyn", "admittotrach")))
summary(pedalfast[pedalfast$newtrachyn == 1, c("newtrachyn", "admittotrach")])

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable %in% c("newgastyn", "admittogast")))
summary(pedalfast[pedalfast$newgastyn == 1, c("newgastyn", "admittogast")])

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable %in% c("decomcranyn", "admittocrani")))
summary(pedalfast[pedalfast$decomcranyn == 1, c("decomcranyn", "admittocrani")])

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable %in% c("lmbrdrainyn", "admittolmbdrain")))
summary(pedalfast[pedalfast$lmbrdrainyn == 1, c("lmbrdrainyn", "admittolmbdrain")])

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable %in% c("epihemyn", "admittoedhevac")))
summary(pedalfast[pedalfast$epihemyn == 1, c("epihemyn", "admittoedhevac")])

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable %in% c("subhemyn", "admittosdhevac")))
summary(pedalfast[pedalfast$subhemyn == 1, c("subhemyn", "admittosdhevac")])

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, grepl("^rx", variable)))
summary(pedalfast[, grepl("^rx", names(pedalfast))])

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable %in% c("tpnyn", "admittotpn")))
summary(pedalfast[pedalfast$tpnyn == 1, c("tpnyn", "admittotpn")])

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable %in% c("entnutyn", "admittoentnut")))
summary(pedalfast[pedalfast$entnutyn == 1, c("entnutyn", "admittoentnut")])

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, variable %in% c("hosplos", "hospdisposition")))
summary(pedalfast[, c("hosplos", "hospdisposition")])
table(pedalfast$hospdisposition, useNA = "always")

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, grepl("^cardiac", variable)))
summary(pedalfast[, grepl("^cardiac", names(pedalfast))])

## -----------------------------------------------------------------------------
knitr::kable(subset(pedalfast_metadata, grepl("fss", variable)))
summary(pedalfast[, grepl("fss", names(pedalfast))])

