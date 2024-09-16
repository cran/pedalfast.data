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
library(qwraps2) # load and attach so you can use `%s%` easily
options(qwraps2_markup = "markdown")
options(knitr.kable.NA = '')
knitr::opts_chunk$set(collapse = TRUE)

## ----label = "list_exported_datasets", results = "markup"---------------------
data(package = "pedalfast.data")$results[, c("Item", "Title")]

## ----label = "pedalfast_data", echo = TRUE, results = "hide"------------------
library(pedalfast.data)

## -----------------------------------------------------------------------------
data(pedalfast,          package = "pedalfast.data")
data(pedalfast_metadata, package = "pedalfast.data")

str(pedalfast,          max.level = 0)
str(pedalfast_metadata, max.level = 0)

## -----------------------------------------------------------------------------
head(pedalfast[, 1:3])
pedalfast_metadata[1:3, ]

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, variable == "studyid") |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
str(pedalfast$studyid)

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, variable == "age") |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast$age)          # in days
summary(pedalfast$age / 365.25) # in years

## ----echo = TRUE, results = "markup"------------------------------------------
fitbir_ages <-
  data.frame(age  = pedalfast$age / 365.25,
             char = round_age(pedalfast$age / 365.25),
             num  = round_age(pedalfast$age / 365.25, type = "numeric"))

plot(x = fitbir_ages$age,
     y = fitbir_ages$num,
     xlab = "Age (years)",
     ylab = "FITBIR Age (Years)")

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, variable == "female") |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
with(pedalfast, {table(female)})
with(pedalfast, {prop.table(table(female))})

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, variable %in% c("sourceinj", "injurytoadmit", "injurymech")) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[, c("sourceinj", "injurytoadmit", "injurymech")])

## ----echo = TRUE, results = "markup"------------------------------------------
table(pedalfast$injurymech, useNA = "always")

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, grepl("^gcs.*ed$", variable)) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[, grep("^gcs.*ed$", names(pedalfast))])

## ----results = 'asis'---------------------------------------------------------
data.frame(integer = 1:6,
           eye      = gcs_as_factor(1:6, scale = "eye"),
           motor    = gcs_as_factor(1:6, scale = "motor"),
           verbal   = gcs_as_factor(1:6, scale = "verbal")
           ) |>
  knitr::kable(format = "html", row.names = FALSE, align = "clll") |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## -----------------------------------------------------------------------------
gcs_example_data <-
  data.frame(los       = pedalfast$hosplos,
             motor_int = pedalfast$gcsmotored,
             motor_f1  = gcs_as_factor(pedalfast$gcsmotored, scale = "eye"),
             motor_f2  = gcs_as_factor(pedalfast$gcsmotored, scale = "eye", highest_first = TRUE))

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
identical(
  pedalfast$gcsed,
  pedalfast$gcseyeed + pedalfast$gcsmotored + pedalfast$gcsverbaled
)

identical(
  pedalfast$gcsed,
  as.integer(gcs_as_factor(pedalfast$gcseyeed,    "eye")) +
    as.integer(gcs_as_factor(pedalfast$gcsmotored,  "motor")) +
    as.integer(gcs_as_factor(pedalfast$gcsverbaled, "verbal"))
)

identical(
  pedalfast$gcsed,
  as.integer(gcs_as_factor(pedalfast$gcseyeed,    "eye",    highest_first = TRUE)) +
    as.integer(gcs_as_factor(pedalfast$gcsmotored,  "motor",  highest_first = TRUE)) +
    as.integer(gcs_as_factor(pedalfast$gcsverbaled, "verbal", highest_first = TRUE))
)

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, variable == "eddisposition") |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
table(pedalfast$eddisposition, useNA = "always")

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, grepl("^(admitto)*ct", variable)) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----label = "summary table of imaging", echo = FALSE, results = "asis"-------
qs <-
  c(qwraps2::qsummary(pedalfast["admittoct"])[1],
    list(
      "Findings" = list(
        "ctskullfrac"    = ~ qwraps2::n_perc(ctskullfrac,    na_rm = TRUE, show_denom = "always"),
        "ctce"           = ~ qwraps2::n_perc(ctce,           na_rm = TRUE, show_denom = "always"),
        "ctmidlineshift" = ~ qwraps2::n_perc(ctmidlineshift, na_rm = TRUE, show_denom = "always"),
        "ctcompress"     = ~ qwraps2::n_perc(ctcompress,     na_rm = TRUE, show_denom = "always"),
        "ctintraventhem" = ~ qwraps2::n_perc(ctintraventhem, na_rm = TRUE, show_denom = "always"),
        "ctsubarchhem"   = ~ qwraps2::n_perc(ctsubarchhem,   na_rm = TRUE, show_denom = "always"),
        "ctintraventhem" = ~ qwraps2::n_perc(ctintraventhem, na_rm = TRUE, show_denom = "always"),
        "ctsubhematoma"  = ~ qwraps2::n_perc(ctsubhematoma,  na_rm = TRUE, show_denom = "always"),
        "ctepihematoma"  = ~ qwraps2::n_perc(ctepihematoma,  na_rm = TRUE, show_denom = "always")
      )
    )
  )

qwraps2::summary_table(pedalfast, qs)

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, variable == "sourceicu") |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
table(pedalfast$sourceicu, useNA = "always")

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, variable == "puplrcticu") |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
table(pedalfast$puplrcticu, useNA = "always")

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, grepl("^gcs.*icu$", variable)) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[, grepl("^gcs.*icu$", names(pedalfast))])

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, grepl("^admittoicu", variable)) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[, grepl("^admittoicu", names(pedalfast))])

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, variable %in% c("ventyn", "admittoint", "admittoext")) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[, c("ventyn", "admittoint", "admittoext")])

## ----label = "icp summary", echo = FALSE, results = "asis"--------------------
knitr::kable(subset(pedalfast_metadata, grepl("^(admitto)*icp.+\\d", variable)))


## ----label = "icp summary table", echo = FALSE, results = "markup"------------
summary(pedalfast[pedalfast$icpyn1 == 1, grepl("^(admitto)*icp.+\\d", names(pedalfast))])

## ----include = FALSE----------------------------------------------------------
icptypes <- unique(na.omit(with(pedalfast, c(icptype1, icptype2, icptype3))));

## ----echo = TRUE, results = "hide"--------------------------------------------
icptypes <-
  xtabs( ~ icptype1 + icptype2 + icptype3, data = pedalfast, addNA = TRUE) |>
  as.data.frame()
icptypes <- subset(icptypes, Freq > 0)


## ----echo = FALSE, results = "asis"-------------------------------------------
icptypes |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, grepl("^(admitto)*cath.+\\d", variable)) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(
  subset(pedalfast,
         !is.na(cathtype1),
         select = grep("cath.*1$", names(pedalfast)))
)
table(pedalfast$cathtype1)

## ----echo = TRUE, results = "markup"------------------------------------------
summary(
  subset(pedalfast,
         !is.na(cathtype2),
         select = grep("cath.*2$", names(pedalfast)))
)
table(pedalfast$cathtype2)

## ----echo = TRUE, results = "markup"------------------------------------------
summary(
  subset(pedalfast,
         !is.na(cathtype3),
         select = grep("cath.*3$", names(pedalfast)))
)
table(pedalfast$cathtype3)

## ----echo = TRUE, results = "markup"------------------------------------------
summary(
  subset(pedalfast,
         !is.na(cathtype4),
         select = grep("cath.*4$", names(pedalfast)))
)
table(pedalfast$cathtype4)

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, variable %in% c("newtrachyn", "admittotrach")) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[pedalfast$newtrachyn == 1, c("newtrachyn", "admittotrach")])

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, variable %in% c("newgastyn", "admittogast")) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[pedalfast$newgastyn == 1, c("newgastyn", "admittogast")])

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, variable %in% c("decomcranyn", "admittocrani")) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[pedalfast$decomcranyn == 1, c("decomcranyn", "admittocrani")])

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, variable %in% c("lmbrdrainyn", "admittolmbdrain")) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[pedalfast$lmbrdrainyn == 1, c("lmbrdrainyn", "admittolmbdrain")])

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, variable %in% c("epihemyn", "admittoedhevac")) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[pedalfast$epihemyn == 1, c("epihemyn", "admittoedhevac")])

## ----echo = FALSE, results = "asis"-------------------------------------------
subset(pedalfast_metadata, variable %in% c("subhemyn", "admittosdhevac")) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[pedalfast$subhemyn == 1, c("subhemyn", "admittosdhevac")])

## -----------------------------------------------------------------------------
subset(pedalfast_metadata, grepl("^rx", variable)) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[, grepl("^rx", names(pedalfast))])

## -----------------------------------------------------------------------------
subset(pedalfast_metadata, variable %in% c("tpnyn", "admittotpn")) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[pedalfast$tpnyn == 1, c("tpnyn", "admittotpn")])

## -----------------------------------------------------------------------------
subset(pedalfast_metadata, variable %in% c("entnutyn", "admittoentnut")) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[pedalfast$entnutyn == 1, c("entnutyn", "admittoentnut")])

## -----------------------------------------------------------------------------
subset(pedalfast_metadata, variable %in% c("hosplos", "hospdisposition")) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[, c("hosplos", "hospdisposition")])

## ----echo = TRUE, results = "markup"------------------------------------------
qwraps2::summary_table(pedalfast["hospdisposition"])

## -----------------------------------------------------------------------------
subset(pedalfast_metadata, grepl("^cardiac", variable)) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[, grepl("^cardiac", names(pedalfast))])

## -----------------------------------------------------------------------------
subset(pedalfast_metadata, grepl("fss", variable)) |>
  knitr::kable(format = "html", row.names = FALSE) |>
  kableExtra::kable_styling(bootstrap_options = "striped")

## ----echo = TRUE, results = "markup"------------------------------------------
summary(pedalfast[, grepl("fss", names(pedalfast))])

