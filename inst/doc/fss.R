## ----label = "setup", include = FALSE-----------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

## ----label = "system-location-of-vignette", eval = FALSE----------------------
#  system.file("doc/fss.Rmd", package = "pedalfast.data")

## ----label = "namespaces"-----------------------------------------------------
library(pedalfast.data)
library(knitr)
library(qwraps2)
library(ggplot2)
options(qwraps2_markup = 'markdown')

data(pedalfast, pedalfast_metadata)

## ----label = "build-gcsusing"-------------------------------------------------
# gather all the gcs values form the ed.
gcs_ed_vars <- grep("^gcs.*ed$", names(pedalfast), value = TRUE)
gcs_ed_vars

# create a set of gcs _using variables.
for(j in gcs_ed_vars) {
  pedalfast[[sub("ed$", "_using", j)]] <-
    ifelse(is.na(pedalfast[[j]]), pedalfast[[ sub("ed$", "icu", j) ]], pedalfast[[j]])
}

summary(pedalfast$gcs_using)

# Inspect the rows with missing values:
pedalfast[is.na(pedalfast$gcs_using), ]

## ----label = "subset-via-gcsusing"--------------------------------------------
pedalfast <- subset(pedalfast, !is.na(gcs_using))

## -----------------------------------------------------------------------------
pedalfast$gcseye_using_cat    <- gcs_as_factor(pedalfast$gcseye_using, "eye")
pedalfast$gcsmotor_using_cat  <- gcs_as_factor(pedalfast$gcsmotor_using, "motor")
pedalfast$gcsverbal_using_cat <- gcs_as_factor(pedalfast$gcsverbal_using, "verbal")

## -----------------------------------------------------------------------------
pedalfast$severetbi <- as.integer(pedalfast$gcs_using <= 8)

## -----------------------------------------------------------------------------
fsscols <- grep("^fss", names(pedalfast), value = TRUE)
fsscols

pedalfast$fsstotal <- rowSums(pedalfast[, fsscols])
summary(pedalfast$fsstotal)

## -----------------------------------------------------------------------------
with(pedalfast, table(hospdisposition, is.na(fsstotal)))

## -----------------------------------------------------------------------------
pedalfast$fsstotal_cat  <- fss_as_factor(pedalfast$fsstotal, long_label = FALSE)
pedalfast$fsstotal_cat2 <- fss_as_factor(pedalfast$fsstotal, long_label = TRUE)

table(pedalfast$fsstotal_cat)
table(pedalfast$fsstotal_cat2)

fss_labeller <- function(x) {
  x <- sub("fssmental", "Mental", x)
  x <- sub("fsssensory", "Sensory", x)
  x <- sub("fsscommun", "Communication", x)
  x <- sub("fssmotor", "Motor", x)
  x <- sub("fssfeeding", "Feeding", x)
  x <- sub("fssresp", "Respiratory", x)
  x
}

## ----label = "dysfunction_summary"--------------------------------------------
some_dysfunction <-
  lapply(grep("^fss(?!total)", names(pedalfast), perl = TRUE, value = TRUE),
         function(x) {
           rtn <-
             data.frame(variable = x,
                        p        = mean(na.omit(pedalfast[[x]]) > 1))
           rtn$plb <- paste0(frmt(rtn$p * 100), "% (", sub("^fss(\\w)", "\\U\\1", x, perl = TRUE), ")")
           rtn
         })

some_dysfunction <- do.call(rbind, some_dysfunction)
some_dysfunction

# when at least one of the fss scales reported a value of 2, and 2 is the max
# value across all six sclaes, the most common scale with a value of 2 is:
fss_matrix <- pedalfast[, grep("^fss(?!total)", names(pedalfast), perl = TRUE, value = TRUE)]

fss_matrix <- suppressWarnings(cbind(fss_matrix, max = apply(fss_matrix, 1, max, na.rm = TRUE)))
fss_matrix <- fss_matrix[fss_matrix$max > -Inf, ]

score_mode <-
  lapply(2:5, function(i) apply(fss_matrix[fss_matrix$max == i, ], 2, function(x) sum(x == i, na.rm = TRUE)))
score_mode <- do.call(rbind, score_mode)
score_mode
colSums(score_mode)

apply(score_mode[, -7], 1, which.max)

## -----------------------------------------------------------------------------
fit <- lm(fsstotal ~ injurymech + 0 + I(age / 365.25), data = pedalfast)
summary(fit)
car::Anova(fit, type = 2)
fsstotal_lm_cis <- qwraps2::frmtci(cbind(coef(fit), confint(fit)), show_level = TRUE)
fsstotal_lm_cis

## -----------------------------------------------------------------------------
fit <- lm(fsstotal ~ gcs_using, data = pedalfast)
fss_by_gcs_ci <- qwraps2::frmtci(cbind(coef(fit), confint(fit)), show_level = TRUE)

mean_fss_by_gcs <-
  aggregate(fsstotal ~ gcs_using, data = pedalfast, FUN = mean_sd, show_n = "never", na_rm = TRUE)

mean_fss_by_gcsm <-
  aggregate(fsstotal ~ gcsmotor_using, data = pedalfast, FUN = mean_sd, show_n = "never", na_rm = TRUE)

## -----------------------------------------------------------------------------
summary(pedalfast[, grep("^ct", names(pedalfast))])
pedalfast$anyblood <- with(pedalfast, as.integer(ctintraparhem + ctsubarchhem + ctintraventhem + ctsubhematoma + ctepihematoma > 0))

## ----label = "figure1", warning = FALSE, fig.width = 7, fig.height = 7--------
# data.table::melt or tidyr::pivot_longer would make this data step much easier
figure1_data <-
  lapply(grep("^fss(?!total)", names(pedalfast), perl = TRUE, value = TRUE),
         function(x) data.frame(variable = x, value = pedalfast[, x]))
figure1_data <- do.call(rbind, figure1_data)

ggplot(figure1_data) +
  aes(x = value, y = (..count..)/sum(..count..)) +
  geom_bar() +
  facet_wrap( ~ variable) +
  theme_classic(base_size = 18) +
  ylab("") +
  scale_y_continuous(labels = scales::percent)

## ----label = "figure2", warning = FALSE, fig.width = 7, fig.height = 7--------
ggplot(pedalfast) +
  aes(x = gcs_using, y = fsstotal) +
  stat_sum(aes(size = ..n..), alpha = 0.2) +
  scale_size_area(breaks = c(5, 10, 15), "Count", max_size = 7) +
  stat_smooth(method = "lm", formula = y ~ x, size = 0.5, alpha = 0.4, level = 0.95, color = "black", linetype = 2) +
  scale_x_continuous(breaks = seq(3, 15, 1)) +
  scale_y_continuous(breaks = c(6, 10, 15, 20, 25), limits = c(5, 25)) +
  xlab("Glasgow Coma Scale") +
  ylab("Discharge FSS") +
  theme_classic(base_size = 16) +
  theme(legend.position = c(0.9, 0.9))

## ----label = "figure3", warning = FALSE, fig.width = 7, fig.height = 7--------
figure3_data <-
  lapply(grep("^ct.+(hem|toma)$", names(pedalfast), perl = TRUE, value = TRUE),
         function(x) data.frame(studyid = pedalfast[["studyid"]],
                                fsstotal = pedalfast[["fsstotal"]],
                                variable = x,
                                value = pedalfast[, x]))
figure3_data <- do.call(rbind, figure3_data)

figure3_data$variable <-
  factor(figure3_data$variable,
         levels = c("ctintraparhem", "ctsubarchhem", "ctintraventhem", "ctsubhematoma", "ctepihematoma"),
         labels = c("Intraparenchymal\nHemorrhage", "Subarachnoid\nHemorrhage", "Intraventricular\nHemorrhage", "Subdural Hematoma", "Epidural Hematoma"))

ggplot(data = figure3_data) +
  aes(x = fsstotal, fill = as.factor(value)) +
  geom_bar() +
  facet_wrap( ~ variable) +
  scale_fill_manual(name = "Hemorrhage", labels = c("No", "Yes"), values = c("grey", "black")) +
  xlim(5.5,30.5) +
  xlab("Discharge FSS") +
  ylab("Count") +
  theme_classic(base_size=16) +
  theme(legend.position = "bottom")

## ----label = "figure4", warning = FALSE, fig.width = 7, fig.height = 7--------
figure4_data <-
  rbind(cbind(cp = "All TBI", pedalfast[, c("fsstotal", "newgastyn")]),
        cbind(cp = "Non-Severe TBI", subset(pedalfast, severetbi == 0, c("fsstotal", "newgastyn"))),
        cbind(cp = "Severe TBI",     subset(pedalfast, severetbi == 1, c("fsstotal", "newgastyn"))))
figure4_data$cp <- factor(figure4_data$cp, levels = c("All TBI", "Non-Severe TBI", "Severe TBI"))

ggplot(data = figure4_data) +
  aes(x = fsstotal, fill = newgastyn) +
  geom_histogram(binwidth = 1, bins = seq(5.5, 30.5, by = 1)) +
  scale_fill_manual(name = "New G-Tube", values = c("gray", "black")) +
  xlim(5.5,30.5) +
  xlab("Discharge FSS") +
  ylab("Count") +
  theme_classic(base_size=20) +
  theme(legend.position = "bottom") +
  facet_wrap( ~ cp)

## ----label = "figure5", warning = FALSE, fig.width = 7, fig.height = 7--------
ggplot(data = subset(figure3_data, !is.na(value))) +
  aes(y = fsstotal, x = factor(value, c(0, 1), c("No", "Yes"))) +
  geom_violin() +
  stat_summary(fun = mean, geom = "point") +
  xlab("") +
  ylab("Discharge FSS") +
  facet_wrap( ~ variable) +
  theme_classic(base_size=16) +
  theme(legend.position = "bottom")

## ----label = "table1", results = 'asis'---------------------------------------
qs <- list(
             "Patient Characteristics" =
               list("Age (in years)" = ~ mean_sd(age / 365.25),
                    "Female"         = ~ n_perc0(female == 1))
           , "Injury Mechanism" = qsummary(subset(pedalfast, select = "injurymech"))[[1]]
           , "TBI Severity" =
               list("GCS = 3"             = ~ n_perc0(gcs_using == 3),
                    "Severe (GCS 3-8)"    = ~ n_perc0(gcs_using %in% seq(3, 8)),
                    "Moderate (GCS 9-12)" = ~ n_perc0(gcs_using %in% seq(9, 12)),
                    "Mild (GCS 13-15)"    = ~ n_perc0(gcs_using %in% seq(13, 15))
                   )
           , "GCS Eye"    = qsummary(subset(pedalfast, select = "gcseye_using_cat"))[[1]]
           , "GCS Verbal" = qsummary(subset(pedalfast, select = "gcsverbal_using_cat"))[[1]]
           , "GCS Motor"  = qsummary(subset(pedalfast, select = "gcsmotor_using_cat"))[[1]]
           , "Pupil Reactivity on ICU admission" = qsummary(subset(pedalfast, select = "puplrcticu"))[[1]]
           , "Initial ICU LOS" = list("median (IQR) (days)" = ~ median_iqr(admittoicudc1, na_rm = TRUE, digits = 0))
           , "Hospital LOS" = list("median (IQR) (days)" = ~ median_iqr(hosplos, na_rm = TRUE, digits = 0))
           , "Hospital Disposition" = qsummary(subset(pedalfast, select = "hospdisposition"))[[1]]
           )

# NOTE: the levels of Pupil Reactivity in the ICU include "Unknown" which is
# qwraps2::qsummary default label for NA values.  A patch is provided here, a
# bug report and feature request for qwraps2 has been posted on github.
names(qs[["Pupil Reactivity on ICU admission"]])[ length( names(qs[["Pupil Reactivity on ICU admission"]]) )] <- "Missing"

# print the summary table
st <-
  cbind(summary_table(pedalfast, summaries = qs),
        summary_table(pedalfast, summaries = qs, by = "severetbi"))

colnames(st) <-
  c("Whole Cohort", "Non-severe TBI (GCS > 8)", "Severe TBI (GCS <= 8)")

st

## ----label = "table2", results = "asis"---------------------------------------
qs <- list(
             "FSS Total" =
               list("median (IQR)" = ~ median_iqr(fsstotal, show_n = "never", na_rm = TRUE, digits = 0),
                    "mean (standard deviation)"    = ~ mean_sd(fsstotal, show_n = "never", na_rm = TRUE, digits = 0))
           , "FSS Total - Categorical: n (%)" = qsummary(subset(pedalfast, select = "fsstotal_cat2"))[[1]]
           , "FSS Mental"        = list("n; Median (IQR)" = ~ median_iqr(fssmental, na_rm = TRUE))
           , "FSS Motor"         = list("n; Median (IQR)" = ~ median_iqr(fssmotor, na_rm = TRUE))
           , "FSS Sensory"       = list("n; Median (IQR)" = ~ median_iqr(fsssensory, na_rm = TRUE))
           , "FSS Respiratory"   = list("n; Median (IQR)" = ~ median_iqr(fssresp, na_rm = TRUE))
           , "FSS Feeding"       = list("n; Median (IQR)" = ~ median_iqr(fssfeeding, na_rm = TRUE))
           , "FSS Communication" = list("n; Median (IQR)" = ~ median_iqr(fsscommun, na_rm = TRUE))
           )


st <-
  cbind(summary_table(pedalfast, summaries = qs),
        summary_table(pedalfast, summaries = qs, by = "severetbi"))

colnames(st) <-
  c("Whole Cohort", "Non-severe TBI (GCS > 8)", "Severe TBI (GCS <= 8)")

st

## ----label = 'appendix_table_1', results = "asis"-----------------------------

appendix_table_1 <-
  subset(pedalfast_metadata,
         variable %in% grep("^fss(?!total)", pedalfast_metadata$variable, perl = TRUE, value = TRUE))

appendix_table_1 <-
  cbind(appendix_table_1, do.call(rbind, strsplit(gsub("\\d,\\ ", "", appendix_table_1$values), split = "\\ \\|\\ ")))

appendix_table_1 <- appendix_table_1[, c("description", as.character(1:5))]
colnames(appendix_table_1) <-
  c("",
    "Normal (Score = 1)",
    "Mild Dysfunction (Score = 2)",
    "Moderate Dysfunction (Score = 3)",
    "Severely Dysfunction (Score = 4)",
    "Severely Dysfunction (Score = 5)")

knitr::kable(appendix_table_1, row.names = FALSE)

## ----label = "session_info"---------------------------------------------------
sessionInfo()

