library(dplyr)
library(tidyr)
library(gtsummary)
library(nparcomp)
library(purrr)



# fn returns brunner-munzel test statistic and pvalue for
bm_paired_test <- function(data, variable, by, ...) {

  # ---------------
  # Brunner-Munzel Test
  # --------------

  formula <- as.formula(paste(variable, " ~ timepoint"))

  # Intervention
  bm <- npar.t.test.paired(formula,
                            data = data,
                            conf.level = 0.95,
                            alternative = "two.sided",
                            nperm = 10000,
                            rounds = 3,
                            info = FALSE,
                            plot.simci = FALSE
                          )

  df <- data.frame(
    Lower = bm$Analysis[1, 1],
    p.hat = bm$Analysis[1, 2],
    Upper = bm$Analysis[1, 3],
    t.value = bm$Analysis[1, 4],
    p.value = bm$Analysis[1, 5]
  )

  df <- df %>%
    dplyr::mutate(
      ci.value = glue::glue("{Lower}, {Upper}")
    ) %>%
    dplyr::select(c(p.hat, ci.value, t.value, p.value))

  return(df)
}

create_pairwise_tbl <- function(data, dvs, label_list, type_list, abbreviations, subscale_mode) {

  if (subscale_mode) {
    # ensure that there are no na's
  data <- data %>%
    drop_na(all_of(dvs[1]))
  }

  tbl <- data %>%
    dplyr::select(all_of(dvs), timepoint) %>%
    tbl_summary(
      by = timepoint,
      type = type_list,,
      statistic = all_continuous() ~ c(
            "{mean} ({sd})",
            "{median} ({p25}, {p75})",
            "{min}, {max}"
          ),
      missing_text = "Missing",
      label = label_list
    ) %>%
    # brunner-munzel test
    add_stat(fns = everything() ~ bm_paired_test) %>%
    add_q(method = "holm") %>%
    modify_header(
      p.hat = "**p-hat**",
      t.value = "**t**",
      ci.value = "**95% CI**",
      p.value = "**p-value**"
    ) %>%
    bold_labels() %>%
    # damit quarto citations im footer erkeent, dürfen keine footnotes vorhanden sein!
    remove_footnote_header(columns = "q.value") %>%
    # abbreviations
    modify_abbreviation("*Q1* = 25th percentile") %>%
    modify_abbreviation("*Q3* = 75th percentile") %>%
    modify_abbreviation("*95% CI* = Confidence interval of the estimated relative effect") %>%
    modify_abbreviation("*p-hat* = estimated relative effect") %>%
    modify_abbreviation("*t* = Brunner–Munzel test statistic for paired samples") %>%
    modify_abbreviation("*q-value* = Holm-Bonferroni adjusted p-value")

  # add abbreviations
  for (i in 1:length(abbreviations)) {
    tbl <- tbl %>%
        modify_abbreviation(abbreviations[i])
  }

  return(tbl)

}

create_tbl_cap <- function(abbreviations, title) {

  statistik_abbreviations <- c(
    "*Q1* = 25th percentile",
    "*Q3* = 75th percentile",
    "*95% CI* = Confidence interval of the estimated relative effect",
    "*p-hat* = estimated relative effect",
    "*t* = Brunner–Munzel test statistic for paired samples",
    "*q-value* = Holm-Bonferroni adjusted p-value"
  )

  # Kombinieren der Vektoren
  alle_abbreviations <- c(abbreviations, statistik_abbreviations)

  # Alphabetische Sortierung der Einträge
  alle_abbreviations <- sort(alle_abbreviations)

  # Umwandeln in eine einzelne Zeile mit Komma-getrennten Einträgen
  string_abbreviations <- paste(alle_abbreviations, collapse = ", ")
  string_abbreviations <- paste0(title, " **Abbreviations**: ", string_abbreviations, ".")
}

create_pairwise_tbls <- function(
  mbsb = FALSE,
  dvs,
  dvs_mbsb,
  types,
  labels,
  abbreviations,
  subgroup_str = "Intervention",
  subscale_mode = FALSE
) {

  # create dvs and grouping_var based on mbsb
  if (mbsb) {
    dvs <- dvs_mbsb
    # grouping variable (which variables should be compared - must have two levels)
    grouping_var <- "treatment"
    data_intervention <- dat_complete_sec_intervention
    levels(data_intervention$timepoint) <- c("Pre", "Post")
    data_placebo <- dat_complete_sec_placebo
    levels(data_placebo$timepoint) <- c("Pre", "Post")
  } else {
    # grouping variable (which variables should be compared - must have two levels)
    grouping_var <- "client_group"
    stop("Only mbsb = TRUE is implemented")
  }

  # create label_list
  label_list <- purrr::map2(dvs, labels, ~rlang::new_formula(sym(.x), .y))

  # create list of types
  type_list <- purrr::map2(dvs, types, ~rlang::new_formula(sym(.x), .y))

  if (subgroup_str == "Intervention") {
    tbl <- create_pairwise_tbl(data_intervention, dvs, label_list, type_list, abbreviations, subscale_mode)
  } else if (subgroup_str == "Placebo") {
    tbl <- create_pairwise_tbl(data_placebo, dvs, label_list, type_list, abbreviations, subscale_mode)
  } else {
    stop("subgroup_str must be either 'Intervention' or 'Placebo'")
  }

  return(tbl)
}