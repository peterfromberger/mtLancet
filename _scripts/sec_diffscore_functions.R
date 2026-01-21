library(dplyr)
library(tidyr)
library(gtsummary)
library(nparcomp)
library(purrr)

# read in data with difference scores from mbsb preprocessed data
get_diffscore_mbsb <- function(dv, grouping_var) {
  dv_diff = paste0("diff_", dv)
  data <- df_diff %>%
    dplyr::select(all_of(c("client_id", grouping_var, dv_diff)))

}

# function to create data with difference scores
# dv: dependent variable (e.g. "cvtrq_calc_total")
# grouping_var: variable by which to group (e.g. client_group or treatment)
# sample_str: either "itt" or "pp"
get_diffscore_data <- function(
    df,
    dv = "cvtrq_calc_total",
    grouping_var = "client_group",
    sample_str = "itt"  # "itt" oder "pp"
) {
  # define variables
  dv_post <- paste0(dv, "_post")
  dv_pre <- paste0(dv, "_pre")
  dv_diff <- paste0(dv, "_diff")

  # client_id 194 muss ausgeschlossen werden
  data <- df %>%
    filter(client_id != 194)

  # ITT-Sample:
  if (sample_str == "itt") {
    data <- data %>%
      filter(m1s4l7_calc_lession_finished == TRUE)
  # per-protocol sample
  } else if (sample_str == "pp") {
     data <- data %>%
      filter(m6s4l6_calc_lession_finished == TRUE)
  } else {
    print("Wrong sample: please use 'itt' or 'pp' for per-protocol sample.")
  }

  # ensure that there are no na's
  data <- data %>%
    drop_na(all_of(dv_pre)) %>%
    drop_na(all_of(dv_post))

  # calculate diffscore
  data[dv_diff] = data[[dv_post]] - data[[dv_pre]]
    #data[dv_diff] <- lapply(data[dv_diff], as.numeric)

  # selct only relevant variables
  data <- data %>%
    dplyr::select(all_of(c("client_id", grouping_var, dv_diff)))

  # relevel
  data[grouping_var] <- factor(data[[grouping_var]], ordered = FALSE)
  data[grouping_var] <- relevel(data[[grouping_var]], ref = "Placebo")

  return(data)
}

# function to create data for tbl
# dvs: vector of dependent variables
# grouping_var: variable by which to group (e.g. client_group or treatment)
# sample_str: either "itt" or "pp"
# mbsb: if TRUE, use df_diff with mbsb difference scores
create_diffscore_tbl_data <- function(df, dvs, grouping_var, sample_str, mbsb) {

  tbl_data <- data.frame()

  for (i in 1:length(dvs)) {

    if (mbsb == FALSE) {
      if (i == 1) {
            tbl_data <- tmp
          } else {
            tbl_data <- tbl_data %>%
              left_join(tmp, by = c("client_id", grouping_var))
          }
      return(tbl_data)
    } else {
      return(df)
    }

    

  }
  

}

# fn returns brunner-munzel test statistic and pvalue
bm_unpaired_test <- function(data, variable, by, ...) {

  # ---------------
  # Brunner-Munzel Test for unpaired data
  # ---------------

  bm_intervention <- npar.t.test(
    data[[variable]] ~ data[[by]],
    data = data,
    conf.level = 0.95,
    alternative = "two.sided",
    nperm = 10000,
    rounds = 3,
    info = FALSE,
    plot.simci = FALSE
)

  df <- data.frame(
    Effect = c(bm_intervention$Analysis$Effect),
    p.hat = c(bm_intervention$Analysis$Estimator),
    Lower = c(bm_intervention$Analysis$Lower),
    Upper = c(bm_intervention$Analysis$Upper),
    t.value = c(bm_intervention$Analysis$T),
    p.value = c(bm_intervention$Analysis$p)
  )

  df <- df %>%
    dplyr::mutate(
      ci.value = glue::glue("{Lower}; {Upper}")
    ) %>%
    dplyr::select(c(p.hat, ci.value, t.value, p.value))

  return(df)
}


# Function to create summary table for difference from baseline
# using Brunner-Munzel test for two independent groups
# for given dependent variables (dvs) and their corresponding
# labels, types, and abbreviations
# grouping_var is the variable by which to group (e.g. client_group or treatment)
# sample_str is either "itt" or "pp"
create_sec_diff_tbl <- function(data, dvs, label_list, grouping_var, abbreviations, type_list, mbsb) {

  if (mbsb == FALSE) {
    data <- data %>% dplyr::select(-client_id)
  } else {
    score_columns <- c("diff_cvtrq_calc_total", "diff_rcq_calc_total", "diff_fsozu_calc_total", "diff_ors_calc_total",
                   "diff_ucla_calc_total", "diff_bis_calc_total", "diff_cusi_calc_total", "diff_ders_calc_imp",
                   "diff_narq_calc_ris", "diff_spsi_calc_total", "diff_kvm_score", "diff_ekk_calc_total",
                   "diff_esiq_calc_total", "diff_hbi_calc_total", "diff_ssik_calc_total", "diff_soi_total_score")
    data <- df_diff %>% select(treatment, all_of(score_columns))
  }

  tbl <- data %>%
    tbl_summary(
      by = grouping_var,
      type = type_list,
      statistic = all_continuous() ~ c(
          "{mean} ({sd})",
          "{median} ({p25}, {p75})",
          "{min}, {max}"
        ),
      missing_text = "Missing",
      label = label_list
    ) %>%
    # brunner-munzel test
    add_stat(fns = everything() ~ bm_unpaired_test) %>%
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
    modify_abbreviation("*t* = Brunner–Munzel test statistic for unpaired samples") %>%
    modify_abbreviation("*q-value* = Holm-Bonferroni adjusted p-value")


  # add abbreviations
  for (i in 1:length(abbreviations)) {
    tbl <- tbl %>%
        modify_abbreviation(abbreviations[i])
  }

  tbl <- tbl %>%
    as_gt() %>%
    gt::tab_options(
      #table.font.names = "Source Sans Pro",
      table.font.size = 12,
      quarto.use_bootstrap = FALSE,
      quarto.disable_processing = TRUE,
      data_row.padding = px(2),
      summary_row.padding = gt::px(2),
      grand_summary_row.padding = gt::px(2),
      #footnotes.padding = gt::px(2),
      #source_notes.padding = gt::px(2),
      row_group.padding = gt::px(2)
    )

  return(tbl)
}

# Function to create pairwise table for difference from baseline
# for given dependent variables (dvs) and their corresponding
# labels, types, and abbreviations
create_tbl_diffscore_bm_unpaired <- function(
  mbsb,
  sample_str,
  dvs,
  dvs_mbsb,
  labels,
  types,
  abbreviations
) {

  # create dvs and grouping_var based on mbsb
  if (mbsb) {
    dvs <- dvs_mbsb
    dvs_diff <- paste0("diff_", dvs)
    # grouping variable (which variables should be compared - must have two levels)
    grouping_var <- "treatment"
  } else {
    dvs_diff <- paste0(dvs, "_diff")
    # grouping variable (which variables should be compared - must have two levels)
    grouping_var <- "client_group"
  }

  # create label_list
  label_list <- purrr::map2(dvs_diff, labels, ~rlang::new_formula(sym(.x), .y))

  # create list of types
  type_list <- purrr::map2(dvs_diff, types, ~rlang::new_formula(sym(.x), .y))

  # data for tbl
  tbl_data <- create_diffscore_tbl_data(df, dvs, grouping_var, sample_str, mbsb)

  # create table
  tbl <- create_sec_diff_tbl(tbl_data, dvs, label_list, grouping_var, abbreviations, type_list, mbsb)

  # show table
  return(tbl)
}

# df_diff <- df_diff %>%
#   dplyr::select(client_id, treatment, all_of(contains("diff_")))

# tbl <- df_diff %>%
#   tbl_summary(
#     by = treatment
#   )



