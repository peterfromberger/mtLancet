library(dplyr)
library(tidyr)
library(gtsummary)
library(nparcomp)
library(purrr)

# read in data with difference scores from mbsb preprocessed data
get_diffscore_mbsb_prim <- function(data, dvs, grouping_var) {

  if (grepl("diff_reduced", dvs[1])) {
    data <- data %>%
      dplyr::rename(
        "diff_reduced_module_1" = `diff_IoD_reduced Module 1`,
        "diff_reduced_module_2" = `diff_IoD_reduced Module 2`,
        "diff_reduced_module_3" = `diff_IoD_reduced Module 3`,
        "diff_reduced_module_4" = `diff_IoD_reduced Module 4`,
        "diff_reduced_module_5" = `diff_IoD_reduced Module 5`,
        "diff_reduced_module_6" = `diff_IoD_reduced Module 6`
      )
  } else {
    data <- data %>%
      dplyr::rename(
        "diff_module_1" = `diff_IoD Module 1`,
        "diff_module_2" = `diff_IoD Module 2`,
        "diff_module_3" = `diff_IoD Module 3`,
        "diff_module_4" = `diff_IoD Module 4`,
        "diff_module_5" = `diff_IoD Module 5`,
        "diff_module_6" = `diff_IoD Module 6`
      )
  }


}

# fn returns brunner-munzel test statistic and pvalue
bm_unpaired_test <- function(data, variable, by, ...) {

  # ---------------
  # Brunner-Munzel Test for unpaired data
  # ---------------

  formula <- as.formula(paste(variable, " ~ client_group"))

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
      ci.value = glue::glue("{Lower}, {Upper}")
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
create_prim_diff_tbl <- function(data, dvs, label_list, grouping_var, abbreviations, type_list) {

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
      data_row.padding = gt::px(2),
      summary_row.padding = gt::px(2),
      grand_summary_row.padding = gt::px(2),
      #footnotes.padding = gt::px(2),
      #source_notes.padding = gt::px(2),
      row_group.padding = gt::px(2)
    )

  return(tbl)
}

create_diffscore_dataframe <- function(variable) {

  df <- dat_complete %>%
    dplyr::filter(!is.na(.data[[variable]]))

  dat_complete_module1_iod <- df %>% dplyr::filter(timepoint %in% c("Baseline", "Module 1 (post)"))  %>%
    group_by(client_id) %>%
    filter(n()==2) %>%
    mutate(timepoint=case_match(timepoint,
                                "Baseline" ~ "pre",
                                "Module 1 (post)" ~ "post") %>%
            factor()) %>%
    dplyr::rename(`IoD pre/post Module 1` = variable)

  dat_complete_module2_iod <- df %>% dplyr::filter(timepoint %in% c("Module 1 (post)", "Module 2 (post)"))  %>%
    group_by(client_id) %>%
    filter(n()==2) %>%
    mutate(timepoint=case_match(timepoint,
                                "Module 1 (post)" ~ "pre",
                                "Module 2 (post)" ~ "post") %>%
            factor()) %>%
    dplyr::rename(`IoD pre/post Module 2` = variable)
  dat_complete_module3_iod <- df %>% dplyr::filter(timepoint %in% c("Module 2 (post)", "Module 3 (post)"))  %>%
    group_by(client_id) %>%
    filter(n()==2) %>%
    mutate(timepoint=case_match(timepoint,
                                "Module 2 (post)" ~ "pre",
                                "Module 3 (post)" ~ "post") %>%
            factor()) %>%
    dplyr::rename(`IoD pre/post Module 3` = variable)
  dat_complete_module4_iod <- df %>% dplyr::filter(timepoint %in% c("Module 3 (post)", "Module 4 (post)"))  %>%
    group_by(client_id) %>%
    filter(n()==2) %>%
    mutate(timepoint=case_match(timepoint,
                                "Module 3 (post)" ~ "pre",
                                "Module 4 (post)" ~ "post") %>%
            factor()) %>%
    dplyr::rename(`IoD pre/post Module 4` = variable)
  dat_complete_module5_iod <- df %>% dplyr::filter(timepoint %in% c("Module 4 (post)", "Module 5 (post)"))  %>%
    group_by(client_id) %>%
    filter(n()==2) %>%
    mutate(timepoint=case_match(timepoint,
                                "Module 4 (post)" ~ "pre",
                                "Module 5 (post)" ~ "post") %>%
            factor()) %>%
    dplyr::rename(`IoD pre/post Module 5` = variable)
  dat_complete_module6_iod <- df %>% dplyr::filter(timepoint %in% c("Module 5 (post)", "Module 6 (post)"))  %>%
    group_by(client_id) %>%
    filter(n()==2) %>%
    mutate(timepoint=case_match(timepoint,
                                "Module 5 (post)" ~ "pre",
                                "Module 6 (post)" ~ "post") %>%
            factor()) %>%
    dplyr::rename(`IoD pre/post Module 6` = variable)

  df <- dat_complete_module1_iod %>%
    dplyr::select(contains("pre/post"), client_id, timepoint, treatment) %>%
    dplyr::filter(!client_id %in% c(253, 396)) %>%
    merge(.,
          dat_complete_module2_iod %>%
            dplyr::select(contains("pre/post"), client_id, timepoint) %>%
            dplyr::filter(!client_id %in% c(253, 396)),
          by=c("client_id", "timepoint"),
          all=TRUE) %>%
    merge(.,
          dat_complete_module3_iod %>%
            dplyr::select(contains("pre/post"), client_id, timepoint) %>%
            dplyr::filter(!client_id %in% c(253, 396)),
          by=c("client_id", "timepoint"),
          all=TRUE) %>%
    merge(.,
          dat_complete_module4_iod %>%
            dplyr::select(contains("pre/post"), client_id, timepoint) %>%
            dplyr::filter(!client_id %in% c(253, 396)),
          by=c("client_id", "timepoint"),
          all=TRUE) %>%
    merge(.,
          dat_complete_module5_iod %>%
            dplyr::select(contains("pre/post"), client_id, timepoint) %>%
            dplyr::filter(!client_id %in% c(253, 396)),
          by=c("client_id", "timepoint"),
          all=TRUE) %>%
    merge(.,
          dat_complete_module6_iod %>%
            dplyr::select(contains("pre/post"), client_id, timepoint) %>%
            dplyr::filter(!client_id %in% c(253, 396)),
          by=c("client_id", "timepoint"),
          all=TRUE) %>%
    arrange(client_id, desc(timepoint)) %>%
    dplyr::filter(!is.na(timepoint)) %>%
    dplyr::mutate(timepoint = relevel(timepoint, "pre"))

  # Reshape the data: separate "pre" and "post" values into columns
  df_wide <- df %>%
    dplyr::select(contains("pre/post"), timepoint, client_id, treatment) %>%
    tidyr::pivot_wider(names_from = timepoint, values_from = -c(client_id, timepoint, treatment)) %>%
    dplyr::mutate(`treatment` = `treatment` %>% relevel(ref = "Placebo"))

  names(df_wide) %<>% gsub("pre/post ", "", .)

  # Compute the differences (post - pre)
  df_diff <- df_wide %>%
    mutate(across(ends_with("post"), ~ . - get(sub("post", "pre", cur_column())), .names = "diff_{.col}")) %>%
    rename_with(~ sub("_post$", "", .), starts_with("diff_")) %>%
    dplyr::select(client_id, starts_with("diff_"), treatment)

  df_diff <- df_diff %>%
    dplyr::select(-client_id) %>%
    filter(!is.na(treatment))

   return(df_diff)
}

create_prim_diffscore_tbl <- function(
  variable
) {

  mbsb <- TRUE
  grouping_var <- "treatment"

  dvs <- c(
    "Module 1",
    "Module 2",
    "Module 3",
    "Module 4",
    "Module 5",
    "Module 6"
  )


  dvs_mbsb <- c(
    "diff_module_1",
    "diff_module_2",
    "diff_module_3",
    "diff_module_4",
    "diff_module_5",
    "diff_module_6"
  )

  labels <- c(
    "Module 1 (post)",
    "Module 2 (post)",
    "Module 3 (post)",
    "Module 4 (post)",
    "Module 5 (post)",
    "Module 6 (post)"
  )

  types = rep("continuous2", 6)

  abbreviations <- c("")

  sample_str <- "itt"

  dvs <- dvs_mbsb

  # create label_list
  label_list <- purrr::map2(dvs, labels, ~rlang::new_formula(sym(.x), .y))

  # create list of types
  type_list <- purrr::map2(dvs, types, ~rlang::new_formula(sym(.x), .y))

  df_diff <- create_diffscore_dataframe(variable)

  tbl_data <- get_diffscore_mbsb_prim(df_diff, dvs, grouping_var)

  tbl <- create_prim_diff_tbl(tbl_data, dvs, label_list, grouping_var, abbreviations, type_list)

  # show table
  return(tbl)
}
