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

# read in data with difference scores from mbsb preprocessed data
rename_colnames_iod <- function(data, dv, grouping_var) {

  if (grepl("reduced", dv)) {
    data <- data %>%
    dplyr::rename(
      "reduced_module_1" = `IoD_reduced pre/post Module 1`,
      "reduced_module_2" = `IoD_reduced pre/post Module 2`,
      "reduced_module_3" = `IoD_reduced pre/post Module 3`,
      "reduced_module_4" = `IoD_reduced pre/post Module 4`,
      "reduced_module_5" = `IoD_reduced pre/post Module 5`,
      "reduced_module_6" = `IoD_reduced pre/post Module 6`
    ) #%>%
    #dplyr::select(all_of(c("client_id", grouping_var, dv)))
  } else {
    data <- data %>%
    dplyr::rename(
      "module_1" = `IoD pre/post Module 1`,
      "module_2" = `IoD pre/post Module 2`,
      "module_3" = `IoD pre/post Module 3`,
      "module_4" = `IoD pre/post Module 4`,
      "module_5" = `IoD pre/post Module 5`,
      "module_6" = `IoD pre/post Module 6`
    ) #%>%
    #dplyr::select(all_of(c("client_id", grouping_var, dv)))
  }

  return(data)


}

create_prim_pairwise_tbl <- function(data, dvs, label_list, type_list, abbreviations) {

  tbl <- data %>%
    dplyr::select(all_of(dvs), timepoint) %>%
    tbl_summary(
      by = timepoint,
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
    add_stat(fns = everything() ~ bm_paired_test) %>%
    add_q(method = "holm") %>%
    modify_header(
      label = "**Module**",
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

create_pairwise_dataframe <- function(variable = "IoD") {

  # Paired tests for IoD:
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

    return(df)
}

create_prim_pairwise_tbls <- function(
  variable,
  subgroup_str = "Intervention"
) {

  dvs <- c(
    "Module 1",
    "Module 2",
    "Module 3",
    "Module 4",
    "Module 5",
    "Module 6"
  )


  dvs_mbsb <- c(
    "module_1",
    "module_2",
    "module_3",
    "module_4",
    "module_5",
    "module_6"
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
  mbsb <- TRUE
  grouping_var <- "treatment"

  # create dvs and grouping_var based on mbsb
  if (mbsb) {
    dvs <- dvs_mbsb
    # grouping variable (which variables should be compared - must have two levels)
    grouping_var <- "treatment"

    if (grepl("reduced", dvs[1])) {
      print("It must be a reduced variable!")
      dat <- create_pairwise_dataframe(paste0(variable, "_reduced"))
        dat_i <- dat %>% filter(treatment == "Intervention") %>%
                    dplyr::select(contains("pre/post"), timepoint)
        dat_p <- dat %>% filter(treatment == "Placebo") %>%
        dplyr::select(contains("pre/post"), timepoint)
    } else {
      dat <- create_pairwise_dataframe(variable)
      dat_i <- dat %>% filter(treatment == "Intervention") %>%
                  dplyr::select(contains("pre/post"), timepoint)
      dat_p <- dat %>% filter(treatment == "Placebo") %>%
        dplyr::select(contains("pre/post"), timepoint)
    }

    data_intervention <- rename_colnames_iod(
      data = dat_i,
      dv = dvs[1],
      grouping_var
    )

    data_placebo <- rename_colnames_iod(
      data = dat_p,
      dv = dvs[1],
      grouping_var
    )
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
    tbl <- create_prim_pairwise_tbl(data_intervention, dvs, label_list, type_list, abbreviations)
  } else if (subgroup_str == "Placebo") {
    tbl <- create_prim_pairwise_tbl(data_placebo, dvs, label_list, type_list, abbreviations)
  } else {
    stop("subgroup_str must be either 'Intervention' or 'Placebo'")
  }

  return(tbl)
}
