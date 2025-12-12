# load each dataset for each secondary endpoint and filter for non-missing values
dat_complete_sec_cvtrq <- dat_complete %>% dplyr::filter(!is.na(cvtrq_calc_total))
dat_complete_sec_cvtrq %<>% mutate(timepoint=case_match(timepoint,
                                                 "Baseline" ~ "pre",
                                                 "Module 1 (post)" ~ "post") %>%
                               factor())
dat_complete_sec_rcq <- dat_complete %>% dplyr::filter(!is.na(rcq_calc_total))
dat_complete_sec_rcq %<>% mutate(timepoint=case_match(timepoint,
                                                        "Baseline" ~ "pre",
                                                        "Module 1 (post)" ~ "post") %>%
                                     factor())

dat_complete_sec_fsozu <- dat_complete %>% dplyr::filter(!is.na(fsozu_calc_total))
dat_complete_sec_fsozu %<>% mutate(timepoint=case_match(timepoint,
                                                      "Module 1 (post)" ~ "pre",
                                                      "Module 2 (post)" ~ "post") %>%
                                   factor())
dat_complete_sec_ors <- dat_complete %>% dplyr::filter(!is.na(ors_calc_total))
dat_complete_sec_ors %<>% mutate(timepoint=case_match(timepoint,
                                                        "Module 1 (post)" ~ "pre",
                                                        "Module 2 (post)" ~ "post") %>%
                                     factor())
dat_complete_sec_ucla <- dat_complete %>% dplyr::filter(!is.na(ucla_calc_total))
dat_complete_sec_ucla %<>% mutate(timepoint=case_match(timepoint,
                                                      "Module 1 (post)" ~ "pre",
                                                      "Module 2 (post)" ~ "post") %>%
                                   factor())
dat_complete_sec_bis <- dat_complete %>% dplyr::filter(!is.na(bis_calc_total))
dat_complete_sec_bis %<>% mutate(timepoint=case_match(timepoint,
                                                       "Module 2 (post)" ~ "pre",
                                                       "Module 3 (post)" ~ "post") %>%
                                    factor())
dat_complete_sec_cusi <- dat_complete %>% dplyr::filter(!is.na(cusi_calc_total))
dat_complete_sec_cusi %<>% mutate(timepoint=case_match(timepoint,
                                                      "Module 2 (post)" ~ "pre",
                                                      "Module 3 (post)" ~ "post") %>%
                                   factor())
dat_complete_sec_ders <- dat_complete %>% dplyr::filter(!is.na(ders_calc_imp))
dat_complete_sec_ders %<>% mutate(timepoint=case_match(timepoint,
                                                       "Module 2 (post)" ~ "pre",
                                                       "Module 3 (post)" ~ "post") %>%
                                    factor())
dat_complete_sec_narq <- dat_complete %>% dplyr::filter(!is.na(narq_calc_ris))
dat_complete_sec_narq %<>% mutate(timepoint=case_match(timepoint,
                                                       "Module 2 (post)" ~ "pre",
                                                       "Module 3 (post)" ~ "post") %>%
                                    factor())

dat_complete_sec_spsi <- dat_complete %>% dplyr::filter(!is.na(spsi_calc_total))
dat_complete_sec_spsi %<>% mutate(timepoint=case_match(timepoint,
                                                       "Module 3 (post)" ~ "pre",
                                                       "Module 4 (post)" ~ "post") %>%
                                    factor())

dat_complete_sec_kvm <- dat_complete %>% dplyr::filter(!is.na(kvm_score))
dat_complete_sec_kvm %<>% mutate(timepoint=case_match(timepoint,
                                                       "Module 4 (post)" ~ "pre",
                                                       "Module 5 (post)" ~ "post") %>%
                                    factor())

dat_complete_sec_ekk <- dat_complete %>% dplyr::filter(!is.na(ekk_calc_total))
dat_complete_sec_ekk %<>% mutate(timepoint=case_match(timepoint,
                                                      "Module 5 (post)" ~ "pre",
                                                      "Module 6 (post)" ~ "post") %>%
                                   factor())
dat_complete_sec_esiq <- dat_complete %>% dplyr::filter(!is.na(esiq_calc_total))
dat_complete_sec_esiq %<>% mutate(timepoint=case_match(timepoint,
                                                      "Module 5 (post)" ~ "pre",
                                                      "Module 6 (post)" ~ "post") %>%
                                   factor())
dat_complete_sec_hbi <- dat_complete %>% dplyr::filter(!is.na(hbi_calc_total))
dat_complete_sec_hbi %<>% mutate(timepoint=case_match(timepoint,
                                                       "Module 5 (post)" ~ "pre",
                                                       "Module 6 (post)" ~ "post") %>%
                                    factor())
dat_complete_sec_ssik <- dat_complete %>% dplyr::filter(!is.na(ssik_calc_total))
dat_complete_sec_ssik %<>% mutate(timepoint=case_match(timepoint,
                                                      "Module 5 (post)" ~ "pre",
                                                      "Module 6 (post)" ~ "post") %>%
                                   factor())

dat_complete_sec_soi <- dat_complete %>% dplyr::filter(!is.na(soi_total_score))
dat_complete_sec_soi %<>% mutate(timepoint=case_match(timepoint,
                                                      "Module 5 (post)" ~ "pre",
                                                      "Module 6 (post)" ~ "post") %>%
                                   factor())

# merge all datasets to a single dataset containing all secondary endpoints
dat_complete_sec <- dat_complete_sec_cvtrq %>%
                    dplyr::select(contains("cvtrq"), client_id, timepoint) %>%
                    dplyr::filter(!client_id %in% c(253, 396)) %>%
                    merge(.,
                          dat_complete_sec_rcq %>%
                          dplyr::select(contains("rcq"), client_id, timepoint) %>%
                          dplyr::filter(!client_id %in% c(253, 396)),
                          by=c("client_id", "timepoint"),
                          all=TRUE) %>% # remove clients having no post value %>%
  merge(.,
        dat_complete_sec_fsozu %>%
        dplyr::select(contains("fsozu"), client_id, timepoint) %>%
        dplyr::filter(!client_id %in% (dat_complete_sec_ucla %>% group_by(client_id) %>%  summarise(n=n()) %>% dplyr::filter(n!=2) %>% .$client_id)),
        by=c("client_id", "timepoint"), all=TRUE) %>%
  merge(.,
        dat_complete_sec_ors %>%
        dplyr::select(contains("ors"), client_id, timepoint) %>%
        dplyr::filter(!client_id %in% (dat_complete_sec_ucla %>% group_by(client_id) %>%  summarise(n=n()) %>% dplyr::filter(n!=2) %>% .$client_id)),
        by=c("client_id", "timepoint"), all=TRUE) %>%
  merge(.,
        dat_complete_sec_ucla %>%
        dplyr::select(contains("ucla"), client_id, timepoint) %>%
        dplyr::filter(!client_id %in% (dat_complete_sec_ucla %>% group_by(client_id) %>%  summarise(n=n()) %>% dplyr::filter(n!=2) %>% .$client_id)),
        by=c("client_id", "timepoint"), all=TRUE) %>%
  merge(.,
        dat_complete_sec_bis %>%
        dplyr::select(contains("bis"), client_id, timepoint) %>%
        dplyr::filter(!client_id %in% (dat_complete_sec_narq %>% group_by(client_id) %>%  summarise(n=n()) %>% dplyr::filter(n!=2) %>% .$client_id)),
        by=c("client_id", "timepoint"), all=TRUE) %>%
  merge(.,
        dat_complete_sec_cusi %>%
        dplyr::select(contains("cusi"), client_id, timepoint) %>%
        dplyr::filter(!client_id %in% (dat_complete_sec_narq %>% group_by(client_id) %>%  summarise(n=n()) %>% dplyr::filter(n!=2) %>% .$client_id)),
        by=c("client_id", "timepoint"), all=TRUE) %>%
  merge(.,
        dat_complete_sec_ders %>%
        dplyr::select(contains("ders"), client_id, timepoint) %>%
        dplyr::filter(!client_id %in% (dat_complete_sec_narq %>% group_by(client_id) %>%  summarise(n=n()) %>% dplyr::filter(n!=2) %>% .$client_id)),
        by=c("client_id", "timepoint"), all=TRUE) %>%
  merge(.,
        dat_complete_sec_narq %>%
        dplyr::select(contains("narq"), client_id, timepoint) %>%
        dplyr::filter(!client_id %in% (dat_complete_sec_narq %>% group_by(client_id) %>%  summarise(n=n()) %>% dplyr::filter(n!=2) %>% .$client_id)),
        by=c("client_id", "timepoint"), all=TRUE) %>%
  merge(.,
        dat_complete_sec_spsi %>%
        dplyr::select(contains("spsi"), client_id, timepoint) %>%
        dplyr::filter(!client_id %in% (dat_complete_sec_spsi %>% group_by(client_id) %>%  summarise(n=n()) %>% dplyr::filter(n!=2) %>% .$client_id)),
        by=c("client_id", "timepoint"), all=TRUE) %>%
  merge(.,
        dat_complete_sec_kvm %>%
        dplyr::select(contains("kvm"), client_id, timepoint) %>%
        dplyr::filter(!client_id %in% (dat_complete_sec_kvm %>% group_by(client_id) %>%  summarise(n=n()) %>% dplyr::filter(n!=2) %>% .$client_id)),
        by=c("client_id", "timepoint"), all=TRUE) %>%
  merge(.,
        dat_complete_sec_ekk %>%
        dplyr::select(contains("ekk"), client_id, timepoint) %>%
        dplyr::filter(!client_id %in% (dat_complete_sec_ssik %>% group_by(client_id) %>%  summarise(n=n()) %>% dplyr::filter(n!=2) %>% .$client_id)),
        by=c("client_id", "timepoint"), all=TRUE) %>%
  merge(.,
        dat_complete_sec_esiq %>%
        dplyr::select(contains("esiq"), client_id, timepoint) %>%
        dplyr::filter(!client_id %in% (dat_complete_sec_ssik %>% group_by(client_id) %>%  summarise(n=n()) %>% dplyr::filter(n!=2) %>% .$client_id)),
        by=c("client_id", "timepoint"), all=TRUE) %>%
  merge(.,
        dat_complete_sec_hbi %>%
        dplyr::select(contains("hbi"), client_id, timepoint) %>%
        dplyr::filter(!client_id %in% (dat_complete_sec_ssik %>% group_by(client_id) %>%  summarise(n=n()) %>% dplyr::filter(n!=2) %>% .$client_id)),
        by=c("client_id", "timepoint"), all=TRUE) %>%
  merge(.,
        dat_complete_sec_ssik %>%
        dplyr::select(contains("ssik"), client_id, timepoint) %>%
        dplyr::filter(!client_id %in% (dat_complete_sec_ssik %>% group_by(client_id) %>%  summarise(n=n()) %>% dplyr::filter(n!=2) %>% .$client_id)),
        by=c("client_id", "timepoint"), all=TRUE) %>%
  merge(.,
        dat_complete_sec_soi %>%
        dplyr::select(contains("soi"), client_id, timepoint) %>%
        dplyr::filter(!client_id %in% (dat_complete_sec_soi %>% group_by(client_id) %>%  summarise(n=n()) %>% dplyr::filter(n!=2) %>% .$client_id)),
        by=c("client_id", "timepoint"), all=TRUE) %>%
  merge(.,
        dat_complete %>%
        dplyr::filter(timepoint=="Baseline") %>%
        dplyr::select(client_id, `Indexdelikt`, `Aktuelle Betreuung`, `Aktuelle zusätzliche Behandlung`, `Baseline Static-99-Score`, static99_modified_calc, treatment), all.x = TRUE) %>%
  arrange(client_id, desc(timepoint)) %>%
  dplyr::filter(!is.na(timepoint)) %>%
  dplyr::mutate(timepoint = relevel(timepoint, "pre"))

# wir brauchen englische Begrifflichkeiten
# translate prior placement
ordered_labels <- c(
  "Yes",
  "No"
)

translation <- c(
  "Ja" = "Yes",
  "Nein" = "No"
)

dat_complete_sec$`Aktuelle zusätzliche Behandlung` <- factor(translation[as.character(dat_complete_sec$`Aktuelle zusätzliche Behandlung`)], levels = ordered_labels)

#print(dat_clean$Aktuelle_zusatzliche_Behandlung)

library(forcats)
dat_complete_sec <- dat_complete_sec %>%
  mutate(
    `Aktuelle Betreuung` = fct_recode(
      `Aktuelle Betreuung`,
      "community supervision" = "Bewährungshilfe ohne Führungsaufsicht",
      "post-release supervision" = "Führungsaufsicht"
    ),
    `Indexdelikt` = fct_recode(
      `Indexdelikt`,
      "Hands-off (only §184b StGB)" = "Nur § 184b",
      "Hands-on (at least one conviction §176 ff StGB)" = "Auch §§ 176, 176a, 176b StGB"
      # weitere Kategorien nach Bedarf
    )
  )

# split dataset by treatment group
dat_complete_sec_intervention <- dat_complete_sec %>%
                                 dplyr::filter(treatment=="Intervention")
dat_complete_sec_placebo <- dat_complete_sec %>%
  dplyr::filter(treatment=="Placebo")

#
tests <- c(`cvtrq_calc_total` = "w.nparcomp.paired",
           rcq_calc_total = "w.nparcomp.paired",
           # rcq_calc_action = "w.nparcomp.paired",
           # rcq_calc_contemplation = "w.nparcomp.paired",
           fsozu_calc_total = "w.nparcomp.paired",
           ors_calc_total = "w.nparcomp.paired",
           ucla_calc_total = "w.nparcomp.paired",
           bis_calc_total = "w.nparcomp.paired",
           cusi_calc_total = "w.nparcomp.paired",
           ders_calc_imp = "w.nparcomp.paired",
           narq_calc_ris = "w.nparcomp.paired",
           spsi_calc_total = "w.nparcomp.paired",
           # spsi_calc_ppo = "w.nparcomp.paired",
           # spsi_calc_rps = "w.nparcomp.paired",
           # spsi_calc_npo = "w.nparcomp.paired",
           # spsi_calc_ics = "w.nparcomp.paired",
           kvm_score = "w.nparcomp.paired",
           ekk_calc_total = "w.nparcomp.paired",
           esiq_calc_total = "w.nparcomp.paired",
           hbi_calc_total = "w.nparcomp.paired",
           ssik_calc_total = "w.nparcomp.paired")

dtab_sec <- dat_complete_sec_intervention %>%
  dplyr::select(cvtrq_calc_total, rcq_calc_total, fsozu_calc_total, ors_calc_total, ucla_calc_total, bis_calc_total, cusi_calc_total, ders_calc_imp, narq_calc_ris, spsi_calc_total, kvm_score, ekk_calc_total, esiq_calc_total, hbi_calc_total, ssik_calc_total, soi_total_score, timepoint) %>%
  descsuppR::buildDescrTbl(
    groupby = "timepoint",
    useutf8 = "utf8",
    show.IQR = TRUE,
    includeNAs = TRUE,
    factorlevellimit = 45,
    dopvals = TRUE,
    tests = tests,
    descr_digits = 3,
    pvals_digits = 3,
    orderedAsUnordered = TRUE,
    report_tests = TRUE,
    report_testmessages = FALSE)

dtab_sec_placebo <- dat_complete_sec_placebo %>%
  dplyr::select(cvtrq_calc_total, rcq_calc_total, fsozu_calc_total, ors_calc_total, ucla_calc_total, bis_calc_total, cusi_calc_total, ders_calc_imp, narq_calc_ris, spsi_calc_total, kvm_score, ekk_calc_total, esiq_calc_total, hbi_calc_total, ssik_calc_total, soi_total_score, timepoint) %>%
  descsuppR::buildDescrTbl(
    groupby = "timepoint",
    useutf8 = "utf8",
    show.IQR = TRUE,
    includeNAs = TRUE,
    factorlevellimit = 45,
    dopvals = TRUE,
    tests = tests,
    descr_digits = 3,
    pvals_digits = 3,
    orderedAsUnordered = TRUE,
    report_tests = TRUE,
    report_testmessages = FALSE)

dtab_sec_expl <- dat_complete_sec_intervention %>%
  dplyr::select("esiq_calc_fan", "esiq_calc_ver", "spsi_calc_as", "spsi_calc_ppo", "spsi_calc_rps", "spsi_calc_npo", "spsi_calc_ics", timepoint) %>%
  descsuppR::buildDescrTbl(
    groupby = "timepoint",
    useutf8 = "utf8",
    show.IQR = TRUE,
    includeNAs = TRUE,
    factorlevellimit = 45,
    dopvals = TRUE,
    tests = tests,
    descr_digits = 3,
    pvals_digits = 3,
    orderedAsUnordered = TRUE,
    report_tests = TRUE,
    report_testmessages = FALSE)

dtab_sec_placebo_expl <- dat_complete_sec_placebo %>%
  dplyr::select("esiq_calc_fan", "esiq_calc_ver", "spsi_calc_as", "spsi_calc_ppo", "spsi_calc_rps", "spsi_calc_npo", "spsi_calc_ics", timepoint) %>% descsuppR::buildDescrTbl(
    groupby = "timepoint",
    useutf8 = "utf8",
    show.IQR = TRUE,
    includeNAs = TRUE,
    factorlevellimit = 45,
    dopvals = TRUE,
    tests = tests,
    descr_digits = 3,
    pvals_digits = 3,
    orderedAsUnordered = TRUE,
    report_tests = TRUE,
    report_testmessages = FALSE)
score_columns <- c("cvtrq_calc_total", "rcq_calc_total", "fsozu_calc_total", "ors_calc_total",
                   "ucla_calc_total", "bis_calc_total", "cusi_calc_total", "ders_calc_imp",
                   "narq_calc_ris", "spsi_calc_total", "kvm_score", "ekk_calc_total",
                   "esiq_calc_total", "hbi_calc_total", "ssik_calc_total", "soi_total_score")
score_columns_exploratory <- c("esiq_calc_fan", "esiq_calc_ver", "rcq_calc_current_stage", "spsi_calc_as", "spsi_calc_ppo", "spsi_calc_rps", "spsi_calc_npo", "spsi_calc_ics")



# Reshape the data: separate "pre" and "post" values into columns
df_wide <- dat_complete_sec %>%
  dplyr::select(all_of(score_columns), timepoint, client_id, treatment, `Indexdelikt`, `Aktuelle Betreuung`, `Aktuelle zusätzliche Behandlung`, `Baseline Static-99-Score`, static99_modified_calc) %>%
  tidyr::pivot_wider(names_from = timepoint, values_from = -c(client_id, timepoint, treatment, `Indexdelikt`, `Aktuelle Betreuung`, `Aktuelle zusätzliche Behandlung`, `Baseline Static-99-Score`, static99_modified_calc)) %>%
  dplyr::mutate(`treatment` = `treatment` %>% relevel(ref = "Placebo"),
#                Indexdelikt = Indexdelikt %>% factor() %>% relevel(ref = "Nur § 184b"),
                `Aktuelle zusätzliche Behandlung` = `Aktuelle zusätzliche Behandlung` %>% relevel(ref = "No"))



df_wide_expl <- dat_complete_sec %>%
  dplyr::select(all_of(score_columns_exploratory), timepoint, client_id, treatment, `Indexdelikt`, `Aktuelle Betreuung`, `Aktuelle zusätzliche Behandlung`, `Baseline Static-99-Score`, static99_modified_calc) %>%
  tidyr::pivot_wider(names_from = timepoint, values_from = -c(client_id, timepoint, treatment, `Indexdelikt`, `Aktuelle Betreuung`, `Aktuelle zusätzliche Behandlung`, `Baseline Static-99-Score`, static99_modified_calc)) %>%
  dplyr::mutate(`treatment` = `treatment` %>% relevel(ref = "Placebo"),
                #                Indexdelikt = Indexdelikt %>% factor() %>% relevel(ref = "Nur § 184b"),
                `Aktuelle zusätzliche Behandlung` = `Aktuelle zusätzliche Behandlung` %>% relevel(ref = "No"))


# Compute the differences (post - pre)
df_diff <- df_wide %>%
  mutate(across(ends_with("post"), ~ . - get(sub("post", "pre", cur_column())), .names = "diff_{.col}")) %>%
  rename_with(~ sub("_post$", "", .), starts_with("diff_")) %>%
  dplyr::select(client_id, starts_with("diff_"), treatment)
df_diff_expl <- df_wide_expl %>%
  mutate(across(ends_with("post"), ~ . - get(sub("post", "pre", cur_column())), .names = "diff_{.col}")) %>%
  rename_with(~ sub("_post$", "", .), starts_with("diff_")) %>%
  dplyr::select(client_id, starts_with("diff_"), treatment) %>%
  mutate(diff_rcq_calc_current_stage = case_when(df_wide_expl$rcq_calc_current_stage_pre>df_wide_expl$rcq_calc_current_stage_post ~ "Verbesserung",
                                                 df_wide_expl$rcq_calc_current_stage_pre==df_wide_expl$rcq_calc_current_stage_post ~ "keine Veränderung",
                                                 df_wide_expl$rcq_calc_current_stage_pre<df_wide_expl$rcq_calc_current_stage_post ~ "Verschlechterung") %>%
           factor(levels = c("Verschlechterung", "keine Veränderung", "Verbesserung"), ordered = TRUE))

variable_names <- paste0("diff_", score_columns)
tests <- generate_named_vector(variable_names, "w.npar.t.test")

dtab_sec2 <- df_diff %>% dplyr::select(-client_id) %>%
                         filter(!is.na(treatment)) %>%
                         descsuppR::buildDescrTbl(groupby = "treatment",
                                                  useutf8 = "utf8",
                                                  show.IQR = TRUE,
                                                  includeNAs = TRUE,
                                                  factorlevellimit = 45,
                                                  dopvals = TRUE,
                                                  tests = tests,
                                                  descr_digits = 3,
                                                  pvals_digits = 3,
                                                  orderedAsUnordered = TRUE,
                                                  report_tests = TRUE,
                                                  report_testmessages = FALSE)

dplot_sec2 <- dtab_sec2 %>%
  descsuppRplots::plotDescTbl(centrality.plotting = FALSE)

variable_names_expl <- paste0("diff_", score_columns_exploratory)
tests <- generate_named_vector(variable_names_expl, "w.npar.t.test")
dtab_sec2_expl <- df_diff_expl %>% dplyr::select(-client_id) %>%
  filter(!is.na(treatment)) %>%
  descsuppR::buildDescrTbl(groupby = "treatment",
                           useutf8 = "utf8",
                           show.IQR = TRUE,
                           includeNAs = TRUE,
                           factorlevellimit = 45,
                           dopvals = TRUE,
                           tests = tests,
                           descr_digits = 3,
                           pvals_digits = 3,
                           orderedAsUnordered = TRUE,
                           report_tests = TRUE,
                           report_testmessages = FALSE,
                           p.adjust.method = NULL)

dplot_sec2_expl <- dtab_sec2_expl %>%
  descsuppRplots::plotDescTbl(centrality.plotting = FALSE)

# Regression models
pretty_Pvalues <- function(p_vals,
                           digits = 3,
                           signiflev = 0.05,
                           lhs = NULL,
                           lhs_sep = "=",
                           orgbold = FALSE,
                           roundonly = FALSE)
  descutils::prettyPvalues(p_vals = p_vals,
                           digits = digits,
                           signiflev = signiflev,
                           lhs = lhs,
                           lhs_sep = lhs_sep,
                           orgbold = orgbold,
                           roundonly = roundonly)

italic_p <- function(x, t = 0.05, q = FALSE) {
  updated_call_list <- c(x$call_list, list(italic_p = match.call()))

  # checking input table has a p.value column
  if (q == FALSE && !"p.value" %in% names(x$table_body)) {
    stop("There is no p-value column. `x$table_body` must have a column called 'p.value'",
         call. = FALSE
    )
  }

  # checking input table has a q.value column
  if (q == TRUE && !"q.value" %in% names(x$table_body)) {
    stop("There is no q-value column. `x$table_body` must have a column called 'q.value'",
         call. = FALSE
    )
  }


  # update table_styling -------------------------------------------------------
  # storing column name to bold
  col_name <- ifelse(q == FALSE, "p.value", "q.value")

  # modifying table_styling with bold threshold
  x <-
    modify_table_styling(
      x,
      columns = col_name,
      rows = !!expr(!!sym(col_name) <= !!t),
      text_format = "italic"
    )

  # returning results ----------------------------------------------------------
  x$call_list <- updated_call_list
  x
}

my_log_lm_tidy <- function(x, ...) {
  df_tidy <- broom::tidy(x, ...)

  # exponentiate coef and CI
  if (TRUE) {
    df_tidy <-
      df_tidy %>%
      mutate_at(vars(any_of(c("estimate", "conf.low", "conf.high"))), exp)
  }

  df_tidy
}

# ------
# table for linear model
# ------

tab_lm <- function(fit, exponentiate=FALSE)
{
  if (!exponentiate)
    fit %>%
    tbl_regression(
      add_estimate_to_reference_rows = TRUE,
      intercept = TRUE,
      pvalue_fun = pretty_Pvalues,
      label = list(
        static99_modified_calc = "Static recidivism risk (baseline)",
        `Aktuelle Betreuung` = "Type of supervision",
        `Aktuelle zusätzliche Behandlung` = "Additional treatment",
        Indexdelikt = "Offense type",
        Treatment = "Treatment",
        cvtrq_calc_total_pre = "Baseline value (pre)",
        rcq_calc_total_pre = "Baseline value (pre)",
        ors_calc_total_pre = "Baseline value (pre)",
        fsozu_calc_total_pre = "Baseline value (pre)",
        ucla_calc_total_pre = "Baseline value (pre)",
        bis_calc_total_pre = "Baseline value (pre)",
        cusi_calc_total_pre = "Baseline value (pre)",
        ders_calc_imp_pre = "Baseline value (pre)",
        narq_calc_ris_pre = "Baseline value (pre)",
        spsi_calc_total_pre = "Baseline value (pre)",
        kvm_calc_total_pre = "Baseline value (pre)",
        esiq_calc_total_pre = "Baseline value (pre)",
        hbi_calc_total_pre = "Baseline value (pre)",
        ssik_calc_total_pre = "Baseline value (pre)",
        ekk_calc_total_pre = "Baseline value (pre)",
        soi_total_score_pre = "Baseline value (pre)"
    )) %>%
    add_n(location = "level") %>% #add_nevent(location = "level") %>%
    modify_header(label ~ "**Variable**") %>%
    #    add_global_p(keep=TRUE) %>%
    modify_table_styling(
      columns = c(estimate, conf.low, conf.high),
      rows = reference_row %in% TRUE,
      missing_symbol = "Ref.") %>%
    modify_footnote(abbreviation = TRUE) %>%
    bold_labels() %>%
    italic_p(t = 0.05) %>%
    # CI in round brackets:
    modify_column_merge(pattern = "({conf.low}, {conf.high})",
                        rows =  conf.low!="") %>%
    modify_table_body(
      ~ .x %>%
        dplyr::mutate(adjusted_p_value = "")) %>%
    modify_header(estimate ~ "**β**", adjusted_p_value = "**adjusted p value**") else
    fit %>%
    tbl_regression(
      add_estimate_to_reference_rows = TRUE,
      intercept = TRUE,
      tidy_fun = my_log_lm_tidy,
      estimate_fun = purrr::partial(style_ratio, digits = 2),
      pvalue_fun = pretty_Pvalues,
      label = list(
        static99_modified_calc = "Static recidivism risk (baseline)",
        `Aktuelle Betreuung` = "Type of supervision",
        `Aktuelle zusätzliche Behandlung` = "Additional treatment",
        Indexdelikt = "Offense type",
        Treatment = "Treatment",
        cvtrq_calc_total_pre = "Baseline value (pre)",
        rcq_calc_total_pre = "Baseline value (pre)",
        ors_calc_total_pre = "Baseline value (pre)",
        fsozu_calc_total_pre = "Baseline value (pre)",
        ucla_calc_total_pre = "Baseline value (pre)",
        bis_calc_total_pre = "Baseline value (pre)",
        cusi_calc_total_pre = "Baseline value (pre)",
        ders_calc_imp_pre = "Baseline value (pre)",
        narq_calc_ris_pre = "Baseline value (pre)",
        spsi_calc_total_pre = "Baseline value (pre)",
        kvm_calc_total_pre = "Baseline value (pre)",
        esiq_calc_total_pre = "Baseline value (pre)",
        hbi_calc_total_pre = "Baseline value (pre)",
        ssik_calc_total_pre = "Baseline value (pre)",
        ekk_calc_total_pre = "Baseline value (pre)",
        soi_total_score_pre = "Baseline value (pre)"
    )) %>%
    add_n(location = "level") %>% #add_nevent(location = "level") %>%
    modify_header(label ~ "**Variable**") %>%
    #    add_global_p(keep=TRUE) %>%
    modify_table_styling(
      columns = c(estimate, conf.low, conf.high),
      rows = reference_row %in% TRUE,
      missing_symbol = "Ref.") %>%
    modify_footnote(abbreviation = TRUE) %>%
    bold_labels() %>%
    italic_p(t = 0.05) %>%
    # CI in round brackets:
    modify_column_merge(pattern = "({conf.low}, {conf.high})",
                        rows =  conf.low!="") %>%
    modify_table_body(
      ~ .x %>%
        dplyr::mutate(adjusted_p_value = rep("", 15))) %>%
    modify_header(estimate ~ "**exp(β)**", adjusted_p_value = "**adjusted p value**")
}

# ------
# table for glm model
# ------

tab_glm <- function(fit)
{
  if(class(fit)[1]=="glm")
  fit %>%
    tbl_regression(exponentiate = TRUE,
                   add_estimate_to_reference_rows = TRUE,
                   estimate_fun = purrr::partial(style_ratio, digits = 2), pvalue_fun = pretty_Pvalues,
                   label = list(
                      static99_modified_calc = "Static recidivsm risk (baseline)",
                      `Aktuelle Betreuung` = "Type of supervision",
                      `Aktuelle zusätzliche Behandlung` = "Additional treatment",
                      `Indexdelikt` = "Offense type",
                      Treatment = "Treatment",
                      cvtrq_calc_total_pre = "Baseline value (pre)",
                      ekk_calc_total_pre = "Baseline value (pre)",
                      rcq_calc_total_pre = "Baseline value (pre)",
                      ors_calc_total_pre = "Baseline value (pre)",
                      fsozu_calc_total_pre = "Baseline value (pre)",
                      ucla_calc_total_pre = "Baseline value (pre)",
                      bis_calc_total_pre = "Baseline value (pre)",
                      cusi_calc_total_pre = "Baseline value (pre)",
                      ders_calc_imp_pre = "Baseline value (pre)",
                      narq_calc_ris_pre = "Baseline value (pre)",
                      spsi_calc_total_pre = "Baseline value (pre)",
                      `kvm_score_pre>=43` = "Baseline value (pre)",
                      esiq_calc_total_pre = "Baseline value (pre)",
                      `hbi_calc_total_pre>=24` = "Baseline value (pre)",
                      `ssik_calc_total_pre>=30` = "Baseline value (pre)",
                      ssik_calc_total_pre = "Baseline value (pre)",
                      soi_total_score_pre = "Baseline value (pre)")) %>%
    add_n(location = "level") %>%
    add_nevent(location = "level") %>%
    modify_header(label ~ "**Variable**")  %>%
    #  add_global_p(keep=TRUE, include = dat %>% select(indep_var_string) %>% select_if(~ is.factor(.) & nlevels(as.factor(.))>2) %>% names) %>%
    modify_table_styling(
      columns = c(estimate, conf.low, conf.high),
      rows = reference_row %in% TRUE,
      missing_symbol = "Ref."
    ) %>%
    modify_footnote(abbreviation = TRUE) %>%
    bold_labels() %>%
    italic_p(t = 0.05) %>%
    # CI in round brackets:
    modify_column_merge(pattern = "({conf.low}, {conf.high})",
                        rows =  conf.low!="") %>%
    modify_table_body(
      ~ .x %>%
        dplyr::mutate(adjusted_p_value = "")) %>%
    modify_header(adjusted_p_value = "**adjusted p value**") else
  fit %>%
    tbl_regression(exponentiate = TRUE,
                   add_estimate_to_reference_rows = TRUE,
                   estimate_fun = purrr::partial(style_ratio, digits = 2), pvalue_fun = pretty_Pvalues,
                   label = list(
                      static99_modified_calc = "Static recidivsm risk (baseline)",
                      `Aktuelle Betreuung` = "Type of supervision",
                      `Aktuelle zusätzliche Behandlung` = "Additional treatment",
                      `Indexdelikt` = "Offense type",
                      Treatment = "Treatment",
                      cvtrq_calc_total_pre = "Baseline value (pre)",
                      ekk_calc_total_pre = "Baseline value (pre)",
                      rcq_calc_total_pre = "Baseline value (pre)",
                      ors_calc_total_pre = "Baseline value (pre)",
                      fsozu_calc_total_pre = "Baseline value (pre)",
                      ucla_calc_total_pre = "Baseline value (pre)",
                      bis_calc_total_pre = "Baseline value (pre)",
                      cusi_calc_total_pre = "Baseline value (pre)",
                      ders_calc_imp_pre = "Baseline value (pre)",
                      narq_calc_ris_pre = "Baseline value (pre)",
                      spsi_calc_total_pre = "Baseline value (pre)",
                      `kvm_score_pre>=43` = "Baseline value (pre)",
                      esiq_calc_total_pre = "Baseline value (pre)",
                      `hbi_calc_total_pre>=24` = "Baseline value (pre)",
                      `ssik_calc_total_pre>=30` = "Baseline value (pre)",
                      ssik_calc_total_pre = "Baseline value (pre)",
                      soi_total_score_pre = "Baseline value (pre)")) %>%
    add_n(location = "level") %>% #add_nevent(location = "level") %>%
    modify_header(label ~ "**Variable**")  %>%
    #  add_global_p(keep=TRUE, include = dat %>% select(indep_var_string) %>% select_if(~ is.factor(.) & nlevels(as.factor(.))>2) %>% names) %>%
    modify_table_styling(
      columns = c(estimate, conf.low, conf.high),
      rows = reference_row %in% TRUE,
      missing_symbol = "Ref."
    ) %>%
    modify_footnote(abbreviation = TRUE) %>%
    bold_labels() %>%
    italic_p(t = 0.05) %>%
    # CI in round brackets:
    modify_column_merge(pattern = "({conf.low}, {conf.high})",
                        rows =  conf.low!="") %>%
    modify_table_body(
      ~ .x %>%
        dplyr::mutate(adjusted_p_value = "")) %>%
    modify_header(adjusted_p_value = "**adjusted p value**")
}

fit_cvtrq_calc_total <- lm(cvtrq_calc_total_post ~ cvtrq_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
lm_Table_cvtrq_calc_total <- fit_cvtrq_calc_total %>% tab_lm

fit_rcq_calc_total <- lm(rcq_calc_total_post ~ rcq_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
# fit_rcq_calc_action <- lm(rcq_calc_action_post ~ rcq_calc_action_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
# fit_rcq_calc_contemplation <- lm(rcq_calc_contemplation_post ~ rcq_calc_contemplation_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
# sensitivity analysis:
# fit_rcq_calc_precontemplation <- lm(log(61-rcq_calc_precontemplation_post) ~ log(61-rcq_calc_precontemplation_pre) + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
lm_Table_rcq_calc_total <- fit_rcq_calc_total %>% tab_lm
# lm_Table_rcq_calc_contemplation <- fit_rcq_calc_contemplation %>% tab_lm
# lm_Table_rcq_calc_action <- fit_rcq_calc_action %>% tab_lm

fit_fsozu_calc_total <- lm(fsozu_calc_total_post ~ fsozu_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
lm_Table_fsozu_calc_total <- fit_fsozu_calc_total %>% tab_lm

fit_ors_calc_total <- lm(ors_calc_total_post ~ ors_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
lm_Table_ors_calc_total <- fit_ors_calc_total %>% tab_lm

fit_ucla_calc_total <- lm(ucla_calc_total_post ~ ucla_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
lm_Table_ucla_calc_total <- fit_ucla_calc_total %>% tab_lm

fit_bis_calc_total <- lm(log(bis_calc_total_post) ~ log(bis_calc_total_pre) + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)

fit_bis_calc_total2 <- lm((bis_calc_total_post) ~ (bis_calc_total_pre) + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
lm_Table_bis_calc_total <- fit_bis_calc_total2 %>% tab_lm(exponentiate = FALSE)
# ggResidpanel::resid_panel(fit_bis_calc_total, plots=c("qq", "resid"))
# ggResidpanel::resid_compare(list(fit_bis_calc_total, fit_bis_calc_total2), plots = c("resid", "qq"))

fit_cusi_calc_total <- lm(cusi_calc_total_post ~ cusi_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
lm_Table_cusi_calc_total <- fit_cusi_calc_total %>% tab_lm

fit_ders_calc_imp <- lm(ders_calc_imp_post ~ ders_calc_imp_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)

fit_narq_calc_ris <- lm(narq_calc_ris_post ~ narq_calc_ris_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
lm_Table_narq_calc_ris <- fit_narq_calc_ris %>% tab_lm

fit_spsi_calc_total <- lm(spsi_calc_total_post ~ spsi_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
lm_Table_spsi_calc_total <- fit_spsi_calc_total %>% tab_lm

fit_kvm_score <- lm(kvm_score_post ~ kvm_score_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)

fit_ekk_calc_total <- lm(ekk_calc_total_post ~ ekk_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
lm_Table_ekk_calc_total <- fit_ekk_calc_total %>% tab_lm

fit_esiq_calc_total <- lm(esiq_calc_total_post ~ esiq_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
lm_Table_esiq_calc_total <- fit_esiq_calc_total %>% tab_lm

fit_hbi_calc_total <- lm(hbi_calc_total_post ~ hbi_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)

fit_ssik_calc_total <- lm(ssik_calc_total_post ~ ssik_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)

fit_soi_calc_total <- lm(soi_total_score_post ~ soi_total_score_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)


library(nlme)
df_wide_clean <- janitor::clean_names(df_wide, case="none")
fit_gls <- gls(ekk_calc_total_post ~ ekk_calc_total_pre + treatment +
                 `Indexdelikt` + `Aktuelle_Betreuung` +
                 `Aktuelle_zusatzliche_Behandlung` + `Baseline_Static_99_Score`,
               data = df_wide_clean,
               weights = varPower(form= ~ekk_calc_total_pre),
               na.action=na.omit)  # modeling variance

#-------------------
# Ordinal regression
# -------------------

library(MASS)
library(ordinal)
df_wide %<>% dplyr::mutate(ders_calc_imp_post_ordinal = case_match(ders_calc_imp_post,
  5 ~ "5",
  6 ~ "6",
  7 ~ "7",
  8 ~ "8",
  9 ~ "9",
  10:15 ~ ">9") %>%
factor(ordered = TRUE, levels = c("5", "6", "7", "8", "9", ">9")),
  ders_calc_imp_pre_ordinal = case_match(ders_calc_imp_pre,
  5 ~ "5",
  6 ~ "6",
  7 ~ "7",
  8 ~ "8",
  9 ~ "9",
  10:15 ~ ">9") %>%
  factor(ordered = TRUE, levels = c("5", "6", "7", "8", "9", ">9")),
  narq_calc_ris_post_ordinal = case_match(narq_calc_ris_post,
    0 ~ "0",
    1 ~ "1",
    2 ~ "2",
    3 ~ "3",
    4 ~ "4",
    5:15 ~ ">4") %>%
  factor(ordered = TRUE, levels = c("0", "1", "2", "3", "4", ">4")),
  narq_calc_ris_pre_ordinal = case_match(narq_calc_ris_pre,
  0 ~ "0",
  1 ~ "1",
  2 ~ "2",
  3 ~ "3",
  4 ~ "4",
  5:15 ~ ">4") %>%
  factor(ordered = TRUE, levels = c("0", "1", "2", "3", "4", ">4")),
  kvm_score_post_ordinal = case_when(kvm_score_post<40 ~ "<40",
  kvm_score_post<50 & kvm_score_post>=40 ~ "40-49",
  kvm_score_post>49 ~ ">49") %>%
  factor(ordered = TRUE, levels = c("<40", "40-49", ">49")),
  kvm_score_pre_ordinal = case_when(kvm_score_pre<40 ~ "<40",
  kvm_score_pre<50 & kvm_score_pre>=40 ~ "40-49",
  kvm_score_pre>49 ~ ">49") %>%
  factor(ordered = TRUE, levels = c("<40", "40-49", ">49")),
  esiq_calc_total_post_ordinal = case_when(esiq_calc_total_post<80 ~ "<80",
    esiq_calc_total_post==80 ~ "80",
    esiq_calc_total_post>80 ~ ">80") %>%
  factor(ordered = TRUE, levels = c("<80", "8",  ">80")),
  esiq_calc_total_pre_ordinal = case_when(esiq_calc_total_pre<80 ~ "<80",
    esiq_calc_total_pre==80 ~ "80",
    esiq_calc_total_pre>80 ~ ">80") %>%
  factor(ordered = TRUE, levels = c("<80", "8",  ">80")))

df_diff %<>% dplyr::mutate(diff_esiq_calc_total_ordinal = case_when(diff_esiq_calc_total<0 ~ "Verbesserung",
                                                                     diff_esiq_calc_total==0 ~ "keine Veränderung",
                                                                     diff_esiq_calc_total>0 ~ "Verschlechterung") %>% factor(levels = c("Verschlechterung", "keine Veränderung", "Verbesserung"), ordered = TRUE),
                           diff_ders_calc_imp_ordinal = case_when(diff_ders_calc_imp<0 ~ "Verbesserung",
                                                                    diff_ders_calc_imp==0 ~ "keine Veränderung",
                                                                    diff_ders_calc_imp>0 ~ "Verschlechterung") %>% factor(levels = c("Verschlechterung", "keine Veränderung", "Verbesserung"), ordered = TRUE),
                           diff_kvm_score_ordinal = case_when(diff_kvm_score<0 ~ "Verbesserung",
                                                                  diff_kvm_score==0 ~ "keine Veränderung",
                                                                  diff_kvm_score>0 ~ "Verschlechterung") %>% factor(levels = c("Verschlechterung", "keine Veränderung", "Verbesserung"), ordered = TRUE),
                           diff_hbi_calc_total_ordinal = case_when(diff_hbi_calc_total<0 ~ "Verbesserung",
                                                              diff_hbi_calc_total==0 ~ "keine Veränderung",
                                                              diff_hbi_calc_total>0 ~ "Verschlechterung") %>% factor(levels = c("Verschlechterung", "keine Veränderung", "Verbesserung"), ordered = TRUE),
                           diff_narq_calc_ris_ordinal = case_when(diff_narq_calc_ris<0 ~ "Verbesserung",
                                                                       diff_narq_calc_ris==0 ~ "keine Veränderung",
                                                                       diff_narq_calc_ris>0 ~ "Verschlechterung") %>% factor(levels = c("Verschlechterung", "keine Veränderung", "Verbesserung"), ordered = TRUE)) %>%
  merge(., df_wide %>%
  dplyr::select(client_id, `Indexdelikt`, `Aktuelle Betreuung`, `Aktuelle zusätzliche Behandlung`, `Baseline Static-99-Score`, static99_modified_calc))

df_diff_expl %<>% dplyr::mutate(diff_esiq_calc_fan_ordinal = case_when(diff_esiq_calc_fan<0 ~ "Verbesserung",
                                                                       diff_esiq_calc_fan==0 ~ "keine Veränderung",
                                                                       diff_esiq_calc_fan>0 ~ "Verschlechterung") %>% factor(levels = c("Verschlechterung", "keine Veränderung", "Verbesserung"), ordered = TRUE),
                                diff_esiq_calc_ver_ordinal = case_when(diff_esiq_calc_ver<0 ~ "Verbesserung",
                                                                       diff_esiq_calc_ver==0 ~ "keine Veränderung",
                                                                       diff_esiq_calc_ver>0 ~ "Verschlechterung") %>% factor(levels = c("Verschlechterung", "keine Veränderung", "Verbesserung"), ordered = TRUE),
                                diff_esiq_calc_ver_ordinal = case_when(diff_esiq_calc_ver<0 ~ "Verbesserung",
                                                                       diff_esiq_calc_ver==0 ~ "keine Veränderung",
                                                                       diff_esiq_calc_ver>0 ~ "Verschlechterung") %>% factor(levels = c("Verschlechterung", "keine Veränderung", "Verbesserung"), ordered = TRUE)) %>%
  merge(., df_wide %>%
          dplyr::select(client_id, `Indexdelikt`, `Aktuelle Betreuung`, `Aktuelle zusätzliche Behandlung`, `Baseline Static-99-Score`, static99_modified_calc))

fit_ders_calc_imp_ordinal <- clm(ders_calc_imp_post_ordinal ~ ders_calc_imp_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
# scale_test(fit_ders_calc_imp_ordinal)
glm_Table_ders_calc_imp <- fit_ders_calc_imp_ordinal %>% tab_glm

fit_ders_calc_imp_ordinal2 <- polr(ders_calc_imp_post_ordinal ~ ders_calc_imp_pre_ordinal + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide, Hess=TRUE)
## coefficient test
library("AER")
# coeftest(fit_ders_calc_imp_ordinal2)
# poTest(fit_ders_calc_imp_ordinal2)

glm(factor(ders_calc_imp_post>6) ~ factor(ders_calc_imp_pre>6) + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide, family = "binomial") %>% summary()

# narq #
fit_narq_calc_ris_ordinal <- clm(narq_calc_ris_post_ordinal ~ narq_calc_ris_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
# scale_test(fit_narq_calc_ris_ordinal)
glm_Table_narq_calc_ris <- fit_narq_calc_ris_ordinal %>% tab_glm

fit_narq_calc_ris_ordinal2 <- polr(narq_calc_ris_post_ordinal ~ narq_calc_ris_pre_ordinal + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide, Hess=TRUE)
## coefficient test
library("AER")
# coeftest(fit_narq_calc_ris_ordinal2)
# poTest(fit_narq_calc_ris_ordinal2)


# kvm #
fit_kvm_score_ordinal <- clm(kvm_score_post_ordinal ~ kvm_score_pre_ordinal + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
scale_test(fit_kvm_score_ordinal)
fit_kvm_score_ordinal2 <- polr(kvm_score_post_ordinal ~ kvm_score_pre_ordinal + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide, Hess=TRUE)
poTest(fit_kvm_score_ordinal2)
glm(factor(kvm_score_post>39) ~ factor(kvm_score_pre>39) + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide, family = "binomial") %>% summary()
glm(factor(kvm_score_post>49) ~ factor(kvm_score_pre>49) + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide, family = "binomial") %>% summary()

fit_kvm_score_logistic <- glm(`kvm_score_post>=43` ~ `kvm_score_pre>=43` + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide %>% dplyr::mutate(`kvm_score_post>=43` = factor(kvm_score_post>=43), `kvm_score_pre>=43` = factor(kvm_score_pre>=43))
, family = "binomial")
glm_Table_kvm_score <- fit_kvm_score_logistic %>% tab_glm

# esiq #
fit_esiq_calc_total_ordinal_diff <- clm(diff_esiq_calc_total_ordinal ~ treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_diff)
glm_Table_esiq_calc_total_ordinal_diff <- fit_esiq_calc_total_ordinal_diff %>% tab_glm

fit_narq_calc_ris_ordinal_diff <- clm(diff_narq_calc_ris_ordinal ~ treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_diff)
glm_Table_narq_calc_ris_ordinal_diff <- fit_narq_calc_ris_ordinal_diff %>% tab_glm

fit_esiq_calc_ver_ordinal_diff <- clm(diff_esiq_calc_ver_ordinal ~ treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_diff_expl)
glm_Table_esiq_calc_ver_ordinal_diff <- fit_esiq_calc_ver_ordinal_diff %>% tab_glm

fit_rcq_calc_current_stage_ordinal_diff <- clm(diff_rcq_calc_current_stage ~ treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_diff_expl)
glm_Table_rcq_calc_current_stage_ordinal_diff <- fit_rcq_calc_current_stage_ordinal_diff %>% tab_glm

fit_esiq_calc_fan_ordinal_diff <- clm(diff_esiq_calc_fan_ordinal ~ treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_diff_expl)
glm_Table_esiq_calc_fan_ordinal_diff <- fit_esiq_calc_fan_ordinal_diff %>% tab_glm

glm(factor(diff_esiq_calc_total>0) ~ treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_diff, family = "binomial") %>% summary()

fit_esiq_calc_total_ordinal <- clm(esiq_calc_total_post_ordinal ~ esiq_calc_total_pre_ordinal + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)

fit_esiq_calc_total_ordinal <- clm(esiq_calc_total_post_ordinal ~ esiq_calc_total_pre_ordinal + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide)
# scale_test(fit_esiq_calc_total_ordinal)
# fit_esiq_calc_total_ordinal2 <- polr(esiq_calc_total_post_ordinal ~ kvm_score_pre_ordinal + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide, Hess=TRUE)
# poTest(fit_esiq_calc_total_ordinal2)
glm(factor(esiq_calc_total_post>0) ~ factor(esiq_calc_total_pre>90) + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide, family = "binomial") %>% summary()

# hbi #
glm(factor(hbi_calc_total_post>=24) ~ factor(hbi_calc_total_pre>=24) + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide, family = "binomial") %>% summary()
fit_hbi_calc_total_logistic <- glm(`hbi_calc_total_post>=24` ~ `hbi_calc_total_pre>=24` + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide %>% dplyr::mutate(`hbi_calc_total_post>=24` = factor(hbi_calc_total_post>23), `hbi_calc_total_pre>=24` = factor(hbi_calc_total_pre>23))
, family = "binomial")
glm_Table_hbi_calc_total <- fit_hbi_calc_total_logistic %>% tab_glm

# ssik #
glm(factor(ssik_calc_total_post>29) ~ factor(ssik_calc_total_pre>29) + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide, family = "binomial") %>% summary()
fit_ssik_calc_total_logistic <- glm(`ssik_calc_total_post>=30` ~ `ssik_calc_total_pre>=30` + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide %>% dplyr::mutate(`ssik_calc_total_post>=30` = factor(ssik_calc_total_post>29), `ssik_calc_total_pre>=30` = factor(ssik_calc_total_pre>29))
, family = "binomial")
glm_Table_ssik_calc_total <- fit_ssik_calc_total_logistic %>% tab_glm


p.val.df <- lm_Table_cvtrq_calc_total$table_body$p.value[-(1:4)]
p.val.df <- bind_rows(as.data.frame(t(p.val.df)), as.data.frame(t(lm_Table_rcq_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_fsozu_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_ors_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_ucla_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_bis_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_ors_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_cusi_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(glm_Table_ders_calc_imp$table_body$p.value[-(1:3)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(glm_Table_narq_calc_ris$table_body$p.value[-(1:3)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_spsi_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(glm_Table_kvm_score$table_body$p.value[-(1:5)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_ekk_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_esiq_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(glm_Table_hbi_calc_total$table_body$p.value[-(1:5)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(glm_Table_ssik_calc_total$table_body$p.value[-(1:5)])))
# p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_rcq_calc_contemplation$table_body$p.value[-(1:4)])))
# p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_rcq_calc_action$table_body$p.value[-(1:4)])))
# p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_spsi_calc_ppo$table_body$p.value[-(1:4)])))
# p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_spsi_calc_rps$table_body$p.value[-(1:4)])))
# p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_spsi_calc_npo$table_body$p.value[-(1:4)])))
# p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_spsi_calc_ics$table_body$p.value[-(1:4)])))

p.val.df %<>% mutate_all(~p.adjust(., method = "holm") %>% pretty_Pvalues())

lm_Table_cvtrq_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[1,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_rcq_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[2,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_fsozu_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[3,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_ors_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[4,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_ucla_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[5,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_bis_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[6,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_ors_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[7,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_cusi_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[8,] %>% as.numeric() %>% pretty_Pvalues()
glm_Table_ders_calc_imp$table_body$adjusted_p_value[-(1:3)] <- p.val.df[9,] %>% as.numeric() %>% pretty_Pvalues()
glm_Table_narq_calc_ris$table_body$adjusted_p_value[-(1:3)] <- p.val.df[10,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_spsi_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[11,] %>% as.numeric() %>% pretty_Pvalues()
glm_Table_kvm_score$table_body$adjusted_p_value[-(1:5)] <- p.val.df[12,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_ekk_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[13,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_esiq_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[14,] %>% as.numeric() %>% pretty_Pvalues()
glm_Table_hbi_calc_total$table_body$adjusted_p_value[-(1:5)] <- p.val.df[15,] %>% as.numeric() %>% pretty_Pvalues()
glm_Table_ssik_calc_total$table_body$adjusted_p_value[-(1:5)] <- p.val.df[16,] %>% as.numeric() %>% pretty_Pvalues()
# lm_Table_rcq_calc_contemplation$table_body$adjusted_p_value[-(1:4)] <- p.val.df[17,] %>% as.numeric() %>% pretty_Pvalues()
# lm_Table_rcq_calc_action$table_body$adjusted_p_value[-(1:4)] <- p.val.df[18,] %>% as.numeric() %>% pretty_Pvalues()
# lm_Table_spsi_calc_ppo$table_body$adjusted_p_value[-(1:4)] <- p.val.df[20,] %>% as.numeric() %>% pretty_Pvalues()
# lm_Table_spsi_calc_rps$table_body$adjusted_p_value[-(1:4)] <- p.val.df[21,] %>% as.numeric() %>% pretty_Pvalues()
# lm_Table_spsi_calc_npo$table_body$adjusted_p_value[-(1:4)] <- p.val.df[22,] %>% as.numeric() %>% pretty_Pvalues()
# lm_Table_spsi_calc_ics$table_body$adjusted_p_value[-(1:4)] <- p.val.df[19,] %>% as.numeric() %>% pretty_Pvalues()

# Adjust p values for the ordinal regression model with change in esiq as dependent variable:
p.val.df <- lm_Table_cvtrq_calc_total$table_body$p.value[-(1:4)]
p.val.df <- bind_rows(as.data.frame(t(p.val.df)), as.data.frame(t(lm_Table_rcq_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_fsozu_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_ors_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_ucla_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_bis_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_ors_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_cusi_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(glm_Table_ders_calc_imp$table_body$p.value[-(1:3)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(glm_Table_narq_calc_ris$table_body$p.value[-(1:3)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_spsi_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(glm_Table_kvm_score$table_body$p.value[-(1:5)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_ekk_calc_total$table_body$p.value[-(1:4)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(glm_Table_esiq_calc_total_ordinal_diff$table_body$p.value[-(1:2)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(glm_Table_hbi_calc_total$table_body$p.value[-(1:5)])))
p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(glm_Table_ssik_calc_total$table_body$p.value[-(1:5)])))
# p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_rcq_calc_contemplation$table_body$p.value[-(1:4)])))
# p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_rcq_calc_action$table_body$p.value[-(1:4)])))
# p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_spsi_calc_ppo$table_body$p.value[-(1:4)])))
# p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_spsi_calc_rps$table_body$p.value[-(1:4)])))
# p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_spsi_calc_npo$table_body$p.value[-(1:4)])))
# p.val.df <- bind_rows(as.data.frame(p.val.df), as.data.frame(t(lm_Table_spsi_calc_ics$table_body$p.value[-(1:4)])))

p.val.df %<>% mutate_all(~p.adjust(., method = "holm") %>% pretty_Pvalues())
glm_Table_esiq_calc_total_ordinal_diff$table_body$adjusted_p_value[-(1:2)] <- p.val.df[14,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_cvtrq_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[1,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_rcq_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[2,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_fsozu_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[3,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_ors_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[4,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_ucla_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[5,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_bis_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[6,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_ors_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[7,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_cusi_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[8,] %>% as.numeric() %>% pretty_Pvalues()
glm_Table_ders_calc_imp$table_body$adjusted_p_value[-(1:3)] <- p.val.df[9,] %>% as.numeric() %>% pretty_Pvalues()
glm_Table_narq_calc_ris$table_body$adjusted_p_value[-(1:3)] <- p.val.df[10,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_spsi_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[11,] %>% as.numeric() %>% pretty_Pvalues()
glm_Table_kvm_score$table_body$adjusted_p_value[-(1:5)] <- p.val.df[12,] %>% as.numeric() %>% pretty_Pvalues()
lm_Table_ekk_calc_total$table_body$adjusted_p_value[-(1:4)] <- p.val.df[13,] %>% as.numeric() %>% pretty_Pvalues()
glm_Table_hbi_calc_total$table_body$adjusted_p_value[-(1:5)] <- p.val.df[15,] %>% as.numeric() %>% pretty_Pvalues()
glm_Table_ssik_calc_total$table_body$adjusted_p_value[-(1:5)] <- p.val.df[16,] %>% as.numeric() %>% pretty_Pvalues()
# lm_Table_rcq_calc_contemplation$table_body$adjusted_p_value[-(1:4)] <- p.val.df[17,] %>% as.numeric() %>% pretty_Pvalues()
# lm_Table_rcq_calc_action$table_body$adjusted_p_value[-(1:4)] <- p.val.df[18,] %>% as.numeric() %>% pretty_Pvalues()
# lm_Table_spsi_calc_ppo$table_body$adjusted_p_value[-(1:4)] <- p.val.df[20,] %>% as.numeric() %>% pretty_Pvalues()
# lm_Table_spsi_calc_rps$table_body$adjusted_p_value[-(1:4)] <- p.val.df[21,] %>% as.numeric() %>% pretty_Pvalues()
# lm_Table_spsi_calc_npo$table_body$adjusted_p_value[-(1:4)] <- p.val.df[22,] %>% as.numeric() %>% pretty_Pvalues()
# lm_Table_spsi_calc_ics$table_body$adjusted_p_value[-(1:4)] <- p.val.df[19,] %>% as.numeric() %>% pretty_Pvalues()


fit_spsi_calc_as <- lm(spsi_calc_as_post ~ spsi_calc_as_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide_expl)
lm_Table_spsi_calc_as <- fit_spsi_calc_as %>% tab_lm

fit_spsi_calc_ppo <- lm(spsi_calc_ppo_post ~ spsi_calc_ppo_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide_expl)
lm_Table_spsi_calc_ppo <- fit_spsi_calc_ppo %>% tab_lm

fit_spsi_calc_rps <- lm(spsi_calc_rps_post ~ spsi_calc_rps_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide_expl)
lm_Table_spsi_calc_rps <- fit_spsi_calc_rps %>% tab_lm

fit_spsi_calc_npo <- lm(spsi_calc_npo_post ~ spsi_calc_npo_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide_expl)
lm_Table_spsi_calc_npo <- fit_spsi_calc_npo %>% tab_lm

fit_spsi_calc_ics <- lm(spsi_calc_ics_post ~ spsi_calc_ics_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `static99_modified_calc`, data = df_wide_expl)
lm_Table_spsi_calc_ics <- fit_spsi_calc_ics %>% tab_lm
