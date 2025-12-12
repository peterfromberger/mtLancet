library(rbmi)
library(tibble)

fit <- mmrm(
  formula = IoD ~ `IoD_baseline` + treatment*timepoint + `Indexdelikt` + `Aktuelle_Betreuung` + `Aktuelle_zusatzliche_Behandlung` +
    ad(timepoint | client_id),
  data = dat_clean
)

dat_clean2 <- dat_clean %>% tidyr::drop_na(`IoD_baseline`, treatment, `Indexdelikt`, `Aktuelle_Betreuung`, `Aktuelle_zusatzliche_Behandlung`, `static99_modified_calc`, timepoint, client_id)
# remove clients with missing baseline values or treatment:
dat_clean2 <- dat_clean2 %>% filter(client_id %in% (dat_clean2 %>% filter(timepoint=="Baseline") %>% .$client_id) & !client_id %in% c(533, 253, 396))  %>%
  filter(!timepoint=="Baseline") %>%
  mutate(client_id = droplevels(client_id),
         timepoint = droplevels(timepoint))

# Use expand_locf to add rows corresponding to visits with missing outcomes to the dataset
dat_clean2 <- expand_locf(
  dat_clean2,
  client_id = levels(dat_clean2$client_id), # expand by PATIENT and VISIT
  timepoint = levels(dat_clean2$timepoint),
  vars = c("IoD_baseline", "treatment", "Indexdelikt", "Aktuelle_Betreuung", "Aktuelle_zusatzliche_Behandlung", "static99_modified_calc"), # fill with LOCF BASVAL and THERAPY
  group = c("client_id"),
  order = c("client_id", "timepoint")
)

dat_ice <- dat_clean2 %>%
  arrange(client_id, timepoint) %>%
  filter(is.na(IoD)) %>%
  group_by(client_id) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  select(client_id, timepoint) %>%
  mutate(strategy = "JR")

vars <- set_vars(
  outcome = "IoD",
  visit = "timepoint",
  subjid = "client_id",
  group = "treatment",
  covariates = c("IoD_baseline", "treatment*timepoint", "Indexdelikt", "Aktuelle_Betreuung", "Aktuelle_zusatzliche_Behandlung", "static99_modified_calc")
)
# Define which imputation method to use (here: Bayesian multiple imputation with 150 imputed datsets)
method <- method_condmean(n_samples = 20, covariance = "us")

set.seed(987)
drawObj <- draws(
  data = dat_clean2,
  data_ice = dat_ice,
  vars = vars,
  method = method,
  quiet = TRUE
)

drawObj

imputeObj <- impute(
  drawObj,
  references = c("Intervention" = "Intervention", "Placebo" = "Placebo")
)
imputeObj

ancova_modified <- function(data, ...) {
  data2 <- data %>% mutate(IoD = IoD - IoD_baseline)
  rbmi::ancova(data2, ...)
}

anaObj <- analyse(
  imputeObj,
  ancova_modified,
  vars = set_vars(
    subjid = "client_id",
    outcome = "IoD",
    visit = "timepoint",
    group = "treatment",
    covariates = c("IoD_baseline", "Indexdelikt", "Aktuelle_Betreuung", "Aktuelle_zusatzliche_Behandlung", "static99_modified_calc")
  )
)
anaObj

poolObj <- pool(
  anaObj,
  conf.level = 0.95,
  alternative = "two.sided"
)
poolObj

rbmi_tab <- poolObj %>% tibble::as_tibble() %>%
  mutate(parameter = parameter %>%
           gsub("trt_", "group_difference_", .) %>% gsub("lsm_ref_", "least_square_mean_Intervention_", .) %>% gsub("lsm_alt_", "least_square_mean_Placebo_", .),
         pval = pretty_Pvalues(pval, orgbold=TRUE),
         ci = paste0("(", round(lci, 2), ", ", round(uci, 2), ")"),
         est = round(est, 2)) %>%
  dplyr::select(parameter, est, ci, pval)
