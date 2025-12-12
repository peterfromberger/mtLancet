df_no_na  <- df_wide %>%
  dplyr::select(cusi_calc_total_post, cusi_calc_total_pre, treatment, `Indexdelikt`, `Aktuelle Betreuung`, `Aktuelle zusätzliche Behandlung`, `Baseline Static-99-Score`) %>%
  na.omit

# calculate Cook's distances
cook_distance <- cooks.distance(fit_cusi_calc_total)

# find influential cases
influential <- as.numeric(names(cook_distance)[cook_distance > (4 / nrow(df_no_na))])

# remove influential cases
df_no_influentials <- df_no_na[-influential,]

# fit model again on this new dataset
fit_cusi_calc_total2 <- lm(cusi_calc_total_post ~ cusi_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `Baseline Static-99-Score`, data = df_no_influentials)

# Create a column of the residuals
df_no_na$residuals <- residuals(fit_cusi_calc_total)

# Calculate the IQR of the residuals
residual_IQR <- IQR(df_no_na$residuals, na.rm = TRUE)

# Calculate threshold values for 1.5 * IQR
lower_threshold <- quantile(df_no_na$residuals, 0.25) - 1.5 * residual_IQR
upper_threshold <- quantile(df_no_na$residuals, 0.75) + 1.5 * residual_IQR

# Create a new df excluding residuals larger than 1.5 * IQR
df_wide_no_res_outliers <- df_no_na[!(df_no_na$residuals < lower_threshold | df_no_na$residuals > upper_threshold), ]

# Remove the residuals column (as we don't need it in this filtered df)
df_wide_no_res_outliers$residuals <- NULL

# fit model again on this new dataset
fit_cusi_calc_total3 <- lm(cusi_calc_total_post ~ cusi_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `Baseline Static-99-Score`, data = df_wide_no_res_outliers)

# fit_rcq_calc_total

df_no_na  <- df_wide %>%
  dplyr::select(rcq_calc_total_post, rcq_calc_total_pre, treatment, `Indexdelikt`, `Aktuelle Betreuung`, `Aktuelle zusätzliche Behandlung`, `Baseline Static-99-Score`) %>%
  na.omit

# calculate Cook's distances
cook_distance <- cooks.distance(fit_rcq_calc_total)

# find influential cases
influential <- as.numeric(names(cook_distance)[cook_distance > (4 / nrow(df_no_na))])

# remove influential cases
df_no_influentials <- df_no_na[-influential,]

# fit model again on this new dataset
fit_rcq_calc_total2 <- lm(rcq_calc_total_post ~ rcq_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `Baseline Static-99-Score`, data = df_no_influentials)

# Create a column of the residuals
df_no_na$residuals <- residuals(fit_rcq_calc_total)

# Calculate the IQR of the residuals
residual_IQR <- IQR(df_no_na$residuals, na.rm = TRUE)

# Calculate threshold values for 1.5 * IQR
lower_threshold <- quantile(df_no_na$residuals, 0.25) - 1.5 * residual_IQR
upper_threshold <- quantile(df_no_na$residuals, 0.75) + 1.5 * residual_IQR

# Create a new df excluding residuals larger than 1.5 * IQR
df_wide_no_res_outliers <- df_no_na[!(df_no_na$residuals < lower_threshold | df_no_na$residuals > upper_threshold), ]

# Remove the residuals column (as we don't need it in this filtered df)
df_wide_no_res_outliers$residuals <- NULL

# fit model again on this new dataset
fit_rcq_calc_total3 <- lm(rcq_calc_total_post ~ rcq_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `Baseline Static-99-Score`, data = df_wide_no_res_outliers)

# # fit_rcq_calc_contemplation #

# df_no_na  <- df_wide %>%
#   dplyr::select(rcq_calc_contemplation_post, rcq_calc_contemplation_pre, treatment, `Indexdelikt`, `Aktuelle Betreuung`, `Aktuelle zusätzliche Behandlung`, `Baseline Static-99-Score`) %>%
#   na.omit

# # calculate Cook's distances
# cook_distance <- cooks.distance(fit_rcq_calc_contemplation)

# # find influential cases
# influential <- as.numeric(names(cook_distance)[cook_distance > (4 / nrow(df_no_na))])

# # remove influential cases
# df_no_influentials <- df_no_na[-influential,]

# # fit model again on this new dataset
# fit_rcq_calc_contemplation2 <- lm(rcq_calc_contemplation_post ~ rcq_calc_contemplation_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `Baseline Static-99-Score`, data = df_no_influentials)

# # Create a column of the residuals
# df_no_na$residuals <- residuals(fit_rcq_calc_contemplation)

# # Calculate the IQR of the residuals
# residual_IQR <- IQR(df_no_na$residuals, na.rm = TRUE)

# # Calculate threshold values for 1.5 * IQR
# lower_threshold <- quantile(df_no_na$residuals, 0.25) - 1.5 * residual_IQR
# upper_threshold <- quantile(df_no_na$residuals, 0.75) + 1.5 * residual_IQR

# # Create a new df excluding residuals larger than 1.5 * IQR
# df_wide_no_res_outliers <- df_no_na[!(df_no_na$residuals < lower_threshold | df_no_na$residuals > upper_threshold), ]

# # Remove the residuals column (as we don't need it in this filtered df)
# df_wide_no_res_outliers$residuals <- NULL

# # fit model again on this new dataset
# fit_rcq_calc_contemplation3 <- lm(rcq_calc_contemplation_post ~ rcq_calc_contemplation_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `Baseline Static-99-Score`, data = df_wide_no_res_outliers)

# fit_esiq_calc_total #

df_no_na  <- df_wide %>%
  dplyr::select(esiq_calc_total_post, esiq_calc_total_pre, treatment, `Indexdelikt`, `Aktuelle Betreuung`, `Aktuelle zusätzliche Behandlung`, `Baseline Static-99-Score`) %>%
  na.omit

# calculate Cook's distances
cook_distance <- cooks.distance(fit_esiq_calc_total)

# find influential cases
influential <- as.numeric(names(cook_distance)[cook_distance > (4 / nrow(df_no_na))])

# remove influential cases
df_no_influentials <- df_no_na[-influential,]

# fit model again on this new dataset
fit_esiq_calc_total2 <- lm(esiq_calc_total_post ~ esiq_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `Baseline Static-99-Score`, data = df_no_influentials)

# Create a column of the residuals
df_no_na$residuals <- residuals(fit_esiq_calc_total)

# Calculate the IQR of the residuals
residual_IQR <- IQR(df_no_na$residuals, na.rm = TRUE)

# Calculate threshold values for 1.5 * IQR
lower_threshold <- quantile(df_no_na$residuals, 0.25) - 1.5 * residual_IQR
upper_threshold <- quantile(df_no_na$residuals, 0.75) + 1.5 * residual_IQR

# Create a new df excluding residuals larger than 1.5 * IQR
df_wide_no_res_outliers <- df_no_na[!(df_no_na$residuals < lower_threshold | df_no_na$residuals > upper_threshold), ]

# Remove the residuals column (as we don't need it in this filtered df)
df_wide_no_res_outliers$residuals <- NULL

# fit model again on this new dataset
fit_esiq_calc_total3 <- lm(esiq_calc_total_post ~ esiq_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `Baseline Static-99-Score`, data = df_wide_no_res_outliers)

# fit_ucla_calc_total #

df_no_na  <- df_wide %>%
  dplyr::select(ucla_calc_total_post, ucla_calc_total_pre, treatment, `Indexdelikt`, `Aktuelle Betreuung`, `Aktuelle zusätzliche Behandlung`, `Baseline Static-99-Score`) %>%
  na.omit

# calculate Cook's distances
cook_distance <- cooks.distance(fit_ucla_calc_total)

# find influential cases
influential <- as.numeric(names(cook_distance)[cook_distance > (4 / nrow(df_no_na))])

# remove influential cases
df_no_influentials <- df_no_na[-influential,]

# fit model again on this new dataset
fit_ucla_calc_total2 <- lm(ucla_calc_total_post ~ ucla_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `Baseline Static-99-Score`, data = df_no_influentials)

# Create a column of the residuals
df_no_na$residuals <- residuals(fit_ucla_calc_total)

# Calculate the IQR of the residuals
residual_IQR <- IQR(df_no_na$residuals, na.rm = TRUE)

# Calculate threshold values for 1.5 * IQR
lower_threshold <- quantile(df_no_na$residuals, 0.25) - 1.5 * residual_IQR
upper_threshold <- quantile(df_no_na$residuals, 0.75) + 1.5 * residual_IQR

# Create a new df excluding residuals larger than 1.5 * IQR
df_wide_no_res_outliers <- df_no_na[!(df_no_na$residuals < lower_threshold | df_no_na$residuals > upper_threshold), ]

# Remove the residuals column (as we don't need it in this filtered df)
df_wide_no_res_outliers$residuals <- NULL

# fit model again on this new dataset
fit_ucla_calc_total3 <- lm(ucla_calc_total_post ~ ucla_calc_total_pre + treatment + `Indexdelikt` + `Aktuelle Betreuung` + `Aktuelle zusätzliche Behandlung` + `Baseline Static-99-Score`, data = df_wide_no_res_outliers)
