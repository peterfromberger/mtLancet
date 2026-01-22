library(ggeffects)
library(ggplot2)
library(ggsci)
library(ggtext)
library(glue)
library(ggrain)
library(emmeans)
library(stringr)
library(patchwork)
#library(svglite)

# Set global options
options(
  ggplot2.discrete.colour = ggsci::scale_colour_lancet,
  ggplot2.discrete.fill = ggsci::scale_fill_lancet
)

add_gg_theme <- function(plt) {
  plt <- plt +
    ggplot2::theme_minimal() + 
    ggplot2::theme(
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
      )

  return(plt)
}

# define color_palette
lanonc_colors <- pal_lancet(palette = "lanonc", alpha = 1)(9)

# ################
# PRIMARY OUTCOMES
# ################

# ------------------
# CARES total score
# ------------------

# Estimated marginal means
emm <- emmeans(fit2, ~ treatment | timepoint)
emm_df <- as.data.frame(emm)

plt <- ggplot(
    emm_df, 
    aes(
      x = timepoint,
      y = emmean, 
      color = treatment, 
      group = treatment
    )
  ) +
  geom_line() +
  geom_point(size = 3) +
  geom_ribbon(
    aes(
      ymin = lower.CL, 
      ymax = upper.CL
    ),
    alpha = 0.2
  ) +
  labs(
    y = "Estimated marginal means +- 95%-CI",
    x = "Timepoint",
    color = "Treatment"
  )
  
plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-cares-estimated-marginal-means.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

ggsave(
  here::here("_figures/fig-cares-estimated-marginal-means.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

# Estimated marginal means with ggplot2
emm <- emmeans(fit2_c, ~ treatment | timepoint)
emm_df <- as.data.frame(emm)

plt <- ggplot(
    emm_df, 
    aes(
      x = timepoint,
      y = emmean, 
      color = treatment, 
      group = treatment
    )
  ) +
  geom_line() +
  geom_point(size = 3) +
  geom_ribbon(
    aes(ymin = lower.CL, ymax = upper.CL),
    alpha = 0.2
  ) +
  labs(
    y = "Estimated marginal mean +- 95%-CI (CARES total score)",
    x = "Timepoint",
    color = "Treatment"
  )

plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-cares-estimated-marginal-means-time-since-baseline.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

ggsave(
  here::here("_figures/fig-cares-estimated-marginal-means-time-since-baseline.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)


# RAIN plot (descriptive)
plt <- ggplot(
    data = dat_clean %>% filter(!client_id %in% c(253, 396) & !is.na(client_group)),
    aes(fill = client_group, x = timepoint, y = IoD_reduced, color = client_group)
  ) +
  geom_rain(
    seed = 42,
    rain.side = 'r',
    #id.long.var = "IoD_reduced",
    point.args = list(
      alpha = 0.3,
      shape = 21
    ),
    point.args.pos = rlang::list2(
      position = position_jitter(
        width = 0.1,
        height = 0.1
      )
    ),
    boxplot.args = list(
      outlier.shape = NA,
      color = "black",
      alpha = .7
    ),
    boxplot.args.pos = list(
      position = ggpp::position_dodgenudge(
        x = .25,
        width = 0.2
      ), 
      width = 0.15
    ),
    violin.args = list(
      alpha = .2
    ),
    violin.args.pos = list(
      position = ggpp::position_dodgenudge(
        x = .4,
        width = 0
      ), 
      width = 1.2
    ),
  ) +
  labs(
    y = "CARES (total score)",
    x = "Timepoint",
    color = "Treatment"
  )

plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-cares-rainplot.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8,
)

ggsave(
  here::here("_figures/fig-cares-rainplot.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

# RAIN plot (difference from baseline)
plt <- ggplot(
    data = df_diff_iod_reduced %>% 
      filter(!client_id %in% c(253, 396) & !is.na(treatment)) %>%
      pivot_longer(
        cols = starts_with("diff_IoD_reduced Module"),
        names_to = "timepoint",
        values_to = "value"
      ) %>%
      mutate(
        timepoint = case_when(
          str_detect(timepoint, "Module 1") ~ "Module 1 (post)",
          str_detect(timepoint, "Module 2") ~ "Module 2 (post)",
          str_detect(timepoint, "Module 3") ~ "Module 3 (post)",
          str_detect(timepoint, "Module 4") ~ "Module 4 (post)",
          str_detect(timepoint, "Module 5") ~ "Module 5 (post)",
          str_detect(timepoint, "Module 6") ~ "Module 6 (post)"
        )
      ),
    aes(fill = treatment, x = timepoint, y = value, color = treatment)
  ) +
  geom_rain(
    seed = 42,
    rain.side = 'r',
    #id.long.var = "IoD_reduced",
    point.args = list(
      alpha = 0.3,
      shape = 21
    ),
    point.args.pos = rlang::list2(
      position = position_jitter(
        width = 0.1,
        height = 0.1
      )
    ),
    boxplot.args = list(
      outlier.shape = NA,
      color = "black",
      alpha = .7
    ),
    boxplot.args.pos = list(
      position = ggpp::position_dodgenudge(
        x = .25,
        width = 0.2
      ), 
      width = 0.15
    ),
    violin.args = list(
      alpha = .2
    ),
    violin.args.pos = list(
      position = ggpp::position_dodgenudge(
        x = .4,
        width = 0
      ), 
      width = 1.2
    ),
  ) +
  labs(
    y = "Difference from baseline (CARES total score)",
    x = "Timepoint",
    color = "Treatment"
  )

plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-cares-rainplot-difference-from-baseline.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8,
)

ggsave(
  here::here("_figures/fig-cares-rainplot-difference-from-baseline.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

# ------------------
# IOD total score
# ------------------

# Estimated marginal means
emm <- emmeans(fit, ~ treatment | timepoint)
emm_df <- as.data.frame(emm)

plt <- ggplot(
    emm_df, 
    aes(
      x = timepoint,
      y = emmean, 
      color = treatment, 
      group = treatment
    )
  ) +
  geom_line() +
  geom_point(size = 3) +
  geom_ribbon(
    aes(ymin = lower.CL, ymax = upper.CL),
    alpha = 0.2
  ) +
  labs(
    y = "Estimated marginal mean +- 95%-CI",
    x = "Timepoint",
    color = "Treatment"
  )
  
plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-iod-estimated-marginal-means.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

ggsave(
  here::here("_figures/fig-iod-estimated-marginal-means.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

# Estimated marginal means with ggplot2
emm <- emmeans(fit1_c, ~ treatment | timepoint)
emm_df <- as.data.frame(emm)

plt <- ggplot(
    emm_df, 
    aes(
      x = timepoint,
      y = emmean, 
      color = treatment, 
      group = treatment
    )
  ) +
  geom_line() +
  geom_point(size = 3) +
  geom_ribbon(
    aes(ymin = lower.CL, ymax = upper.CL),
    alpha = 0.2
  ) +
  labs(
    y = "Estimated marginal mean +- 95%-CI",
    x = "Timepoint",
    color = "Treatment"
  )

plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-iod-estimated-marginal-means-time-since-baseline.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

ggsave(
  here::here("_figures/fig-iod-estimated-marginal-means-time-since-baseline.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)


# RAIN plot (descriptive)
plt <- ggplot(
    data = dat_clean %>% filter(!client_id %in% c(253, 396) & !is.na(client_group)),
    aes(fill = client_group, x = timepoint, y = IoD, color = client_group)
  ) +
  geom_rain(
    seed = 42,
    rain.side = 'r',
    #id.long.var = "IoD_reduced",
    point.args = list(
      alpha = 0.3,
      shape = 21
    ),
    point.args.pos = rlang::list2(
      position = position_jitter(
        width = 0.1,
        height = 0.1
      )
    ),
    boxplot.args = list(
      outlier.shape = NA,
      color = "black",
      alpha = .7
    ),
    boxplot.args.pos = list(
      position = ggpp::position_dodgenudge(
        x = .25,
        width = 0.2
      ), 
      width = 0.15
    ),
    violin.args = list(
      alpha = .2
    ),
    violin.args.pos = list(
      position = ggpp::position_dodgenudge(
        x = .4,
        width = 0
      ), 
      width = 1.2
    ),
  ) +
  labs(
    y = "Index of Desistance (IoD)",
    x = "Timepoint",
    color = "Treatment"
  )

plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-iod-rainplot.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8,
)

ggsave(
  here::here("_figures/fig-iod-rainplot.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)

# RAIN plot (difference from baseline)
plt <- ggplot(
    data = df_diff_iod %>% 
      filter(!client_id %in% c(253, 396) & !is.na(treatment)) %>%
      pivot_longer(
        cols = starts_with("diff_IoD Module"),
        names_to = "timepoint",
        values_to = "value"
      ) %>%
      mutate(
        timepoint = case_when(
          str_detect(timepoint, "Module 1") ~ "Module 1 (post)",
          str_detect(timepoint, "Module 2") ~ "Module 2 (post)",
          str_detect(timepoint, "Module 3") ~ "Module 3 (post)",
          str_detect(timepoint, "Module 4") ~ "Module 4 (post)",
          str_detect(timepoint, "Module 5") ~ "Module 5 (post)",
          str_detect(timepoint, "Module 6") ~ "Module 6 (post)"
        )
      ),
    aes(fill = treatment, x = timepoint, y = value, color = treatment)
  ) +
  geom_rain(
    seed = 42,
    rain.side = 'r',
    #id.long.var = "IoD_reduced",
    point.args = list(
      alpha = 0.3,
      shape = 21
    ),
    point.args.pos = rlang::list2(
      position = position_jitter(
        width = 0.1,
        height = 0.1
      )
    ),
    boxplot.args = list(
      outlier.shape = NA,
      color = "black",
      alpha = .7
    ),
    boxplot.args.pos = list(
      position = ggpp::position_dodgenudge(
        x = .25,
        width = 0.2
      ), 
      width = 0.15
    ),
    violin.args = list(
      alpha = .2
    ),
    violin.args.pos = list(
      position = ggpp::position_dodgenudge(
        x = .4,
        width = 0
      ), 
      width = 1.2
    ),
  ) +
  labs(
    y = "Difference from baseline (IoD total score)",
    x = "Timepoint",
    color = "Treatment"
  )

plt <- add_gg_theme(plt)

ggsave(
  here::here("_figures/fig-iod-rainplot-difference-from-baseline.png"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8,
)

ggsave(
  here::here("_figures/fig-iod-rainplot-difference-from-baseline.pdf"),
  plot = plt,
  dpi = 300,
  width = 16,
  height = 8
)


# ############
# SECONDARY OUTCOMES
# ############

dvs <- c(
  "cvtrq_calc_total",
  "rcq_calc_total",
  "fsozu_calc_total",
  "ors_calc_total",
  "ucla_calc_total",
  "bis_calc_total",
  "cusi_calc_total",
  "ders_calc_imp",
  "narq_calc_ris",
  "spsi_calc_total",
  "kvm_score",
  "esiq_calc_total",
  "soi_total_score",
  "ssik_calc_total",
  "ekk_calc_total",
  "hbi_calc_total"
)

labels <- c(
  "CVTRQ (total score)",
  "RCQ (total score)",
  "F-Soz-U (total score)",
  "OQMPR (total score)",
  "UCLA (total score)",
  "BIS-15 (total score)",
  "CUSI (total score)",
  "DERS (subscale impulsivity)",
  "NARQ (subscale externalizing strategies)",
  "SPSI-R (total score)",
  "BMS (total score)",
  "ESIQ (total score)",
  "SOI (Item 2a)",
  "SSIC (total score)",
  "EKK-R (total score)",
  "HBI-19 (total score)"
)


dv_map <- tibble::tibble(
  var   = dvs,
  label = labels,
  var_nice = gsub("_", "-", dvs)
)

plot_and_save_dv <- function(var, label, var_nice, data) {

  plt <- ggplot(
    data,
    aes(
      x = timepoint,
      y = .data[[var]],
      fill = treatment,
      color = treatment
    )
  ) +
    geom_rain(
      seed = 42,
      alpha = .5,
      rain.side = "f2x2",
      point.args = list(alpha = 0.3, shape = 21),
      boxplot.args = list(
        outlier.shape = NA,
        color = "black",
        alpha = .7
      ),
      violin.args = list(alpha = .2)
    ) +
    labs(
      y = label,
      x = "Timepoint",
      color = "Treatment"
    )

  plt <- add_gg_theme(plt)

  name_png <- glue::glue("_figures/fig-sec-rainplot-{var_nice}.png")
  name_pdf <- glue::glue("_figures/fig-sec-rainplot-{var_nice}.pdf")

  ggsave(
    here::here(name_png),
    plot = plt,
    dpi = 300,
    width = 10.1,
    height = 10.1
  )

  ggsave(
    here::here(name_pdf),
    plot = plt,
    dpi = 300,
    width = 10.1,
    height = 10.1
  )

  invisible(plt)
}

for (i in seq_len(nrow(dv_map))) {
  plot_and_save_dv(
    var      = dv_map$var[i],
    label    = dv_map$label[i],
    var_nice = dv_map$var_nice[i],
    data     = dat_complete_sec
  )
}