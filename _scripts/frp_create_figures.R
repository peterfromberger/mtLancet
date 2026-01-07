library(ggemmeans)

# CARES total score

# Predicted values of MMRM


fig_CARES_total_score_emmeans <- ggemmeans(fit2b, terms = c("timepoint", "treatment")) %>% plot(show_data = FALSE)