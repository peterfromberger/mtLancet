# -----------------------
# all this tables are helpers
# to create the flowchart in a
# correct manner
# - missing data is shown
# - correct client_ids
# - correct Ns
# - the flowchart itself was made with drawio
# ---------------


library(dplyr)
library(tidyr)
library(gt)
library(gtsummary)
library(purrr)

# create table with n, n dropouts and client ids...
create_client_summary_table <- function(df) {
  # Ensure proper factor levels for timepoint and treatment
  df <- df %>%
    mutate(
      timepoint = factor(timepoint, levels = levels(df$timepoint)),
      treatment = factor(treatment, levels = levels(df$treatment))
    )
  
  # Get all unique combinations of timepoint and treatment
  all_combinations <- expand.grid(
    timepoint = levels(df$timepoint),
    treatment = levels(df$treatment)
  ) %>%
    as_tibble() %>%
    arrange(timepoint, treatment)
  
  # Count clients with data points (where IoD_reduced is not NA) for each combination
  # Fix: Count distinct client_id for each timepoint-treatment combination
  clients_with_data <- df %>%
    filter(!is.na(IoD_reduced)) %>%  # Only consider rows where IoD_reduced has data
    group_by(timepoint, treatment) %>%
    summarise(
      clients_with_data = n_distinct(client_id),
      .groups = "drop"
    )
  
  # Join to get all combinations
  result <- all_combinations %>%
    left_join(clients_with_data, by = c("timepoint", "treatment")) %>%
    mutate(clients_with_data = replace_na(clients_with_data, 0))
  
  # Add client IDs with data (where IoD_reduced is not NA)
  client_ids_with_data <- df %>%
    filter(!is.na(IoD_reduced)) %>%  # Only consider rows where IoD_reduced has data
    group_by(timepoint, treatment) %>%
    summarise(
      client_ids_with_data = paste(unique(client_id), collapse = ", "),
      .groups = "drop"
    )
  
  result <- result %>%
    left_join(client_ids_with_data, by = c("timepoint", "treatment"))
  
  # Calculate clients who miss data at current timepoint but have it at next timepoint
  result <- result %>%
    mutate(
      clients_missing_then_having = map2(timepoint, treatment, function(tp, tr) {
        # Get all clients for this specific timepoint and treatment
        clients_at_timepoint <- df %>%
          filter(timepoint == tp & treatment == tr & !is.na(IoD_reduced)) %>%
          distinct(client_id) %>%
          pull(client_id)
        
        # Get all clients for next timepoint with same treatment
        timepoint_order <- levels(df$timepoint)
        current_idx <- which(timepoint_order == tp)
        if(current_idx < length(timepoint_order)) {
          next_tp <- timepoint_order[current_idx + 1]
          clients_next_timepoint <- df %>%
            filter(timepoint == next_tp & treatment == tr & !is.na(IoD_reduced)) %>%
            distinct(client_id) %>%
            pull(client_id)
          
          # Clients who were missing at current but present at next
          clients_missing_then_having <- setdiff(unique(df$client_id), clients_at_timepoint)
          clients_missing_then_having <- intersect(clients_missing_then_having, clients_next_timepoint)
          length(clients_missing_then_having)
        } else {
          0
        }
      }) %>% unlist(),
      
      client_ids_missing_then_having = map2(timepoint, treatment, function(tp, tr) {
        # Get all clients for this specific timepoint and treatment
        clients_at_timepoint <- df %>%
          filter(timepoint == tp & treatment == tr & !is.na(IoD_reduced)) %>%
          distinct(client_id) %>%
          pull(client_id)
        
        # Get all clients for next timepoint with same treatment
        timepoint_order <- levels(df$timepoint)
        current_idx <- which(timepoint_order == tp)
        if(current_idx < length(timepoint_order)) {
          next_tp <- timepoint_order[current_idx + 1]
          clients_next_timepoint <- df %>%
            filter(timepoint == next_tp & treatment == tr & !is.na(IoD_reduced)) %>%
            distinct(client_id) %>%
            pull(client_id)
          
          # Clients who were missing at current but present at next
          clients_missing_then_having <- setdiff(unique(df$client_id), clients_at_timepoint)
          clients_missing_then_having <- intersect(clients_missing_then_having, clients_next_timepoint)
          paste(clients_missing_then_having, collapse = ", ")
        } else {
          ""
        }
      }) %>% unlist()
    )
  
  # Calculate dropout values for each timepoint (excluding the last one)
  # For each timepoint, we want to know: how many clients were present in the previous timepoint 
  # but not in the current timepoint (and not in the "missing then having" group)
  result <- result %>%
    mutate(
      temp_clients_dropout = map2(timepoint, treatment, function(tp, tr) {
        # Get all clients for this specific timepoint and treatment
        clients_at_timepoint <- df %>%
          filter(timepoint == tp & treatment == tr & !is.na(IoD_reduced)) %>%
          distinct(client_id) %>%
          pull(client_id)
        
        # Get all clients for previous timepoint with same treatment
        timepoint_order <- levels(df$timepoint)
        current_idx <- which(timepoint_order == tp)
        if(current_idx > 1) {
          prev_tp <- timepoint_order[current_idx - 1]
          clients_prev_timepoint <- df %>%
            filter(timepoint == prev_tp & treatment == tr & !is.na(IoD_reduced)) %>%
            distinct(client_id) %>%
            pull(client_id)
          
          # Clients who were in previous timepoint but not in current timepoint
          clients_dropout <- setdiff(clients_prev_timepoint, clients_at_timepoint)
          
          # Exclude clients who are in clients_missing_then_having
          missing_then_having_ids <- strsplit(client_ids_missing_then_having[which(result$timepoint == tp & result$treatment == tr)], ", ") %>% 
            unlist() %>% 
            trimws() %>% 
            na.omit()
          
          clients_dropout <- setdiff(clients_dropout, missing_then_having_ids)
          paste(clients_dropout, collapse = ", ")
        } else {
          # No previous timepoint - no dropout possible
          ""
        }
      }) %>% unlist()
    )
  
  # Now process the temp column to remove duplicates across rows
  # We'll do this in a separate step to avoid reference issues
  result <- result %>%
    mutate(
      # Initialize the final dropout columns
      clients_dropout = rep(0, nrow(result)),
      client_ids_dropout = rep("", nrow(result))
    )
  
  # Process each row individually to ensure no duplicate client IDs across rows
  for(i in 1:nrow(result)) {
    tp <- result$timepoint[i]
    tr <- result$treatment[i]
    
    # Skip calculation for the last timepoint (no next timepoint)
    timepoint_order <- levels(df$timepoint)
    current_idx <- which(timepoint_order == tp)
    # if(current_idx == length(timepoint_order)) {
    #   # Last timepoint - no dropout possible
    #   result$clients_dropout[i] <- 0
    #   result$client_ids_dropout[i] <- ""
    # } else {
      # Not last timepoint - calculate dropout
      # Get the temporary dropout IDs for this row
    temp_dropout_ids <- strsplit(result$temp_clients_dropout[i], ",") %>% 
      unlist() %>% 
      trimws() %>% 
      na.omit()
    
    # Get all previously processed dropout IDs for same treatment (only up to current row)
    previous_dropout_ids <- result %>%
      slice(1:(i-1)) %>%
      filter(treatment == tr) %>%
      pull(temp_clients_dropout) %>%
      strsplit(",") %>%
      unlist() %>%
      trimws() %>%
      na.omit()
    
    # Remove duplicates from current row
    unique_dropout_ids <- setdiff(temp_dropout_ids, previous_dropout_ids)
    
    # Update the final columns
    result$clients_dropout[i] <- length(unique_dropout_ids)
    result$client_ids_dropout[i] <- paste(unique_dropout_ids, collapse = ", ")
    #}
  }
  
  # Clean up temporary column
  result <- result %>%
    dplyr::select(-temp_clients_dropout)
  
  # Reshape data to have timepoint as rows and treatment as span headers
  # Group by timepoint and create wide format
  final_result <- result %>%
    pivot_wider(
      names_from = treatment,
      values_from = c(clients_with_data, client_ids_with_data, clients_missing_then_having, 
                     client_ids_missing_then_having, clients_dropout, client_ids_dropout),
      names_glue = "{treatment}_{.value}"
    ) %>%
    arrange(timepoint)
  
  # Create the gt table with span headers
  tbl <- final_result %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::md("Client Data Summary"),
      subtitle = gt::md("Number of clients with data points by timepoint and treatment")
    ) %>%
    # Set column labels for the span headers - corrected order
    gt::cols_label(
      timepoint = "Timepoint",
      Intervention_clients_with_data = "Intervention - Clients with Data",
      Placebo_clients_with_data = "Placebo - Clients with Data",
      Intervention_client_ids_with_data = "Intervention - Client IDs with Data",
      Placebo_client_ids_with_data = "Placebo - Client IDs with Data",
      Intervention_clients_missing_then_having = "Intervention - Missing Then Having",
      Placebo_clients_missing_then_having = "Placebo - Missing Then Having",
      Intervention_client_ids_missing_then_having = "Intervention - Client IDs Missing Then Having",
      Placebo_client_ids_missing_then_having = "Placebo - Client IDs Missing Then Having",
      Intervention_clients_dropout = "Intervention - Dropout",
      Placebo_clients_dropout = "Placebo - Dropout",
      Intervention_client_ids_dropout = "Intervention - Client IDs Dropout",
      Placebo_client_ids_dropout = "Placebo - Client IDs Dropout"
    ) %>%
    # Add span headers
    gt::tab_spanner(
      label = "Clients with Data",
      columns = c(Intervention_clients_with_data, Placebo_clients_with_data)
    ) %>%
    gt::tab_spanner(
      label = "Client IDs with Data",
      columns = c(Intervention_client_ids_with_data, Placebo_client_ids_with_data)
    ) %>%
    gt::tab_spanner(
      label = "Missing Then Having",
      columns = c(Intervention_clients_missing_then_having, Placebo_clients_missing_then_having)
    ) %>%
    gt::tab_spanner(
      label = "Client IDs Missing Then Having",
      columns = c(Intervention_client_ids_missing_then_having, Placebo_client_ids_missing_then_having)
    ) %>%
    gt::tab_spanner(
      label = "Dropout",
      columns = c(Intervention_clients_dropout, Placebo_clients_dropout)
    ) %>%
    gt::tab_spanner(
      label = "Client IDs Dropout",
      columns = c(Intervention_client_ids_dropout, Placebo_client_ids_dropout)
    )
  
  return (tbl)
}

tbl_n_dropouts_ids <- create_client_summary_table(dat_clean %>% filter(!client_id %in% c(253, 396) & !is.na(client_group)) %>% dplyr::select(client_id, treatment, timepoint, IoD_reduced))




# exclusion reasons by timepoint and treatment
get_exclusion_reason_timepointxtreatment <- function() {

  dat_exclusion_2 <- q_exclusion %>%
    right_join(matching) %>%
    left_join((randdat %>% dplyr::select(ID, treatment, `4.2 Bundesland:`))) %>%
    filter(!is.na(treatment)) %>%
    mutate(
      Category = `Welche Gründe (Bitte Auflistung mittels KOMMA trennen)?` %>% sapply(categorize_reason) %>% factor(),
      `recidivism` = if_else(`Klient hat eine Straftat gemäß §§ 176, 176a oder 176b StGB begangen`=="Yes" | `Klient hat eine Straftat gemäß § 184b StGB begangen` == "Yes" |
                                                      (`Verstoß gegen Weisungen`=="Yes" & `Gegen welche Weisungen wurde verstoßen (Bitte Auflistung mittels KOMMA trennen)?` %in% c("erneute Straftaten", "Kontakt zu Kindern unter 16 Jahren", "Kontaktaufnahme zum Kind am Sportplatz ( darf sich nicht an Plätzen wie Sportstätten etc. aufhalten)", "Sich nicht an Plätzen aufzuhalten, die üblicherweise von Kindern und Jugendlichen frequentiert werden, keinerlei Tätigkeiten auszuüben, die im Zusammenhang mit der Betreuung von Kindern und Jugendlichen stehen.") & `Kam es zu einer Verurteilung?`=="Yes"), "Yes", "No"),
      `evidence_for_recidivism` = if_else(`Klient hat eine Straftat gemäß §§ 176, 176a oder 176b StGB begangen`=="Yes" | `Klient hat eine Straftat gemäß § 184b StGB begangen` == "Yes" |
                                    (`Verstoß gegen Weisungen`=="Yes" & `Gegen welche Weisungen wurde verstoßen (Bitte Auflistung mittels KOMMA trennen)?` %in% c("erneute Straftaten", "Kontakt zu Kindern unter 16 Jahren", "Kontaktaufnahme zum Kind am Sportplatz ( darf sich nicht an Plätzen wie Sportstätten etc. aufhalten)", "Sich nicht an Plätzen aufzuhalten, die üblicherweise von Kindern und Jugendlichen frequentiert werden, keinerlei Tätigkeiten auszuüben, die im Zusammenhang mit der Betreuung von Kindern und Jugendlichen stehen.") & `Kam es zu einer Verurteilung?`=="Yes") | `Welche Gründe (Bitte Auflistung mittels KOMMA trennen)?` %in% c("Handy seit mehreren Monaten bei der Polizei, weitere Straftaten unklar, arbeitet nicht mit", "Aufgrund neuer Strafanzeigen in U-Haft, Gerichtsverhandlung noch ausstehend", "Anordnung Untersuchungshaft"#, "Schwebendes Verfahren (bereits vor Aufnahme) ist abgeurteilt (Straftat nach 184b)"
                                  ), "Yes", "No"),
      status = ifelse(excluded_at >= as.Date('2024-10-02') & status == 7,9,status)
    ) %>%
    mutate(
      client_status = factor(as.numeric(status),
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                  labels = c('active - logged in', 'Aktive Teilnahme (Einführung abgeschlossen)', 'Aktive Teilnahme am Programm', 'Inhalt abgeschlossen', 'Programm abgeschlossen', 'Ausschluss beantragt', 'Ausgeschlossen', 'In Datenbank registriert', 'Klinischer Trial beendet vor Abschluss'))
    )

  dat_excl <- dat_exclusion_2 %>%
    filter(
      client_status == "Ausgeschlossen" | client_status == 'Klinischer Trial beendet vor Abschluss'
    ) %>%
    mutate(
      end_of_trial = factor(
        ifelse(
          excluded_at >= "2024-10-02",
          "Yes",
          "No"
        ),
        levels = c("Yes", "No")
      ),
      mixed_offense = factor(
        ifelse(`Klient hat eine Straftat gemäß § 184b StGB begangen` == "Yes" & `Klient hat eine Straftat gemäß §§ 176, 176a oder 176b StGB begangen` == "Yes", "Yes", "No"),
        levels = c("Yes", "No")
      )
    ) %>%
    mutate(
      csem_offense_only = case_when(
          `Klient hat eine Straftat gemäß § 184b StGB begangen` == "Yes" & mixed_offense == "Yes" ~ "No",
          `Klient hat eine Straftat gemäß § 184b StGB begangen` == "Yes" & mixed_offense == "No" ~ "Yes",
          `Klient hat eine Straftat gemäß § 184b StGB begangen` == "No" ~ "No"
        ),
      csa_offense_only = case_when(
          `Klient hat eine Straftat gemäß §§ 176, 176a oder 176b StGB begangen` == "Yes" & mixed_offense == "Yes" ~ "No",
          `Klient hat eine Straftat gemäß §§ 176, 176a oder 176b StGB begangen` == "Yes" & mixed_offense == "No" ~ "Yes",
          `Klient hat eine Straftat gemäß §§ 176, 176a oder 176b StGB begangen` == "No" ~ "No"
        ),
      violation_probation_conditions = factor(ifelse(
        `Verstoß gegen Weisungen` == "Yes" | `Verstoß gegen Auflagen` == "Yes",
        "Yes",
        "No"),
        levels = c("Yes", "No")
      )
    ) %>%
    dplyr::select(
      client_id,
      treatment,
      end_of_trial,
      recidivism,
      `Widerruf der Einverständniserklärung`,
      `Die Zeit der Bewährungsaufsicht/Führungsaufsicht ist abgelaufen`,
      `Klient hat eine andere Straftat begangen (nicht gemäß §§ 176, 176a, 176b oder 184b StGB)`,
      `akute Selbstgefährdung des Klienten (z.B. konkrete Suizidplanung)`,
      `andere Gründe als die bisher genannten Gründe`,
      mixed_offense,
      csem_offense_only,
      csa_offense_only,
      violation_probation_conditions
    ) %>%
    rename(
      "Withdrawal of informed consent" = `Widerruf der Einverständniserklärung`,
      "Probationary supervision expired" = `Die Zeit der Bewährungsaufsicht/Führungsaufsicht ist abgelaufen`,
      "Other offense (not CSA or CSEM)" = `Klient hat eine andere Straftat begangen (nicht gemäß §§ 176, 176a, 176b oder 184b StGB)`,
      "Acute suicide risk" = `akute Selbstgefährdung des Klienten (z.B. konkrete Suizidplanung)`,
      "Other reasons" = `andere Gründe als die bisher genannten Gründe`,
      "End of RCT" = end_of_trial,
      "CSA and CSEM offense" = mixed_offense,
      "CSEM offense" = csem_offense_only,
      "CSA offense" = csa_offense_only,
      "Violation of probation conditions" = violation_probation_conditions
    ) %>%
    # we reduce number of multiple answers by logical thinking: einige Kombinationen bieten keinen zusätzlichen iNfogehalt und sind logische Konsequenz des eigentlichen Grundes
    # withdrawal of informed consent ist dann nicht relevant, wenn mit offense, Probationary supervision expired, kombiniert, da offense einen ausschluss automatisch nach sich zieht
    mutate(
      `Withdrawal of informed consent` = case_when(
        `Withdrawal of informed consent` == "Yes" & `CSA and CSEM offense` == "Yes" ~ "No",
        `Withdrawal of informed consent` == "Yes" & `CSA offense` == "Yes" ~ "No",
        `Withdrawal of informed consent` == "Yes" & `CSEM offense` == "Yes" ~ "No",
        `Withdrawal of informed consent` == "Yes" & `Other offense (not CSA or CSEM)` == "Yes" ~ "No",
        `Withdrawal of informed consent` == "Yes" & `Violation of probation conditions` == "Yes" ~ "No",
        `Withdrawal of informed consent` == "Yes" & `Probationary supervision expired` == "Yes" ~ "No",
        TRUE ~ `Withdrawal of informed consent`
      ),
      # withdrawal of informed consent ist dann nicht relevant, wenn mit offense kombiniert, da offense automatisch einen Verstoß gegen probation conditions bedeutet
      `Violation of probation conditions` = case_when(
        `Violation of probation conditions` == "Yes" & `CSA and CSEM offense` == "Yes" ~ "No",
        `Violation of probation conditions` == "Yes" & `CSA offense` == "Yes" ~ "No",
        `Violation of probation conditions` == "Yes" & `CSEM offense` == "Yes" ~ "No",
        `Violation of probation conditions` == "Yes" & `Other offense (not CSA or CSEM)` == "Yes" ~ "No",
        TRUE ~ `Violation of probation conditions`
      ),
      # see above
      `Probationary supervision expired` = case_when(
        `Probationary supervision expired` == "Yes" & `CSA and CSEM offense` == "Yes" ~ "No",
        `Probationary supervision expired` == "Yes" & `CSA offense` == "Yes" ~ "No",
        `Probationary supervision expired` == "Yes" & `CSEM offense` == "Yes" ~ "No",
        `Probationary supervision expired` == "Yes" & `Other offense (not CSA or CSEM)` == "Yes" ~ "No",
        `Probationary supervision expired` == "Yes" & `End of RCT` == "Yes" ~ "No",
        TRUE ~ `Probationary supervision expired`
      )

    )

  # all NAs are in the sense of "No"
  dat_excl[is.na(dat_excl)] <- "No"

  dat_excl <- dat_excl %>%
    filter(!client_id %in% c(253, 396)) %>%
    mutate(
      exclusion_reason_count = rowSums(across(c(
        `Withdrawal of informed consent`,
        `Probationary supervision expired`,
        `Violation of probation conditions`,
        `Other offense (not CSA or CSEM)`,
        `CSA and CSEM offense`,
        `CSEM offense`,
        `CSA offense`,
        `Acute suicide risk`,
        `Other reasons`,
        `End of RCT`), 
        ~ . == "Yes")),
      exclusion_reasons_list = apply(dplyr::select(., 
        `Withdrawal of informed consent`,
        `Probationary supervision expired`,
        `Violation of probation conditions`,
        `CSA and CSEM offense`,
        `CSEM offense`,
        `CSA offense`,
        `Other offense (not CSA or CSEM)`,
        `Acute suicide risk`,
        `Other reasons`,
        `End of RCT`),
        1, 
        function(row) {
          yes_cols <- names(row[row == "Yes"])
          paste(yes_cols, collapse = ", ")
        })
    )

    return(dat_excl)
}

tmp <- get_exclusion_reason_timepointxtreatment()



m1_dropout_intervention_ids <- c(346)
m1_dropout_placebo_ids <- c(234, 404, 424, 430, 515, 533, 555)

m2_dropout_intervention_ids <- c(201, 266, 285, 289, 293, 343, 373, 381, 401, 421, 423, 434, 450, 456, 470, 472, 488, 491, 493, 501, 517)
m2_dropout_placebo_ids <- c(186, 336, 344, 367, 372, 399, 411, 460, 476, 500, 516, 534, 550, 554)

m3_dropout_intervention_ids <- c(204, 226, 231, 263, 326, 332, 341, 342, 348, 400, 403, 413, 442, 468, 504)
m3_dropout_placebo_ids <- c(184, 185, 208, 210, 222, 276, 281, 284, 327, 334, 394, 474, 483, 521, 535)

m4_dropout_intervention_ids <- c(220, 264, 287, 301, 375, 406, 453, 508, 510)
m4_dropout_placebo_ids <- c(252, 279, 300, 362, 467, 503, 549, 551)

m5_dropout_intervention_ids <- c(217, 306, 371, 418, 487, 495, 514, 519, 526, 532, 544, 548)
m5_dropout_placebo_ids <- c(205, 236, 259, 388, 395, 479, 497, 505, 520, 523, 530, 540)

m6_dropout_intervention_ids <- c(187, 283, 304, 473, 490, 513, 518, 541)
m6_dropout_placebo_ids <- c(213, 531)

dropout_tables <- list()

# Define all dropout ID lists
dropout_lists <- list(
  m1_dropout_intervention_ids = m1_dropout_intervention_ids,
  m1_dropout_placebo_ids = m1_dropout_placebo_ids,
  m2_dropout_intervention_ids = m2_dropout_intervention_ids,
  m2_dropout_placebo_ids = m2_dropout_placebo_ids,
  m3_dropout_intervention_ids = m3_dropout_intervention_ids,
  m3_dropout_placebo_ids = m3_dropout_placebo_ids,
  m4_dropout_intervention_ids = m4_dropout_intervention_ids,
  m4_dropout_placebo_ids = m4_dropout_placebo_ids,
  m5_dropout_intervention_ids = m5_dropout_intervention_ids,
  m5_dropout_placebo_ids = m5_dropout_placebo_ids,
  m6_dropout_intervention_ids = m6_dropout_intervention_ids,
  m6_dropout_placebo_ids = m6_dropout_placebo_ids
)

# Create tables for each dropout group
for(i in seq_along(dropout_lists)) {
  group_name <- names(dropout_lists)[i]
  client_ids <- dropout_lists[[i]]
  
  # Filter data for this group
  group_data <- tmp %>%
    filter(client_id %in% client_ids) %>%
    dplyr::select(-client_id)
  
  # Create summary table
  if(nrow(group_data) > 0) {
    tbl <- group_data %>%
      tbl_summary(by = treatment)
    
    dropout_tables[[group_name]] <- tbl
  }
}

# Example usage:
# View the first dropout table
# dropout_tables$m6_dropout_placebo_ids

ids_baseline <- c(181, 182, 184, 185, 186, 191, 198, 200, 205, 208, 210, 213, 215, 219, 222, 228, 234, 235, 236, 238, 241, 242, 248, 252, 255, 258, 259, 261, 273, 275, 276, 279, 281, 284, 286, 290, 298, 300, 309, 311, 312, 313, 319, 320, 324, 327, 328, 329, 330, 334, 336, 339, 344, 352, 353, 361, 362, 367, 372, 382, 383, 388, 391, 394, 395, 399, 404, 405, 411, 414, 417, 424, 427, 429, 430, 432, 447, 448, 458, 459, 460, 464, 465, 467, 474, 476, 477, 479, 480, 483, 492, 497, 500, 503, 505, 515, 516, 520, 521, 523, 528, 529, 530, 531, 533, 534, 535, 540, 549, 550, 551, 554, 555, 187, 196, 201, 203, 204, 211, 217, 218, 220, 223, 226, 230, 231, 233, 239, 250, 256, 257, 263, 264, 266, 267, 268, 269, 282, 283, 285, 287, 288, 289, 293, 297, 301, 304, 306, 310, 315, 318, 322, 323, 326, 332, 341, 342, 343, 346, 348, 351, 365, 370, 371, 373, 375, 377, 378, 381, 384, 397, 400, 401, 403, 406, 413, 418, 421, 423, 425, 433, 434, 441, 442, 446, 449, 450, 453, 456, 461, 463, 468, 470, 472, 473, 478, 484, 486, 487, 488, 489, 490, 491, 493, 495, 501, 504, 507, 508, 510, 513, 514, 517, 518, 519, 526, 532, 539, 541, 544, 548)

excluded_before_baseline <- tmp %>%
  filter(!client_id %in% ids_baseline)

tbl_excluded_before_baseline <- excluded_before_baseline %>%
  dplyr::select(
    -client_id
  ) %>%
  tbl_summary(by = treatment)

# in tbl_excluded_before_baseline sind in placebo 2 zuviel 71 statt 69!!!!

ids_two_too_much <- excluded_before_baseline %>% filter(treatment == "Placebo")

ids_flowchart <- c(188, 195, 189, 197, 207, 212, 216, 221, 227, 232, 240, 246, 254, 262, 270,
            272, 280, 274, 291, 294, 305, 299, 307, 314, 333, 335, 345, 347, 355, 356,
            357, 359, 366, 374, 376, 379, 380, 386, 392, 393, 398, 408, 409, 410, 419,
            420, 435, 436, 437, 438, 440, 451, 454, 471, 466, 475, 481, 485, 494, 498,
            499, 506, 509, 522, 524, 527, 543, 537, 552)

# Vergleich zeigt: 412 und 511 sind im Vergleich zur Flowchart zu viel!
tbl_excluded_before_baseline <- excluded_before_baseline %>%
  filter(client_id != 412 & client_id != 511) %>%
  dplyr::select(
    -client_id
  ) %>%
  tbl_summary(by = treatment)