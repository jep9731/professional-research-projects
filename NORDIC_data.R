# Import library ----------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(readr)

# Read excel files ------------------------------------------------------------
process_workbook <- function(file_path, method_name) {
  wb <- loadWorkbook(file_path)
  
  # Initialize list for group data
  group_data <- list()
  
  for (sheet_name in wb$sheet_names) {
    # Read and process sheet data
    sheet_data <- read.xlsx(wb, sheet = sheet_name) %>%
      # Filter out Mean/Stdev rows
      filter(!apply(., 1, function(row) {
        any(grepl("mean|stdev", as.character(row), ignore.case = TRUE))
      })) %>%
      # Convert factors to characters
      mutate(across(where(is.factor), as.character)) %>%
      # Clean Subject IDs
      mutate(
        Subject = str_extract(Subject, "\\w{1,2}\\d{3}[a-z]"),
        Subject = str_replace(Subject, "[a-z]$", "")
      ) %>%
      # Add metadata
      mutate(
        Group = gsub("_(LH|RH)$", "", sheet_name),
        Hemisphere = ifelse(grepl("_LH$", sheet_name), "LH", "RH"),
        Method = method_name
      )
    
    # Only keep our target groups
    if (sheet_data$Group[1] %in% c("CON", "PPA_S", "PPA_G", "PPA_L")) {
      group <- sheet_data$Group[1]
      group_data[[group]] <- if (group %in% names(group_data)) {
        bind_rows(group_data[[group]], sheet_data)
      } else {
        sheet_data
      }
    }
  }
  
  return(group_data)
}

# Process both files
orig_data <- process_workbook("ORIG_Raw_data.xlsx", "ORIG")
nordic_data <- process_workbook("NORDIC_Raw_data.xlsx", "NORDIC")

# Combine methods for each group
combined_groups <- list()

for (group in c("CON", "PPA_S", "PPA_G", "PPA_L")) {
  combined_groups[[group]] <- bind_rows(
    orig_data[[group]],
    nordic_data[[group]]
  )
}

# Create separate dataframes for each group (with both methods)
list2env(combined_groups, envir = .GlobalEnv)

# Export files to excel ---------------------------
write_xlsx(CON, "CON_data.xlsx")
write_xlsx(PPA_G, "PPA-G_data.xlsx")
write_xlsx(PPA_L, "PPA-L_data.xlsx")
write_xlsx(PPA_S, "PPA-S_data.xlsx")

# RMA ---------------------------------------------------------------------
library(rstatix)

# 1. First prepare your data in long format
combined_data <- bind_rows(
  CON %>% mutate(Group = "CON"),
  PPA_S %>% mutate(Group = "PPA_S"),
  PPA_G %>% mutate(Group = "PPA_G"), 
  PPA_L %>% mutate(Group = "PPA_L")
) %>%
  pivot_longer(
    cols = c(IFG_MTG, IFG_ATL, MTG_ATL),
    names_to = "Connection",
    values_to = "Value"
  )

# Find remove duplicates
combined_data <- combined_data %>%
  group_by(Subject, Method, Connection, Group) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

# 2. Run RM-ANOVA for each group and connection
results <- combined_data %>%
  group_by(Group, Connection) %>%
  do({
    data <- .
    # Check if both methods exist
    if (length(unique(data$Method)) > 1) {
      anova_test(
        data = data,
        dv = Value,
        wid = Subject,
        within = Method,
        effect.size = "pes"
      ) %>% 
        as_tibble() %>%
        mutate(Group = unique(data$Group),
               Connection = unique(data$Connection))
    } else {
      tibble(Group = unique(data$Group),
             Connection = unique(data$Connection),
             note = "Not enough methods to compare")
    }
  })

# 3. View results
results %>%
  select(Group, Connection, Effect, DFn, DFd, F, p, pes) %>%
  arrange(Group, Connection)

# 4. Pairwise comparisons (if ANOVA is significant)
pairwise_results <- combined_data %>%
  group_by(Group, Connection) %>%
  pairwise_t_test(
    Value ~ Method,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  )

# 5. Visualization
plot <- ggplot(combined_data, aes(x = Method, y = Value, fill = Method)) +
  geom_boxplot() +
  facet_grid(Connection ~ Group) +
  labs(title = "Method Comparison by Group and Connection") +
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
plot

# Export results ----------------------------------------------------------
# ANOVA results
results %>%
  select(Group, Connection, Effect, DFn, DFd, F, p, pes) %>%
  arrange(Group, Connection) %>%
  write_csv("anova_results.csv")

# Pairwise results
pairwise_results %>%
  select(Group, Connection, group1, group2, p, p.adj, p.adj.signif) %>%
  write_csv("pairwise_results_cleaned.csv")

# Plot
ggsave("method_comparison_plot.png", plot = plot, width = 10, height = 6)
