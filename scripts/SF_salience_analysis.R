# ==============================================================================
# SHRIMP INDUSTRY SALIENCE ANALYSIS SCRIPT
# Metrics: DSI = Linear Sum (S + I + R) & Prevalence (% of documents present)
# ==============================================================================

# 1. LOAD PACKAGES & DATA
library(tidyverse)

# Load data (adjust file path as necessary)
df_raw <- read.csv("data/Salience scores for all interviews2.csv", encoding = "latin1")

# Clean up column spaces and add Macro-Region classifications
df_clean <- df_raw %>%
  select(Document_ID, Document_Type, Cohort, State, Theme_Category, Parent_Code, S, I, R, DSI) %>%
  mutate(
    # Clean leading/trailing spaces from text columns
    Parent_Code = trimws(Parent_Code),
    Cohort = trimws(Cohort),
    Document_Type = trimws(Document_Type),
    State = trimws(State),
    Theme_Category = trimws(Theme_Category),
    # Ensure scores are numeric
    S = as.numeric(S),
    I = as.numeric(I),
    R = as.numeric(R),
    DSI = as.numeric(DSI),
    # Map Macro-Regions
    Macro_Region = case_when(
      State %in% c("AL", "FL", "LA", "MS", "TX") ~ "Gulf",
      State %in% c("GA", "NC", "SC") ~ "South Atlantic",
      TRUE ~ "Unknown"
    )
  )

# ==============================================================================
# PHASE 1: MACRO ANALYSIS (Supply Chain Transcripts Only, No Experts)
# ==============================================================================
cat("\n--- RUNNING PHASE 1: MACRO ANALYSIS ---\n")

macro_data <- df_clean %>%
  filter(Document_Type == "Transcript" & Cohort != "Expert") %>%
  group_by(Theme_Category, Parent_Code) %>%
  summarize(
    Mean_DSI = mean(DSI, na.rm = TRUE),
    Prevalence_Pct = mean(DSI > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(Theme_Category, desc(Mean_DSI))

print(macro_data)

# Visualization: Macro Trends
p1 <- ggplot(macro_data, aes(x = reorder(Parent_Code, Mean_DSI), y = Mean_DSI, fill = Theme_Category)) +
  geom_bar(stat = "identity", alpha = 0.85, width = 0.75) +
  coord_flip() +
  facet_wrap(~Theme_Category, scales = "free_y", ncol = 1) +
  labs(
    title = "Macro Analysis: Industry-Wide Theme Salience",
    subtitle = "Verbatim Transcripts Only (Excludes Experts)",
    x = NULL,
    y = "Mean DSI Score (Linear Sum: S + I + R)"
  ) +
  theme_minimal(base_size = 14) + # Scaled up base size
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(color = "dimgray", size = 13, margin = margin(b = 10)),
    axis.text.y = element_text(face = "bold", color = "black", size = 12), # Prominent labels
    axis.text.x = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 14, hjust = 0),
    panel.grid.minor = element_blank()
  )

ggsave("plots/01_macro_industry_trends.png", plot = p1, width = 9, height = 8, dpi = 300)

# ==============================================================================
# PHASE 2: COHORT ANALYSIS (Supply Chain Transcripts Only)
# ==============================================================================
cat("\n--- RUNNING PHASE 2: COHORT ANALYSIS ---\n")

# Re-calculate cohort data but append global prevalence to the row labels
cohort_summary_with_prev <- df_clean %>%
  filter(Document_Type == "Transcript" & Cohort != "Expert") %>%
  # Group globally first to calculate prevalence percentages
  group_by(Parent_Code) %>%
  mutate(Global_Prev = round(mean(DSI > 0, na.rm = TRUE) * 100)) %>%
  ungroup() %>%
  # Create a clean label combining name and percentage
  mutate(Label_With_Prev = paste0(Parent_Code, " (", Global_Prev, "%)")) %>%
  # Now group by cohort for the final scores
  group_by(Cohort, Theme_Category, Label_With_Prev) %>%
  summarize(Mean_DSI = mean(DSI, na.rm = TRUE), .groups = "drop")

p2_updated <- ggplot(cohort_summary_with_prev, aes(x = Mean_DSI, y = reorder(Label_With_Prev, Mean_DSI))) +
  geom_line(aes(group = Label_With_Prev), color = "gray85", size = 1) +
  geom_point(aes(color = Cohort), size = 5.5, alpha = 0.9) + # Slightly larger dots to balance text
  facet_wrap(~Theme_Category, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Harvester" = "#0284c7", "Processor" = "#7c3aed", "Dealer/Dock" = "#f59e0b")) +
  labs(
    title = "Industry Priorities: Who Cares Most About What?",
    subtitle = "Comparing average theme salience scores. Percentages (%) show how many stakeholders raised the topic.",
    x = "Community Salience Score (0 = Low Priority, 9 = High Priority)",
    y = NULL,
    color = "Supply Chain Role:"
  ) +
  theme_minimal(base_size = 15) + # Scaled up base size
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(color = "dimgray", size = 13, margin = margin(b = 12)),
    strip.text = element_text(face = "bold", size = 15, color = "black"),
    axis.text.y = element_text(face = "bold", color = "gray10", size = 12), # Bold y-axis labels
    axis.text.x = element_text(size = 12, face = "bold"),
    legend.position = "top",
    legend.text = element_text(face = "bold", size = 13),
    legend.title = element_text(face = "bold", size = 13),
    panel.grid.major.x = element_line(color = "gray92", linetype = "dashed"),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2.5, "lines")
  )

ggsave("plots/02_cohort_priority_track.png", plot = p2_updated, width = 13, height = 7, dpi = 300)

# ==============================================================================
# PHASE 3: EXPERT CONCERNS AND IDEAS (Expert Notes Only)
# ==============================================================================
cat("\n--- RUNNING PHASE 3: EXPERT ANALYSIS ---\n")

expert_data <- df_clean %>%
  filter(Cohort == "Expert") %>%
  group_by(Theme_Category, Parent_Code) %>%
  summarize(
    Mean_DSI = mean(DSI, na.rm = TRUE),
    Prevalence_Pct = mean(DSI > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(Theme_Category, desc(Mean_DSI))

print(expert_data)

# Visualization: Expert Profile
p3 <- ggplot(expert_data, aes(x = reorder(Parent_Code, Mean_DSI), y = Mean_DSI, fill = Theme_Category)) +
  geom_bar(stat = "identity", fill = "#475569", alpha = 0.85, width = 0.75) +
  coord_flip() +
  facet_wrap(~Theme_Category, scales = "free_y", ncol = 1) +
  labs(
    title = "Expert Perspectives: Salience and Priorities",
    subtitle = "Based on Briefings & Macro-Level Policy Notes from Regional Experts",
    x = NULL,
    y = "Mean DSI Score (Linear Sum: S + I + R)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(color = "dimgray", size = 13, margin = margin(b = 10)),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    axis.text.x = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 14, hjust = 0),
    panel.grid.minor = element_blank()
  )

ggsave("plots/03_expert_priorities_profile.png", plot = p3, width = 9, height = 8, dpi = 300)

# ==============================================================================
# PHASE 4: REGIONAL TRENDS BY COHORT (Gulf vs. South Atlantic)
# ==============================================================================
cat("\n--- RUNNING PHASE 4: REGIONAL TRENDS BY COHORT ---\n")

# ==============================================================================
# PLOT 4: REGIONAL TRENDS DUMBBELL PLOT (Harvesters & Docks, No Processors)
# Incorporates clear prevalence rates directly into public-facing labels.
# ==============================================================================

# 1. Calculate global theme prevalence to ensure unique strings per row
regional_summary_with_prev <- df_clean %>%
  filter(Document_Type == "Transcript" & !Cohort %in% c("Expert", "Processor")) %>%
  # Group globally by theme to get one universal percentage per row
  group_by(Parent_Code) %>%
  mutate(Global_Prev = round(mean(DSI > 0, na.rm = TRUE) * 100)) %>%
  ungroup() %>%
  # Combine name and percentage into a single static string
  mutate(Label_With_Prev = paste0(Parent_Code, " (", Global_Prev, "%)")) %>%
  # Group by cohort and region for the final dumbbell points
  group_by(Cohort, Theme_Category, Macro_Region, Label_With_Prev) %>%
  summarize(Mean_DSI = mean(DSI, na.rm = TRUE), .groups = "drop")

# 2. Generate the single-row dumbbell matrix
p4_updated <- ggplot(regional_summary_with_prev) +
  geom_line(aes(x = Mean_DSI, y = reorder(Label_With_Prev, Mean_DSI), group = Label_With_Prev), 
            color = "gray85", size = 1.2) +
  geom_point(aes(x = Mean_DSI, y = Label_With_Prev, color = Macro_Region), size = 5) + # Larger dots
  facet_grid(Theme_Category ~ Cohort, scales = "free_y", space = "free_y") +
  scale_color_manual(values = c("Gulf" = "#1e3a8a", "South Atlantic" = "#ea580c")) +
  labs(
    title = "The Regional Gap: Gulf Coast vs. South Atlantic",
    subtitle = "Comparing average issue salience. Percentages (%) indicate industry-wide topic prevalence.",
    x = "Community Salience Score (0 = Low Priority, 9 = High Priority)",
    y = NULL,
    color = "Region:"
  ) +
  theme_bw(base_size = 15) + # Scaled up base size
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(color = "dimgray", size = 13, margin = margin(b = 12)),
    strip.text = element_text(face = "bold", size = 13, color = "white"),
    strip.background = element_rect(fill = "#27272a"),
    axis.text.y = element_text(face = "bold", color = "gray10", size = 12), # High readability theme tracking
    axis.text.x = element_text(size = 12, face = "bold"),
    legend.position = "top",
    legend.text = element_text(face = "bold", size = 13),
    legend.title = element_text(face = "bold", size = 13),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines")
  )

ggsave("plots/04_regional_gap_matrix.png", plot = p4_updated, width = 13, height = 8, dpi = 300)

# ==============================================================================
# END OF SCRIPT 
# ==============================================================================