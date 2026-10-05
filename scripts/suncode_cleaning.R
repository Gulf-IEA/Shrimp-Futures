# cleaning the sub-codes

library(tidyverse)

# 1. Define Master Codebook Vector Verbatim
master_subcodes <- c(
  "Fuel", "Aging Vessels Equipment", "Maintenance", "Insurance", "Financing",
  "Market Dynamics", "Low prices", "Unfair Trade Practices", 
  "Lack of consumer Awareness & Demand for domestic shrimp", "Quality concerns",
  "TEDs & BRDs", "Vessel Trackers", "Other Mandatory Gear Modifications", 
  "Observer Programs", "Vessel Inspections", "Crew Composition Rules", 
  "Bureaucratic Hurdles for Relief Funds", "Certification expenses and bureaucratic hurdles",
  "Local Processing Capabilities", "Hurricane & Natural Hazard Damage", 
  "Urban Development Pressures", "Lack of Investment",
  "Demanding Nature of the work & Logistics", "Crew pay", "Visa Program Regulations", 
  "Greying of the fleet", "Alternative on-shore wage competition",
  "Shrimp size", "Shrimp distribution", "Habitat loss", "Dredging", 
  "Shark populations", "Pathogenic outbreaks", "Recruitment shifts",
  "Intra-industry cohesion", "Inter-industry cohesion",
  "Direct-to-Consumer Marketing & Sales", "Value-Added Products", 
  "National Marketing Campaigns", "Consumer Education", "Restaurant Partnerships", 
  "Restaurant Menu-labeling enforcement", "Vertical Integration", 
  "Engaging in other fisheries", "Additional non-fisheries income",
  "Fuel Subsidies", "Disaster Relief Funding", "Crop Insurance", "Infrastructure Support",
  "Fuel-efficient gear", "On-board processing and freezing",
  "Import Regulations & Inspections", "Tariffs", "COOL (country-of-origin labeling)", 
  "Review of NOAA regulations & law enforcement", "Visa Programs", 
  "Workforce Training", "Financial Planning & Literacy", "Vocational programs"
)

parent_codes <- c(
  'Costs', 'Import Competition', 'Regulatory Burdens', 'Workforce Shortages', 
  'Environmental Changes', 'Lack of Industry Cohesion', 'Marketing, quality, and partnerships', 
  'Technological Improvements', 'Regulations & Policy Change', 'Workforce Development',
  'Deteriorating Infrastructure', 'Income diversification', 'Funding / government support'
)

# 2. Load and Clean
df_clean <- read_csv("data/cleaned_salience.csv") %>%
  mutate(
    # Specific fixes for DOC_12
    Sub_Codes = case_when(
      Document_ID == "DOC_12_FL_DEALER_ALVAREZ" & Sub_Codes == "Regulatory Burdens" ~ 
        "Certification expenses and bureaucratic hurdles",
      Document_ID == "DOC_12_FL_DEALER_ALVAREZ" & Sub_Codes == "Environmental Changes" ~ 
        "Shrimp distribution",
      TRUE ~ Sub_Codes
    ),
    # Formatting fixes
    Sub_Codes = str_replace_all(Sub_Codes, "Aging Vessels Equipment Maintenance Insurance Financing", "Aging Vessels Equipment; Maintenance; Insurance; Financing"),
    Sub_Codes = str_replace_all(Sub_Codes, "Market Dynamics Low prices", "Market Dynamics; Low prices"),
    Sub_Codes = str_replace_all(Sub_Codes, "Equipment Maintenance", "Maintenance"),
    Sub_Codes = if_else(str_detect(Sub_Codes, "Aging Vessels") & !str_detect(Sub_Codes, "Equipment"), 
                        str_replace(Sub_Codes, "Aging Vessels", "Aging Vessels Equipment"), Sub_Codes),
    Sub_Codes = if_else(str_detect(Sub_Codes, "Intra-industry cohesion : :"), "Intra-industry cohesion", Sub_Codes)
  )

# 3. Function to clean individual cell strings
clean_cell <- function(x) {
  if (is.na(x)) return(NA_character_)
  items <- str_split(x, ";")[[1]] %>% str_trim()
  valid_items <- items[items != "" & tolower(items) != "none" & !(items %in% parent_codes)]
  if (length(valid_items) == 0) return(NA_character_)
  return(paste(valid_items, collapse = "; "))
}

df_clean$Sub_Codes <- sapply(df_clean$Sub_Codes, clean_cell)

# 4. Save Cleaned Dataset to CSV
write_csv(df_clean, "cleaned_salience_verbatim.csv")

message("Done! Saved as 'cleaned_salience_verbatim.csv'")



# 3. Unnest Sub-Codes & Standardize
df_exploded <- df_clean %>%
  separate_rows(Sub_Codes, sep = ";") %>%
  mutate(Sub_Code = str_trim(Sub_Codes)) %>%
  filter(!is.na(Sub_Code) & Sub_Code != "" & tolower(Sub_Code) != "none" & !(Sub_Code %in% parent_codes)) %>%
  mutate(
    Sub_Code = case_when(
      Sub_Code == "shrimp distribution" ~ "Shrimp distribution",
      Sub_Code == "COOL" ~ "COOL (country-of-origin labeling)",
      Sub_Code == "Direct-to-Consumer Marketing" ~ "Direct-to-Consumer Marketing & Sales",
      Sub_Code == "Certification expenses" ~ "Certification expenses and bureaucratic hurdles",
      Sub_Code == "habitat loss" ~ "Habitat loss",
      str_detect(Sub_Code, "^Low prices -") ~ "Low prices",
      TRUE ~ Sub_Code
    )
  )

# 4. Deduplicate to Document Level
doc_subcodes <- df_exploded %>%
  select(Document_ID, Cohort, Macro_Region, Parent_Code, Sub_Code) %>%
  distinct()

# Denominators (Unique Document Counts)
total_docs_all <- n_distinct(df_raw$Document_ID)

cohort_denominators <- df_clean %>%
  group_by(Cohort) %>%
  summarise(Total_Cohort_Docs = n_distinct(Document_ID))

region_cohort_denominators <- df_clean %>%
  group_by(Macro_Region, Cohort) %>%
  summarise(Total_Region_Cohort_Docs = n_distinct(Document_ID))

# ------------------------------------------------------------------------------
# TABLE 1: Percent of ALL Documents Mentioning Sub-Theme
# ------------------------------------------------------------------------------
table_1 <- doc_subcodes %>%
  group_by(Parent_Code, Sub_Code) %>%
  summarise(Doc_Count = n_distinct(Document_ID), .groups = "drop") %>%
  mutate(
    Pct_of_All_Docs = round((Doc_Count / total_docs_all) * 100, 1),
    Rank_in_Parent = min_rank(desc(Doc_Count))
  ) %>%
  arrange(Parent_Code, Rank_in_Parent, Sub_Code)

# ------------------------------------------------------------------------------
# TABLE 2: Percent of Documents WITHIN Cohort Mentioning Sub-Theme
# ------------------------------------------------------------------------------
table_2 <- doc_subcodes %>%
  group_by(Cohort, Parent_Code, Sub_Code) %>%
  summarise(Doc_Count = n_distinct(Document_ID), .groups = "drop") %>%
  left_join(cohort_denominators, by = "Cohort") %>%
  mutate(
    Pct_in_Cohort = round((Doc_Count / Total_Cohort_Docs) * 100, 1),
    Rank_in_Cohort = min_rank(desc(Doc_Count))
  ) %>%
  arrange(Cohort, Rank_in_Cohort, Sub_Code)

# ------------------------------------------------------------------------------
# TABLE 3: Percent of Documents WITHIN Cohort & Macro-Region
# ------------------------------------------------------------------------------
table_3 <- doc_subcodes %>%
  group_by(Macro_Region, Cohort, Parent_Code, Sub_Code) %>%
  summarise(Doc_Count = n_distinct(Document_ID), .groups = "drop") %>%
  left_join(region_cohort_denominators, by = c("Macro_Region", "Cohort")) %>%
  mutate(
    Pct_in_Region_Cohort = round((Doc_Count / Total_Region_Cohort_Docs) * 100, 1),
    Rank_in_Region_Cohort = min_rank(desc(Doc_Count))
  ) %>%
  arrange(Macro_Region, Cohort, Rank_in_Region_Cohort, Sub_Code)
