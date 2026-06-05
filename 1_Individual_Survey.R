
# Set Up ----
## Loading Libraries
library(haven)
library(tidyverse)
library(survey)
library(srvyr)
library(ggplot2)
library(showtext)

# Font
font_add_google("Playfair Display", "playfair")
showtext_auto()

# Loading Data
individual_survey <- read_dta("data/edu_policy_knowledge copy.dta")
household_survey <- read_dta("data/1_roster_edu_mig copy 2.dta")
weights <- read_dta ("data/design_weights copy.dta")

# Merging Datasets----

# Unify memid datatype
household_survey <- household_survey |>
  mutate(memid = as.character(memid))

individual_survey <- individual_survey |>
  mutate(memid = as.character(memid))

# Merging
m_survey_1 <- household_survey |>
  left_join(
    individual_survey |>
      select(-fourp_status, -random_select, -nonrandom_select),
    by = c("province", "brgy_code", "hhid")
  )

m_survey_1 <- m_survey_1 |>
  left_join(
    weights,
    by = c("province", "brgy_code", "fourp_status")
  )

# did not match 2163
sum(is.na(m_survey_1$norm_design_weight))

# Data Cleaning----

# Created edu_level_group variable
m_survey_1 <- m_survey_1 |>
  mutate(
    edu_level_group = case_when(
      
      edu_completed == 0 ~ "No Schooling",
      
      edu_completed >= 1  & edu_completed <= 8  ~ "Elementary",
      
      edu_completed >= 9  & edu_completed <= 12 ~ "Junior High",
      
      edu_completed >= 13 & edu_completed <= 14 ~ "Senior High",
      
      edu_completed >= 15 & edu_completed <= 17 ~ "Vocational/Associate",
      
      edu_completed >= 18 & edu_completed <= 21 ~ "Some College",
      
      edu_completed >= 22 & edu_completed <= 23 ~ "College+",
      
      edu_completed %in% c(-999, -888, -666) ~ NA_character_,
      
      TRUE ~ NA_character_
    )
  )

m_survey_1 <- m_survey_1 |>
  mutate(
    edu_level_group = factor(
      edu_level_group,
      levels = c(
        "No Schooling",
        "Elementary",
        "Junior High",
        "Senior High",
        "Vocational/Associate",
        "Some College",
        "College+"
      )
    )
  )

# Creating Weight Object ----

# Remove unmatched
m_survey_clean <- m_survey_1 |>
  filter(!is.na(norm_design_weight))

options(survey.lonely.psu = "adjust") 

design <- svydesign(
  id = ~brgy_code + hhid,
  strata = ~province + fourp_status,
  weights = ~norm_design_weight,
  fpc = ~fpc_brgy + fpc_hh,
  data = m_survey_clean,
  nest = TRUE
)

# edu_completed vs ed_expect ----

# Calculate the mean of ed_expect for each edu_level_group
edu_expect_completed <- svyby(
  ~ed_expect,
  ~edu_level_group,
  design,
  svymean,
  na.rm = TRUE
)

# Convert to a data frame for plotting
edu_expect_df <- as.data.frame(edu_expect_completed)

# Plotting
ggplot(
  edu_expect_df,
  aes(
    x = edu_level_group,
    y = ed_expect,
  ),
  color = "#397F94"
) +
  geom_point(size = 2) +
  geom_line(
    aes(group = 1),
    linewidth = 1,
    color = "#397F94"
  ) +
  geom_errorbar(
    aes(
      ymin = ed_expect - se,
      ymax = ed_expect + se
    ),
    width = 0.2
  ) +
  scale_y_continuous(
    breaks = c(18, 19, 20, 21, 22),
    labels = c(
      "1st Year\nCollege",
      "2nd Year\nCollege",
      "3rd Year\nCollege",
      "4th Year+\nCollege",
      "College\nGraduate"
    )
  ) +
  labs(
    x = "Parent's Highest Education Completed",
    y = "Average Parent Expectation \nfor Child's Educational Attainment",
    title = "Parents With Higher Education \nExpect Higher Education for Their Children",
    caption = "Weighted PSPS Survey Data"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# Tertiary Subsidy ----

subsidy_counts <- m_survey_clean |>
  summarise(
    across(
      starts_with("avail_1_b_"),
      ~sum(. == 1, na.rm = TRUE)
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "reason",
    values_to = "count"
  ) |>
  mutate(
    reason = case_when(
      reason == "avail_1_b_complex_process" ~ "Process too complicated",
      reason == "avail_1_b_expect_discontinue" ~ "Expect program to discontinue",
      reason == "avail_1_b_has_funds" ~ "Has sufficient funds",
      reason == "avail_1_b_inelig_finNeed" ~ "Financial need criteria",
      reason == "avail_1_b_low_priority" ~ "Unlikely to be selected",
      reason == "avail_1_b_noInterest_HE" ~ "Student uninterested",
      reason == "avail_1_b_noInterest_elig_insti" ~ "Won't attend eligible school",
      reason == "avail_1_b_oth" ~ "Other reason",
      reason == "avail_1_b_oth_needier" ~ "Other families more deserving",
      reason == "avail_1_b_other_support" ~ "Seeking other support",
      TRUE ~ reason
    )
  )

ggplot(
  subsidy_counts,
  aes(
    x = reorder(reason, count),
    y = count
  )
) +
  geom_col(fill = "#397F94") +
  geom_text(
    aes(label = count),
    hjust = -0.2,
    size = 4
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    x = "",
    y = "Number of Respondents",
    title = "Reasons for Not Taking Up the Tertiary Education Subsidy"
  ) +
  theme_minimal(base_size = 13)
