
# Set Up ----
## Loading Libraries
library(haven)
library(tidyverse)
library(survey)
library(ggplot2)
library(showtext)
library(labelled)
library(here)

font_add_google("Playfair Display", "playfair")
showtext_auto()

## Loading Data
household_survey <- read_dta("data/1_roster_edu_mig copy 2.dta")
individual_survey <- read_dta("data/edu_policy_knowledge copy.dta")
weights <- read_dta ("data/design_weights copy.dta")

## Merging the two datasets
household_survey_w <- household_survey |>
  left_join(
    weights,
    by = c("brgy_code", "fourp_status", "province")
  ) |>
  filter(!is.na(norm_design_weight))

# did not match 2163
sum(is.na(household_survey_w$norm_design_weight))

## Creating cohort variable
household_survey_w <- household_survey_w |>
  mutate(
    birth_year = 2023 - age_est,
    cohort10 = paste0(floor((2023 - age_est) / 10) * 10, "s")
  )

## Creating weighted design object
options(survey.lonely.psu = "adjust") 

design <- svydesign(
  id = ~brgy_code + hhid,
  strata = ~province + fourp_status,
  weights = ~norm_design_weight,
  fpc = ~fpc_brgy + fpc_hh,
  data = household_survey_w,
  nest = TRUE
)

# Edu_Completed Analysis ----

## Convert to labels to factors
household_survey_w$edu_completed <- haven::as_factor(
  household_survey_w$edu_completed
)

## Create binary indicator for college graduate or above
household_survey_w$college_grad <- ifelse(
  is.na(household_survey_w$edu_completed),
  NA,
  ifelse(
    household_survey_w$edu_completed %in% c(
      "College graduate",
      "Education beyond college"
    ),
    1, 0
  )
)

## Update design with new variable
design <- update(
  design,
  college_grad = household_survey_w$college_grad
)

## Percentage of college graduates or higher for each cohort
college_by_cohort <- svyby(
  ~college_grad,
  ~cohort10,
  subset(design, cohort10 >= 1930 & cohort10 <= 2000),
  svymean,
  na.rm = TRUE
)

## Graphing results
# number of college graduates is increasing overtime
ggplot(college_by_cohort, aes(
    x = factor(cohort10), 
    y = college_grad * 100, 
    fill = college_grad * 100)) +
  geom_col() +
  geom_errorbar(
    aes(
      ymin = (college_grad - se) * 100,
      ymax = (college_grad + se) * 100
    ),
    width = 0.2
  ) +
  scale_fill_gradientn(
    colours = c("#D6EAF0", "#7FB3C2", "#397F94"),
    values = c(0, 0.1, 1)
  ) +
  labs(
    x = "Birth Cohort",
    y = "Percent of College Graduate or Beyond",
    title = "Percent College Graduate or Beyond by Birth Cohort",
    caption = "Data: PSPS 2023 Household Survey"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 18,
      hjust = 0.5),
    plot.margin = margin(
        t = 30,  # top
        r = 30,  # right
        b = 30,  # bottom
        l = 30   # left
      ), 
    legend.position = "none"
    )

# Edu_reasons1 analysis ----

## Means
edu_reasons <- svymean(
  ~edu_reasons1_NoMoney +
    edu_reasons1_Job +
    edu_reasons1_Marriage +
    edu_reasons1_Disability +
    edu_reasons1_Sick +
    edu_reasons1_HomeObligation +
    edu_reasons1_NoSchool_Teacher +
    edu_reasons1_NoTime +
    edu_reasons1_ParentsDeath +
    edu_reasons1_ParentsSep +
    edu_reasons1_Pregnant,
  
  subset(design, age_est > 5 & age_est < 26),
  na.rm = TRUE
)

edu_reasons

## Creating dataframe
reasons_df <- data.frame(
  reason = names(coef(edu_reasons)),
  percent = coef(edu_reasons) * 100,
  se = SE(edu_reasons) * 100
)

## Clean labels

reasons_df$reason <- c(
  "No Money",
  "Need to Work",
  "Marriage",
  "Disability",
  "Sick",
  "Home Obligations",
  "No School / Teacher",
  "No Time",
  "Parents Died",
  "Parents Separated",
  "Pregnant"
)

## Graphing results
ggplot(
  reasons_df,
  aes(
    x = reorder(reason, percent),
    y = percent,
    fill = percent
  )
) +
  geom_col() +
  geom_errorbar(
    aes(
      ymin = percent - se,
      ymax = percent + se
    ),
    width = 0.2
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.12)),
    breaks = seq(0, 30, by = 5),
    limits = c(0, 30)
    ) +
  scale_fill_gradient(
    low = "#D6EAF0",
    high = "#397F94"
  ) +
  labs(
    x = "",
    y = "Percent",
    title = "Reasons for Not Currently Enrolled in School",
    caption = "Ages 6–25 | Weighted PSPS Survey Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(
      t = 30,  # top
      r = 50,  # right
      b = 30,  # bottom
      l = 5   # left
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_text(
    data = reasons_df |> slice_max(percent, n = 4),
    aes(label = paste0(round(percent, 1), "%")),
    hjust = -1,
    fontface = "bold"
  )

## Edu_reasons1 by school level ----

## Creating school_level variable
# household_survey_w <- household_survey_w |>
#   mutate(
#     school_level = case_when(
#       edu_grade %in% 1:8 ~ "Elementary",
#       edu_grade %in% 9:12 ~ "Junior High",
#       edu_grade %in% 13:14 ~ "Senior High",
#       edu_grade %in% 15:21 ~ "Post-Secondary",
#       TRUE ~ NA_character_
#     ),
#     
#     school_level = factor(
#       school_level,
#       levels = c(
#         "Elementary",
#         "Junior High",
#         "Senior High",
#         "Post-Secondary"
#       )
#     )
#   )
# 
# ## Update design with new variable
# design <- update(
#   design,
#   school_level = household_survey_w$school_level
# )

household_survey_w <- household_survey_w |>
  mutate(
    school_level = case_when(
      age_est >= 5  & age_est <= 12 ~ "Elementary",
      age_est >= 13 & age_est <= 16 ~ "Junior High",
      age_est >= 17 & age_est <= 18 ~ "Senior High",
      age_est >= 19 & age_est <= 25 ~ "Post-Secondary",
      TRUE ~ NA_character_
    ),
    
    school_level = factor(
      school_level,
      levels = c(
        "Elementary",
        "Junior High",
        "Senior High",
        "Post-Secondary"
      )
    )
  )

design <- update(
  design,
  school_level = household_survey_w$school_level
)

## Means by school level
edu_reasons_by_level <- svyby(
  ~edu_reasons1_NoMoney +
    edu_reasons1_Job +
    edu_reasons1_Marriage +
    edu_reasons1_Disability +
    edu_reasons1_Sick +
    edu_reasons1_HomeObligation +
    edu_reasons1_NoSchool_Teacher +
    edu_reasons1_NoTime +
    edu_reasons1_ParentsDeath +
    edu_reasons1_ParentsSep +
    edu_reasons1_Pregnant,
  ~school_level,
  subset(design, age_est >= 5 & age_est <= 25),
  svymean,
  na.rm = TRUE
)

edu_reasons_by_level

## Creating dataframe
reasons_df_level <- edu_reasons_by_level |>
  as.data.frame() |>
  pivot_longer(
    cols = starts_with("edu_reasons1_"),
    names_to = "reason",
    values_to = "percent"
  ) |>
  pivot_longer(
    cols = starts_with("se."),
    names_to = "se_reason",
    values_to = "se"
  ) |>
  mutate(
    reason = str_remove(reason, "edu_reasons1_"),
    se_reason = str_remove(se_reason, "se.edu_reasons1_")
  ) |>
  filter(reason == se_reason) |>
  mutate(
    percent = percent * 100,
    se = se * 100
  )

## Graphing by class

reasons_df_level |>
  filter(school_level != "Post-Secondary") |>
  ggplot(
    aes(
      x = reorder(reason, percent),
      y = percent,
      fill = percent
      )
    )+
  geom_col() +
  geom_errorbar(
    aes(
      ymin = percent - se,
      ymax = percent + se
    ),
    width = 0.2
  ) +
  coord_flip() +
  facet_wrap(~school_level) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.12)),
    breaks = seq(0, 100, by = 10)
  ) +
  scale_fill_gradient(
    low = "#D6EAF0",
    high = "#397F94"
  ) +
  labs(
    x = "",
    y = "Percent",
    title = "Reasons for Not Currently Enrolled in School by School Level",
    caption = "Ages 6–25 | Weighted PSPS Survey Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines")
  ) +
  coord_flip(clip = "off") +
  geom_text(
    data = reasons_df_level |>
      filter(school_level != "Post-Secondary") |>
      group_by(school_level) |>
      slice_max(percent, n = 4) |>
      ungroup(),
    
    aes(label = paste0(round(percent, 1), "%")),
    hjust = -0.85,
    fontface = "bold",
    size = 3
  )

