# Load packages
library(tidyverse)
library(NHANES)

# Looking at data
glimpse(NHANES)



# Selecting columns -------------------------------------------------------

select(NHANES, Age)

select(NHANES, Age, Weight, BMI)

select(NHANES, -HeadCirc)

select(NHANES, starts_with("BP"))

select(NHANES, ends_with("Day"))

select(NHANES, contains("Age"))

# Creating smaller NHANES dataset

nhanes_small <- select(
  NHANES, Age, Gender, BMI, Diabetes, PhysActive,
  BPSysAve, BPDiaAve, Education
)



# Renaming columns --------------------------------------------------------

nhanes_small <- rename_with(
  nhanes_small,
  snakecase::to_snake_case
)


# Renaming specific columns -----------------------------------------------

nhanes_small <- rename(nhanes_small, sex = gender)

nhanes_small


# Trying out the pipe -----------------------------------------------------

colnames(nhanes_small)

nhanes_small %>%
  colnames()

nhanes_small %>%
  select(phys_active) %>%
  rename(physically_active = phys_active)


# Exercise 7.8 ------------------------------------------------------------

nhanes_small %>% select(bp_sys_ave, education)

nhanes_small %>% rename(bp_sys = bp_sys_ave, bp_dia = bp_dia_ave)

nhanes_small %>% select(bmi, contains("age"))

nhanes_small %>%
  select(starts_with("bp_")) %>%
  rename(bp_systolic = bp_sys_ave)


# Filtering ---------------------------------------------------------------
nhanes_small %>% dplyr::filter(phys_active == "No")
nhanes_small %>% filter(bmi >= 25)


# Combining logical operators ---------------------------------------------

nhanes_small %>%
  filter(bmi >= 25 & phys_active == "No")

nhanes_small %>% filter(bmi >= 25 | phys_active == "No")
nhanes_small %>% filter(bmi >= 25 | phys_active == "No")

# Arrange data ------------------------------------------------------------

nhanes_small %>% arrange(age)

nhanes_small %>% arrange(age, education)


# Transform data ----------------------------------------------------------

nhanes_small %>% mutate(age_months = age * 12)


# Exercise 7.12 -----------------------------------------------------------

nhanes_small %>% filter(bmi >= 20 & bmi <= 40 & diabetes == "Yes")

nhanes_modified <- nhanes_small %>% mutate(
        mean_arterial_pressure =((2* bp_sys_ave) + bp_dia_ave)/3 ,
        young_child=if_else(age <= 6, "Yes", "No")
)


nhanes_modified




# Creating summary statistics ---------------------------------------------

nhanes_small %>% summarise(
    max_bmi = max(bmi, na.rm=T),
    min_bmi = min(bmi, na.rm=T)
    )


diabeetus <- nhanes_small %>%
    filter(!is.na(diabetes)) %>%
    group_by(diabetes) %>%
    summarise(
        max_bmi = max(bmi, na.rm=T),
        min_bmi = min(bmi, na.rm=T)
    ) %>%
    ungroup()

readr::write_csv(nhanes_small, here::here("data/nhanes_small.csv"))






