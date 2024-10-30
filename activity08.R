# --- Activity 08 ---


# reinstating the table we need for the armed forces data, each case is a person
# I've copied this code from my previous work
# Load packages ----
library(googlesheets4)
library(tidyverse)
library(dplyr)

# Load data ----
gs4_deauth()
armed_forces_raw <- read_sheet(
  ss = 'https://docs.google.com/spreadsheets/d/1cn4i0-ymB1ZytWXCwsJiq6fZ9PhGLUvbMBHlzqG4bwo/edit?gid=597536282#gid=597536282',
  skip = 1  # skip the first row so we can write our own column names that aren't merged
)

# clean data. sort by collection of soldiers. dont worry about having a row for every single person
# plan is the following:
# start by naming the removed rows. should go 'branch, branch, total, branch branch total... etc etc'
# check if the N/A values for air and space force come as N/A or in character forms.
# delete total cols.
# delete the total rows. delete the row that says "Source: ..."
# make a new column for gender and branch of military
# also a colun for the pay grade- can rename them to the official rank name rather than the two letter rank

armed_forces_clean <- armed_forces_raw %>%
  rename(
    pay_grade = `...1`,
    army.m = Army,
    army.f = `...3`,
    total.a = `...4`,
    navy.m = Navy,
    navy.f = `...6`,
    total.n = `...7`,
    marines.m = `Marine Corps`,
    marines.f = `...9`,
    total.mc = `...10`,
    air_force.m = `Air Force`,
    air_force.f = `...12`,
    total.af = `...13`,
    space_force.m = `Space Force`,
    space_force.f = `...15`,
    total_sf = `...16`,
    total_m = Total,
    total_f = `...18`,
    total_all = `...19`
  )
# instead of piping, reassign the dataframe to drop some rows. Not sure why R wouldnt let me chain the subsetting operator
armed_forces_clean <- armed_forces_clean[-(c(1,11,17,28,29,30)),]
# very quickly drop a bunch of the total rows and start chaining again

# we want to convert this wide data into a much longer data frame. Lets try this with just one branch, the army.
# i just want to see if what i expect to happen happens, if it does I can add in the other branches to the cols argument
armed_forces_clean <- armed_forces_clean %>%
  pivot_longer(  # long pivot all the columns at once
    cols = c(army.m, army.f, navy.m, navy.f, marines.m, marines.f, air_force.m, air_force.f, space_force.m, space_force.f),
    names_to = 'branch_and_gender',
    values_to = c('count')
  )  # now get rid of all columns that contain the word 'total', With the way we have it set up I can just drop cols 2-9 by reassigning the dataframe.
armed_forces_clean <- armed_forces_clean[-c(2:9)] %>%
  # now split the 'branch_and_gender' column into two columns for branch and gender
  separate_wider_delim(
    cols = branch_and_gender,
    names = c("branch", "gender"),
    delim = '.',
  )  # fix some  other stuff (change branch and gender to full words if needed, convert NAs and numbers to their appropriate type. etc- this stuff can go in the 'polishing section'), but as is, the data has been tidied.
# but we will do one last change. We need to give the actual rank names of the groups rather than just the two letters.
# to do that, We import the pay grade/rank table provided, clean the data/ extract what we need and then just match it to our us armed forces data.

# get the rank and title table from the web and clean it
# load rvest
library(rvest)
rank_title_table <- read_html("https://neilhatfield.github.io/Stat184_PayGradeRanks.html") %>%
  html_elements(css = 'table') %>%
  html_table(header = FALSE)

rank_title_raw <- rank_title_table[[1]] # get rid of the first row and rename cols
rank_title_clean <- rank_title_raw %>%
  slice(
    -c(1,n())
  ) %>%
  rename(
    staff_type = X1,
    pay_grade = X2,
    title.army = X3,
    title.navy = X4,
    title.marines = X5,
    title.air_force = X6,
    title.space_force = X7,
    cg = X8  # name doesn't matter since we will drop
  ) %>%  # drop the coast guard column
  select(-cg)
# drop the first 2 and the last rows
rank_title_clean <- rank_title_clean[-c(1,2,nrow(rank_title_clean) - 1),] %>%
  # make this table longer by pivot(ing)_longer on the title columns and then getting the values from the rank column
  pivot_longer(
    cols = c(3:7),
    names_to = 'title_branch'
  ) %>%  # now separate the branch_title col into the branch and title cols
  separate_wider_delim(
    col = title_branch,
    delim = '.',
    names = c("title", 'branch')
  ) %>%  # drop the title and staff_type cols
  select(-c(title, staff_type))


final_table_groups <- left_join(
  x = armed_forces_clean,
  y = rank_title_clean,
  by = join_by(branch == branch, pay_grade == pay_grade)
) %>%
  rename(
    title = value
  ) %>%
  mutate(
    gender = if_else(gender == 'm', 'Male', 'Female'),
    count = as.numeric(count),
    count = replace_na(count, 0),
  ) %>%
  select(-pay_grade)

# make the dataframe even longer.
starting_table <- final_table_groups %>%
  uncount(count)


# --------------------- making the summary tables

# load the required packages
library(janitor)
library(knitr)
library(kableExtra)

# male_table <- starting_table %>%
#   filter(gender == 'Male') %>%
#   tabyl(branch, title) %>%
#
#   adorn_totals(where = c('row', 'col')) %>%
#   adorn_percentages(denominator = 'all') %>%
#   adorn_pct_formatting(digits = 2) %>%
#   adorn_title(
#     placement = 'combined',
#     row_name = 'Branch',
#     col_name = 'Title'
#   )
# # using the code from class to add relative frequencies
# format_abs_freq <- attr(male_table, 'core') %>%
#   adorn_totals(where = c('row', 'col')) %>%
#   mutate(
#     across(where(is.numeric), format, big.mark = ',')
#   )
# # make table including frequencies
# male_table <- male_table %>%
#   adorn_ns(position = 'front', ns = format_abs_freq)
#
# # make final table with kable
# male_table_final <- kable(
#   male_table,
#   caption = 'Absolute and Relative (To All Enlisted Soldiers) Frequencies of Male Enlsited Soldiers Organized by Branch and Title',
#   align = 'c'
# ) %>%
#   kableExtra::kable_classic()



# -------------- polished code -------

# load the required packages
library(janitor)
library(knitr)
library(kableExtra)

male_table <- starting_table %>%
 filter(gender == 'Male') %>%
  tabyl(branch, title) %>%
  adorn_totals(where = c('row', 'col')) %>%
  adorn_percentages(denominator = 'all') %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_title(
    placement = 'combined',
    row_name = 'Branch',
    col_name = 'Title')

format_abs_freq_male <- attr(male_table, 'core') %>%
  adorn_totals(where = c('row', 'col')) %>%
  mutate(
    across(where(is.numeric), format, big.mark = ',')
  )

male_table_polished <-  male_table %>%
  adorn_ns(position = 'front', ns = format_abs_freq_male) %>%
  kable(
    caption = 'Absolute and Relative (To All Enlisted Soldiers) Frequencies of Female Enlisted Soldiers Organized by Branch and Title',
    align = 'c') %>%
  kableExtra::kable_classic()


# making the female table

female_table <- starting_table %>%
  filter(gender == 'Female') %>%
  tabyl(branch, title) %>%
  adorn_totals(where = c('row', 'col')) %>%
  adorn_percentages(denominator = 'all') %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_title(
    placement = 'combined',
    row_name = 'Branch',
    col_name = 'Title')

format_abs_freq_female <- attr(female_table, 'core') %>%
  adorn_totals(where = c('row', 'col')) %>%
  mutate(
    across(where(is.numeric), format, big.mark = ',')
  )

female_table_polished <-  female_table %>%
  adorn_ns(position = 'front', ns = format_abs_freq_female) %>%
  kable(
    caption = 'Absolute and Relative (To All Enlisted Soldiers) Frequencies of Female Enlisted Soldiers Organized by Branch and Title',
    align = 'c') %>%
  kableExtra::kable_classic()


### --- Working with the diamond data

library(ggplot2)
library(tidyverse)
library(janitor)
library(knitr)
library(kableExtra)

diamond_z_table <- diamonds %>%
  select(cut, z) %>%
  filter(z > 0 ) %>% #select only diamonds with greater than 0 depth to be realistic
  group_by(cut) %>%
  summarize(
    count = n(),
    min = min(z),
    first_quintile = quantile(z, .20, na.rm = TRUE),
    second_quintile = quantile(z, .40, na.rm = TRUE),
    median = median(z),
    third_quintile = quantile(z, 0.60, na.rm = TRUE),
    fourth_quintile = quantile(z, 0.80, na.rm = TRUE),
    max = max(z),
    arith_mean = mean(z),
    arith_sd = sd(z)
  ) %>% # polish the table with the other functions that we have the basic structure
  kable(
    caption = "Statistics for the Depth (Z) of the 'Diamonds' Data Set",
    align = 'c') %>%
  kable_classic()

diamond_z_table


### --- Working with the palmer penguin data
library(palmerpenguins)
library(tidyverse)
library(janitor)
library(knitr)
library(kableExtra)

penguins_filtered <- penguins %>%
  drop_na() %>%
  filter(species == 'Adelie')
penguins_regression_model <- lm(
    formula = body_mass_g ~ bill_depth_mm + bill_length_mm + flipper_length_mm + sex + island,
    data = penguins_filtered
  )

coeff_table <- kable(
  penguins_regression_model$coefficients,
  caption = 'Regression Summary from Regressing Body Mass (in grams) on Bill Length, Bil Depth, Flipper Length, Sex, and Island',
  digits = 3,
  align = 'c',
  col.names = c("Estimate", 'Standard Error', 't-value', 'P(x>|t|)')
) %>%
  kableExtra::kable_classic()
coeff_table
