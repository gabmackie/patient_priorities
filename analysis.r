# install.packages("openxlsx")
# install.packages("here")
# install.packages("magrittr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("janitor")
# install.packages("brunnermunzel")

library(openxlsx)
library(here)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(janitor)
library(brunnermunzel)

# These initial steps have to be done every time
# After that, each section can be run independently
# Main has to be run for both Further and Subgroup to be run (except where noted)
# Further and Subgroup are independent, either can be run without the other
# *** indicates a user decision
# ### indicates where in the paper the code has been used

# *** REPLACE WITH YOUR DATA FOLDER ***
data_folder <- here("data", "merged")
# Read in data
df <- read.xlsx(file.path(data_folder, "analysis_data.xlsx"), na.strings = c("", "NA"))


# Subgroup Analysis Function ----------------------------------------------
### Paper reference: Supplementary Materials 4 ###
# Define subgroups and their complementary pairs
subgroup_pairs <- list(
  "No Depression" = "Depression",
  "Depression" = "No Depression",
  "GAD" = "No GAD",
  "No GAD" = "GAD",
  "No physical" = "Physical condition",
  "Physical condition" = "No physical",
  "Men" = "Women",
  "Women" = "Men",
  "Neurodivergent" = "Not Neurodivergent",
  "Not Neurodivergent" = "Neurodivergent",
  "Workshop Sample" = "Online Sample",
  "Online Sample" = "Workshop Sample"
)

# For all statistical tests, tests are run both ways round (e.g. Men/Women and Women/Men)
# This is partly to ensure that the numbers match up
# It is also to extract subgroup mean and sd

run_t_tests <- function(data, subgroups, subgroup_pairs, options) {
  t_test_results <- bind_rows(lapply(names(subgroups), function(group_name) {
    subgroup_data <- subgroups[[group_name]] %>%
      select(all_of(options)) %>%
      pivot_longer(cols = everything(), names_to = "source", values_to = "value")

    complementary_data <- subgroups[[subgroup_pairs[[group_name]]]] %>%
      select(all_of(options)) %>%
      pivot_longer(cols = everything(), names_to = "source", values_to = "value")

    tests <- suppressWarnings({
      subgroup_data %>%
        group_by(source) %>%
        summarise(
          bm_test = list(brunnermunzel.test(value, complementary_data$value[complementary_data$source == source])$p.value)
        ) %>%
        unnest(bm_test) %>%
        mutate(group = group_name)
    })
    # Warnings are due to summarise not liking how data is passed to it, but does not affect output

    return(tests)
  }))

  # Adjust p-values for multiple comparisons using Bonferroni correction
  t_test_results <- t_test_results %>%
    mutate(bm_adjusted = pmin(bm_test * length(options), 1))

  return(t_test_results)
}

# Define the color palette for subgroup graphs
group_colors <- c(
  "No Depression" = "#1b9e77", "Depression" = "#a6dba0",
  "GAD" = "#d95f02", "No GAD" = "#fdbb84",
  "No physical" = "#66a61e", "Physical condition" = "#b3de69",
  "Men" = "#e7298a", "Women" = "#fbb4b9",
  "Neurodivergent" = "#7570b3", "Not Neurodivergent" = "#c2a5cf",
  "Workshop Sample" = "#e6ab02", "Online Sample" = "#fee08b"
)


# Descriptives 1 - Main: ----------------------------------------------------------------
df_desc_all <- df
df_desc_workshop <- df[df$source == "Workshop", ]
df_desc_online <- df[df$source == "Online", ]

# *** Choose which dataset to analyse ***
df_desc <- df_desc_all
# df_desc <- df_desc_workshop
# df_desc <- df_desc_online

### Paper reference: Table 1 ###
# Gender
df_desc %>%
  group_by(gender) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Diagnoses
df_desc %>%
  mutate(diagnosis = str_replace_all(diagnosis, " \\(.*?\\)", "")) %>%
  mutate(diagnosis = str_split(diagnosis, pattern = ",")) %>%
  unnest(diagnosis) %>%
  group_by(diagnosis) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / nrow(df_desc) * 100) %>%
  arrange(desc(count))

# Number of diagnoses
df_desc %>%
  mutate(diagnosis = str_split(diagnosis, pattern = ",")) %>%
  unnest(diagnosis) %>%
  group_by(id) %>%
  summarise(num_diagnoses = n()) %>%
  ungroup() %>%
  count(num_diagnoses) %>%
  mutate(percentage = n / nrow(df_desc) * 100)

### Paper reference: Supplementary Table 1 ###
# Number of physical health conditions (use for count data)
df_desc %>%
  mutate(physical_x = str_split(physical_x, pattern = ",")) %>%
  unnest(physical_x) %>%
  group_by(physical_x) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Percentage of people with a physical health condition in each category
# This removes duplicates because some people had, say, 3 gastrointenstinal conditions, which skews the percentage figure
df_desc %>%
  mutate(physical_x = str_split(physical_x, pattern = ",")) %>%
  mutate(physical_x = lapply(physical_x, function(x) unique(trimws(x)))) %>%
  unnest(physical_x) %>%
  group_by(physical_x) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / nrow(df_desc) * 100) %>%
  arrange(desc(count))

# Number of physical diagnoses
df_desc %>%
  filter(!physical_x %in% c("No", "Prefer not to say")) %>% # Exclude "No" and "Prefer not to say"
  mutate(physical_x = str_split(physical_x, pattern = ",")) %>%
  unnest(physical_x) %>%
  group_by(id) %>%
  summarise(num_physical = n(), .groups = "drop") %>%
  count(num_physical) %>%
  mutate(percentage = n / nrow(df_desc) * 100)

# Physical other
df_desc %>%
  filter(!is.na(physical_details)) %>%
  mutate(physical_details = str_split(physical_details, pattern = ",")) %>%
  unnest(physical_details) %>%
  group_by(physical_details) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / nrow(df_desc) * 100) %>%
  print(n = Inf)

### Paper reference: Supplementary Table 2 ###
# Neurodivergence
df_desc %>%
  mutate(neurodivergent_specific = str_replace_all(neurodivergent_specific, " \\(.*?\\)", "")) %>%
  mutate(neurodivergent_specific = str_split(neurodivergent_specific, pattern = ",")) %>%
  unnest(neurodivergent_specific) %>%
  filter(!is.na(neurodivergent_specific), neurodivergent_specific != "") %>% # Remove NA and empty values
  group_by(neurodivergent_specific) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / nrow(df_desc) * 100) %>%
  arrange(desc(count)) %>%
  bind_rows(
    tibble(
      neurodivergent_specific = c("No", "Prefer not to say"),
      count = c(
        sum(df_desc$neurodivergent == "No", na.rm = TRUE),
        sum(df_desc$neurodivergent == "Prefer not to say", na.rm = TRUE)
      ),
      percentage = c(
        sum(df_desc$neurodivergent == "No", na.rm = TRUE) / nrow(df_desc) * 100,
        sum(df_desc$neurodivergent == "Prefer not to say", na.rm = TRUE) / nrow(df_desc) * 100
      )
    )
  )

# Number of neurodiversities
df_desc %>%
  filter(neurodivergent == "Yes") %>% # Keep only those who answered "Yes"
  mutate(neurodivergent_specific = str_split(neurodivergent_specific, pattern = ",")) %>%
  unnest(neurodivergent_specific) %>%
  group_by(id) %>%
  summarise(num_neurodiversities = n(), .groups = "drop") %>%
  count(num_neurodiversities) %>%
  mutate(percentage = n / nrow(df_desc) * 100)


# Descriptives 2 - Main: ----------------------------------------------------------------
### Paper reference: Table 1 ###
# Age and Ethnicity have been stored in another dataframe with randomised order to ensure anonymity
df_demographics <- read.xlsx(file.path(data_folder, "demographic_data.xlsx"), na.strings = c("", "NA"))

df_desc_all_2 <- df_demographics
df_desc_workshop_2 <- df_demographics[df_demographics$source == "Workshop", ]
df_desc_online_2 <- df_demographics[df_demographics$source == "Online", ]

# *** Choose which dataset to analyse ***
df_desc_2 <- df_desc_all_2
#df_desc_2 <- df_desc_workshop_2
#df_desc_2 <- df_desc_online_2

# Age
summary(df_desc_2$age)
sd(df_desc_2$age, na.rm = TRUE)

# Ethnicity
df_desc_2 %>%
  group_by(ethnicity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(desc(count))


# Ranking Sources - Main: ---------------------------------------------------------
### Paper reference: Experience of bodily sensations - Quantitative analyses ###
### Paper reference: Figure 1C - Importance scores for each bodily source ###
rank_df <- df %>%
  select(id, which_sources, rank_sources, depression, source, gad, physical, gender, neurodivergent)

# Remove the option "None"
rank_df <- mutate(rank_df,
  rank_sources = str_remove_all(rank_sources, "; None"),
  which_sources = str_remove_all(which_sources, "; None")
)

# Ranked sources should only be those selected in which sources
rank_df <- rank_df %>%
  rowwise() %>%
  mutate(
    rank_sources = list(str_split(rank_sources, pattern = ";") %>% unlist() %>% str_trim()),
    which_sources = list(str_split(which_sources, pattern = ";") %>% unlist() %>% str_trim()),
    rank_sources = rank_sources[rank_sources %in% which_sources] %>% str_c(collapse = "; ")
  ) %>%
  ungroup()

rank_options <- c("Heartbeat", "Breathing", "Stomach", "Bladder", "Hunger", "Thirst", "Body temperature", "Muscle tension", "Other")

# Create empty columns for each source
for (source in rank_options) {
  rank_df[[source]] <- NA
}

# Remove any rows with NA in rank_sources
rank_df <- rank_df %>% drop_na(rank_sources)

# Run through the sources each person has ranked and assign each a value based on it's order in the ranking
for (i in 1:nrow(rank_df)) {
  current_sources <- rank_df[i, "rank_sources"] %>%
    str_split(pattern = ";") %>%
    unlist() %>%
    str_trim()

  # There were nine total sources, so the highest rank source is assigned nine
  for (j in 1:length(current_sources)) {
    rank_df[i, current_sources[j]] <- 10 - j
  }
}

# Fill all NAs with 0
rank_df[is.na(rank_df)] <- 0

# Clean up names
rank_df %<>% clean_names()

rank_order <- c("heartbeat", "stomach", "breathing", "muscle_tension", "hunger", "body_temperature", "other", "thirst", "bladder")

rank_label <- c(
  "heartbeat" = "Heartbeat",
  "breathing" = "Breathing",
  "stomach" = "Stomach",
  "bladder" = "Bladder",
  "hunger" = "Hunger",
  "thirst" = "Thirst",
  "body_temperature" = "Body\nTemperature",
  "muscle_tension" = "Muscle\nTension",
  "other" = "Other"
)


# Ranking Sources - Further -----------------------------------------------
# Get the data into shape to be put into a figure
rank_long <- rank_df %>%
  select(-c(id, which_sources, rank_sources, depression, source, gad, physical, gender, neurodivergent)) %>%
  pivot_longer(cols = everything(), names_to = "group", values_to = "value")

rank_summary <- rank_long %>%
  group_by(group) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    se = sd(value, na.rm = TRUE) / sqrt(n())
  )

# For plotting, order the sources by importance
rank_summary <- rank_summary %>% mutate(group = factor(group, levels = rank_order))

# Plot rank sources
ggplot(rank_summary, aes(x = group, y = mean, fill = group)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(x = "Group", y = "Mean Value") +
  scale_x_discrete(labels = rank_label) +
  scale_y_continuous(limits = c(0, 7), expand = expansion(mult = c(0, 0.05))) +
  theme(
    axis.text.x = element_text(size = rel(0.8)),
    legend.position = "none"
  )


# Ranking Sources - Subgroup: -----------------------------------------------------------
### Paper reference: Supplementary Material 4 ###
### Paper reference: Figure S2 - Importance ###
# Define your subgroups
rank_df <- rank_df %>%
  rename(
    temperature = body_temperature,
    tension = muscle_tension
  )

rank_options <- c("heartbeat", "breathing", "stomach", "bladder", "hunger", "thirst", "temperature", "tension", "other")

# Define your subgroups
rank_subgroups <- list(
  "No Depression" = rank_df %>% filter(depression == FALSE),
  "Depression" = rank_df %>% filter(depression == TRUE),
  "GAD" = rank_df %>% filter(gad == TRUE),
  "No GAD" = rank_df %>% filter(gad == FALSE),
  "No physical" = rank_df %>% filter(physical == FALSE),
  "Physical condition" = rank_df %>% filter(physical == TRUE),
  "Men" = rank_df %>% filter(gender == "Man"),
  "Women" = rank_df %>% filter(gender == "Woman"),
  "Neurodivergent" = rank_df %>% filter(neurodivergent == "Yes"),
  "Not Neurodivergent" = rank_df %>% filter(neurodivergent == "No"),
  "Workshop Sample" = rank_df %>% filter(source == "Workshop"),
  "Online Sample" = rank_df %>% filter(source == "Online")
)
# Note: look at the above dataframe to obtain sample sizes for each subgroup (nrow = sample size)

# Define the order of subgroups
rank_subgroup_order <- c("No Depression", "Depression", "GAD", "No GAD", "No physical", "Physical condition", "Men", "Women", "Neurodivergent", "Not Neurodivergent", "Workshop Sample", "Online Sample")
rank_order <- c("heartbeat", "stomach", "breathing", "tension", "hunger", "temperature", "other", "thirst", "bladder")

# Calculate overall mean for each source
rank_overall_means <- rank_df %>%
  select(all_of(rank_options)) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "source", values_to = "overall_mean")

# Calculate mean for each subgroup for each source
rank_subgroup_means <- bind_rows(lapply(names(rank_subgroups), function(group_name) {
  rank_subgroups[[group_name]] %>%
    select(all_of(rank_options)) %>%
    summarise(across(everything(), list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)))) %>%
    pivot_longer(cols = everything(), names_to = c("source", ".value"), names_sep = "_") %>%
    mutate(group = group_name)
}))

# Merge overall means with subgroup means
rank_difference_scores <- rank_subgroup_means %>%
  left_join(rank_overall_means, by = "source") %>%
  mutate(difference = mean - overall_mean)

# Create a new data frame for plotting
rank_plot_data <- rank_difference_scores %>%
  select(group, source, difference) %>%
  mutate(
    source = factor(source, levels = rank_order),
    group = factor(group, levels = rank_subgroup_order)
  )

# Define labels for the sources
rank_subgroup_label <- c(
  "heartbeat" = "Heartbeat",
  "breathing" = "Breathing",
  "stomach" = "Stomach",
  "bladder" = "Bladder",
  "hunger" = "Hunger",
  "thirst" = "Thirst",
  "temperature" = "Body\nTemperature",
  "tension" = "Muscle\nTension",
  "other" = "Other"
)

# Plot the difference scores
ggplot(rank_plot_data, aes(x = source, y = difference, fill = group)) +
  geom_col(position = "dodge", color = "black") +
  labs(x = "Source", y = "Difference from Overall Mean") +
  scale_fill_manual(values = group_colors) +
  scale_x_discrete(labels = rank_subgroup_label) +
  scale_y_continuous(limits = c(-9, 9), expand = expansion(mult = c(0, 0)), breaks = seq(-9, 9, by = 3)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = rel(0.8)),
    legend.position = "bottom"
  )

# Run t-tests
rank_t_test_results <- run_t_tests(rank_df, rank_subgroups, subgroup_pairs, rank_options)

# Merge t-test results with subgroup means and overall means
rank_t_test_results <- rank_t_test_results %>%
  left_join(rank_subgroup_means, by = c("source", "group")) %>%
  left_join(rank_overall_means, by = "source")

# Print t-test results with adjusted p-values
filter(rank_t_test_results, bm_adjusted < 0.05)


# Body Sources - Main: ----------------------------------------------------------------
### Paper reference: Experience of bodily sensations - Quantitative analyses ###
### Paper reference: Figure 1C - Percentage of Contributors for each bodily source ###
# Add Other to which_sources if it was written about in the Other freetext but not selected
df_sources <- df %>%
  mutate(
    which_sources = str_trim(which_sources),
    which_sources = na_if(which_sources, "")
  ) %>%
  mutate(which_sources = ifelse(!str_detect(which_sources, "Other") & !is.na(which_sources_other),
    str_c(which_sources, "; Other"),
    which_sources
  )) %>%
  filter(!is.na(which_sources)) # One person didn't select any sources

# Which did people select
sources <- df_sources %>%
  mutate(which_sources = str_split(which_sources, pattern = ";")) %>%
  unnest(which_sources) %>%
  mutate(which_sources = str_trim(which_sources)) %>%
  group_by(which_sources) %>%
  summarise(selected = n()) %>%
  mutate(percentage = selected / nrow(df_sources) * 100) %>%
  arrange(desc(selected))

# Groups are ordered based on the Importance score above
sources_order <- c("Heartbeat", "Stomach", "Breathing", "Muscle tension", "Hunger", "Body temperature", "Other", "Thirst", "Bladder")
sources <- sources %>% mutate(which_sources = factor(which_sources, levels = sources_order))
sources_label <- c("Heartbeat", "Stomach", "Breathing", "Muscle\nTension", "Hunger", "Body\nTemperature", "Other", "Thirst", "Bladder")

# Body Sources - Further --------------------------------------------------
# Plot % who selected each source
ggplot(sources, aes(x = which_sources, y = percentage, fill = which_sources)) +
  geom_col(color = "black") +
  labs(x = "Source", y = "Percentage of Contributors") +
  scale_x_discrete(labels = sources_label) +
  scale_y_continuous(limits = c(0, 80), expand = expansion(mult = c(0, 0.05))) +
  theme(
    axis.text.x = element_text(size = rel(0.8)),
    legend.position = "none"
  )

# What Other sources did people select?
df_sources %>%
  filter(!is.na(which_sources_other)) %>%
  mutate(which_sources_other = str_split(which_sources_other, pattern = ",")) %>%
  unnest(which_sources_other) %>%
  group_by(which_sources_other) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / nrow(df_sources) * 100) %>%
  print(n = Inf)

# How many did people select?
df_summary <- df_sources %>%
  mutate(which_sources = str_split(which_sources, pattern = ";")) %>%
  unnest(which_sources) %>%
  mutate(which_sources = str_trim(which_sources)) %>%
  group_by(id) %>%
  summarise(num_sources = n())

summary(df_summary)

df_summary %>%
  ungroup() %>%
  count(num_sources) %>%
  mutate(percentage = n / sum(n) * 100)


# Body Sources - Subgroup: -----------------------------------------------------------
### Paper reference: Supplementary Material 4 ###
### Paper reference: Figure S2 - Percentage ###
# Calculate overall mean for each source
sources_overall_means <- sources %>%
  group_by(which_sources) %>%
  summarise(overall_mean = percentage)

# Define subgroups
sources_subgroups <- list(
  "No Depression" = df_sources %>% filter(depression == FALSE),
  "Depression" = df_sources %>% filter(depression == TRUE),
  "GAD" = df_sources %>% filter(gad == TRUE),
  "No GAD" = df_sources %>% filter(gad == FALSE),
  "No physical" = df_sources %>% filter(physical == FALSE),
  "Physical condition" = df_sources %>% filter(physical == TRUE),
  "Men" = df_sources %>% filter(gender == "Man"),
  "Women" = df_sources %>% filter(gender == "Woman"),
  "Neurodivergent" = df_sources %>% filter(neurodivergent == "Yes"),
  "Not Neurodivergent" = df_sources %>% filter(neurodivergent == "No"),
  "Workshop Sample" = df_sources %>% filter(source == "Workshop"),
  "Online Sample" = df_sources %>% filter(source == "Online")
)
# Note: look at the above dataframe to obtain sample sizes for each subgroup (nrow = sample size)

# Define the order of subgroups
subgroup_order <- c("No Depression", "Depression", "GAD", "No GAD", "No physical", "Physical condition", "Men", "Women", "Neurodivergent", "Not Neurodivergent", "Workshop Sample", "Online Sample")

# Calculate mean for each subgroup for each source
sources_subgroup_means <- bind_rows(lapply(names(sources_subgroups), function(group_name) {
  sources_subgroups[[group_name]] %>%
    mutate(which_sources = str_split(which_sources, pattern = ";")) %>%
    unnest(which_sources) %>%
    mutate(which_sources = str_trim(which_sources)) %>%
    group_by(which_sources) %>%
    summarise(subgroup_count = n()) %>%
    mutate(subgroup_mean = subgroup_count / nrow(sources_subgroups[[group_name]]) * 100) %>%
    mutate(group = group_name)
}))

# Merge overall means with subgroup means
sources_difference_scores <- sources_subgroup_means %>%
  left_join(sources_overall_means, by = "which_sources") %>%
  mutate(difference = subgroup_mean - overall_mean)

# Create a new data frame for plotting
sources_plot_data <- sources_difference_scores %>%
  select(group, which_sources, difference) %>%
  mutate(
    which_sources = factor(which_sources, levels = sources_order),
    group = factor(group, levels = subgroup_order)
  )

# Plot the difference scores
ggplot(sources_plot_data, aes(x = which_sources, y = difference, fill = group)) +
  geom_col(position = "dodge", color = "black") +
  labs(x = "Source", y = "Difference from Overall Percentage") +
  scale_x_discrete(labels = sources_label) +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(limits = c(-100, 100), expand = expansion(mult = c(0, 0)), breaks = seq(-100, 100, by = 25)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = rel(0.8)),
    legend.position = "bottom"
  )

# Statistical Tests - Fisher's Exact
# This is to evaluate the number of people in each group who did/did not select each bodily source
fisher_test_results <- bind_rows(lapply(names(sources_subgroups), function(group_name) {
  subgroup_data <- sources_subgroups[[group_name]] %>%
    mutate(which_sources = str_split(which_sources, pattern = ";")) %>%
    unnest(which_sources) %>%
    mutate(which_sources = str_trim(which_sources)) %>%
    group_by(which_sources) %>%
    summarise(subgroup_count = n())

  complementary_group_name <- subgroup_pairs[[group_name]]
  complementary_data <- sources_subgroups[[complementary_group_name]] %>%
    mutate(which_sources = str_split(which_sources, pattern = ";")) %>%
    unnest(which_sources) %>%
    mutate(which_sources = str_trim(which_sources)) %>%
    group_by(which_sources) %>%
    summarise(complementary_count = n())

  fisher_tests <- bind_rows(lapply(unique(subgroup_data$which_sources), function(source) {
    subgroup_count <- subgroup_data %>%
      filter(which_sources == source) %>%
      pull(subgroup_count)
    complementary_count <- complementary_data %>%
      filter(which_sources == source) %>%
      pull(complementary_count)

    if (length(subgroup_count) == 0) subgroup_count <- 0
    if (length(complementary_count) == 0) complementary_count <- 0

    contingency_table <- matrix(
      c(
        subgroup_count, complementary_count,
        nrow(sources_subgroups[[group_name]]) - subgroup_count,
        nrow(sources_subgroups[[complementary_group_name]]) - complementary_count
      ),
      nrow = 2
    )

    fisher_test <- fisher.test(contingency_table)

    data.frame(
      group = group_name,
      which_sources = source,
      fisher_statistic = fisher_test$estimate,
      p_value = fisher_test$p.value
    )
  }))

  return(fisher_tests)
}))

# Adjust p-values for multiple comparisons using Bonferroni correction - 9 sources
fisher_test_results <- fisher_test_results %>%
  mutate(p_value_bonferroni = pmin(p_value * 9, 1))

filter(fisher_test_results, p_value_bonferroni < 0.05)

### Paper reference: Table S4 ###
# T Tests
df_ttest_sources <- df_sources %>%
  mutate(which_sources = str_split(which_sources, pattern = ";")) %>%
  unnest(which_sources) %>%
  mutate(which_sources = str_trim(which_sources)) %>%
  group_by(id) %>%
  summarise(num_sources = n())

# Join df_ttest_sources with df to include necessary variables for filtering
df_ttest_sources <- df_ttest_sources %>%
  left_join(df, by = "id")

# Define subgroups for the number of sources analysis
sources_ttest_subgroups <- list(
  "No Depression" = df_ttest_sources %>% filter(depression == FALSE),
  "Depression" = df_ttest_sources %>% filter(depression == TRUE),
  "GAD" = df_ttest_sources %>% filter(gad == TRUE),
  "No GAD" = df_ttest_sources %>% filter(gad == FALSE),
  "No physical" = df_ttest_sources %>% filter(physical == FALSE),
  "Physical condition" = df_ttest_sources %>% filter(physical == TRUE),
  "Men" = df_ttest_sources %>% filter(gender == "Man"),
  "Women" = df_ttest_sources %>% filter(gender == "Woman"),
  "Neurodivergent" = df_ttest_sources %>% filter(neurodivergent == "Yes"),
  "Not Neurodivergent" = df_ttest_sources %>% filter(neurodivergent == "No"),
  "Workshop Sample" = df_ttest_sources %>% filter(source == "Workshop"),
  "Online Sample" = df_ttest_sources %>% filter(source == "Online")
)
# Note: look at the above dataframe to obtain sample sizes for each subgroup (nrow = sample size)

# Calculate overall mean for the number of sources selected
sources_overall_mean <- df_ttest_sources %>%
  summarise(
    overall_mean = mean(num_sources, na.rm = TRUE),
    overall_sd = sd(num_sources, na.rm = TRUE)
  )
sources_overall_mean

# Calculate mean for each subgroup for the number of sources selected
sources_ttest_means <- bind_rows(lapply(names(sources_ttest_subgroups), function(group_name) {
  sources_ttest_subgroups[[group_name]] %>%
    summarise(
      subgroup_mean = mean(num_sources, na.rm = TRUE),
      subgroup_sd = sd(num_sources, na.rm = TRUE)
    ) %>%
    mutate(group = group_name)
}))

# Run t-tests
sources_t_test_results <- run_t_tests(df_ttest_sources, sources_ttest_subgroups, subgroup_pairs, "num_sources")

# Merge t-test results with subgroup means and overall means
sources_t_test_results <- sources_t_test_results %>%
  left_join(sources_ttest_means, by = "group") %>%
  mutate(overall_mean = sources_overall_mean$overall_mean)

# Print t-test results with adjusted p-values
filter(sources_t_test_results, bm_adjusted < 0.05)
sources_t_test_results
# Note: bm_adjusted and bm_test should be identical, no correction for multiple comparisons has been performed


# Distress - Main ----------------------------------------------------------------
### Paper reference: Experience of bodily sensations - Quantitative analyses ###
# Note: None of Distress - Main needs to be run for you to run Distress - Subgroups
distress_level <- as.numeric(df$distress_level)
mean(distress_level, na.rm = TRUE)
sd(distress_level, na.rm = TRUE)


# Distress - Subgroups ---------------------------------------------------
### Paper reference: Supplementary Material 4 ###
### Paper reference: Table S4 ###
# Calculate overall mean for distress level
df_distress <- df %>%
  mutate(distress_level = as.numeric(distress_level)) %>%
  filter(!is.na(distress_level))

# Define your subgroups for distress level analysis
distress_subgroups <- list(
  "No Depression" = df_distress %>% filter(depression == FALSE),
  "Depression" = df_distress %>% filter(depression == TRUE),
  "GAD" = df_distress %>% filter(gad == TRUE),
  "No GAD" = df_distress %>% filter(gad == FALSE),
  "No physical" = df_distress %>% filter(physical == FALSE),
  "Physical condition" = df_distress %>% filter(physical == TRUE),
  "Men" = df_distress %>% filter(gender == "Man"),
  "Women" = df_distress %>% filter(gender == "Woman"),
  "Neurodivergent" = df_distress %>% filter(neurodivergent == "Yes"),
  "Not Neurodivergent" = df_distress %>% filter(neurodivergent == "No"),
  "Workshop Sample" = df_distress %>% filter(source == "Workshop"),
  "Online Sample" = df_distress %>% filter(source == "Online")
)
# Note: look at the above dataframe to obtain sample sizes for each subgroup (nrow = sample size)

distress_overall_mean <- df_distress %>%
  summarise(
    overall_mean = mean(distress_level, na.rm = TRUE),
    overall_sd = sd(distress_level, na.rm = TRUE)
  )

# Calculate mean for each subgroup for distress level
distress_subgroup_means <- bind_rows(lapply(names(distress_subgroups), function(group_name) {
  distress_subgroups[[group_name]] %>%
    summarise(
      subgroup_mean = mean(distress_level, na.rm = TRUE),
      subgroup_sd = sd(distress_level, na.rm = TRUE)
    ) %>%
    mutate(group = group_name)
}))

# Run t-tests
distress_t_test_results <- run_t_tests(df_distress, distress_subgroups, subgroup_pairs, "distress_level")

# Merge t-test results with subgroup means and overall means
distress_t_test_results <- distress_t_test_results %>%
  left_join(distress_subgroup_means, by = "group") %>%
  mutate(overall_mean = distress_overall_mean$overall_mean)

# Print t-test results with adjusted p-values
print(filter(distress_t_test_results, bm_adjusted < 0.05))
distress_t_test_results


# Interaction with Healthcare - Main: ------------------------------------------------
### Paper reference: Interactions with the healthcare system ###
# Mental health discussion
mental_discuss <- df %>%
  mutate(mental_discuss = str_trim(mental_discuss)) %>%
  filter(!is.na(mental_discuss)) %>%
  group_by(mental_discuss) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / nrow(df) * 100)

mental_discuss

# *** Choose which group you want to focus on ***
group <- "Yes - bodily signals AND ways to manage them"
#group <- "Yes - bodily signals"

# Were these discussions helpful?
df %>%
  mutate(
    mental_helpful = str_trim(mental_helpful),
    mental_discuss = str_trim(mental_discuss)
  ) %>%
  filter(mental_discuss == group) %>%
  group_by(mental_helpful) %>%
  summarise(count = n()) %>% # Can run with count or percentage
  mutate(percentage = (count / nrow(filter(df, str_trim(mental_discuss) == group))) * 100)

# Physical health discussion
physical_discuss <- df %>%
  mutate(physical_discuss = str_trim(physical_discuss)) %>%
  filter(!is.na(physical_discuss)) %>%
  group_by(physical_discuss) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / nrow(df) * 100)

physical_discuss

# Were these discussions helpful?
df %>%
  mutate(
    physical_helpful = str_trim(physical_helpful),
    physical_discuss = str_trim(physical_discuss)
  ) %>%
  filter(physical_discuss == "Yes") %>%
  group_by(physical_helpful) %>%
  summarise(count = n()) %>% # Can run with count or percentage
  mutate(percentage = (count / nrow(filter(df, str_trim(physical_discuss) == "Yes"))) * 100)


# Research Priorities - Main: ---------------------------------------------------------
### Paper reference: Research priorities ###
### Paper reference: Supplementary Material 3 ###
### Paper reference: Figure S1 ###
# Average ratings
df_priorities <- df %>%
  filter(source == "Online") %>%
  select(id, starts_with("rp"), depression, source, gad, physical, gender, neurodivergent)

# Rename columns for improved readability
# These column names are in no particular order
old_names <- colnames(df)[grep("^rp", colnames(df))]
new_names <- paste0("RP", 1:17)

rename_vector <- setNames(new_names, old_names)

df_priorities <- df_priorities %>%
  rename_with(~ rename_vector[.x], all_of(old_names))

# Convert data to long format for plotting
df_priorities_long <- df_priorities %>%
  pivot_longer(cols = starts_with("RP"), names_to = "rp", values_to = "score")

# The order of the RP's from the ranking section below
rp_order <- c("RP14", "RP15", "RP1", "RP3", "RP10", "RP13", "RP17", "RP16", "RP8", "RP11", "RP2", "RP6", "RP12", "RP9", "RP4", "RP5", "RP7")


# Research Priorities - Further -------------------------------------------
# Ensure the questions are in numeric order
df_priorities_long$rp <- factor(df_priorities_long$rp, levels = paste0("RP", 1:17))

# Add a new column to categorize questions into different groups
df_priorities_long <- df_priorities_long %>%
  mutate(group = case_when(
    rp %in% paste0("RP", 1:4) ~ "Causes", # Columns 45-48
    rp %in% paste0("RP", 5:12) ~ "Management", # Columns 49-56
    rp %in% paste0("RP", 13:17) ~ "Ways of Working" # Columns 57-61
  ))

# Calculate mean and standard deviation for each question
df_priorities_stats <- df_priorities_long %>%
  group_by(rp, group) %>%
  summarize(
    mean = mean(score, na.rm = TRUE),
    se = sd(score, na.rm = TRUE) / sqrt(length(score))
  )

ggplot(df_priorities_stats, aes(x = reorder(rp, desc(mean)), y = mean, fill = group)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = mean - se, ymax = pmin(mean + se, 10)), width = 0.2) +
  labs(
    x = "Research Priority",
    y = "Mean Score"
  ) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10, expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Causes" = "green", "Management" = "red", "Ways of Working" = "blue")) +
  theme(axis.text.x = element_text(size = rel(0.75)))

# Agreement
df_counts <- df_priorities_long %>%
  group_by(rp, group) %>%
  summarize(
    low = sum(score < 4, na.rm = TRUE),
    medium = sum(score >= 4 & score <= 6, na.rm = TRUE),
    high = sum(score > 6, na.rm = TRUE),
  ) %>%
  pivot_longer(cols = c(low, medium, high), names_to = "category", values_to = "count")

# Ensure the 'category' factor levels are ordered
df_counts$category <- factor(df_counts$category, levels = c("high", "medium", "low"))

# Order the RPs to match the above graph
df_counts <- df_counts %>% mutate(rp = factor(rp, levels = rp_order))

# Create the stacked bar chart with position = "fill"
ggplot(df_counts, aes(x = rp, y = count, fill = category)) +
  geom_bar(stat = "identity", position = "fill", colour = "black") +
  scale_fill_manual(
    values = c("low" = "red", "medium" = "blue", "high" = "green"),
    labels = c("low" = "Under 4", "medium" = "4 to 6", "high" = "Over 6")
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Research Priority", y = "Proportion", fill = "Category") +
  theme(axis.text.x = element_text(size = rel(0.8)))


# Research Priorities - Subgroup ------------------------------------------------
### Paper reference: Supplementary Material 4 ###
### Paper reference: Figure S3 ###
# Calculate overall mean for each RP
priority_overall_means <- df_priorities_long %>%
  group_by(rp) %>%
  summarize(overall_mean = mean(score, na.rm = TRUE))

# Define your subgroups
priority_subgroups <- list(
  "No Depression" = df_priorities %>% filter(depression == FALSE),
  "Depression" = df_priorities %>% filter(depression == TRUE),
  "GAD" = df_priorities %>% filter(gad == TRUE),
  "No GAD" = df_priorities %>% filter(gad == FALSE),
  "No physical" = df_priorities %>% filter(physical == FALSE),
  "Physical condition" = df_priorities %>% filter(physical == TRUE),
  "Men" = df_priorities %>% filter(gender == "Man"),
  "Women" = df_priorities %>% filter(gender == "Woman"),
  "Neurodivergent" = df_priorities %>% filter(neurodivergent == "Yes"),
  "Not Neurodivergent" = df_priorities %>% filter(neurodivergent == "No")
)
# Note: look at the above dataframe to obtain sample sizes for each subgroup (nrow = sample size)

# Define the order of subgroups
priority_subgroup_order <- c("No Depression", "Depression", "GAD", "No GAD", "No physical", "Physical condition", "Men", "Women", "Neurodivergent", "Not Neurodivergent")

# Calculate mean for each subgroup for each RP
priority_subgroup_means <- bind_rows(lapply(names(priority_subgroups), function(group_name) {
  priority_subgroups[[group_name]] %>%
    pivot_longer(cols = starts_with("RP"), names_to = "rp", values_to = "score") %>%
    group_by(rp) %>%
    summarize(
      subgroup_mean = mean(score, na.rm = TRUE),
      sd = sd(score, na.rm = TRUE)
    ) %>%
    mutate(group = group_name)
}))

# Merge overall means with subgroup means
priority_difference_scores <- priority_subgroup_means %>%
  left_join(priority_overall_means, by = "rp") %>%
  mutate(difference = subgroup_mean - overall_mean)

# Create a new data frame for plotting
# The order is based on the Main analysis
priority_plot_data <- priority_difference_scores %>%
  select(group, rp, difference) %>%
  mutate(
    rp = factor(rp, levels = rp_order),
    group = factor(group, levels = priority_subgroup_order)
  )

# Plot the difference scores
ggplot(priority_plot_data, aes(x = rp, y = difference, fill = group)) +
  geom_col(position = "dodge", color = "black") +
  labs(x = "Research Priority", y = "Difference from Overall Mean") +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(limits = c(-10, 10), expand = expansion(mult = c(0, 0)), breaks = seq(-10, 10, by = 2)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = rel(0.75)),
    legend.position = "bottom"
  )

# T Tests
priority_t_test_results <- run_t_tests(df_priorities, priority_subgroups, subgroup_pairs, rp_order)

# Merge t-test results with subgroup means and overall means
priority_t_test_results <- priority_t_test_results %>%
  rename(rp = source) %>%
  left_join(priority_subgroup_means, by = c("rp", "group")) %>%
  left_join(priority_overall_means, by = "rp")

# Print t-test results with adjusted p-values
print(filter(priority_t_test_results, bm_test < 0.05))
