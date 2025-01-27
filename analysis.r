library(openxlsx)
library(here)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(janitor)
library(here)

# These initial steps have to be done every time
# After that, each section can be run independently

# Read in data
df_all <- read.xlsx(here("data", "merged", "All data_merged.xlsx"), na.strings = c("", "NA"))

# Remove T11: No mental health conditions
df_all <- df_all[df_all$id != 'T11', ]

df_workshop <- df_all[df_all$Source == "Workshop", ]
df_online <- df_all[df_all$Source == "Online", ]

# choose which dataset to analyse
df <- df_all
#df <- df_workshop
#df <- df_online


# Descriptives: ----------------------------------------------------------------
# Table of contributor descriptives: Age, Gender, Ethnicity, Mental Health Diagnosis, Number of Diagnoses
# Age
summary(df$age)
sd(df$age, na.rm = TRUE)

# Gender
df %>%
  group_by(gender) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Ethnicity
df %>%
  group_by(ethnicity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(desc(count))

# Diagnoses
df %>%
  mutate(diagnosis = str_replace_all(diagnosis, " \\(.*?\\)", "")) %>%
  mutate(diagnosis = str_split(diagnosis, pattern = ",")) %>%
  unnest(diagnosis) %>%
  group_by(diagnosis) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / nrow(df) * 100) %>%
  arrange(desc(count))

# Number of diagnoses
df %>%
  mutate(diagnosis = str_split(diagnosis, pattern = ",")) %>%
  unnest(diagnosis) %>%
  group_by(id) %>%
  summarise(num_diagnoses = n()) %>%
  ungroup() %>%
  count(num_diagnoses) %>%
  mutate(percentage = n / nrow(df) * 100)

# Neurodivergence
df %>%
  mutate(neurodivergent_specific = str_replace_all(neurodivergent_specific, " \\(.*?\\)", "")) %>%
  mutate(neurodivergent_specific = str_split(neurodivergent_specific, pattern = ",")) %>%
  unnest(neurodivergent_specific) %>%
  group_by(neurodivergent_specific) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / nrow(df) * 100) %>%
  arrange(desc(count))

# Number of neurodiversities
df %>%
  mutate(neurodivergent_specific = str_split(neurodivergent_specific, pattern = ",")) %>%
  unnest(neurodivergent_specific) %>%
  group_by(id) %>%
  summarise(num_neurodiversities = n()) %>%
  ungroup() %>%
  count(num_neurodiversities) %>%
  mutate(percentage = n / nrow(df) * 100)

# Number of physical health conditions (use for count data)
df %>%
  mutate(physical.x = str_split(physical.x, pattern = ",")) %>%
  unnest(physical.x) %>%
  group_by(physical.x) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / nrow(df) * 100) %>%
  arrange(desc(count))

# Percentage of people with a physical health condition in each category
# This removes duplicates because some people had, say, 3 gastrointenstinal conditions, which skews the percentage figure
df %>%
  mutate(physical.x = str_split(physical.x, pattern = ",")) %>%
  mutate(physical.x = lapply(physical.x, function(x) unique(trimws(x)))) %>%
  unnest(physical.x) %>%
  group_by(physical.x) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / nrow(df) * 100) %>%
  arrange(desc(count))

# Number of physical diagnoses
df %>%
  mutate(physical.x = str_split(physical.x, pattern = ",")) %>%
  unnest(physical.x) %>%
  group_by(id) %>%
  summarise(num_physical = n()) %>%
  ungroup() %>%
  count(num_physical) %>%
  mutate(percentage = n / nrow(df) * 100)

# Physical other
df %>%
  filter(!is.na(physical_details)) %>%
  mutate(physical_details = str_split(physical_details, pattern = ",")) %>%
  unnest(physical_details) %>%
  group_by(physical_details) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / nrow(df) * 100) %>%
  print(n = Inf)


# Body sources: ----------------------------------------------------------------
# Which are important
sources <- df %>%
  mutate(which_sources = str_split(which_sources, pattern = ";")) %>%
  unnest(which_sources) %>%
  mutate(which_sources = str_trim(which_sources)) %>%
  filter(!is.na(which_sources)) %>%
  group_by(which_sources) %>%
  summarise(selected = n())

# Some people didn't select Other but wrote something in the freeform
num_others <- df %>% filter(!is.na(which_sources_other)) %>% nrow()

sources %<>%
  mutate(selected = ifelse(which_sources == "Other", num_others, selected)) %>%
  mutate(percentage_of_people = selected / 71 * 100) %>%
  arrange(desc(selected)) %>%
  print()

# What Other sources did people select?
df %>%
  filter(!is.na(which_sources_other)) %>%
  mutate(which_sources_other = str_split(which_sources_other, pattern = ",")) %>%
  unnest(which_sources_other) %>%
  group_by(which_sources_other) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / 71 * 100) %>%
  print(n = Inf)

# How many did people select?
df %>%
    mutate(which_sources = str_split(which_sources, pattern = ";")) %>%
    unnest(which_sources) %>%
    mutate(which_sources = str_trim(which_sources)) %>%
    filter(!is.na(which_sources)) %>%
    group_by(id) %>%
    summarise(num_sources = n()) %>%
    #summary() # Hide this line for a different summary
    ungroup() %>%
    count(num_sources) %>%
    mutate(percentage = n / sum(n) * 100)

# Groups are ordered based on the Importance score below
source_order <- c("Heartbeat", "Stomach", "Breathing", "Muscle tension", "Body temperature", "Other", "Hunger", "Thirst", "Bladder")
sources <- sources %>% mutate(which_sources = factor(which_sources, levels = source_order))
source_label <- c("Heartbeat", "Stomach", "Breathing", "Muscle\nTension", "Body\nTemperature", "Other", "Hunger", "Thirst", "Bladder")

# Graph number of sources selected
ggplot(sources, aes(x = which_sources, y = percentage_of_people, fill = which_sources)) +
  geom_col(color = "black") +
  labs(x = "Source", y = "Percentage of Contributors") +
  scale_x_discrete(labels = source_label) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(axis.text.x = element_text(size = rel(0.8)),
        legend.position = "none")

# Ranking of sources: ---------------------------------------------------------
rank_df  <- df %>%
  select(id, which_sources, rank_sources)

# Remove the option "None"
rank_df <- mutate(rank_df, 
                  rank_sources = str_remove_all(rank_sources, "; None"),
                  which_sources = str_remove_all(which_sources, "; None"))

# Ranked sources should only be those selected in which sources
rank_df <- rank_df %>% 
    rowwise() %>% 
    mutate(rank_sources = list(str_split(rank_sources, pattern = ";") %>% unlist() %>% str_trim()),
    which_sources = list(str_split(which_sources, pattern = ";") %>% unlist() %>% str_trim()),
    rank_sources = rank_sources[rank_sources %in% which_sources] %>% str_c(collapse = "; ")) %>% 
    ungroup()

options <- c("Heartbeat", "Breathing", "Stomach", "Bladder", "Hunger", "Thirst", "Body temperature", "Muscle tension", "Other")

# Create empty columns for each source
for (source in options){
    rank_df[[source]] <- NA
}

# Remove any rows with NA in rank_sources
rank_df <- rank_df %>% drop_na(rank_sources)

# Run through the sources each person has ranked and assign each a value based on it's order in the ranking
for (i in 1:nrow(rank_df)){
    current_sources  <- rank_df[i, "rank_sources"] %>% str_split(pattern = ";") %>% unlist() %>% str_trim()

    # There were nine total sources, so the highest rank source is assigned nine
    for (j in 1:length(current_sources)){
        rank_df[i, current_sources[j]] <- 10 - j
    }
}

# Fill all NAs with 0
rank_df[is.na(rank_df)] <- 0

# Clean up names
rank_df <- rank_df %>%
    clean_names() %>%
    select(-c(id, which_sources, rank_sources))

# Get the data into shape to be put into a figure
rank_long <- rank_df %>%
    pivot_longer(cols = everything(), names_to = "group", values_to = "value")

rank_summary <- rank_long %>%
    group_by(group) %>%
    summarise(
        mean = mean(value, na.rm = TRUE),
        se = sd(value, na.rm = TRUE) / sqrt(n())
    )

# Order the sources
source_order <- c("heartbeat", "stomach", "breathing", "muscle_tension", "hunger", "body_temperature", "other", "thirst", "bladder")
rank_summary <- rank_summary %>% mutate(group = factor(group, levels = source_order))

label <- c(
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

ggplot(rank_summary, aes(x = group, y = mean, fill = group)) +
    geom_col(color = "black") +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
    labs(x = "Group", y = "Mean Value") +
    scale_x_discrete(labels = label) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme(axis.text.x = element_text(size = rel(0.8)),
          legend.position = "none")


# Distress ----------------------------------------------------------------
# Summarize the data to get counts for each distress level
df_summary <- df %>%
  mutate(distress_level = str_trim(distress_level),
         distress_level = as.numeric(distress_level)) %>%
  count(distress_level)

ggplot(df_summary, aes(x = distress_level, y = n, fill = distress_level)) +
  geom_col(color = "black") +
  scale_fill_gradient(low = "green", high = "red") +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  labs(x = "Distress Level",
       y = "Number of Contributors") +
  theme(legend.position = "none")

distress_level <- as.numeric(df$distress_level)
mean(distress_level, na.rm = TRUE)
sd(distress_level, na.rm = TRUE)


# Interaction with healthcare: ------------------------------------------------
# Mental health discussion
mental_discuss <- df %>%
  mutate(mental_discuss = str_trim(mental_discuss)) %>%
  filter(!is.na(mental_discuss)) %>%
  group_by(mental_discuss) %>%
  summarise(count = n()) %>%
  arrange(mental_discuss)

mental_discuss

# Were these discussions helpful?
df %>%
  mutate(mental_helpful = str_trim(mental_helpful),
        mental_discuss = str_trim(mental_discuss)) %>%
  filter(mental_discuss == "Yes - bodily signals AND ways to manage them") %>%
  group_by(mental_helpful) %>%
  summarise(count = n()) #%>% # Can do it with count or percentage
  #mutate(percentage = (count / 25) * 100)

df %>%
  mutate(mental_helpful = str_trim(mental_helpful),
         mental_discuss = str_trim(mental_discuss)) %>%
  filter(mental_discuss %in% c("Yes - bodily signals"))%>%
  group_by(mental_helpful) %>%
  summarise(count = n()) #%>% # Can do it with count or percentage
  #mutate(percentage = (count / 19) * 100)

# Physical health discussion
physical_discuss <- df %>%
  mutate(physical_discuss = str_trim(physical_discuss)) %>%
  filter(!is.na(physical_discuss)) %>%
  group_by(physical_discuss) %>%
  summarise(count = n())

physical_discuss

# Were these discussions helpful?
df %>%
  mutate(physical_helpful = str_trim(physical_helpful),
         physical_discuss = str_trim(physical_discuss)) %>%
  filter(physical_discuss == "Yes") %>%
  group_by(physical_helpful) %>%
  summarise(count = n()) #%>% # Can do it with count or percentage
  #mutate(percentage = (count / 38) * 100)


# Research priorities: ---------------------------------------------------------
# Average ratings
# Select columns 2 (Participant ID) and 45-61 for plotting
df_subset <- df_online %>%
  select(2, 45:61)

# Rename columns for better readability
# RPs are named here based on column order, not any particular ranking
colnames(df_subset) <- c("id", paste0("RP", 1:17))

# Convert data to long format for plotting
df_long <- df_subset %>%
  pivot_longer(cols = -id, names_to = "rp", values_to = "score")

# Ensure the questions are in numeric order
df_long$rp <- factor(df_long$rp, levels = paste0("RP", 1:17))

# Add a new column to categorize questions into different groups
df_long <- df_long %>%
  mutate(group = case_when(
    rp %in% paste0("RP", 1:4) ~ "Causes",     # Columns 45-48
    rp %in% paste0("RP", 5:12) ~ "Management",    # Columns 49-56
    rp %in% paste0("RP", 13:17) ~ "Ways of Working"    # Columns 57-61
  ))

# Calculate mean and standard deviation for each question
df_stats <- df_long %>%
  group_by(rp, group) %>%
  summarize(
    mean = mean(score, na.rm = TRUE),
    se = sd(score, na.rm = TRUE) / sqrt(length(score))
  )

ggplot(df_stats, aes(x = reorder(rp, desc(mean)), y = mean, fill = group)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = mean - se, ymax = pmin(mean + se, 10)), width = 0.2) +
  labs(x = "Research Priority",
       y = "Mean Score") +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10, expand = expansion(mult = c(0, 0.05))) + 
  scale_fill_manual(values = c("Causes" = "green", "Management" = "red", "Ways of Working" = "blue")) +
  theme(axis.text.x = element_text(size = rel(0.75)))
            
# Agreement
df_counts <- df_long %>%
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
rp_order <- c("RP14", "RP15", "RP1", "RP3", "RP10", "RP13", "RP17", "RP16", "RP8", "RP11", "RP2", "RP6", "RP12", "RP9", "RP4", "RP5", "RP7")
df_counts <- df_counts %>% mutate(rp = factor(rp, levels = rp_order))

# Create the stacked bar chart with position = "fill"
ggplot(df_counts, aes(x = rp, y = count, fill = category)) +
  geom_bar(stat = "identity", position = "fill", colour = "black") +
  scale_fill_manual(values = c("low" = "red", "medium" = "blue", "high" = "green"),
                    labels = c("low" = "Under 4", "medium" = "4 to 6", "high" = "Over 6")) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Research Priority", y = "Proportion", fill = "Category") +
  theme(axis.text.x = element_text(size = rel(0.8)))

