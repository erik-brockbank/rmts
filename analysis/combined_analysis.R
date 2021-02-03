
#
# Analysis for RMTS results in Brockbank, Lombrozo, Gopnik & Walker (2021)
#



# SETUP ====
#
rm(list = ls())
setwd(here::here())
library(DescTools)
library(lubridate)
library(tidyverse)


# GLOBALS ====
#

DATA_DIR = "data"
MATCH_DATA_FILE = "raw_match_data.csv"
EXPLANATION_DATA_FILE = "raw_explanation_data.csv"


# FUNCTIONS ====
#


# Read in match data
# NB: this does not read data in long form; we convert later on
read_match_data = function(data_file) {
  data = read_csv(data_file)
  data = data %>%
    rowwise() %>%
    mutate(DOB_formatted = as.POSIXlt(DOB, format = "%m/%d/%y", tz = "America/Los_Angeles"),
           Test_date_formatted = as.POSIXlt(`Testing Date`, format = "%m/%d/%y", tz = "America/Los_Angeles"),
           Age = as.numeric((difftime(Test_date_formatted, DOB_formatted, units = "days") / 365) * 12),
           Total_match = sum(`Trial 2a Match Choice`,
                             `Trial 2b Match Choice`,
                             `Trial 4a Match Choice`,
                             `Trial 4b Match Choice`,
                             `Trial 6a Match Choice`,
                             `Trial 6b Match Choice`),
           First_two_match = sum(`Trial 2a Match Choice`,
                                 `Trial 2b Match Choice`),
           Last_two_match = sum(`Trial 6a Match Choice`,
                                `Trial 6b Match Choice`))
  # following raw data here, which inputs value of 68 for missing DOB participant
  data$Age[is.na(data$Age)] = 68

  return(data)
}



# ANALYSIS: Match data ====
#

# Read data
# NB: we don't read data in long form here, but we do convert later on
match_data = read_match_data(paste(DATA_DIR, MATCH_DATA_FILE, sep = "/"))
glimpse(match_data)


# Age statistics
# TODO where is gender in raw match data?
mean(match_data$Age)
sd(match_data$Age)
range(match_data$Age)
table(match_data$Condition)
t.test(match_data$Age[match_data$Condition == "Explanation"],
       match_data$Age[match_data$Condition == "Describe"])
  # No difference in age across conditions


# Preference for relational match in each group
match_data %>%
  group_by(Condition) %>%
  summarize(mean_total_match = mean(Total_match),
            sd_total_match = sd(Total_match))

# t-tests
t.test(match_data$Total_match[match_data$Condition == "Describe"], mu = 3)
  # Describe participants significantly more likely to choose object match

t.test(match_data$Total_match[match_data$Condition == "Explanation"], mu = 3)
  # Explainers significantly more likely to choose relational match

t.test(match_data$Total_match[match_data$Condition == "Explanation"],
       match_data$Total_match[match_data$Condition == "Describe"],
       var.equal = T)
  # The two groups are significantly different in number of relational choices



# Change in performance across first and last two trials
match_data %>%
  group_by(Condition) %>%
  summarize(mean_first_two = mean(First_two_match),
            sd_first_two = sd(First_two_match),
            mean_last_two = mean(Last_two_match),
            sd_last_two = sd(Last_two_match))

# t-tests
t.test(match_data$First_two_match[match_data$Condition == "Explanation"],
       match_data$Last_two_match[match_data$Condition == "Explanation"],
       paired = T)
  # Explainers not any more likely to choose relational match in last two rounds
  # compared to first two

t.test(match_data$First_two_match[match_data$Condition == "Describe"],
       match_data$Last_two_match[match_data$Condition == "Describe"],
       paired = T)
  # Describers significantly more likely to choose relational match
  # in last two rounds compared to first two

binom.test(x = sum(match_data$`Trial 6b Match Choice`[match_data$Condition == "Describe"]),
           n = length(match_data$`Trial 6b Match Choice`[match_data$Condition == "Describe"]),
           p = 0.5)
  # Participants in the describe condition did not differ significantly from chance
  # even on the very last trial

# Difference between conditions on very first trial
match_data %>%
  group_by(Condition) %>%
  summarize(mean_first_trial = mean(`Trial 2a Match Choice`),
            sd_first_trial = sd(`Trial 2a Match Choice`))

# t-test
t.test(match_data$`Trial 2a Match Choice`[match_data$Condition == "Explanation"],
       match_data$`Trial 2a Match Choice`[match_data$Condition == "Describe"],
       equal.var = T)
  # The two conditions already show significantly different rates of choosing
  # the relational match on the very first trial



# PLOTS: Match data ====

# Re-organize match_data to long form
match_data_long = match_data %>%
  gather(trial, match_correct, `Trial 2a Match Choice`:`Trial 6b Match Choice`)

# sanity check long data
glimpse(match_data_long)
table(match_data_long$`Study ID`)

# Generate plot
trial_labels = c(
  "Trial 2a Match Choice" = "Trial 1",
  "Trial 2b Match Choice" = "Trial 2",
  "Trial 4a Match Choice" = "Trial 3",
  "Trial 4b Match Choice" = "Trial 4",
  "Trial 6a Match Choice" = "Trial 5",
  "Trial 6b Match Choice" = "Trial 6"
)

# Match percentage across trials by condition
match_data_long %>%
  group_by(Condition, trial) %>%
  summarize(correct = sum(match_correct),
            total = n(),
            prop = correct / total,
            se = sqrt((prop * (1 - prop)) / total)) %>%
  ggplot(aes(x = trial, y = prop, group = Condition)) +
  geom_point(aes(shape = Condition),
             size = 6) +
  geom_line(aes(linetype = Condition),
            size = 1) +
  geom_hline(yintercept = 0.5, linetype = "twodash") +
  geom_errorbar(aes(ymin = prop - se, ymax = prop + se),
                width = 0.25) +
  ylim(c(0, 1)) +
  scale_x_discrete(labels = trial_labels) +
  # scale_color_viridis(discrete = T) +
  # scale_color_grey(start = 0.2, end = 0.2) +
  labs(x = element_blank(),
       y = "Proportion of relational matches") +
  theme(
    # titles
    axis.title.y = element_text(face = "bold", size = 22),
    legend.title = element_blank(),
    # axis text
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold",
                               angle = 45, vjust = 0.5),
    # legend text
    legend.text = element_text(size = 16, face = "bold"),
    # backgrounds, lines
    panel.background = element_blank(),
    strip.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    # positioning
    legend.position = "bottom",
    legend.key = element_rect(colour = "transparent", fill = "transparent")
  )

# Save plot
figure_name = "match_plot.png"
ggsave(paste(here::here(), "analysis", "img", figure_name, sep = "/"),
       width = 8, height = 6, units = "in")



# ANALYSIS: Explanation data ====

# Read in coded explanation data
explanation_data = read_csv(paste(DATA_DIR, EXPLANATION_DATA_FILE, sep = "/"))


# Convert to long form
explanation_data_long = explanation_data %>%
  gather(trial, explanation_code,
         `Trial 1a Explanation OR Description Choice`:`Trial 6b Explanation OR Description Choice`)

# Sanity check conversion to long form
glimpse(explanation_data_long)


# Percentage of explanations were "2" i.e. Relational?
explanation_data_long %>%
  group_by(explanation_code) %>%
  summarize(trials = n()) %>%
  mutate(pct = trials / sum(trials))


# Calculate modal explanation type for each child
# TODO how to handle ties?
explanation_data = explanation_data %>%
  rowwise() %>%
  mutate(
    modal_explanation = Mode(
      c(`Trial 1a Explanation OR Description Choice`,
        `Trial 1b Explanation OR Description Choice`,
        `Trial 2a Explanation OR Description Choice`,
        `Trial 2b Explanation OR Description Choice`,
        `Trial 3a Explanation OR Description Choice`,
        `Trial 3b Explanation OR Description Choice`,
        `Trial 4a Explanation OR Description Choice`,
        `Trial 4b Explanation OR Description Choice`,
        `Trial 5a Explanation OR Description Choice`,
        `Trial 5b Explanation OR Description Choice`,
        `Trial 6a Explanation OR Description Choice`,
        `Trial 6b Explanation OR Description Choice`)
      )[1],  # NB: this chooses the "first" mode but doesn't handle ties
    modal_explanation_experimenter_trials = Mode(
      c(
        `Trial 1a Explanation OR Description Choice`,
        `Trial 1b Explanation OR Description Choice`,
        `Trial 3a Explanation OR Description Choice`,
        `Trial 3b Explanation OR Description Choice`,
        `Trial 5a Explanation OR Description Choice`,
        `Trial 5b Explanation OR Description Choice`
      )
    )[1],
    modal_explanation_child_trials = Mode(
      c(
        `Trial 2a Explanation OR Description Choice`,
        `Trial 2b Explanation OR Description Choice`,
        `Trial 4a Explanation OR Description Choice`,
        `Trial 4b Explanation OR Description Choice`,
        `Trial 6a Explanation OR Description Choice`,
        `Trial 6b Explanation OR Description Choice`
      )
    )[1]
  )

# Sanity check the above
table(explanation_data$modal_explanation)
table(explanation_data$modal_explanation_experimenter_trials)
table(explanation_data$modal_explanation_child_trials)


# Evaluate likelihood of selecting relational response for each modal explanation
joint_data = match_data %>%
  filter(Condition == "Explanation") %>%
  left_join(explanation_data, by = c('Study ID', 'Condition', 'Testing Date', 'DOB'))

glimpse(joint_data) # sanity check

# Mean and SD of match values (out of 6) for each modal explanation
joint_data %>%
  group_by(modal_explanation) %>%
  summarize(mean_match = mean(Total_match),
            sd_match = sd(Total_match),
            subjects = n())

# Is there a difference in match values between relational and object modal responders?
t.test(
  joint_data$Total_match[joint_data$modal_explanation == 2], # relational
  joint_data$Total_match[joint_data$modal_explanation == 1], # object
  var.equal = T # NB: t value quite different if we don't assume equal variance
)

# Is there a difference in match values between object modal responders and controls?
match_data %>%
  filter(Condition == "Describe") %>%
  group_by(Condition) %>%
  summarize(mean_match = mean(Total_match),
            sd_match = sd(Total_match),
            subjects = n())

t.test(
  joint_data$Total_match[joint_data$modal_explanation == 1], # object
  match_data$Total_match[match_data$Condition == "Describe"], # control
  var.equal = T # NB: t value pretty similar if we don't assume equal variance
)


# What behavior do we see from children who were not more likely to select object or relational?
joint_data %>%
  filter(!modal_explanation %in% c(1, 2)) %>%
  group_by(modal_explanation) %>%
  summarize(mean_match = mean(Total_match),
            sd_match = sd(Total_match),
            subjects = n())


# Did the proportion of relational explanations change over the course of the 12 trials?
explanation_pcts = explanation_data_long %>%
  filter(trial %in% c("Trial 1a Explanation OR Description Choice",
                      "Trial 6b Explanation OR Description Choice")) %>%
  group_by(trial, explanation_code) %>%
  summarize(num_explanations = n()) %>%
  group_by(trial) %>%
  mutate(total = sum(num_explanations),
         pct = num_explanations / sum(num_explanations))
explanation_pcts


tmp = explanation_pcts %>%
  mutate(other = total - num_explanations) %>%
  ungroup() %>%
  filter(explanation_code == 2) %>%
  select(num_explanations, other) %>%
  as.matrix()
tmp

chisq.test(tmp[1,], tmp[2,])


# How consistent were children's modal explanation types across experimenter and child trials?
explanation_data %>% # Percent of modal relational explanations in experimenter trials
  group_by(modal_explanation_experimenter_trials) %>%
  summarize(subjects = n()) %>%
  mutate(total = sum(subjects),
         pct = subjects / total) %>%
  filter(modal_explanation_experimenter_trials == 2)

explanation_data %>% # Percent of modal relational explanations in child trials
  group_by(modal_explanation_child_trials) %>%
  summarize(subjects = n()) %>%
  mutate(total = sum(subjects),
         pct = subjects / total) %>%
  filter(modal_explanation_child_trials == 2)



# PLOTS: Explanation data ====

explanation_code_lookup = c(
  "1" = "Object",
  "2" = "Relational",
  "3" = "Other",
  "4" = "No response"
)
explanation_code_levels = c(
  "Relational", "Object", "Other", "No response"
)

# Add columns for match values split across experimenter

# Table 1: Mean relational matches as a function of modal explanation type
# Mean of match values (out of 6) grouped by modal explanation on
# experimenter trials, child trials, and all trials combined

  # Experimenter trials
joint_data %>%
  rowwise() %>%
  mutate(`Experimenter trials` =
           factor(explanation_code_lookup[modal_explanation_experimenter_trials],
                  levels = explanation_code_levels)) %>%
  group_by(`Experimenter trials`) %>%
  summarize(
    Frequency = n(),
    `Mean Relational Matches` = round(mean(Total_match), 1))

  # Child trials
joint_data %>%
  rowwise() %>%
  mutate(`Child trials` =
           factor(explanation_code_lookup[modal_explanation_child_trials],
                  levels = explanation_code_levels)) %>%
  group_by(`Child trials`) %>%
  summarize(
    Frequency = n(),
    `Mean Relational Matches` = round(mean(Total_match), 1))

  # All trials combined
joint_data %>%
  rowwise() %>%
  mutate(`Combined trials` =
           factor(explanation_code_lookup[modal_explanation],
                  levels = explanation_code_levels)) %>%
  group_by(`Combined trials`) %>%
  summarize(
    Frequency = n(),
    `Mean Relational Matches` = round(mean(Total_match), 1))




