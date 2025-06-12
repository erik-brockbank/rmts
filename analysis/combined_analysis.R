
#
# Analysis for RMTS results in Brockbank, Lombrozo, Gopnik & Walker (2021)
#



# SETUP ====
#
rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(DescTools)
library(lme4)
# library(lubridate)
library(pwr)
library(tidyverse)


# GLOBALS ====
#

DATA_DIR = "../data"
MATCH_DATA_FILE = "raw_match_data.csv"
EXPLANATION_DATA_FILE = "raw_explanation_data.csv"
EXPLANATION_TEXT_FILE = "explanation_text.csv"
UPDATE_PLOT = T # toggle to save figure when running this


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
           Mid_two_match = sum(`Trial 4a Match Choice`,
                               `Trial 4b Match Choice`),
           Last_two_match = sum(`Trial 6a Match Choice`,
                                `Trial 6b Match Choice`))
  # following raw data here, which inputs value of 68 for missing DOB participant
  data$Age[is.na(data$Age)] = 68

  data = data %>%
    mutate(Condition = ifelse(Condition == 'Describe', 'Report', 'Explain'))

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
t.test(match_data$Age[match_data$Condition == "Explain"],
       match_data$Age[match_data$Condition == "Report"])
  # No difference in age across conditions


# Preference for relational match in each group
match_data %>%
  group_by(Condition) %>%
  summarize(mean_total_match = mean(Total_match),
            sd_total_match = sd(Total_match))

# t-tests
t.test(match_data$Total_match[match_data$Condition == "Report"], mu = 3)
  # Describe participants significantly more likely to choose object match

t.test(match_data$Total_match[match_data$Condition == "Explain"], mu = 3)
  # Explainers significantly more likely to choose relational match

t.test(match_data$Total_match[match_data$Condition == "Explain"],
       match_data$Total_match[match_data$Condition == "Report"],
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
t.test(match_data$First_two_match[match_data$Condition == "Explain"],
       match_data$Last_two_match[match_data$Condition == "Explain"],
       paired = T)
  # Explainers not any more likely to choose relational match in last two rounds
  # compared to first two

t.test(match_data$Last_two_match[match_data$Condition == "Report"],
       match_data$First_two_match[match_data$Condition == "Report"],
       paired = T)
  # Describers significantly more likely to choose relational match
  # in last two rounds compared to first two

binom.test(x = sum(match_data$`Trial 6b Match Choice`[match_data$Condition == "Report"]),
           n = length(match_data$`Trial 6b Match Choice`[match_data$Condition == "Report"]),
           p = 0.5)
  # Participants in the describe condition did not differ significantly from chance
  # even on the very last trial

# Difference between conditions on very first trial
match_data %>%
  group_by(Condition) %>%
  summarize(mean_first_trial = mean(`Trial 2a Match Choice`),
            sd_first_trial = sd(`Trial 2a Match Choice`))

# t-test
t.test(match_data$`Trial 2a Match Choice`[match_data$Condition == "Explain"],
       match_data$`Trial 2a Match Choice`[match_data$Condition == "Report"],
       equal.var = T)
  # The two conditions already show significantly different rates of choosing
  # the relational match on the very first trial



# PLOT: Match data ====

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
  geom_errorbar(aes(ymin = prop - (1.96 * se), ymax = prop + (1.96 * se)),
                width = 0.25) +
  # ylim(c(0, 1)) +
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
if (UPDATE_PLOT) {
  figure_name = "match_plot.png"
  ggsave(paste("img", figure_name, sep = "/"),
         width = 8, height = 6, units = "in")
}



# ANALYSIS: Explanation data ====

# Read in coded explanation data
explanation_data = read_csv(paste(DATA_DIR, EXPLANATION_DATA_FILE, sep = "/"))
explanation_data = explanation_data %>%
  mutate(Condition = ifelse(Condition == 'Describe', 'Report', 'Explain'))

# Convert to long form
explanation_data_long = explanation_data %>%
  gather(trial, explanation_code,
         `Trial 1a Explanation OR Description Choice`:`Trial 6b Explanation OR Description Choice`)

# Sanity check conversion to long form
glimpse(explanation_data_long)



# Percentage of explanations that were "2" (Relational)?
explanation_data_long %>%
  group_by(explanation_code) %>%
  summarize(trials = n()) %>%
  mutate(pct = trials / sum(trials))


# Calculate modal explanation type for each child
explanation_data = explanation_data %>%
  rowwise() %>%
  mutate(
    modal_explanation = # list(
      Mode(
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
    # )), # One subject (21) has a tie between "1" and "3"
    modal_explanation_experimenter_trials = # list(
      Mode(
      c(
        `Trial 1a Explanation OR Description Choice`,
        `Trial 1b Explanation OR Description Choice`,
        `Trial 3a Explanation OR Description Choice`,
        `Trial 3b Explanation OR Description Choice`,
        `Trial 5a Explanation OR Description Choice`,
        `Trial 5b Explanation OR Description Choice`
      )
    )[1],
    # )), # One subject (19) has a tie between "1" and "3"
    modal_explanation_child_trials = # list(
      Mode(
      c(
        `Trial 2a Explanation OR Description Choice`,
        `Trial 2b Explanation OR Description Choice`,
        `Trial 4a Explanation OR Description Choice`,
        `Trial 4b Explanation OR Description Choice`,
        `Trial 6a Explanation OR Description Choice`,
        `Trial 6b Explanation OR Description Choice`
      )
    )[1]
    # )) # No ties
  )

# Sanity check the above
explanation_data$modal_explanation
explanation_data$modal_explanation_experimenter_trials
explanation_data$modal_explanation_child_trials


# Evaluate likelihood of selecting relational response for each modal explanation
joint_data = match_data %>%
  filter(Condition == "Explain") %>%
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
  filter(Condition == "Report") %>%
  group_by(Condition) %>%
  summarize(mean_match = mean(Total_match),
            sd_match = sd(Total_match),
            subjects = n())

t.test(
  joint_data$Total_match[joint_data$modal_explanation == 1], # object
  match_data$Total_match[match_data$Condition == "Report"], # control
  var.equal = T # NB: t value pretty similar if we don't assume equal variance
)


# What match behavior do we see from children who were not more likely to select object or relational?
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

prop_data = explanation_pcts %>%
  filter(explanation_code == 2)
prop.test(x = prop_data$num_explanations, n = prop_data$total)

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



# TABLE: Explanation data ====

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
  mutate(`All trials` =
           factor(explanation_code_lookup[modal_explanation],
                  levels = explanation_code_levels)) %>%
  group_by(`All trials`) %>%
  summarize(
    Frequency = n(),
    `Mean Relational Matches` = round(mean(Total_match), 1))








# ANALYSIS: Explanation verbalizations ====
#

explanation_text = read_csv(paste(DATA_DIR, EXPLANATION_TEXT_FILE, sep = "/"))
explanation_text = explanation_text %>%
  mutate(Condition = ifelse(Condition == 'Describe', 'Report', 'Explain'))

explanation_text_long = explanation_text %>%
  gather(trial, explanation,
         `Trial 1a Explanation OR Description Choice`:`Trial 6b Explanation OR Description Choice`)


# Add in coded explanation data for comparison
explanation_text_long = explanation_text_long %>%
  inner_join(explanation_data_long, by = c('Study ID', 'Condition', 'Testing Date', 'DOB', 'trial'))

explanation_text_long = explanation_text_long %>%
  rowwise() %>%
  mutate(explanation_code_str = explanation_code_lookup[explanation_code])

# Sanity check the above
glimpse(explanation_text_long)


# What percent of relational explanations did and did not include "same-different"?
rel_str = "same|different"
rel_str_ext = "same|different|alike|similar|match"
explanation_text_long = explanation_text_long %>%
  rowwise() %>%
  mutate(contains_rel = str_detect(explanation, rel_str),
         contains_rel_expansive = str_detect(explanation, rel_str_ext))

relational_summaries = explanation_text_long %>%
  group_by(trial) %>%
  summarize(total_explanations = n(),
            total_relational = sum(explanation_code == 2), # total relational explanations
            total_contain_rel = sum(contains_rel), # total explanations with "same" or "different"
            total_contain_rel_exp = sum(contains_rel_expansive), # total explanations w/ many relational words
            # total_contain_rel_not_rel = sum(explanation_code != 2 & contains_rel), # total non-relational explanations with "s" or "d"
            total_rel_not_contain_rel = sum(explanation_code == 2 & !contains_rel), # total relational without "s" or "d"
            pct_rel_not_contain_rel = total_rel_not_contain_rel / total_relational, # p(without "s" or "d" | relational)
            total_object = sum(explanation_code == 1), # total object explanations
            total_obj_not_contain_rel = sum(explanation_code == 1 & !contains_rel), # total object without "s" or "d"
            pct_obj_not_contain_rel = total_obj_not_contain_rel / total_object
            )
glimpse(relational_summaries)

mean(relational_summaries$pct_rel_not_contain_rel)
sd(relational_summaries$pct_rel_not_contain_rel)

mean(relational_summaries$pct_obj_not_contain_rel)
sd(relational_summaries$pct_obj_not_contain_rel)

# Examples of relational explanations that did not include "same-different"
explanation_text_long %>%
  filter(explanation_code == 2, # relational explanations
         !contains_rel) %>%
  select(explanation) %>%
  as.data.frame()



# Match behavior based on "modal" use of same-different language
relational_modal = explanation_text_long %>%
  group_by(`Study ID`) %>%
  summarize(relational_count = sum(contains_rel)) %>%
  # as.data.frame()
  # NB: toggling the below to > 6 produces similar results, though not if you remove var.equal
  mutate(modal_relational = ifelse(relational_count >= 6, "relational", "non-relational"))

relational_joint = match_data %>%
  filter(Condition == "Explain") %>%
  left_join(relational_modal, by = c('Study ID'))

relational_joint %>%
  group_by(modal_relational) %>%
  summarize(mean_match = mean(Total_match),
            sd_match = sd(Total_match),
            subjects = n())

t.test(relational_joint$Total_match[relational_joint$modal_relational == "relational"],
       relational_joint$Total_match[relational_joint$modal_relational == "non-relational"],
       var.equal = T)

cor.test(relational_joint$relational_count, relational_joint$Total_match)



# On trials where participants used "same" / "different" in explanation, what percent
# chose relational response on subsequent trials?
glimpse(match_data)
glimpse(explanation_data)
glimpse(explanation_text)


explanation_text = explanation_text %>%
  rename('Trial 1a explanation' = 'Trial 1a Explanation OR Description Choice',
         'Trial 1b explanation' = 'Trial 1b Explanation OR Description Choice',
         'Trial 2a explanation' = 'Trial 2a Explanation OR Description Choice',
         'Trial 2b explanation' = 'Trial 2b Explanation OR Description Choice',
         'Trial 3a explanation' = 'Trial 3a Explanation OR Description Choice',
         'Trial 3b explanation' = 'Trial 3b Explanation OR Description Choice',
         'Trial 4a explanation' = 'Trial 4a Explanation OR Description Choice',
         'Trial 4b explanation' = 'Trial 4b Explanation OR Description Choice',
         'Trial 5a explanation' = 'Trial 5a Explanation OR Description Choice',
         'Trial 5b explanation' = 'Trial 5b Explanation OR Description Choice',
         'Trial 6a explanation' = 'Trial 6a Explanation OR Description Choice',
         'Trial 6b explanation' = 'Trial 6b Explanation OR Description Choice')
glimpse(explanation_text)

combined_wide = match_data %>%
  inner_join(explanation_data, by = c('Study ID', 'Condition', 'Testing Date', 'DOB')) %>%
  inner_join(explanation_text, by = c('Study ID', 'Condition', 'Testing Date', 'DOB'))
glimpse(combined_wide)


rel_str = "same|different"
# rel_str = "same|different|alike|similar|match"
combined_wide = combined_wide %>%
  mutate(
    'Relational 1a' = as.numeric(str_detect(`Trial 1a explanation`, rel_str)),
    'Relational 1b' = as.numeric(str_detect(`Trial 1b explanation`, rel_str)),
    'Relational 2a' = as.numeric(str_detect(`Trial 2a explanation`, rel_str)),
    'Relational 2b' = as.numeric(str_detect(`Trial 2b explanation`, rel_str)),
    'Relational 3a' = as.numeric(str_detect(`Trial 3a explanation`, rel_str)),
    'Relational 3b' = as.numeric(str_detect(`Trial 3b explanation`, rel_str)),
    'Relational 4a' = as.numeric(str_detect(`Trial 4a explanation`, rel_str)),
    'Relational 4b' = as.numeric(str_detect(`Trial 4b explanation`, rel_str)),
    'Relational 5a' = as.numeric(str_detect(`Trial 5a explanation`, rel_str)),
    'Relational 5b' = as.numeric(str_detect(`Trial 5b explanation`, rel_str)),
    'Relational 6a' = as.numeric(str_detect(`Trial 6a explanation`, rel_str)),
    'Relational 6b' = as.numeric(str_detect(`Trial 6b explanation`, rel_str))
  )
glimpse(combined_wide)


combined_wide = combined_wide %>%
  # rowwise %>%
  mutate(
    rel2a = (`Relational 1b` & `Trial 2a Match Choice`), # 1B contains "s" or "d" and participants chose correctly on 2A
    nonrel2a = (!`Relational 1b` & `Trial 2a Match Choice`), # 1B *does not* contain "s" or "d" and participants chose correctly on 2A
    rel2b = (`Relational 2a` & `Trial 2b Match Choice`),
    nonrel2b = (!`Relational 2a` & `Trial 2b Match Choice`),
    rel4a = (`Relational 3b` & `Trial 4a Match Choice`),
    nonrel4a = (!`Relational 3b` & `Trial 4a Match Choice`),
    rel4b = (`Relational 4a` & `Trial 4b Match Choice`),
    nonrel4b = (!`Relational 4a` & `Trial 4b Match Choice`),
    rel6a = (`Relational 5b` & `Trial 6a Match Choice`),
    nonrel6a = (!`Relational 5b` & `Trial 6a Match Choice`),
    rel6b = (`Relational 6a` & `Trial 6b Match Choice`),
    nonrel6b = (!`Relational 6a` & `Trial 6b Match Choice`)
  )

# How accurate were participants on the subsequent trial when the previous one did *not* include "s" or "d"?
trial_rel_accuracy = c(
  sum(combined_wide$`rel2a`) / sum(combined_wide$`Relational 1b`), # p(2A correct & 1B "s"/"d" | 1B "s"/"d")
  sum(combined_wide$`rel2b`) / sum(combined_wide$`Relational 2a`),
  sum(combined_wide$`rel4a`) / sum(combined_wide$`Relational 3b`),
  sum(combined_wide$`rel4b`) / sum(combined_wide$`Relational 4a`),
  sum(combined_wide$`rel6a`) / sum(combined_wide$`Relational 5b`),
  sum(combined_wide$`rel6b`) / sum(combined_wide$`Relational 6a`)
)

trial_rel_accuracy
mean(trial_rel_accuracy)
sd(trial_rel_accuracy)

# How accurate were people on the subsequent trial when the previous one did *not* include "s" or "d"?
trial_nonrel_accuracy = c(
  sum(combined_wide$`nonrel2a`) / sum(!combined_wide$`Relational 1b`), # p(2A correct & *no* 1B "s"/"d" | *no* 1B "s"/"d")
  sum(combined_wide$`nonrel2b`) / sum(!combined_wide$`Relational 2a`),
  sum(combined_wide$`nonrel4a`) / sum(!combined_wide$`Relational 3b`),
  sum(combined_wide$`nonrel4b`) / sum(!combined_wide$`Relational 4a`),
  sum(combined_wide$`nonrel6a`) / sum(!combined_wide$`Relational 5b`),
  sum(combined_wide$`nonrel6b`) / sum(!combined_wide$`Relational 6a`)
)
trial_nonrel_accuracy
mean(trial_nonrel_accuracy)
sd(trial_nonrel_accuracy)


# Is the average difference significant?
t.test(c(sum(combined_wide$`rel2a`), sum(combined_wide$`rel2b`), sum(combined_wide$`rel4a`),
         sum(combined_wide$`rel4b`), sum(combined_wide$`rel6a`), sum(combined_wide$`rel6b`)),
       c(sum(combined_wide$`nonrel2a`), sum(combined_wide$`nonrel2b`), sum(combined_wide$`nonrel4a`),
         sum(combined_wide$`nonrel4b`), sum(combined_wide$`nonrel6a`), sum(combined_wide$`nonrel6b`)),
       paired = T)



# Mixed effects model to look at effect of same-different language on match behavior

TRIAL_LOOKUP = c(
  "Trial 1a Explanation OR Description Choice" = 1,
  "Trial 1b Explanation OR Description Choice" = 2,
  "Trial 2a Explanation OR Description Choice" = 3,
  "Trial 2b Explanation OR Description Choice" = 4,
  "Trial 3a Explanation OR Description Choice" = 5,
  "Trial 3b Explanation OR Description Choice" = 6,
  "Trial 4a Explanation OR Description Choice" = 7,
  "Trial 4b Explanation OR Description Choice" = 8,
  "Trial 5a Explanation OR Description Choice" = 9,
  "Trial 5b Explanation OR Description Choice" = 10,
  "Trial 6a Explanation OR Description Choice" = 11,
  "Trial 6b Explanation OR Description Choice" = 12
)

explanation_text_long = explanation_text_long %>%
  rowwise() %>%
  mutate(trial_number = TRIAL_LOOKUP[trial])
# sanity check
table(explanation_text_long$trial_number)


MATCH_TRIAL_LOOKUP = c(
    "Trial 2a Match Choice" = 3,
    "Trial 2b Match Choice" = 4,
    "Trial 4a Match Choice" = 7,
    "Trial 4b Match Choice" = 8,
    "Trial 6a Match Choice" = 11,
    "Trial 6b Match Choice" = 12
)

match_data_long = match_data_long %>%
  rowwise() %>%
  mutate(trial_number = MATCH_TRIAL_LOOKUP[trial])
# sanity check
table(match_data_long$trial_number)

combined_data_long = explanation_text_long %>% # 276 rows = 23 explainers x 12 trials
    inner_join(match_data_long, # 276 rows = 46 subjects x 6 trials
               by = c('Study ID', 'Condition', 'Testing Date', 'DOB', 'trial_number'))
glimpse(combined_data_long) # 138 rows = 23 explainers x 6 trials


# Null: match behavior as random subject variation
m00 = glmer(match_correct ~ (1 | `Study ID`),
            data = combined_data_long,
            family="binomial")

# Alt: match behavior predicted by relational language
m0 = glmer(match_correct ~ contains_rel + (1 | `Study ID`),
           data = combined_data_long,
           family="binomial")
anova(m00, m0, test="LRT")
summary(m0)

m1 = glmer(match_correct ~ contains_rel_expansive + (1 | `Study ID`),
           data = combined_data_long,
           family="binomial")
anova(m00, m1)
summary(m1)



# ANALYSIS: Power calculations ====
#

#' The most relevant comparison from Christie & Gentner (2014) is experiment 4,
#' since this one uses a labeling intervention that might be similar to explanation

#' Christie & Gentner (2014) Expt. 4 report the following effect sizes by age
#' when comparing to chance:
#' age 2: d=.83
#' age 3: d=1.01
#' age 4: d=1.38
#'
#' The actual percentage values compared to E1 as baseline (rather than chance):
#' age 2: E4 M=.65 E1 M=.55
#' age 3: E4 M=.71 E1 M=.56
#' age 4: E4 M=.79 E1 M=.65


#' Christie & Gentner (2007) compares labeling in Expt. 2 compared to E1 as baseline:
#' age 4.5: E2 M=.68 (n.s. compared to chance) E1 M=.17
#' age 8.5: E2 M=.84 E1 M=.34


# Comparison to Christie & Gentner (2014) E4 with 4 year olds
# Since our experiment had 23 participants, we evaluate the power to detect a similar
# effect size (d=1.38)
pwr.t.test(d = 1.38, sig.level = 0.05, n = 23)




# Walker, Bridgers, & Gopnik (2016), Experiment 3
# age: 3-4 year-olds
# N = 48, explain 24, describe 24
# explain: p = .79 relational choice; describe: p = .42 relational choice (chance = .5)

# replicating stats:
binom.test(x=round(.79*24), n = 24, p = .5) # explainers
binom.test(x=round(.42*24), n = 24, p = .5) # describers

# power analysis
pwr.p.test(h = ES.h(.79, .5), sig.level = .05, power = 0.8) # we need n participants to have 80% power
pwr.p.test(h = ES.h(.79, .5), sig.level = .05, n = 23) # this is our power with 23 participants




