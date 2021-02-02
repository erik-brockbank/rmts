
#
# Analysis for RMTS results in Brockbank, Lombrozo, Gopnik & Walker (2021)
#



# SETUP ====
#
rm(list = ls())
setwd(here::here())
library(lubridate)
library(tidyverse)


# GLOBALS ====
#

DATA_FILE = "raw_match_data.csv"
DATA_DIR = "data"

# ANALYSIS FUNCTIONS ====
#

# Read in data
read_data = function(data_file) {
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



# ANALYSIS ====
#

data = read_data(paste(DATA_DIR, DATA_FILE, sep = "/"))
glimpse(data)


# Age statistics
# TODO where is gender in raw data?
mean(data$Age)
sd(data$Age)
range(data$Age)
table(data$Condition)
t.test(data$Age[data$Condition == "Explanation"],
       data$Age[data$Condition == "Describe"])
  # No difference in age across conditions


# Preference for relational match in each group
data %>%
  group_by(Condition) %>%
  summarize(mean_total_match = mean(Total_match),
            sd_total_match = sd(Total_match))

# t-tests
t.test(data$Total_match[data$Condition == "Describe"], mu = 3)
  # Describe participants significantly more likely to choose object match

t.test(data$Total_match[data$Condition == "Explanation"], mu = 3)
  # Explainers significantly more likely to choose relational match

t.test(data$Total_match[data$Condition == "Explanation"],
       data$Total_match[data$Condition == "Describe"],
       var.equal = T)
  # The two groups are significantly different in number of relational choices



# Change in performance across first and last two trials
data %>%
  group_by(Condition) %>%
  summarize(mean_first_two = mean(First_two_match),
            sd_first_two = sd(First_two_match),
            mean_last_two = mean(Last_two_match),
            sd_last_two = sd(Last_two_match))

# t-tests
t.test(data$First_two_match[data$Condition == "Explanation"],
       data$Last_two_match[data$Condition == "Explanation"],
       paired = T)
  # Explainers not any more likely to choose relational match in last two rounds
  # compared to first two

t.test(data$First_two_match[data$Condition == "Describe"],
       data$Last_two_match[data$Condition == "Describe"],
       paired = T)
  # Describers significantly more likely to choose relational match
  # in last two rounds compared to first two

binom.test(x = sum(data$`Trial 6b Match Choice`[data$Condition == "Describe"]),
           n = length(data$`Trial 6b Match Choice`[data$Condition == "Describe"]),
           p = 0.5)
  # Participants in the describe condition did not differ significantly from chance
  # even on the very last trial

# Difference between conditions on very first trial
data %>%
  group_by(Condition) %>%
  summarize(mean_first_trial = mean(`Trial 2a Match Choice`),
            sd_first_trial = sd(`Trial 2a Match Choice`))

# t-test
t.test(data$`Trial 2a Match Choice`[data$Condition == "Explanation"],
       data$`Trial 2a Match Choice`[data$Condition == "Describe"],
       equal.var = T)
  # The two conditions already show significantly different rates of choosing
  # the relational match on the very first trial



# Generate plot
# Re-organize data
trial_labels = c(
  "Trial 2a Match Choice" = "Trial 1",
  "Trial 2b Match Choice" = "Trial 2",
  "Trial 4a Match Choice" = "Trial 3",
  "Trial 4b Match Choice" = "Trial 4",
  "Trial 6a Match Choice" = "Trial 5",
  "Trial 6b Match Choice" = "Trial 6"
)
data = data %>%
  gather(trial, match_correct, `Trial 2a Match Choice`:`Trial 6b Match Choice`)

glimpse(data)
table(data$`Study ID`)


# Plots ====

# Match percentage across trials by condition
data %>%
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




