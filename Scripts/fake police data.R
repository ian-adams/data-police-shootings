library(tidyverse)

generate_police_data <- function(num_obs, start_date, end_date) {
  
  # Create a sequence of dates from start_date to end_date in YYYY-MM-DD format
  dates <- seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day")
  
  # Create a data frame to store the generated data
  data <- data.frame(Date_of_Incident = rep(dates, each = num_obs),
                     Officer_ID = rep(1:num_obs, length(dates)),
                     # set gender to proportions for agency
                     Officer_Gender = sample(c("Male", "Female"), size = num_obs * length(dates), replace = TRUE, prob = c(0.85, 0.15)),
                     # set officer age to proportions for agency
                     Officer_Age = round(rnorm(num_obs * length(dates), mean = 35, sd = 5)),
                     # set officer race to proportions for agency
                     Officer_Race = sample(c("White", "Black", "Hispanic", "Asian"), size = num_obs * length(dates), replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.1)),
                     # set suspect gender to proportions desired
                     Suspect_Gender = sample(c("Male", "Female"), size = num_obs * length(dates), replace = TRUE, prob = c(0.90, 0.1)),
                     # set suspect age to proportions desired
                     Suspect_Age = round(rnorm(num_obs * length(dates), mean = 27, sd = 5)),
                     # set suspect race to proportions desired
                     Suspect_Race = sample(c("White", "Black", "Hispanic", "Asian"), size = num_obs * length(dates), replace = TRUE, prob = c(0.4, 0.4, 0.1, 0.1)),
                     # set "no force" to percentage desired 
                     Use_of_Force_Type = ifelse(runif(num_obs * length(dates)) < 0.92, "No Force", 
                                                sample(c("Physical Force", "Chemical Irritant", "Electronic Control Device", "K9 Bite"), 
                                                       size = num_obs * length(dates), replace = TRUE)))
  
  return(data)
}

# Generate fake data
set.seed(123)
police_data <- generate_police_data(num_obs = 150, start_date = "2022-01-01", end_date = "2022-12-31")

glimpse(police_data)
# Rows: 54,750
# Columns: 9
# $ Date_of_Incident  <date> 2022-01-01, 2022-01-01, 2022-01-01, 2022-01-01, 2022-01-01, 2022-01-01…
# $ Officer_ID        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, …
# $ Officer_Gender    <chr> "Male", "Male", "Male", "Female", "Female", "Male", "Male", "Female", "…
# $ Officer_Age       <dbl> 35, 31, 36, 38, 32, 32, 34, 29, 38, 34, 38, 37, 30, 30, 35, 30, 31, 40,…
# $ Officer_Race      <chr> "White", "White", "White", "White", "White", "White", "White", "White",…
# $ Suspect_Gender    <chr> "Male", "Female", "Male", "Male", "Male", "Male", "Male", "Male", "Male…
# $ Suspect_Age       <dbl> 29, 30, 26, 29, 23, 27, 33, 34, 18, 33, 20, 18, 24, 26, 23, 22, 28, 29,…
# $ Suspect_Race      <chr> "Asian", "White", "Hispanic", "White", "White", "Asian", "Black", "Blac…
# $ Use_of_Force_Type <chr> "No Force", "No Force", "Electronic Control Device", "No Force", "No Fo…


# Calculate the percentage of incidents that involve force by officer and suspect race
force_by_race <- police_data %>%
  filter(Use_of_Force_Type != "No Force") %>% # Remove "No Force" observations
  group_by(Officer_Race, Suspect_Race, Use_of_Force_Type) %>%
  summarise(n = n()) %>%
  group_by(Officer_Race, Suspect_Race) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

# Create a ggplot2 plot
ggplot(force_by_race, aes(x = Officer_Race, y = pct, fill = Suspect_Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Disparities in Use of Force by Officer and Suspect Race",
       x = "Officer Race",
       y = "Percentage of Incidents",
       fill = "Suspect Race")
