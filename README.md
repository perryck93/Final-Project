# Final-Project
#Final Project for Data Vis
library(tidyverse)
library(janitor)
library(ggplot2)
library(scales)

cd_demo <- read_csv ("Combined NYC Community District Demographics.csv")

cd_demo_clean <- clean_names (cd_demo)


# Creating scatterplots to analyze if there are correlations between demographics and people per ramp

# unemployment rate
# Clean the unemployment_rate column since it's in percentage format
clean_unemployment_rate <- as.numeric(gsub("%", "", cd_demo_clean$unemployment_rate))
clean_unemployment_rate <- clean_unemployment_rate[!is.na(clean_unemployment_rate)]
clean_unemployment_rate <- clean_unemployment_rate / 100

# Plot with breaks every 5% on the x-axis, formatting x-axis labels as percentages and add a trend line
ggplot(cd_demo_clean, aes(x = clean_unemployment_rate, y = people_per_ramp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add a trend line
  labs(
    title = "People per Ramp vs. Unemployment Rate",
    x = "Unemployment Rate",
    y = "People per Ramp"
  ) +
  scale_x_continuous(
    breaks = seq(min(clean_unemployment_rate), max(clean_unemployment_rate), by = 0.05),
    labels = percent_format()
  )
##theme(
  ##panel.background = element_rect(fill = "light blue")  # Change the background color
##)


# median_household_income
ggplot(cd_demo_clean, aes(x = median_household_income_dollars, y = people_per_ramp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear trend line without confidence interval
  scale_x_continuous(labels = dollar_format()) +  # Format x-axis labels as dollars
  labs(
    title = "People per Ramp vs. Median Household Income",
    x = "Median Household Income",
    y = "People per Ramp"
  )

# median_age
ggplot(cd_demo_clean, aes(x = median_age, y = people_per_ramp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear trend line without confidence interval
  labs(
    title = "People per Ramp vs. Median Age",
    x = "Median Age",
    y = "People per Ramp"
  )



# Education
ggplot(cd_demo_clean, aes(x = population_with_ged_or_higher, y = people_per_ramp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear trend line without confidence interval
  labs(
    title = "People per Ramp vs. % Population with GED or Higher",
    x = "% Population with GED or Higher",
    y = "People per Ramp"
  )

# % POC Population
# Clean the percent_poc column since it's in percentage format
clean_percent_poc <- as.numeric(gsub("%", "", cd_demo_clean$percent_poc))
clean_percent_poc <- clean_percent_poc[!is.na(clean_percent_poc)]
clean_percent_poc <- clean_percent_poc / 100

# Plot with breaks every 5% on the x-axis, formatting x-axis labels as percentages
ggplot(cd_demo_clean, aes(x = clean_percent_poc, y = people_per_ramp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear trend line without confidence interval
  labs(
    title = "People per Ramp vs. % POC of Population",
    x = "% POC of Population",
    y = "People per Ramp"
  ) +
  scale_x_continuous(
    breaks = seq(min(clean_percent_poc), max(clean_percent_poc), by = 0.05),
    labels = percent
  )

