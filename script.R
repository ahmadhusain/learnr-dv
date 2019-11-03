# import libraries
library(shiny)
library(learnr)
library(sortable)
library(parsons)
library(tidyverse)
library(scales)
library(glue)


# aggregation
data_agg <- data_attrition %>% 
  group_by(education_field) %>% 
  summarise(n = n()) %>% 
  ungroup()

# prepare visualization data
data_viz <- data_agg %>% 
  mutate(
    education_field = education_field %>%
      str_replace_all("_", " ") %>% 
      str_to_title(),
    education_field = reorder(education_field, -n),
    n_percent = round(n / sum(n), 2),
    label = glue("{n} ({n_percent} %)")
  ) %>% 
  select(education_field, n, label)

head(data_viz)

# visualize
ggplot(data_viz, aes(x = education_field, y = n)) +
  geom_col(aes(fill = n)) +
  geom_text(aes(label = label), nudge_y = 25) +
  guides(fill = FALSE) +
  labs(
    title = "Number of employee",
    subtitle = "reported from a batch of employee by education fields",
    caption = "Source: IBM Watson",
    x = NULL,
    y = NULL
  ) +
  theme_minimal()

# visualize
ggplot(data_attrition, aes(x = monthly_income)) +
  geom_histogram(bins = 30, fill = "dodgerblue4", colour = "black") +
  scale_x_continuous(
    labels = dollar_format(scale = 1e-3, suffix = "K")
  ) +
  labs(
    title = "Number of employee by monthly income distribution",
    subtitle = "count value calculated using 30 bins",
    caption = "Source: IBM Watson",
    x = NULL,
    y = NULL
  ) +
  theme_minimal()

# prepare visualization data
data_viz <- data_attrition %>% 
  mutate(
    department = department %>%
      str_replace_all("_", " ") %>% 
      str_to_title() %>% 
      str_replace_all("Research Development", "Research & Development"),
    department = reorder(department, monthly_income),
    attrition = attrition %>% 
      str_to_title() %>% 
      factor(levels = c("Yes", "No"))
  )

head(data_viz)

# visualize
ggplot(data_viz, aes(x = department, y = monthly_income)) +
  geom_boxplot(aes(fill = attrition), outlier.shape = NA) +
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_fill_manual(values = c("#883844", "#208eb7")) +
  labs(
    title = "Monthly income distribution",
    subtitle = "for each department",
    caption = "Source: IBM Watson",
    x = NULL,
    y = NULL,
    fill = "Attrition",
    colour = "Attrition"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top"
  )

# aggregation
data_agg1 <- data_attrition %>% 
  filter(job_role == "laboratory_technician") %>% 
  group_by(years_at_company) %>% 
  summarise(monthly_income = median(monthly_income)) %>% 
  ungroup()

head(data_agg1)

# visualize
ggplot(data_agg1, aes(x = years_at_company, y = monthly_income)) +
  geom_point(colour = "dodgerblue4") +
  geom_smooth(se = FALSE, colour = "darkred") +
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  labs(
    title = "The relation of years at the company and monthly income",
    subtitle = "using median value from laboratory technician samples",
    caption = "Source: IBM Watson",
    x = NULL,
    y = NULL
  ) +
  theme_minimal()

data_agg2 <- data_attrition %>% 
  filter(job_role == "sales_executive") %>% 
  group_by(years_at_company) %>% 
  summarise(monthly_income = median(monthly_income)) %>% 
  ungroup()

head(data_agg2)

# visualize
ggplot(data_agg2, aes(x = years_at_company, y = monthly_income)) +
  geom_point(colour = "dodgerblue4") +
  geom_smooth(se = FALSE, colour = "darkred") +
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  labs(
    title = "The relation of years at the company and monthly income",
    subtitle = "using median value from sales executive samples",
    caption = "Source: IBM Watson",
    x = NULL,
    y = NULL
  ) +
  theme_minimal()