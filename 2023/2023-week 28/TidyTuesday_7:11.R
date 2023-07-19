



library(tidyverse)
library(viridis)




# Read the data from the provided URL
global_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv')

# Calculate the annual mean temperature anomalies
global_temps$Annual_Mean <- rowMeans(global_temps[, 2:13], na.rm = TRUE)

# Plotting global temperature anomalies over the years with a gradient line chart
ggplot(global_temps, aes(x = Year, y = Annual_Mean, color = Annual_Mean)) +
  geom_line(size = 2, lineend = "round") +
  scale_color_viridis_c(option = "magma", direction = 1) +
  scale_x_continuous(breaks = seq(1880, 2020, 20)) +
  labs(x = "Year", y = "Annual Mean Temperature Anomaly (°C)",
       title = "Global Surface Temperature Trend",
       subtitle = "Annual Mean Temperature Anomalies from 1951-1980",
       caption = "Source: GISTEMP Team, 2023: GISS Surface Temperature Analysis (GISTEMP), v4. NASA Goddard Institute for Space Studies") +
  theme_minimal(base_family = "Arial") +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 9, hjust = 0),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.minor.y = element_line(color = "gray95"),
        panel.background = element_rect(fill = "gray98"),
        plot.margin = margin(20, 20, 20, 20))





# Convert Month to a factor with all months in order
heatmap_data$Month <- factor(heatmap_data$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Plotting the heatmap with uniform months
ggplot(heatmap_data, aes(x = Month, y = Year, fill = Temperature_Anomaly)) +
  geom_tile() +
  scale_fill_viridis(option = "magma", direction = 1) +
  labs(x = "Month", y = "Year",
       title = "Global Surface Temperature Anomalies",
       subtitle = "Monthly Temperature Anomalies from 1951-1980",
       caption = "Source: GISTEMP Team, 2023: GISS Surface Temperature Analysis (GISTEMP), v4. NASA Goddard Institute for Space Studies") +
  theme_minimal() +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 9, hjust = 0),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "right",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "gray98"),
        plot.margin = margin(20, 20, 20, 20))


