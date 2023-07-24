#loading library
library(tidyverse)
library(patchwork)

# Read the data from the provided URL
detector <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv')


#Adding fonts for plots and axis 
my_theme <- function() {
  theme_minimal() +                        
    theme(
      plot.title = element_text(hjust=0.5, vjust = 1, size = 16, family = "Proxima Nova Semibold"),
      axis.text.x = element_text(vjust=0.1, hjust = 0.1,family = "Proxima Nova"),
      axis.text.y = element_text(vjust=0.1, hjust = 0.1,family = "Proxima Nova"),
      axis.title.x = element_text(family = "Proxima Nova Bold"),
      axis.title.y = element_text(family = "Proxima Nova Bold"),
    )
}


#viewing dataset and structure
view(detector)
names(detector)
str(detector)



# Rounding .pred_AI to 3 decimal places
detector$.pred_AI <- round(detector$.pred_AI, 3)

# Filter rows where 'kind' is 'Human'
detector_human <- filter(detector, kind == "Human")

# Create a new column 'is_correct' to indicate if the classification is correct or not
detector_human$is_correct <- ifelse(detector_human$kind == detector_human$.pred_class, 'Yes', 'No')

#  Classification Rate by Native Status
classified <- detector_human %>%
  group_by(native) %>%
  summarise(total = n(),
            correct = sum(is_correct == "Yes"),
            rate = round(correct/total, digits = 4))

# Plot the classification rate by 'native' status
plot1 <- ggplot(classified, aes(x = native, y = rate, fill = native)) +
  geom_col(position = "dodge", width = 0.4) +
  labs(title = "Classification Rate by Native Status",
       x = "Native Status",
       y = "Correct Classification Percentage") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  scale_fill_viridis_d(option = "C", begin=0.01, end = 0.9) +
  theme_minimal() +  geom_text(aes(label = paste0(rate*100, "%")), vjust = -0.5) 
plot1

# Incorrect Classification Rate by Detector 
incorrect_classified <- detector_human %>%
  group_by(detector) %>%
  summarise(total = n(),
            incorrect = sum(is_correct == "No"),
            rate_in = round(incorrect/total, digits = 4))

# Calculate the labels outside ggplot
incorrect_classified <- incorrect_classified %>%
  mutate(labels = scales::percent(rate_in, accuracy = 0.01))

# Create the plot for incorrect classification rate by 'detector'
plot2 <- ggplot(incorrect_classified, aes(x = reorder(detector, rate_in), y = rate_in, fill = detector)) +
  geom_col(position = "dodge", width = 0.6) +
  labs(title = "Incorrect Classification Rate by Detector",
       x = "Detectors",
       y = "Incorrect Classification Percentage") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  scale_fill_viridis_d(option = "C", begin=0.01, end = 0.9) +
  theme_minimal() +  geom_text(aes(label = paste0(rate_in*100, "%")), vjust = -0.5) 
plot2

# Incorrect Classification Rate by Detector for Non-Natives
non_native <- detector_human %>%
  filter(native == "No")

errors <- non_native %>%
  group_by(detector) %>%
  summarise(total = n(),
            incorrect = sum(is_correct == "No"),
            rate = round(incorrect/total, digits = 4)) %>%
  mutate(labels = scales::percent(rate, accuracy = 0.01))

errors <- errors %>%
  arrange(rate)

plot3 <- ggplot(errors, aes(x = reorder(detector, rate), y = rate, fill = detector)) +
  geom_col(position = "dodge", width = 0.6) +
  labs(title = "Incorrect Classification Rate by Detector for Non-Natives",
       x = "Detectors",
       y = "Incorrect Classification Percentage") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  scale_fill_viridis_d(option = "C", begin=0.01, end = 0.9) +
  theme_minimal() +  geom_text(aes(label = paste0(rate*100, "%")), vjust = -0.5) 
plot3

# Incorrect Classification Rate by Name for Non-natives
errors <- non_native %>%
  group_by(name) %>%
  summarise(total = n(),
            incorrect_name = sum(is_correct == "No"),
            rate = round(incorrect_name/total, digits = 4)) %>%
  mutate(labels = scales::percent(rate, accuracy = 0.01))

plot4 <- ggplot(errors, aes(x = reorder(name, rate), y = rate, fill = name)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.3) +
  labs(title = "Incorrect Classification Rate by Name for Non-natives",
       x = "Name",
       y = "Incorrect Classification Percentage") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  scale_fill_viridis_d(option = "C", begin=0.01, end = 0.9) +
  theme_minimal() + geom_text(aes(label = paste0(rate*100, "%")), vjust = -0.5) 

plot4

#Incorrect Classification Rate by Name
classified <- detector_human %>%
  group_by(name) %>%
  summarise(total = n(),
            incorrect_name = sum(is_correct == "No"),
            rate = round(incorrect_name/total, digits = 4)) %>%
  mutate(labels = scales::percent(rate, accuracy = 0.01))

plot5 <- ggplot(classified, aes(x = reorder(name, rate), y = rate, fill = name)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Incorrect Classification Rate by Name",
       x = "Name",
       y = "Incorrect Classification Percentage") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  scale_fill_viridis_d(option = "C", begin=0.01, end = 0.9) +
  theme_minimal()  + geom_text(aes(label = paste0(rate*100, "%")), vjust = -0.5) 
plot5


# Distribution of '.pred_AI' with 'native' status 
plot6 <- ggplot(detector_human, aes(x = .pred_AI, fill = native)) +
  geom_histogram(bins = 12, position = "identity", alpha = 0.9) +
  labs(title = "Distribution of '.pred_AI' by Native Status",
       x = ".pred_AI",
       y = "Count") +
  scale_fill_viridis_d(option = "C", begin=0.01, end = 0.9)+
  theme_minimal() 
plot6



# Defining the main title, subtitle, caption
main_title <- "GPT Detectors: Biased Against Non-Native English Writers? \n"

subtitle <- " Here, classification rate is the percentage of correctly classified essays as written by humans.\n"

caption <- "Source: GPT Detectors R Package\n Name: Saurabh Jadhav"

# Applying the custom theme to each individual plot
plot1 <- plot1 + my_theme()
plot2 <- plot2 + my_theme()
plot3 <- plot3 + my_theme()
plot4 <- plot4 + my_theme()
plot5 <- plot5 + my_theme()
plot6 <- plot6 + my_theme()




# Arrange all the plots, subtitle, and the main title
combined_plot <- (
  plot1 + plot2 +
    plot_layout(ncol = 2) +  
    plot3 + plot4 +
    plot_layout(ncol = 2) +  # Set the number of columns for the second row of plots
    plot5 + plot6 +
    plot_layout(ncol = 2) +  # Set the number of columns for the third row of plots
    plot_annotation(title = main_title, theme = theme(plot.title = element_text(size = 28, family = "Roboto", color = "darkblue", face = "bold", hjust = 0.5))) +
    plot_annotation(subtitle = subtitle, theme = theme(plot.subtitle = element_text(size = 24, family = "Roboto-Medium", color = "darkgreen"))) +
    plot_annotation(caption = caption, theme = theme(plot.caption = element_text(size = 20, family = "Roboto", color = "darkgrey",  hjust = 0.5)))
)                         


# Display the combined plot
print(combined_plot)
