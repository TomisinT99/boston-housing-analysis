# boston-housing-analysis
#loading necessary libraries 
library(dpylr)
library(tidyverse)

#loading of dataset
Boston <- read.csv("BostonHousing.csv")

#converting to tibble for easy manipulation of data
Boston <- as_tibble(Boston)

#getting a brief view of the dataset 
glimpse(Boston)
summary(Boston)

#Binning calculation 
bin_rm <- function(rm) {
  if (rm < 4) {
    return('<4')
  } else if (rm >= 4 & rm < 5) {
    return('4-5')
  } else if (rm >= 5 & rm < 6) {
    return('5-6')
  } else if (rm >= 6 & rm < 7) {
    return('6-7')
  } else if (rm >= 7 & rm < 8) {
    return('7-8')
  } else {
    return('>=8')
  }
}

#Calculation of the statistics 
results <- Boston %>%
  mutate(RM_bin = map_chr(rm, bin_rm)) %>%
  group_by(RM_bin) %>%
  summarise(
    count = n(),
    mean_MEDV = mean(medv, na.rm = TRUE),
    max_MEDV = max(medv, na.rm = TRUE),
    min_MEDV = min(medv, na.rm = TRUE),
    stddev_MEDV = sd(medv, na.rm = TRUE)
  )
print(results)

#Visulaization 
ggplot(results, aes(x = RM_bin, y = mean_MEDV, fill = RM_bin)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Median Value of Homes by Number of Rooms",
       x = "Average Number of Rooms (RM)",
       y = "Mean Median Value (MEDV)") +
  theme_minimal() +
  scale_fill_viridis_d()

view(results)
