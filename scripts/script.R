#FILE NAMED stack-overflow-developer-survey-2022.zip WITHIN THE "raw" FOLDER MUST BE 
#UNPACKED BEFORE CONTINUEING TO RUN THIS SCRIPT 



# Install required packages
install.packages(c("tidyverse", "readr", "here", "ggplot2", "dplyr", "tidyr", "plotly"))


# Load Libraries
library(tidyverse)
library(readr)
library(here)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)


# Set Working Directory
setwd(here())

# Load Data - Read the survey results file
survey_data <- read_csv(here("raw", "survey_results_public.csv"))
survey_schema <- read_csv(here("raw", "survey_results_schema.csv"))

# Inspect the structure of the data
str(survey_data)

#Take a look at the schema/codebook for this data
View(survey_schema)

#Take a look at variable names
head(survey_data)
colnames(survey_data)


#Data processing

# Define a function to convert non-numeric values to numeric
convert_years_code_pro <- function(x) {
  if(x == "Less than 1 year") {
    return(0.5) 
  } else if(x == "More than 50 years") {
    return(51) 
  } else {
    return(as.numeric(x))
  }
}


# Use the function to convert 'YearsCodePro' to numeric
prepared_data <- survey_data %>%
  filter(!is.na(YearsCodePro), 
         !is.na(ConvertedCompYearly), 
         !is.na(EdLevel), 
         !is.na(Country)) %>%
  mutate(YearsCodePro = sapply(YearsCodePro, convert_years_code_pro))



# use the filter function to focus on data from the United States
usa_data <- prepared_data %>%
  filter(Country == "United States of America")

#Extreme outliers present in the data, here I use the interquartile range to calculate, define, and then remove outliers
# Calculate IQR
Q1 <- quantile(usa_data$ConvertedCompYearly, 0.25)
Q3 <- quantile(usa_data$ConvertedCompYearly, 0.75)
IQR <- Q3 - Q1

# Define upper and lower bounds
upper_bound <- Q3 + 1.5 * IQR
lower_bound <- Q1 - 1.5 * IQR

# Remove outliers
usa_data_no_outliers <- usa_data %>%
  filter(ConvertedCompYearly >= lower_bound, ConvertedCompYearly <= upper_bound)


#Here we remove all columns which will not be used in the analysis or visualisation
usa_data_no_outliers <- select(usa_data_no_outliers, YearsCodePro, ConvertedCompYearly, EdLevel, Country)
head(usa_data_no_outliers)
colnames(usa_data_no_outliers)

#Save the new data frame to the "processed" data folder
write.csv(usa_data_no_outliers, "usa_data_no_outliers.csv", row.names = FALSE)

# Recode the metadata for specific EdLevel values that work better for plot legend
usa_data_no_outliers <- usa_data_no_outliers %>%
  mutate(EdLevel = recode(EdLevel, "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)" = "Secondary school",
                          "Some college/university study without earning a degree" = "Some college"))

# Verify the changes
unique(usa_data_no_outliers$EdLevel)

# Visualisation 1 
ggplot(usa_data_no_outliers, aes(x = YearsCodePro, y = ConvertedCompYearly, color = YearsCodePro)) +
  geom_point(alpha = 0.5,) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Years of Professional Coding Experience", 
       y = "Yearly Compensation (USD)",
       color = "Years of Experience",
       title = "Professional Coding Experience vs Yearly Compensation") +
  theme_test()

ggsave(filename = "Professional Coding Experience vs Yearly Compensation.png", width = 7, height = 6, dpi = 300)



install.packages("hexbin")
library(hexbin)


#Here, I employ a 2D histogram to examine the data for patterns, clusters, and trends. 
#This visualization technique can unveil information regarding correlations, concentrations,
#and outliers that may remain obscured in alternative plot types. The color or shading employed 
#in the 2D histogram portrays the frequency or density of data points within each bin, 
#facilitating the identification of areas characterized by either high or low concentration. 

#What emerges from the data is in line with the previous plot, as experience grows so does income
#There is a very clear cluster of data in the 0 to 10 year range of experience 
#appearing to show a trend of increasing income with experience.

#2D Histogram
ggplot(usa_data_no_outliers, aes(x = YearsCodePro, y = ConvertedCompYearly)) +
  geom_hex(bins = 50) +
  scale_fill_gradient(low = "red", high = "yellow") +
  scale_y_continuous(labels = function(x) paste0(x / 1000, "k")) +
  labs(x = "Years of Professional Coding Experience", 
       y = "Yearly Compensation (USD)",
       title = "2D Histogram of Professional Coding Experience vs Yearly Compensation") +
  labs( color = "Years experience")+
  theme_test()

ggsave(filename = "2D Histogram of Professional Coding Experience vs Yearly Compensation.png", width = 8, height = 7, dpi = 300)


#Here we begin to look at how education level may impact yearly income in a different way, 
# I calculate the average income for each educational level and then display this in a Lollipop chart for ease of interpretation

#The chart does not show a direct relationship of education level on income across all levels


# Calculate average income for each education level
average_income <- usa_data_no_outliers %>% 
  group_by(EdLevel) %>% 
  summarise(AverageIncome = mean(ConvertedCompYearly, na.rm = TRUE))

# Create the lollipop chart
ggplot(average_income, aes(x = reorder(EdLevel, -AverageIncome), y = AverageIncome)) +
  geom_segment(aes(y = 0, 
                   x = reorder(EdLevel, -AverageIncome), 
                   yend = AverageIncome, 
                   xend = reorder(EdLevel, -AverageIncome)), 
               color = "blue", 
               linewidth = 1.5) +
  geom_point(color = "blue", size = 5) +
  coord_flip() +
  labs(x = "Education Level", 
       y = "Average Yearly Compensation (USD)",
       title = "Average Yearly Compensation by Education Level") +
  theme_minimal()

ggsave(filename = "Average Yearly Compensation by Education Level.jpg",width = 9, height = 7,dpi = 300)




install.packages("ggridges")
library(ggridges)


# Create the ridgeline plot
ggplot(usa_data_no_outliers, aes(x = ConvertedCompYearly, y = EdLevel, fill = as.factor(EdLevel))) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_continuous(labels = scales::dollar) +
  labs(x = "Yearly Compensation (USD)", 
       y = "Education Level", 
       title =  " Yearly Income by Education Level") +
  theme_ridges() +
  theme(legend.position = "none")


ggsave(filename = "Yearly Income by Education Level.jpg",width = 9, height = 7,dpi = 300)



#Create a facetwrap scatterplot with density 

ggplot(usa_data_no_outliers, aes(x=YearsCodePro, y=ConvertedCompYearly/1000)) +
  xlab("Years of Coding Experience") +
  ylab("Annual Income (thousands of USD)") +
  stat_density2d(aes(fill = after_stat(density)), geom = "tile", contour = FALSE) +
  scale_fill_gradient(low = "white", high = "red", 
                      breaks = seq(0, 1, by = 0.01), 
                      labels = function(x) paste0(format(x*1000, nsmall = 0), "K")) +
  geom_jitter(position=position_jitter(0.2), size=0.4) +
  facet_wrap(~EdLevel) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Density and Relationship between Experience and Income by Education Level") +
  theme_test()

ggsave(filename = "Density and Relationship between Experience and Income by Education Level.jpg",width = 9, height = 7,dpi = 300)


#Here I use plotly to create a 3d scatterplot in order to view how the data from each educational level fits into the overall picture when combined
fig <- plot_ly(usa_data_no_outliers, 
               x = ~YearsCodePro, y = ~ConvertedCompYearly, z = ~EdLevel,
               color = ~EdLevel, 
               marker = list(symbol = 'circle', sizemode = 'diameter', size = 5),
               text = ~paste('Years of Experience:', YearsCodePro, '<br>Yearly Compensation:', ConvertedCompYearly,
                             '<br>Education Level:', EdLevel))

# Add layout settings
fig <- fig %>% layout(
  title = 'Relationship between Coding Experience, Income, and Education Level',
  scene = list(
    xaxis = list(title = 'Years of Professional Coding Experience'),
    yaxis = list(title = 'Yearly Compensation (USD)'),
    zaxis = list(title = 'Education Level'),
    lighting = list(ambient = 0.8)  # Adjust scene lighting
  ),
  paper_bgcolor = 'rgba(0,0,0,0)',  # Transparent background
  plot_bgcolor = 'rgba(0,0,0,0)',   # Transparent plot background
  hoverlabel = list(bgcolor = 'white', font = list(color = 'black'))  # Customize hover label appearance
)

fig



