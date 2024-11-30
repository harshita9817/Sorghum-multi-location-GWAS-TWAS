chr3<- read.csv("fixeddataset_chr3.csv")
data<- chr3

# Filter the data for non-NA chr3 values
filtered_data <- subset(data, !is.na(chr3))

# Combine both variables into a long format for easy plotting
library(tidyr)
long_data <- pivot_longer(filtered_data,
                          cols = c(SbDiv_2021_spatially_corrected_blues, SAP_Spatially_Corrected_blues),
                          names_to = "Variable",
                          values_to = "Value")

# Remove NA values for boxplot
long_data <- subset(long_data, !is.na(Value))

# Define a custom order for the x-axis
long_data$Variable <- factor(long_data$Variable,
                             levels = c("SbDiv_2021_spatially_corrected_blues", "SAP_Spatially_Corrected_blues"),
                             labels = c("SbDiv", "SAP"))

# Count the number of values for each group
library(dplyr)
counts <- long_data %>%
  group_by(chr3, Variable) %>%
  summarise(n = n(), .groups = "drop")

# Merge the counts back into the data for plotting
long_data <- merge(long_data, counts, by = c("chr3", "Variable"))

# Create the boxplot
library(ggplot2)
plot1<- ggplot(long_data, aes(x = interaction(Variable, chr3, lex.order = TRUE), y = Value)) +
  geom_boxplot(color = "black", fill = "gray") +  # Custom boxplot color
  geom_text(data = counts, aes(x = interaction(Variable, chr3, lex.order = TRUE), 
                               y = max(long_data$Value, na.rm = TRUE) + 5, 
                               label = paste0("n=", n)), inherit.aes = FALSE, size = 4) +
  scale_x_discrete(labels = c("TT", "GG", "TT", "GG")) +
  labs(
    #title = "Chr03_69067236",
    x = "",
    y = "Days To Flower"
  ) +
  scale_y_continuous(breaks = seq(40, 90, 10)) +  # Custom y-axis ticks
  theme_minimal(base_size = 12) +  # Simplified background
  theme(
    panel.grid = element_blank(),  # Remove grid lines
    panel.background = element_rect(fill = "white"),  # Plain white background
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  annotate("text", x = 1.5, y = 40, label = "Nebraska", size = 4, vjust = -1) +  # Label for Nebraska
  annotate("text", x = 3.5, y = 40, label = "Alabama", size = 4, vjust = -1)    # Label for Alabama

######################################################################################################

chr6<- read.csv("fixeddataset_chr6.csv")
data<- chr6

# Filter the data for non-NA chr3 values
filtered_data <- subset(data, !is.na(chr6))

# Combine both variables into a long format for easy plotting
library(tidyr)
long_data <- pivot_longer(filtered_data,
                          cols = c(SbDiv_2021_spatially_corrected_blues, SAP_Spatially_Corrected_blues),
                          names_to = "Variable",
                          values_to = "Value")

# Remove NA values for boxplot
long_data <- subset(long_data, !is.na(Value))

# Define a custom order for the x-axis
long_data$Variable <- factor(long_data$Variable,
                             levels = c("SbDiv_2021_spatially_corrected_blues", "SAP_Spatially_Corrected_blues"),
                             labels = c("SbDiv", "SAP"))

# Count the number of values for each group
library(dplyr)
counts <- long_data %>%
  group_by(chr6, Variable) %>%
  summarise(n = n(), .groups = "drop")

# Merge the counts back into the data for plotting
long_data <- merge(long_data, counts, by = c("chr6", "Variable"))

# Create the boxplot
library(ggplot2)
plot2<- ggplot(long_data, aes(x = interaction(Variable, chr6, lex.order = TRUE), y = Value)) +
  geom_boxplot(color = "black", fill = "gray") +  # Custom boxplot color
  geom_text(data = counts, aes(x = interaction(Variable, chr6, lex.order = TRUE), 
                               y = max(long_data$Value, na.rm = TRUE) + 5, 
                               label = paste0("n=", n)), inherit.aes = FALSE, size = 4) +
  scale_x_discrete(labels = c("AA", "GG", "AA", "GG")) +
  labs(
    #title = "Chr06_39694475",
    x = "",
    y = "Days To Flower"
  ) +
  scale_y_continuous(breaks = seq(40, 90, 10)) +  # Custom y-axis ticks
  theme_minimal(base_size = 12) +  # Simplified background
  theme(
    panel.grid = element_blank(),  # Remove grid lines
    panel.background = element_rect(fill = "white"),  # Plain white background
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  annotate("text", x = 1.5, y = 40, label = "Nebraska", size = 4, vjust = -1) +  # Label for Nebraska
  annotate("text", x = 3.5, y = 40, label = "Alabama", size = 4, vjust = -1)    # Label for Alabama

##############################################################################################################################################

chr9<- read.csv("fixeddataset_chr9.csv")
data<- chr9

# Filter the data for non-NA chr9 values
filtered_data <- subset(data, !is.na(chr9))

# Combine both variables into a long format for easy plotting
library(tidyr)
long_data <- pivot_longer(filtered_data,
                          cols = c(SbDiv_2021_spatially_corrected_blues, SAP_Spatially_Corrected_blues),
                          names_to = "Variable",
                          values_to = "Value")

# Remove NA values for boxplot
long_data <- subset(long_data, !is.na(Value))

# Define a custom order for the x-axis
long_data$Variable <- factor(long_data$Variable,
                             levels = c("SbDiv_2021_spatially_corrected_blues", "SAP_Spatially_Corrected_blues"),
                             labels = c("SbDiv", "SAP"))

# Count the number of values for each group
library(dplyr)
counts <- long_data %>%
  group_by(chr9, Variable) %>%
  summarise(n = n(), .groups = "drop")

# Drop GA values from counts and long_data
counts <- counts %>% filter(chr9 != "GA")
long_data <- long_data %>% filter(chr9 != "GA")

# Merge the updated counts back into the data for plotting
long_data <- merge(long_data, counts, by = c("chr9", "Variable"))

# Create the boxplot
library(ggplot2)
plot3 <- ggplot(long_data, aes(x = interaction(Variable, chr9, lex.order = TRUE), y = Value)) +
  geom_boxplot(color = "black", fill = "gray") +  # Custom boxplot color
  geom_text(data = counts, aes(x = interaction(Variable, chr9, lex.order = TRUE), 
                               y = max(long_data$Value, na.rm = TRUE) + 5, 
                               label = paste0("n=", n)), inherit.aes = FALSE, size = 4) +
  scale_x_discrete(labels = c("AA", "GG", "AA", "GG")) +
  labs(
    #title = "Chr09_62620720",
    x = "Genotype",
    y = "Days To Flower"
  ) +
  scale_y_continuous(breaks = seq(40, 90, 10)) +  # Custom y-axis ticks
  theme_minimal(base_size = 12) +  # Simplified background
  theme(
    panel.grid = element_blank(),  # Remove grid lines
    panel.background = element_rect(fill = "white"),  # Plain white background
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  annotate("text", x = 1.5, y = 40, label = "Nebraska", size = 4, vjust = -1) +  # Label for Nebraska
  annotate("text", x = 3.5, y = 40, label = "Alabama", size = 4, vjust = -1)    # Label for Alabama

# Display the plot
print(plot3)

#####################################################################################################
# Combine plots horizontally with labels
combined_plot <- plot_grid(
  plot1, plot2, plot3, 
  ncol = 1,              # Arrange in a single column (vertical)
  labels = c("B", "C", "D"),  # Add labels to the plots
  label_size = 14,        # Set label size
  align = "hv"            # Align both horizontally and vertically
)

# Display the combined plot
print(combined_plot)
ggsave("combined_plot.svg", plot = combined_plot, device = "svg", width = 5, height = 10)









