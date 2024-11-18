library(stringr)
library(dplyr)
library(ggplot2)
library(wesanderson)

df <- read.csv("Zbluessbdivsignals.csv")
data_cum <- df %>%
  group_by(CHROM) %>%
  summarise(max_bp = max(POS)) %>%
  mutate(bp_add = lag(cumsum(max_bp + 5000000), default = 0)) %>%  # Add padding to avoid overlap
  select(CHROM, bp_add)

gwas_data <- df %>%
  inner_join(data_cum, by = "CHROM") %>%
  mutate(bp_cum = POS + bp_add)

axis_set <- gwas_data %>%
  group_by(CHROM) %>%
  summarize(center = mean(bp_cum))

# Define color palette for chromosomes
chromosome_colors <- c('#03193F', '#28708C', '#BF930F', '#0f3bbf', '#295E52', 
                       '#e31a1c', '#fdbf6f', '#ff7f00', '#cab2d6', '#6a3d9a')

ggplot(gwas_data, aes(x = bp_cum, y = support, color = as.factor(CHROM))) + 
  geom_point(size = 2, alpha = 1) +  # Opaque points
  geom_hline(yintercept = 0.1, color = "black", linetype = "dashed", lwd = 1) + 
  scale_color_manual(values = chromosome_colors) +  # Color by chromosome
  scale_x_continuous(breaks = axis_set$center, 
                     labels = c("Chr01", "Chr02", "Chr03", "Chr04", "Chr05", 
                                "Chr06", "Chr07", "Chr08", "Chr09", "Chr10")) + 
  theme_classic(base_size = 15) +  # Use a classic theme with larger base font
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels for clarity
        legend.position = "none") + 
  labs(x = "Chromosome", 
       y = "Support",
       #title = "GWAS Manhattan Plot"
       )

ggsave("First_Plot_Modified_NoOverlap.png", width = 10, height = 6, dpi = 300)
