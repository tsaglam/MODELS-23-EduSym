library(ggplot2)
library(extrafont)
library(cowplot)
library(magrittr)
library(dplyr)
source("data_processing.R")

font_import()
loadfonts(quiet = TRUE)
#theme_set(theme_cowplot(font_size=12, font_family="Linux Libertine"))


# functions defined in data_processing.R
data <- read_from_csv()
data <- copy_originals_for_each_plag_type(data)
data <- add_approach_label(data)
relevantTupleTypes <-
  c("Original-Original", "Plag-Original", "Plag-Plag")
data <- subset(data, tupleType %in% relevantTupleTypes)
data <- add_tuple_type_label(data)

blue <- "#005AA0"
blue50 <- "#7FACCF"
green <- "#8CB423"
green50 <- "#C5D990"
orange <- "#F0781E"
orange50 <- "#F7BB8E"

fill_colors <- c(green, orange, blue)

# Create summary stats
baseline <- "Unrelated Pairs"
summary_data <- data %>%
  group_by(approach, tupleTypeLabel) %>%
  summarize(
    median_val = median(value),
    avg_val = mean(value),
    min_val = min(value),
    max_val = max(value),
    lower_q = quantile(value, 0.25),
    upper_q = quantile(value, 0.75)
  ) %>%
  mutate(
    avg_diff = avg_val - avg_val[tupleTypeLabel == baseline],
    median_diff = median_val - median_val[tupleTypeLabel == baseline],
    q_diff = lower_q - upper_q[tupleTypeLabel == baseline],
    minmax_diff = min_val - max_val[tupleTypeLabel == baseline],
  )
summary_data$approach <- as.factor(summary_data$approach)
summary_data <- summary_data %>%
  filter(tupleTypeLabel != baseline)

# Create the summary plot
summary_data_plot <-
  ggplot(summary_data, aes(x = approach, group = tupleTypeLabel)) +
  geom_line(aes(y = avg_diff, color = tupleTypeLabel),
            linetype = "dashed",
            size = 1) +
  geom_line(aes(y = median_diff, color = tupleTypeLabel),
            linetype = "solid",
            size = 1) +
  geom_line(aes(y = q_diff, color = tupleTypeLabel),
            linetype = "dotted",
            size = 1) +
  geom_line(aes(y = minmax_diff, color = tupleTypeLabel),
            linetype = "dotdash",
            size = 1) +
  labs(x = "Approach", y = "Difference") +
  scale_x_discrete(breaks = unique(summary_data$approach)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  scale_color_manual(values = c(orange, blue)) +
  theme_minimal()

summary_data_plot

# function defining the plot
do_the_plot <- function(data, metric) {
  data <-
    subset(data,
           plagTypeGroup != '' &
             variable %in% c("similarity", metric))
  
  plot <-
    ggplot(data, aes(x = tupleTypeLabel, y = value, fill = tupleTypeLabel)) +
    geom_boxplot(width = 0.3) +
    scale_y_continuous(
      labels = function(x)
        paste0(x, "%"),
      name = "Similarity"
    ) +
    expand_limits(y = c(0, 100)) +
    guides(fill = "none", x = guide_axis(angle = 0)) +
    theme(text = element_text(size = 11, family = "times"),
          legend.position = "none") +
    scale_fill_manual(values = fill_colors, name = "") + xlab("")
  return(plot)
}

# create and save the plots for all plagiarism tools
groups <- unique(data$plagTypeGroup)
metric <- "avg.similarity"

do_the_plot(data, metric)

width100 <- 14 / 2 / 1.5
height = width100 / 1.8

print(height)

plot_and_save <- function(width, metric) {
  do_the_plot(data, metric)
  file_name <-
    paste('./output/experiment_', metric, '.pdf', sep = "")
  ggsave(file_name,
         device = cairo_pdf,
         width = width,
         height = height)
}

plot_and_save(width100, "max.similarity")
plot_and_save(width100, "avg.similarity")