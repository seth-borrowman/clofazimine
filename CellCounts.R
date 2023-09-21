library(tidyverse)
library(ggpubr)
library(RColorBrewer)

### Import Data ---
counts <- readxl::read_excel("C:/Users/sborrowman/OneDrive - University of Iowa/Attachments/CellCounts.xlsx")
# Factor treatment, add full names
counts$Treatment <- counts$Treatment %>%
    factor(levels = c("Blank", "DMSO", "Syn", "CLF"),
           labels = c("Control", "DMSO", "Senicapoc", "Clofazimine"))

### Initial plotting ---
# Treatment effect on viability 
ggplot(data = counts, aes(x = Day, y = 1 - Viability, color = Treatment)) +
    geom_point(aes(shape = Line)) +
    geom_smooth(formula = y ~ x + 0, method = "lm", se = F) +
    ylab("% Dead") +
    theme_minimal() +
    scale_color_manual(values = brewer.pal(n = 4, name = "Set2"))

# Cell line effect on viability
ggplot(data = counts, aes(x = Day, y = 1 - Viability, color = Line)) +
    geom_point(aes(shape = Treatment)) +
    geom_smooth(formula = y ~ x + 0, method = "lm", se = F) +
    ylab("% Dead") +
    theme_minimal() +
    scale_color_manual(values = brewer.pal(n = 3, name = "Set2"))

### T-test comparisons ---
pairwise <- compare_means(Viability ~ Treatment,
                          data = counts[which(counts$Day != 0),],
                          method = "t.test", ref.group = "Control",
                          group.by = "Day") %>%
    mutate(Day = case_when(
        Day != 1 ~ Day - 1,
        .default = Day
    ))

### Welch test for group differences ---
gh_sum <- data.frame()
for (i in unique(counts$Day[which(counts$Day != 0)])) {
    print(paste("Day", i))
    day_welch <- oneway.test(Viability ~ Treatment, data = counts[which(counts$Day == i),])
    print(day_welch)
    games_howell <- rstatix::games_howell_test(Viability ~ Treatment, data = counts[which(counts$Day == i),])
    games_howell$Day <- rep(i, 6)
    gh_sum <- rbind(gh_sum, games_howell)
    print(games_howell)
}
gh_sum <- gh_sum %>%
    mutate(Day = case_when(
        Day != 1 ~ Day - 1,
        .default = Day
    ))

### Boxplot ---
ggplot(data = counts[which(counts$Day != 0),], aes(x = factor(Day), y = Viability)) +
    geom_boxplot(aes(color = Treatment), size = 0.7) +
    geom_jitter(aes(color = Treatment, group = Treatment, shape = Line),
                position = position_dodge(width = 0.75), size = 2, alpha = 0.7) +
    xlab("Day") +
    theme_classic2() +
    scale_color_manual(values = brewer.pal(n = 4, name = "Set2"))
ggsave("ViabilityBoxplot.pdf", dpi = 600)

