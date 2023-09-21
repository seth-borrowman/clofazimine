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

### Boxplot ---
ggplot(data = counts[which(counts$Day != 0),], aes(x = factor(Day), y = Viability)) +
    geom_boxplot(aes(color = Treatment)) +
    geom_jitter(aes(color = Treatment, group = Treatment, shape = Line), position = position_dodge(width = 0.75)) +
    stat_pvalue_manual(pairwise, label = "{p.signif}", y.position = 1.06,
                       x = "Day", size = 3.88,
                       hide.ns = T,
                       position = ggpp::position_dodgenudge(width = 0, x = 0)) +
    geom_bracket(xmin = .7, xmax = 1.3, y.position = 1.05, label = "") +
    geom_bracket(xmin = 2.7, xmax = 3.3, y.position = 1.05, label = "") +
    xlab("Day") +
    theme_classic2() +
    scale_color_manual(values = brewer.pal(n = 4, name = "Set2"))

### Welch test for group differences ---
for (i in unique(counts$Day[which(counts$Day != 0)])) {
    print(paste("Day", i))
    day_welch <- oneway.test(Viability ~ Treatment, data = counts[which(counts$Day == i),])
    print(day_welch)
    games_howell <- rstatix::games_howell_test(Viability ~ Treatment, data = counts[which(counts$Day == i),])
    print(games_howell)
}
