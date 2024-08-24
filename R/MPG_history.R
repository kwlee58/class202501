library(magrittr)
M24_1 <- c(257, 445)
M23_2 <- c(289, 525)
M23_1 <- c(354, 517)
M22_2 <- c(355, 632)
M22_1 <- c(316, 504)
M21_2 <- c(182, 359)
M21_1 <- c(143, 295)
M20_2 <- c(93, 174)
M20_1 <- c(58, 87)
M19_2 <- c(62, 84)
MH <- 
  cbind(M19_2, M20_1, M20_2, M21_1, M21_2, M22_1, M22_2, M23_1, M23_2, M24_1)
semesters <- 
  c("19/2", "20/1", "20/2", "21/1", "21/2", "22/1", "22/2", "23/1", "23/2", "24/1")
colnames(MH) <- semesters
apply(MH, MARGIN = 2, proportions) %>% 
  `*`(100) %>% 
  round(1)
mosaicplot(MH)
source("./R/mosaic_gg.R")
library(ggplot2)
library(extrafont)
m_list <- mosaic_gg(as.table(MH))
m <- m_list$m
df <- m_list$df
p_df <- m_list$p_df
str(p_df)
y_min <- min(p_df$y_breaks)
y_max <- sort(unique(p_df$y_breaks[p_df$y_breaks < 0.99]), decreasing = TRUE)[1]
m_list$m +
  geom_text(aes(x = center, y = 1.05),
            family = "KoPubWorldDotum Medium",
            label = p_df[, 2]) + 
  theme_bw() +
  scale_y_continuous(breaks = c(0, y_min, y_max, 1),
                     labels = format(c(0, y_min, y_max, 1) * 100, digits = 2, nsmall = 1)) +
  labs(x = "학기별 수강 인원 누적(%)", y = "연비 10 => 12 vs 연비 30 => 40 (%)") + 
  ggtitle("MPG") + 
  scale_fill_brewer(name = "", 
                    labels = c("연비 10 => 12", "연비 30 => 40"),
                    type = "qual", 
                    palette = "Set2", 
                    direction = 1) +
  theme(axis.title.x = element_text(family = "KoPubWorldDotum Light"),
        axis.title.y = element_text(family = "KoPubWorldDotum Light"),
        legend.text = element_text(family = "KoPubWorldDotum Light"),
        plot.title = element_text(size = 20, hjust = 0.5, family = "KoPubWorldDotum Bold"))
ggsave("./R/pics/MPG_ggplot.png", width = 12, height = 6, dpi = 600)
