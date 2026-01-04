library(reshape2)
library(ggplot2)

data <- structure(c(0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                    0L, 0L, 0L, 1L, 2L, 3L, 2L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                    0L, 0L, 1L, 3L, 2L, 3L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                    0L, 1L, 2L, 3L, 2L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                    0L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                    0L, 1L, 1L, 4L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 
                    5L, 5L, 5L, 5L, 5L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                    0L, 1L, 4L, 1L, 9L, 9L, 8L, 1L, 0L, 0L, 1L, 5L, 5L, 5L, 4L, 
                    4L, 4L, 4L, 4L, 5L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 
                    1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 14L, 1L, 4L, 4L, 4L, 1L, 
                    1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 5L, 5L, 1L, 2L, 
                    2L, 1L, 9L, 8L, 8L, 8L, 9L, 1L, 14L, 14L, 1L, 4L, 1L, 7L, 
                    7L, 7L, 7L, 7L, 7L, 7L, 1L, 0L, 0L, 1L, 5L, 4L, 1L, 2L, 2L, 
                    1L, 9L, 8L, 11L, 11L, 8L, 8L, 9L, 1L, 14L, 1L, 1L, 7L, 1L, 
                    1L, 1L, 1L, 5L, 5L, 1L, 1L, 0L, 1L, 5L, 4L, 4L, 1L, 2L, 1L, 
                    1L, 9L, 10L, 11L, 11L, 8L, 8L, 8L, 1L, 1L, 2L, 3L, 1L, 1L, 
                    9L, 9L, 1L, 5L, 1L, 4L, 4L, 1L, 1L, 4L, 4L, 4L, 1L, 1L, 1L, 
                    8L, 8L, 1L, 8L, 8L, 1L, 1L, 1L, 9L, 1L, 13L, 2L, 3L, 1L, 8L, 
                    9L, 1L, 1L, 4L, 5L, 4L, 1L, 1L, 4L, 4L, 4L, 7L, 6L, 1L, 8L, 
                    8L, 8L, 8L, 1L, 1L, 8L, 8L, 1L, 2L, 2L, 13L, 2L, 3L, 1L, 1L, 
                    4L, 4L, 5L, 4L, 4L, 1L, 1L, 4L, 4L, 4L, 6L, 7L, 1L, 8L, 8L, 
                    8L, 12L, 1L, 9L, 8L, 1L, 1L, 2L, 2L, 13L, 2L, 3L, 1L, 4L, 
                    4L, 5L, 4L, 5L, 4L, 1L, 1L, 4L, 4L, 4L, 7L, 6L, 1L, 8L, 8L, 
                    8L, 8L, 1L, 1L, 8L, 8L, 1L, 2L, 2L, 13L, 2L, 3L, 1L, 1L, 4L, 
                    4L, 5L, 4L, 4L, 1L, 1L, 4L, 4L, 4L, 1L, 1L, 1L, 8L, 8L, 1L, 
                    8L, 8L, 1L, 1L, 1L, 9L, 1L, 13L, 2L, 3L, 1L, 8L, 9L, 1L, 1L, 
                    4L, 5L, 4L, 1L, 1L, 5L, 4L, 4L, 1L, 2L, 1L, 1L, 9L, 10L, 
                    11L, 11L, 8L, 8L, 8L, 1L, 1L, 2L, 3L, 1L, 1L, 9L, 9L, 1L, 
                    5L, 1L, 4L, 4L, 1L, 0L, 1L, 5L, 4L, 1L, 2L, 2L, 1L, 9L, 8L,
                    11L, 11L, 8L, 8L, 9L, 1L, 14L, 1L, 1L, 7L, 1L, 1L, 1L, 1L, 
                    5L, 5L, 1L, 1L, 0L, 0L, 0L, 1L, 5L, 5L, 1L, 2L, 2L, 1L, 9L, 
                    8L, 8L, 8L, 9L, 1L, 14L, 14L, 1L, 4L, 1L, 7L, 7L, 7L, 7L, 
                    7L, 7L, 7L, 1L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 
                    1L, 1L, 1L, 1L, 1L, 14L, 1L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 1L, 
                    1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 4L, 1L, 9L, 9L, 
                    8L, 1L, 0L, 0L, 1L, 5L, 5L, 5L, 4L, 4L, 4L, 4L, 4L, 5L, 1L, 
                    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 4L, 1L, 1L, 1L, 1L, 
                    0L, 0L, 0L, 0L, 1L, 1L, 1L, 5L, 5L, 5L, 5L, 5L, 1L, 0L, 0L, 
                    0L, 0L, 0L, 0L, 0L, 0L, 1L, 2L, 3L, 2L, 1L, 0L, 0L, 0L, 0L, 
                    0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 
                    0L, 0L, 0L, 0L, 0L, 1L, 3L, 2L, 3L, 1L, 0L, 0L, 0L, 0L, 0L, 
                    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                    0L, 0L, 0L, 0L, 1L, 2L, 3L, 2L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 
                    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                    0L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), 
                  dim = c(29L, 25L), 
                  dimnames = list(NULL, c("V1", "V2", "V3", "V4", "V5", 
                                          "V6", "V7", "V8", "V9", "V10", 
                                          "V11", "V12", "V13", "V14", "V15", 
                                          "V16", "V17", "V18", "V19", "V20", 
                                          "V21", "V22", "V23", "V24", "V25")))

df <- as.matrix(data)

df_long <- melt(as.matrix(df))

colors_15 <- c(rgb(1, 0, 0, alpha = 0), "black", "#FFED6F", 
               "#FFFFB3", "#BEBADA", "#BC80BD", 
               "#6D8A2EFF", "#3F6820E8", "#FFDEC4FF",
               "#D2BA9AFF", "white", "#FFD8E1FF",
               "#AF735BD4", "#FDB462", "#CBD5E8")


p <- ggplot(df_long, aes(x = Var2, y = Var1, fill = factor(value))) +
  geom_tile(color = NA) +
  scale_fill_manual(
    values = colors_15,
    guide = "none"
    ) +
  scale_y_reverse() +  
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),    
    axis.ticks = element_blank(),
    axis.title = element_blank()
    ) +
theme(plot.background = element_rect(fill="#b768a2"))

ggsave("Caishen.png", p, 
       width = 5, height = 5.8,
       dpi = 300,
       limitsize = FALSE)

################################################################################
# make gif
################################################################################
library(gganimate)
library(transformr)

df_long$row <- df_long$Var1

df_long$time <- df_long$row

p_animated <- ggplot(df_long, aes(x = Var2, y = Var1, fill = factor(value))) +
  geom_tile(color = NA) +
  scale_fill_manual(
    values = colors_15,
    guide = "none"
    ) +
  scale_y_reverse() +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "#b768a2")
    ) +
  transition_time(time) +
  ease_aes('linear') +
  shadow_mark() +
  labs(title = '招财进宝！像素进度：{round(frame_time/29*100, 0)}%')


animate(p_animated, 
        nframes = 30,  
        fps = 10,      
        width = 500, 
        height = 580,
        bg = "#b768a2",
        renderer = gifski_renderer("Caishen_animation.gif"))
