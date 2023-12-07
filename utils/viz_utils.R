
mapTheme <- function(base_size = 12, title_size = 12, subtitle_size = 10) {
  theme(
    text = element_text(color = "black"),
    plot.title = element_text(size = title_size, colour = "black"),
    plot.subtitle = element_text(face = "italic", size = subtitle_size),
    plot.caption = element_text(hjust = 0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    strip.text.x = element_text(size = 12)
  )
}

plotTheme <- function(base_size = 12, title_size = 12, subtitle_size = 10) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = title_size, colour = "black"), 
    plot.subtitle = element_text(face="italic", size = subtitle_size),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_line(size = 0.5),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14),
    axis.ticks.x = element_line(size = 0.5)
  )
}

### kable table

kablerize <- function(df, caption){
  df %>%
    kbl(caption = caption) %>%
    kable_minimal(full_width = F)
}
