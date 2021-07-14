

ma_theme <- theme(
  line = element_blank(),
  text = element_text(family = "DejaVu Sans Mono", color = "#4a4a4a"),
  panel.background = element_rect(fill = "#efe8d1"),
  panel.grid.major = element_line(colour = "#9ae5de", size = 0.1),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  axis.text.x = element_text(hjust = 0.7),
  plot.background = element_rect(fill = "#efe8d1", color = NA),
  legend.background = element_rect(fill = "#efe8d1"),
  strip.background = element_blank()
)