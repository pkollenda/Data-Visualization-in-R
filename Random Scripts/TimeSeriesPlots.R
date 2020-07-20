df <- structure(list(Country = c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "C", "C", "C", "C", "C"), 
               Year = rep(c(2015, 2016, 2017, 2018, 2019), 3), 
               unempl = c(4.8, 5.8, 6.49, 5.14, 5.61, 8.64, 9.32, 10.5, 9.36, 12.37, 8.3, 7.8, 8.1, 5.14, 5), 
               particip = c(63, 62.64, 62.81, 60.61, 60.09, 74, 75.86, 74.5, 75.63, 77.83, 66.2, 64.6, 65.93, 65.54, 72.19)), 
          row.names = c(NA, -15L), class = c("tbl_df", "tbl", "data.frame"))

above <- ggplot(df, aes(x = Year, group = Country, color =  Country, fill = Country)) + 
  scale_fill_viridis_d() + scale_color_viridis_d() + 
  geom_line(aes(y = unempl), size = 1.5) + ylab("Unemployment Rate") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 0.1), limits = c(0,15), breaks = seq(0,15,2.5)) + 
  cowplot::theme_minimal_hgrid() +
  theme(axis.title = element_text(face = "italic"), axis.text.x = element_blank(), axis.title.x = element_blank(), 
        legend.title = element_text(face = "bold"), legend.margin = margin(t=0.3, r=0.3, b=0.3, l=0.3, unit="cm"),
        legend.background = element_rect(fill="lightgrey"), legend.direction = "vertical")

legend <- cowplot::get_legend(above + theme(legend.box.margin = margin(0, 3, 0, 3, unit = "cm")))
                              
below <- ggplot(df, aes(x = Year, group = Country, color =  Country, fill = Country)) + 
  geom_col(aes(y = particip), position = position_dodge(width = 0.8), alpha = 0.8, show.legend = FALSE) + 
  cowplot::theme_minimal_hgrid() + ylab("Participation Rate") + 
  theme(axis.title = element_text(face = "italic"), axis.title.x = element_text(face = "bold"), axis.text.x = element_text(face = "bold")) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 0.1), limits = c(0, 100)) + 
  scale_fill_viridis_d() + scale_color_viridis_d()

a <- cowplot::plot_grid(above + theme(legend.position = "none", plot.margin = margin(0,0,40,0)), below + theme(legend.position = "none"), nrow = 2, ncol = 1, rel_heights = c(0.7, 0.3))
cowplot::plot_grid(a, legend, rel_widths = c(5, 1.2))
