
# ggplotly ----------------------------------------------------------------

years <- c(1988, 1993, 1998, 2003, 2008)
txsmall <- df %>%
  select(bin_year, contcod, group, RRinc) %>%
  filter(bin_year %in% years)

txsmall %>%
  highlight_key(~contcod) %>% {
    ggplot(., aes(group, RRinc, group = contcod)) + geom_line() +
      facet_wrap(~bin_year, ncol = 2)
  } %>%
  ggplotly(tooltip = "contcod")



# plot_ly -----------------------------------------------------------------

txsmall %>%
  group_by(bin_year) %>%
  do(
    p = highlight_key(., ~contcod, group = "txhousing-trellis") %>%
      plot_ly(showlegend = FALSE) %>%
      group_by(contcod) %>%
      add_lines(
        x = ~group, y = ~RRinc, text = ~contcod,
        hoverinfo = "text"
      ) %>%
      add_annotations(
        text = ~unique(bin_year),
        x = 0.5, y = 1,
        xref = "paper", yref = "paper",
        xanchor = "center", yanchor = "bottom",
        showarrow = FALSE
      )
  ) %>%
  subplot(
    nrows = 2, margin = 0.05,
    shareY = TRUE, shareX = TRUE, titleY = FALSE
  )
