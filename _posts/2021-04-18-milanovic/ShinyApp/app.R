library(shiny)
library(plotly)
library(tidyverse)
library(haven)


df <- read_dta("lm_wpid_web.dta")
attach(df)

f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "lightgrey"
)

f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "black"
)

f2y <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "black"
)

a <- list(
    autotick = FALSE,
    ticks = "outside",
    tick0 = 0,
    dtick = 1,
    ticklen = 5,
    tickwidth = 1.5,
    tickcolor = toRGB("black"),
    title = "Deciles",
    titlefont = f1,
    showticklabels = TRUE,
    tickangle = 0,
    tickfont = f2,
    
    gridcolor = toRGB("gray95"),
    gridwidth = 1
)

b <- list(
    autotick = FALSE,
    ticks = "outside",
    tick0 = 0,
    dtick = 10000,
    ticklen = 5,
    tickwidth = 1.5,
    tickcolor = toRGB("black"),
    title = "USD - PPP 2005",
    titlefont = f1,
    showticklabels = TRUE,
    tickangle = 0,
    tickfont = f2y,
    
    gridcolor = toRGB("gray95"),
    gridwidth = 1
)


ui <- fluidPage(
    titlePanel("Ingreso promedio per cápita (PPP 2005), período 1988-2008"),
    selectizeInput(
        inputId = "country",
        label = "Seleccionar países",
        choices = unique(df$country),
        selected = "Chile",
        multiple = TRUE
    ),
    plotlyOutput(outputId = "p")
)

server <- function(input, output, ...) {
    
    output$p <- renderPlotly({
        
        df %>%
            group_by(bin_year) %>%
            do(
                p = highlight_key(., ~country, group = "txhousing-trellis") %>%
                    plot_ly(x = ~group, y = ~RRinc, color = ~country,
                            colors =  "Accent",
                            showlegend = F, 
                            legendgroup = ~country,
                            width = 1000, 
                            height = 600) %>%
                    filter(country %in% input$country) %>% 
                    group_by(country) %>%
                    add_trace(type = "scatter",
                              mode = "markers+lines") %>%
                    add_lines(x = ~group, y = ~RRinc, 
                              text = ~country,
                              hoverinfo = "text") %>%
                    add_annotations(text = ~unique(bin_year),
                                    x = 0.5, y = 1,
                                    xref = "paper", yref = "paper",
                                    xanchor = "center",
                                    yanchor = "top",
                                    showarrow = FALSE
                                    ) %>% 
                    layout(title = '', 
                           yaxis = b, 
                           xaxis = a,
                           autosize = F, 
                           margin = list(
                               l = 75,
                               r = 50,
                               b = 100,
                               t = 50,
                               pad = 4
                           ))
            ) %>%
            subplot(
                nrows = 2, margin = 0.01,
                shareY = TRUE, shareX = TRUE, titleY = TRUE
            )
  
    }) 
}

shinyApp(ui, server)

