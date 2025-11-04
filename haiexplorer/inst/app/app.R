library(shiny)
library(ggplot2)

.to_long_y <- function(dat){
  rbind(
    data.frame(type = dat$type, component = "YLL", value = dat$yll),
    data.frame(type = dat$type, component = "YLD", value = dat$yld)
  )
}

ui <- fluidPage(
  titlePanel("HAI Burden Explorer — Germany (PPS 2011)"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("metric", "Select metric",
                   choices = c("DALYs"="dalys","YLL"="yll","YLD"="yld","Cases"="cases","Deaths"="deaths"),
                   selected = "dalys"
      ),
      checkboxGroupInput("types", "HAI types",
                         choices = c("HAP","UTI","BSI","SSI","CDI","All"),
                         selected = c("HAP","UTI","BSI","SSI","CDI")
      ),
      checkboxInput("stack_dalys", "When DALYs is selected, show YLL+YLD stacked", TRUE)
    ),
    mainPanel(
      h4("What the fields mean"),
      tags$ul(
        tags$li("Cases: estimated annual cases"),
        tags$li("Deaths: attributable deaths"),
        tags$li("DALYs: disability-adjusted life years"),
        tags$li("YLL: years of life lost (mortality)"),
        tags$li("YLD: years lived with disability (morbidity)")
      ),
      h4("How to interpret"),
      p("Switch metrics to compare mortality (YLL) vs morbidity (YLD). Higher values = greater burden."),
      plotOutput("bar", height = "420px"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output, session){
  dat0 <- hai_germany_2011
  dat_sel <- reactive(subset(dat0, type %in% input$types))

  output$bar <- renderPlot({
    d <- dat_sel()
    if (input$metric == "dalys" && isTRUE(input$stack_dalys)) {
      long <- .to_long_y(d[d$type != "All", , drop = FALSE])
      ggplot(long, aes(reorder(type, value), value, fill = component)) +
        geom_col() + coord_flip() +
        labs(x = "HAI type", y = "DALYs (YLL + YLD)", title = "DALYs split into YLL + YLD") +
        theme_minimal(base_size = 12)
    } else {
      m <- input$metric
      mlo <- paste0(m, "_lo"); mhi <- paste0(m, "_hi")
      use_ci <- all(c(mlo, mhi) %in% names(d))
      ggplot(d, aes(reorder(type, .data[[m]]), .data[[m]])) +
        geom_col() +
        { if (use_ci) geom_errorbar(aes(ymin = .data[[mlo]], ymax = .data[[mhi]]), width = 0.25) } +
        coord_flip() +
        labs(x = "HAI type", y = toupper(m), title = paste("Burden by infection type —", toupper(m))) +
        theme_minimal(base_size = 12)
    }
  })

  output$summary <- renderPrint({
    d <- dat_sel(); m <- input$metric
    d_ord <- d[order(d[[m]], decreasing = TRUE), ]
    list(
      top_type = d_ord$type[1],
      top_value = d_ord[[m]][1],
      total_selected_types = sum(d[[m]], na.rm = TRUE),
      dalys_per_100k_total = attr(hai_germany_2011, "dalys_per_100k_total")
    )
  })
}

shinyApp(ui, server)
