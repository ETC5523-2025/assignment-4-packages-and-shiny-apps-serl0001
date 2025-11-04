# inst/app/app.R
# HAI Explorer (uses package data: hai_germany_2011)

library(shiny)
library(ggplot2)
library(bslib)

# ---------- labels & helpers ----------
metric_label <- function(m) c(
  dalys="DALYs", yll="YLL", yld="YLD", cases="Cases", deaths="Deaths"
)[m]

to_long_y <- function(d){
  rbind(
    data.frame(type = d$type, component = "YLL", value = d$yll),
    data.frame(type = d$type, component = "YLD", value = d$yld)
  )
}

# ---------- pastel palettes ----------
pal_types_pastel <- c(
  HAP = "#AEC9EB",  # pastel blue
  UTI = "#F9CBD6",  # pastel pink
  BSI = "#C9DDF2",  # lighter blue
  SSI = "#FCDDE4",  # lighter pink
  CDI = "#BFD7EE",  # blue
  All = "#E6B8C8"   # rose
)
pal_comp_pastel <- c(
  YLL = "#A7C6ED",  # blue
  YLD = "#F7B7C3"   # pink
)

# ---------- theme ----------
theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary   = "#9BBCE0",  # pastel blue
  secondary = "#F7B7C3",  # pastel pink
  base_font    = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Poppins")
)

# reusable ggplot theme add-on
my_gg <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#E6EEF7"),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "bold", color = "#4D6582")
  )

# ---------- UI ----------
ui <- fluidPage(
  theme = theme,
  tags$head(tags$style(HTML("
    body { background: #FAFBFF; }
    .card {
      background:#FFFFFF; border-radius:18px;
      box-shadow:0 12px 28px rgba(155,188,224,.22);
      border:1px solid rgba(247,183,195,.28);
      padding:18px 20px; margin-bottom:18px;
    }
    .muted { color:#5f6671; }
    .kpi {
      display:inline-block; min-width:200px; margin-right:12px;
      padding:12px 14px; border-radius:14px;
      background:#FDF7F9; border:1px solid #F7B7C3;
    }
    .kpi .t { font-size:11px; letter-spacing:.5px; color:#6b7b8c; text-transform:uppercase; }
    .kpi .v { font-size:22px; font-weight:700; color:#4D6582; }
    .btn, .form-control { border-radius:10px; }
  "))),
  titlePanel("HAI Burden Explorer — Germany (PPS 2011)"),

  fluidRow(
    column(
      width = 4,
      div(class="card",
          h4("Controls"),
          radioButtons("metric", "Metric",
                       choices = c("DALYs (overall)"="dalys","YLL (mortality)"="yll",
                                   "YLD (morbidity)"="yld","Cases"="cases","Deaths"="deaths"),
                       selected = "dalys"
          ),
          checkboxGroupInput("types", "HAI types",
                             choices = c("HAP","UTI","BSI","SSI","CDI","All"),
                             selected = c("HAP","UTI","BSI","SSI","CDI")
          ),
          sliderInput("top_n","Show top N (ranked by metric)", min=1, max=6, value=5, step=1),
          checkboxInput("show_ci","Show 95% uncertainty intervals (if available)", TRUE),
          checkboxInput("stack_dalys","If DALYs selected, show stacked YLL + YLD", TRUE),
          radioButtons("colour_by","Colour bars by",
                       choices = c("Type"="type","Component (YLL/YLD when applicable)"="component"),
                       selected = "type"
          )
      ),
      div(class="card",
          h4("What the fields mean"),
          tags$ul(class="muted",
                  tags$li(tags$b("Cases:"),  " estimated annual cases."),
                  tags$li(tags$b("Deaths:"), " attributable deaths."),
                  tags$li(tags$b("DALYs:"),  " disability-adjusted life years = ", tags$b("YLL + YLD"), "."),
                  tags$li(tags$b("YLL:"),    " years of life lost (mortality)."),
                  tags$li(tags$b("YLD:"),    " years lived with disability (morbidity).")
          )
      ),
      div(class="card",
          h4("Export"),
          downloadButton("dl_plot", "Download plot (PNG)"),
          downloadButton("dl_data", "Download data (CSV)"),
          div(class="muted", style="margin-top:8px;",
              "Exports reflect your current selections.")
      )
    ),

    column(
      width = 8,
      div(class="card",
          h4("Key figures"),
          uiOutput("kpis")
      ),
      div(class="card",
          h4(textOutput("plot_title")),
          plotOutput("bar", height = "460px"),
          div(class="muted",
              "Error bars (when shown) are 95% uncertainty intervals. Germany total DALYs per 100k: ",
              code(attr(hai_germany_2011, "dalys_per_100k_total"))
          )
      ),
      div(class="card",
          h4("How to interpret"),
          p(class="muted",
            "Switch metrics to diagnose whether burden is driven by ",
            strong("mortality (YLL)"), " or ", strong("morbidity (YLD)"), ". ",
            "Stacked DALYs reveals the YLL/YLD composition. ",
            "Colour by ", em("Type"), " to compare infections; ",
            "colour by ", em("Component"), " to emphasise mortality vs morbidity. ",
            "Use Top-N and type filters to focus the narrative."
          ),
          verbatimTextOutput("narrative", placeholder = TRUE)
      ),
      div(class="card",
          h4("Numbers behind the chart"),
          tableOutput("tbl")
      )
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session){

  base <- hai_germany_2011  # package data (no read.csv)

  # reactive selection
  dat_sel <- reactive({
    req(input$metric, input$types, input$top_n)
    d <- base[base$type %in% input$types, , drop = FALSE]
    m <- input$metric
    d <- d[order(d[[m]], decreasing = TRUE), , drop = FALSE]
    utils::head(d, input$top_n)
  })

  # KPIs
  output$kpis <- renderUI({
    d <- dat_sel(); m <- input$metric
    if (!nrow(d)) return(div(class="muted","Select at least one type."))
    total <- sum(d[[m]], na.rm = TRUE)
    topv  <- d[[m]][1]; topn <- d$type[1]
    share <- if (total > 0) paste0(round(100*topv/total,1), "%") else "—"
    tagList(
      div(class="kpi", div(class="t","Metric"), div(class="v", metric_label(m))),
      div(class="kpi", div(class="t","Total (selected)"), div(class="v", format(total, big.mark=","))),
      div(class="kpi", div(class="t","Top contributor"),
          div(class="v", paste0(topn, ": ", format(topv, big.mark=","))),
          div(class="t", paste("Share:", share)))
    )
  })

  output$plot_title <- renderText({
    paste("Burden by infection type —", metric_label(input$metric))
  })

  # main plot
  output$bar <- renderPlot({
    d <- dat_sel(); m <- input$metric
    validate(need(nrow(d) > 0, "No data to display."))

    # stacked DALYs with component colours
    if (m == "dalys" && isTRUE(input$stack_dalys)) {
      long <- to_long_y(subset(d, type != "All"))
      ggplot(long, aes(x = reorder(type, value), y = value,
                       fill = component, colour = component)) +
        geom_col() +
        scale_fill_manual(values = pal_comp_pastel) +
        scale_colour_manual(values = pal_comp_pastel, guide = "none") +
        coord_flip() +
        labs(x = "HAI type", y = "DALYs (YLL + YLD)") +
        my_gg
    } else {
      mlo <- paste0(m, "_lo"); mhi <- paste0(m, "_hi")
      have_ci <- isTRUE(input$show_ci) && all(c(mlo, mhi) %in% names(d))

      if (input$colour_by == "component" && m %in% c("yll","yld")) {
        d$comp_fill <- if (m == "yll") "YLL" else "YLD"
        ggplot(d, aes(reorder(type, .data[[m]]), .data[[m]], fill = comp_fill)) +
          geom_col() +
          { if (have_ci) geom_errorbar(aes(ymin=.data[[mlo]], ymax=.data[[mhi]]), width=0.28) } +
          scale_fill_manual(values = pal_comp_pastel, name = NULL) +
          coord_flip() + labs(x="HAI type", y = metric_label(m)) +
          my_gg
      } else {
        ggplot(d, aes(reorder(type, .data[[m]]), .data[[m]], fill = type)) +
          geom_col() +
          { if (have_ci) geom_errorbar(aes(ymin=.data[[mlo]], ymax=.data[[mhi]]), width=0.28) } +
          scale_fill_manual(values = pal_types_pastel, drop = FALSE, name = NULL) +
          coord_flip() + labs(x="HAI type", y = metric_label(m)) +
          my_gg
      }
    }
  })

  # narrative
  output$narrative <- renderPrint({
    d <- dat_sel(); m <- input$metric
    if (!nrow(d)) return(cat("No selection.\n"))
    ord <- d[order(d[[m]], decreasing = TRUE), ]
    total <- sum(d[[m]], na.rm = TRUE)
    share <- if (total>0) round(100*ord[[m]][1]/total, 1) else NA_real_
    cat(
      sprintf("Top = %s (%s = %s).", ord$type[1], metric_label(m),
              format(ord[[m]][1], big.mark=",")),
      sprintf("\nTotal (selected) = %s.", format(total, big.mark=",")),
      if (!is.na(share)) sprintf("\nTop share = %.1f%%.", share) else "",
      if (m=="dalys"){
        yll <- sum(subset(d, type!="All")$yll); yld <- sum(subset(d, type!="All")$yld)
        sprintf("\nDALY composition (selected, excl. 'All'): %s%% YLL vs %s%% YLD.",
                round(100*yll/(yll+yld),1), round(100*yld/(yll+yld),1))
      } else "",
      sep = ""
    )
  })

  # table
  output$tbl <- renderTable({
    d <- dat_sel(); m <- input$metric
    cols <- c("type","cases","deaths","yll","yld","dalys", paste0(m,"_lo"), paste0(m,"_hi"))
    cols <- intersect(cols, names(d))
    out <- d[, cols, drop = FALSE]
    names(out) <- toupper(names(out))
    out
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

  # downloads
  output$dl_data <- downloadHandler(
    filename = function() paste0("hai_selection_", input$metric, ".csv"),
    content  = function(file) utils::write.csv(dat_sel(), file, row.names = FALSE)
  )

  output$dl_plot <- downloadHandler(
    filename = function() paste0("hai_plot_", input$metric, ".png"),
    content = function(file) {
      d <- dat_sel(); m <- input$metric
      p <- NULL
      if (m == "dalys" && isTRUE(input$stack_dalys)) {
        long <- to_long_y(subset(d, type != "All"))
        p <- ggplot(long, aes(x = reorder(type, value), y = value,
                              fill = component, colour = component)) +
          geom_col() +
          scale_fill_manual(values = pal_comp_pastel) +
          scale_colour_manual(values = pal_comp_pastel, guide = "none") +
          coord_flip() + labs(x="HAI type", y="DALYs (YLL + YLD)") + my_gg
      } else {
        mlo <- paste0(m, "_lo"); mhi <- paste0(m, "_hi")
        have_ci <- isTRUE(input$show_ci) && all(c(mlo, mhi) %in% names(d))
        if (input$colour_by == "component" && m %in% c("yll","yld")) {
          d$comp_fill <- if (m == "yll") "YLL" else "YLD"
          p <- ggplot(d, aes(reorder(type, .data[[m]]), .data[[m]], fill = comp_fill)) +
            geom_col() +
            { if (have_ci) geom_errorbar(aes(ymin=.data[[mlo]], ymax=.data[[mhi]]), width=0.28) } +
            scale_fill_manual(values = pal_comp_pastel, name = NULL) +
            coord_flip() + labs(x="HAI type", y = metric_label(m)) + my_gg
        } else {
          p <- ggplot(d, aes(reorder(type, .data[[m]]), .data[[m]], fill = type)) +
            geom_col() +
            { if (have_ci) geom_errorbar(aes(ymin=.data[[mlo]], ymax=.data[[mhi]]), width=0.28) } +
            scale_fill_manual(values = pal_types_pastel, drop = FALSE, name = NULL) +
            coord_flip() + labs(x="HAI type", y = metric_label(m)) + my_gg
        }
      }
      ggsave(file, p, width = 8.6, height = 5.2, dpi = 300)
    }
  )
}

shinyApp(ui, server)
