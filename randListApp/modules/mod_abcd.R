# =============================================================================
# Module: Adjustable Biased Coin Design (ABCD)
#
# abcdPar(N, a, groups) — 2-arm only
# genSeq(par, seed = seed)
# =============================================================================

mod_abcd_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Parameters"),

      numericInput(ns("n"), "Total sample size:",
                   value = 10, min = 2, max = 10000, step = 1),

      numericInput(ns("a"), "Adjustment parameter (a \u2265 0):",
                   value = 2, min = 0, step = 0.5),

      textInput(ns("name1"), "Name of treatment 1:", value = "A"),
      textInput(ns("name2"), "Name of treatment 2:", value = "B"),

      hr(),
      numericInput(ns("seed"), "Seed (for reproducibility):",
                   value = sample.int(2^31 - 1, 1)),

      actionButton(ns("generate"), "\u25b6  Generate", class = "btn-primary w-100 mt-2"),

      div(class = "rd-sidebar-note",
          tags$small("a = 0 gives Complete Randomization; larger values increase
                      the bias toward balance."))
    ),

    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),

      h2(class = "rd-method-title", "Adjustable Biased Coin Design"),
      span(class = "rd-method-badge badge-coral", "2-arm only"),

      p(class = "rd-description",
        "The Adjustable Biased Coin Design (Baldi Antognini & Giovagnoli, 2004)
        assigns each patient with a probability that depends on the current
        imbalance through a tuning parameter a \u2265 0. The larger the imbalance,
        the more strongly the coin is biased toward the under-represented
        treatment. Setting a = 0 gives Complete Randomization; as a increases
        the design becomes progressively more deterministic and balanced."),

      uiOutput(ns("results_ui"))
    )
  )
}


mod_abcd_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    seq_obj <- reactiveVal(NULL)

    observeEvent(input$generate, {
      n <- min(input$n, 10000)
      a <- max(0, input$a)
      g1 <- if (nzchar(trimws(input$name1))) trimws(input$name1) else "A"
      g2 <- if (nzchar(trimws(input$name2))) trimws(input$name2) else "B"

      par <- abcdPar(N = n, a = a, groups = c(g1, g2))
      seq <- genSeq(par, seed = input$seed)
      seq_obj(seq)
    })

    output$results_ui <- renderUI({
      req(seq_obj())
      tagList(
        div(class = "rd-section-label", tags$span("Randomization Sequence")),
        div(
          class = "rd-readout-wrap",
          div(class = "rd-readout-header",
              span(class = "rd-readout-title-bar", "Sequence")),
          verbatimTextOutput(session$ns("sequence"))
        ),
        div(class = "rd-section-label", tags$span("Randomization Walk")),
        div(class = "rd-plot-wrap",
            plotOutput(session$ns("walk_plot"), height = "280px")),
        div(class = "rd-section-label", tags$span("Export")),
        downloadButton(session$ns("download"), "\u2193  Download Report")
      )
    })

    output$sequence <- renderPrint({
      req(seq_obj())
      cat(as.vector(getRandList(seq_obj())))
    })

    output$walk_plot <- renderPlot({
      req(seq_obj())
      rand_walk_plot(seq_obj()@M)
    })

    output$download <- downloadHandler(
      filename = function() "abcd_randomization.html",
      content  = function(file) {
        rmarkdown::render(
          "Report/Report.Rmd",
          output_file = file,
          params      = list(Seq = seq_obj()),
          envir       = new.env(parent = globalenv())
        )
      }
    )
  })
}
