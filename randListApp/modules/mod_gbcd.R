# =============================================================================
# Module: Generalized Biased Coin Design (GBCD)
#
# gbcdPar(N, rho, groups) — 2-arm only
# genSeq(par, seed = seed)
# =============================================================================

mod_gbcd_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Parameters"),

      numericInput(ns("n"), "Total sample size:",
                   value = 10, min = 2, max = 10000, step = 1),

      numericInput(ns("rho"), "Design parameter (\u03c1 \u2265 0):",
                   value = 2, min = 0, step = 0.5),

      textInput(ns("name1"), "Name of treatment 1:", value = "A"),
      textInput(ns("name2"), "Name of treatment 2:", value = "B"),

      hr(),
      numericInput(ns("seed"), "Seed (for reproducibility):",
                   value = sample.int(2^31 - 1, 1)),

      actionButton(ns("generate"), "\u25b6  Generate", class = "btn-primary w-100 mt-2"),

      div(class = "rd-sidebar-note",
          tags$small("\u03c1 = 0 gives Complete Randomization; \u03c1 = 1 gives
                      Efron's Biased Coin; larger values increase balance."))
    ),

    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),

      h2(class = "rd-method-title", "Generalized Biased Coin Design"),
      span(class = "rd-method-badge badge-coral", "2-arm only"),

      p(class = "rd-description",
        "The Generalized Biased Coin Design (Efron, 1971; Smith, 1984) extends
        Efron's original Biased Coin Design with a continuous parameter \u03c1 \u2265 0.
        The allocation probability for the under-represented treatment is a
        function of the current imbalance and \u03c1. Setting \u03c1 = 0 gives Complete
        Randomization; \u03c1 = 1 corresponds to Efron's original design; larger
        \u03c1 values produce stronger balance at the cost of predictability."),

      uiOutput(ns("results_ui"))
    )
  )
}


mod_gbcd_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    seq_obj <- reactiveVal(NULL)

    observeEvent(input$generate, {
      n   <- min(input$n, 10000)
      rho <- max(0, input$rho)
      g1  <- if (nzchar(trimws(input$name1))) trimws(input$name1) else "A"
      g2  <- if (nzchar(trimws(input$name2))) trimws(input$name2) else "B"

      par <- gbcdPar(N = n, rho = rho, groups = c(g1, g2))
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
      filename = function() "gbcd_randomization.html",
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
