# =============================================================================
# Module: Truncated Binomial Design (TBD)
#
# tbdPar(N, groups) — 2-arm only, N must be even
# genSeq(par, seed = seed)
# =============================================================================

mod_tbd_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Parameters"),

      numericInput(ns("n"), "Total sample size (must be even):",
                   value = 10, min = 2, max = 10000, step = 2),

      textInput(ns("name1"), "Name of treatment 1:", value = "A"),
      textInput(ns("name2"), "Name of treatment 2:", value = "B"),

      hr(),
      numericInput(ns("seed"), "Seed (for reproducibility):",
                   value = sample.int(2^31 - 1, 1)),

      actionButton(ns("generate"), "\u25b6  Generate", class = "btn-primary w-100 mt-2")
    ),

    # ── Main panel ────────────────────────────────────────────────────────
    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),

      h2(class = "rd-method-title", "Truncated Binomial Design"),
      span(class = "rd-method-badge badge-coral", "2-arm only"),

      p(class = "rd-description",
        "The Truncated Binomial Design assigns each patient via an independent fair
        coin toss until N/2 patients have been allocated to one treatment. All
        remaining patients then receive the other treatment. The sample size N must
        be even. The design guarantees perfect balance at the end of the trial while
        retaining some randomness in the assignment of the first N/2 \u2212 1 patients."),

      uiOutput(ns("results_ui"))
    )
  )
}


mod_tbd_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    seq_obj <- reactiveVal(NULL)

    observeEvent(input$generate, {
      n  <- min(input$n, 10000)
      # Enforce even N
      if (n %% 2 != 0) n <- n + 1
      g1 <- if (nzchar(trimws(input$name1))) trimws(input$name1) else "A"
      g2 <- if (nzchar(trimws(input$name2))) trimws(input$name2) else "B"

      par <- tbdPar(N = n, groups = c(g1, g2))
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
      filename = function() "tbd_randomization.html",
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
