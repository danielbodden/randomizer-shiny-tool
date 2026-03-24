# =============================================================================
# Module: Hadamard Randomization (HADA)
#
# hadaPar(N, groups) — 2-arm only, N must be a multiple of 4
# genSeq(par, seed = seed)
# =============================================================================

mod_hada_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Parameters"),

      numericInput(ns("n"), "Total sample size (multiple of 4):",
                   value = 12, min = 4, max = 10000, step = 4),

      textInput(ns("name1"), "Name of treatment 1:", value = "A"),
      textInput(ns("name2"), "Name of treatment 2:", value = "B"),

      hr(),
      numericInput(ns("seed"), "Seed (for reproducibility):",
                   value = sample.int(2^31 - 1, 1)),

      actionButton(ns("generate"), "\u25b6  Generate", class = "btn-primary w-100 mt-2"),

      div(class = "rd-sidebar-note",
          tags$small("N will be rounded up to the nearest multiple of 4."))
    ),

    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),

      h2(class = "rd-method-title", "Hadamard Randomization"),
      span(class = "rd-method-badge badge-coral", "2-arm only"),

      p(class = "rd-description",
        "Hadamard Randomization generates allocation sequences based on Hadamard
        matrices \u2014 square matrices whose entries are \u00b11 and whose rows are mutually
        orthogonal. This mathematical structure guarantees strong balance and
        orthogonality properties. The sample size N must be a multiple of 4.
        The design is particularly useful when strong balance guarantees are
        required alongside certain combinatorial properties."),

      uiOutput(ns("results_ui"))
    )
  )
}


mod_hada_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    seq_obj <- reactiveVal(NULL)

    observeEvent(input$generate, {
      n  <- min(input$n, 10000)
      # Round up to nearest multiple of 4
      if (n %% 4 != 0) n <- ceiling(n / 4) * 4
      g1 <- if (nzchar(trimws(input$name1))) trimws(input$name1) else "A"
      g2 <- if (nzchar(trimws(input$name2))) trimws(input$name2) else "B"

      par <- hadaPar(N = n, groups = c(g1, g2))
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
      filename = function() "hada_randomization.html",
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
