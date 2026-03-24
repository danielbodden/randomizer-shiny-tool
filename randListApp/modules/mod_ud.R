# =============================================================================
# Module: Wei's Urn Design (UD)
#
# udPar(N, ini, add, groups) — 2-arm only
# genSeq(par, seed = seed)
# =============================================================================

mod_ud_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Parameters"),

      numericInput(ns("n"), "Total sample size:",
                   value = 10, min = 2, max = 10000, step = 1),

      numericInput(ns("ini"), "Initial urn composition (balls per treatment):",
                   value = 1, min = 1, max = 100, step = 1),

      numericInput(ns("add"), "Balls added after each allocation:",
                   value = 1, min = 1, max = 100, step = 1),

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

      h2(class = "rd-method-title", "Wei's Urn Design"),
      span(class = "rd-method-badge badge-coral", "2-arm only"),

      p(class = "rd-description",
        "Wei's Urn Design (1978) is a P\u00f3lya urn model. The urn starts with ini
        balls of each treatment type. A ball is drawn at random to determine the
        allocation, replaced, and then add balls of the opposite type are added
        to the urn. This self-correcting mechanism gradually reduces imbalance
        while maintaining unpredictability. Larger add values increase the speed
        of balance correction."),

      uiOutput(ns("results_ui"))
    )
  )
}


mod_ud_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    seq_obj <- reactiveVal(NULL)

    observeEvent(input$generate, {
      n   <- min(input$n, 10000)
      ini <- max(1, input$ini)
      add <- max(1, input$add)
      g1  <- if (nzchar(trimws(input$name1))) trimws(input$name1) else "A"
      g2  <- if (nzchar(trimws(input$name2))) trimws(input$name2) else "B"

      par <- udPar(N = n, ini = ini, add = add, groups = c(g1, g2))
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
      filename = function() "ud_randomization.html",
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
