# =============================================================================
# Module: Chen's Design (CHEN)
#
# chenPar(N, mti, p, groups) — 2-arm only
# genSeq(par, seed = seed)
# =============================================================================

mod_chen_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Parameters"),

      numericInput(ns("n"), "Total sample size:",
                   value = 10, min = 2, max = 10000, step = 1),

      numericInput(ns("mti"), "Maximum tolerated imbalance (MTI):",
                   value = 3, min = 1, step = 1),

      numericInput(ns("p"), "Biasing probability (p):",
                   value = 0.75, min = 0.5, max = 1, step = 0.05),

      textInput(ns("name1"), "Name of treatment 1:", value = "A"),
      textInput(ns("name2"), "Name of treatment 2:", value = "B"),

      hr(),
      numericInput(ns("seed"), "Seed (for reproducibility):",
                   value = sample.int(2^31 - 1, 1)),

      actionButton(ns("generate"), "\u25b6  Generate", class = "btn-primary w-100 mt-2")
    ),

    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),
      rand_steps_ui(),

      h2(class = "rd-method-title", "Chen's Design"),
      span(class = "rd-method-badge badge-coral", "2-arm only"),

      p(class = "rd-description",
        "Chen's Design (2000) combines the Big Stick Design with Efron's Biased
        Coin. When the imbalance is strictly below the MTI, a biased coin with
        probability p is used to favour the under-represented treatment. When the
        MTI is reached, the next patient is assigned deterministically to restore
        balance. Setting p = 0.5 recovers the Big Stick Design; setting
        MTI = N gives Efron's Biased Coin Design."),

      p(class = "rd-reference",
        "Chen YP (1999). Biased coin design with imbalance tolerance.
        Communications in Statistics \u2013 Stochastic Models, 15(5), 953\u2013975.
        doi:10.1080/15326349908807570"),

      uiOutput(ns("results_ui"))
    )
  )
}


mod_chen_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    seq_obj <- reactiveVal(NULL)

    observeEvent(input$generate, {
      n   <- min(input$n, 10000)
      mti <- max(1, input$mti)
      p   <- max(0.5, min(1, input$p))
      g1  <- if (nzchar(trimws(input$name1))) trimws(input$name1) else "A"
      g2  <- if (nzchar(trimws(input$name2))) trimws(input$name2) else "B"

      par <- chenPar(N = n, mti = mti, p = p, groups = c(g1, g2))
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
      filename = function() "chen_randomization.html",
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
