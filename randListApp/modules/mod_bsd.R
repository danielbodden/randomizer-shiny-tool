# =============================================================================
# Module: Big Stick Design (BSD)
#
# bsdPar(N, mti, groups) — 2-arm only
# genSeq(par, seed = seed)
# =============================================================================

mod_bsd_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Parameters"),

      numericInput(ns("n"), "Total sample size:",
                   value = 10, min = 2, max = 10000, step = 1),

      numericInput(ns("mti"), "Maximum tolerated imbalance (MTI):",
                   value = 4, min = 1, step = 1),

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

      # Barcode pattern strip
      div(class = "rd-pattern-strip"),
      rand_steps_ui(),

      # Title + badge
      h2(class = "rd-method-title", "Big Stick Design"),
      span(class = "rd-method-badge badge-coral", "2-arm only"),

      # Description
      p(class = "rd-description",
        "The Big Stick Design corresponds to a fair coin toss within a boundary for
        the maximum tolerated imbalance (MTI). When the imbalance reaches the MTI,
        the next patient is assigned deterministically to restore balance.
        Setting MTI = N/2 is equivalent to Complete Randomization;
        setting MTI = 1 gives Permuted Block Randomization with blocks of size 2."),

      p(class = "rd-reference",
        "Soares JF, Wu CF (1983). Some restricted randomization rules in sequential
        designs. Communications in Statistics \u2013 Theory and Methods, 12(17), 2017\u20132034.
        doi:10.1080/03610928308828593"),

      # Results (hidden until generated)
      uiOutput(ns("results_ui"))
    )
  )
}


mod_bsd_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    seq_obj <- reactiveVal(NULL)

    # Generate sequence on button click
    observeEvent(input$generate, {
      n   <- min(input$n, 10000)
      mti <- max(1, input$mti)
      g1  <- if (nzchar(trimws(input$name1))) trimws(input$name1) else "A"
      g2  <- if (nzchar(trimws(input$name2))) trimws(input$name2) else "B"

      par <- bsdPar(N = n, mti = mti, groups = c(g1, g2))
      seq <- genSeq(par, seed = input$seed)
      seq_obj(seq)
    })

    # All results rendered in one block for clean animation
    output$results_ui <- renderUI({
      req(seq_obj())
      tagList(
        # Section: sequence
        div(class = "rd-section-label", tags$span("Randomization Sequence")),
        div(
          class = "rd-readout-wrap",
          div(class = "rd-readout-header",
              span(class = "rd-readout-title-bar", "Sequence")),
          verbatimTextOutput(session$ns("sequence"))
        ),
        # Section: walk plot
        div(class = "rd-section-label", tags$span("Randomization Walk")),
        div(class = "rd-plot-wrap",
            plotOutput(session$ns("walk_plot"), height = "280px")),
        # Section: download
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
      filename = function() "bsd_randomization.html",
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
