# =============================================================================
# Module: Random Truncated Binomial Design (RTBD)
#
# rtbdPar(N, rb, filledBlock, groups) — 2-arm only
# rb: vector of possible block sizes (one chosen at random per block)
# genSeq(par, seed = seed)
# =============================================================================

mod_rtbd_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Parameters"),

      numericInput(ns("n"), "Total sample size:",
                   value = 20, min = 2, max = 10000, step = 1),

      textInput(ns("rb"), "Possible block sizes (comma-separated, must be even):",
                value = "2, 4, 6"),

      checkboxInput(ns("filled_block"), "Fill last block if incomplete",
                    value = TRUE),

      textInput(ns("name1"), "Name of treatment 1:", value = "A"),
      textInput(ns("name2"), "Name of treatment 2:", value = "B"),

      hr(),
      numericInput(ns("seed"), "Seed (for reproducibility):",
                   value = sample.int(2^31 - 1, 1)),

      actionButton(ns("generate"), "\u25b6  Generate", class = "btn-primary w-100 mt-2"),

      div(class = "rd-sidebar-note",
          tags$small("Block sizes must be even for equal 1:1 allocation within each block."))
    ),

    # ── Main panel ────────────────────────────────────────────────────────
    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),

      h2(class = "rd-method-title", "Random Truncated Binomial Design"),
      span(class = "rd-method-badge badge-coral", "2-arm only"),

      p(class = "rd-description",
        "The Random Truncated Binomial Design applies the Truncated Binomial
        procedure within blocks of randomly chosen sizes. Within each block,
        patients are assigned via coin tosses until half the block is allocated
        to one treatment, with the remaining patients receiving the other.
        Random block sizes prevent prediction of upcoming allocations, combining
        the within-block balance guarantee of the Truncated Binomial Design with
        the unpredictability of random block lengths."),

      uiOutput(ns("results_ui"))
    )
  )
}


mod_rtbd_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    seq_obj <- reactiveVal(NULL)

    observeEvent(input$generate, {
      n    <- min(input$n, 10000)
      rb   <- suppressWarnings(
        as.integer(strsplit(trimws(input$rb), "[,\\s]+")[[1]])
      )
      rb   <- rb[!is.na(rb) & rb >= 2]
      if (length(rb) == 0) rb <- c(2L, 4L)
      # Enforce even block sizes
      rb   <- rb[rb %% 2 == 0]
      if (length(rb) == 0) rb <- c(2L, 4L)

      filled <- isTRUE(input$filled_block)
      g1 <- if (nzchar(trimws(input$name1))) trimws(input$name1) else "A"
      g2 <- if (nzchar(trimws(input$name2))) trimws(input$name2) else "B"

      par <- rtbdPar(N = n, rb = rb, filledBlock = filled, groups = c(g1, g2))
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
      filename = function() "rtbd_randomization.html",
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
