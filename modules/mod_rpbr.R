# =============================================================================
# Module: Random Permuted Block Randomization (RPBR)
#
# rpbrPar(N, rb, filledBlock, groups) — 2-arm only
# rb: vector of possible block sizes (one chosen at random per block)
# genSeq(par, seed = seed)
# =============================================================================

mod_rpbr_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Parameters"),

      numericInput(ns("n"), "Total sample size:",
                   value = 20, min = 2, max = 10000, step = 1),

      textInput(ns("rb"), "Possible block sizes (comma-separated):",
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
          tags$small("Block sizes must be even numbers for equal 1:1 allocation."))
    ),

    # ── Main panel ────────────────────────────────────────────────────────
    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),
      rand_steps_ui(),

      h2(class = "rd-method-title", "Random Permuted Block Randomization"),
      span(class = "rd-method-badge badge-coral", "2-arm only"),

      p(class = "rd-description",
        "Random Permuted Block Randomization applies the permuted block approach
        with block sizes chosen at random from a specified set for each block,
        retaining approximate balance."),

      p(class = "rd-reference",
        "Matts JP, Lachin JM (1988). Properties of permuted-block randomization in
        clinical trials. Controlled Clinical Trials, 9(4), 327\u2013344.
        doi:10.1016/0197-2456(88)90049-2"),

      uiOutput(ns("results_ui"))
    )
  )
}


mod_rpbr_server <- function(id) {
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

      par <- rpbrPar(N = n, rb = rb, filledBlock = filled, groups = c(g1, g2))
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
      filename = function() "rpbr_randomization.html",
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
