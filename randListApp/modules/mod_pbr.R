# =============================================================================
# Module: Permuted Block Randomization (PBR)
#
# pbrPar(bc, groups) — 2-arm only
# bc: vector of block sizes, one entry per block
# genSeq(par, seed = seed)
# =============================================================================

mod_pbr_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Parameters"),

      numericInput(ns("n"), "Target sample size:",
                   value = 12, min = 2, max = 10000, step = 1),

      numericInput(ns("block_size"), "Block size:",
                   value = 4, min = 2, max = 1000, step = 2),

      textInput(ns("name1"), "Name of treatment 1:", value = "A"),
      textInput(ns("name2"), "Name of treatment 2:", value = "B"),

      hr(),
      numericInput(ns("seed"), "Seed (for reproducibility):",
                   value = sample.int(2^31 - 1, 1)),

      actionButton(ns("generate"), "\u25b6  Generate", class = "btn-primary w-100 mt-2"),

      div(class = "rd-sidebar-note",
          tags$small("Note: the actual sample size will be rounded up to the
                      nearest multiple of the block size."))
    ),

    # ── Main panel ────────────────────────────────────────────────────────
    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),
      rand_steps_ui(),

      h2(class = "rd-method-title", "Permuted Block Randomization"),
      span(class = "rd-method-badge badge-coral", "2-arm only"),

      p(class = "rd-description",
        "Permuted Block Randomization divides the patient sequence into blocks of a
        fixed size. Within each block, treatment assignments are a uniformly random
        permutation of equal numbers of each treatment. Balance is guaranteed at the
        end of every block."),

      p(class = "rd-reference",
        "Matts JP, Lachin JM (1988). Properties of permuted-block randomization in
        clinical trials. Controlled Clinical Trials, 9(4), 327\u2013344.
        doi:10.1016/0197-2456(88)90049-2"),

      uiOutput(ns("results_ui"))
    )
  )
}


mod_pbr_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    seq_obj <- reactiveVal(NULL)

    observeEvent(input$generate, {
      n          <- min(input$n, 10000)
      block_size <- max(2, input$block_size)
      # Enforce even block size for equal 1:1 allocation
      if (block_size %% 2 != 0) block_size <- block_size + 1
      n_blocks   <- ceiling(n / block_size)
      bc         <- rep(block_size, n_blocks)

      g1 <- if (nzchar(trimws(input$name1))) trimws(input$name1) else "A"
      g2 <- if (nzchar(trimws(input$name2))) trimws(input$name2) else "B"

      par <- pbrPar(bc = bc, groups = c(g1, g2))
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
      filename = function() "pbr_randomization.html",
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
