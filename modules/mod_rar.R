# =============================================================================
# Module: Random Allocation Rule (RAR)
#
# rarPar(N, K, ratio, groups) — supports 2–6 arms
# genSeq(par, seed = seed)
# Walk plot is only shown for 2-arm designs.
# =============================================================================

mod_rar_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Parameters"),

      numericInput(ns("n"), "Total sample size:",
                   value = 10, min = 2, max = 10000, step = 1),

      selectInput(ns("k"), "Number of treatment arms:",
                  choices = as.character(2:6), selected = "2",
                  selectize = FALSE),

      # Dynamic ratio and name inputs (rendered server-side)
      uiOutput(ns("ratio_inputs")),
      uiOutput(ns("name_inputs")),

      hr(),
      numericInput(ns("seed"), "Seed (for reproducibility):",
                   value = sample.int(2^31 - 1, 1)),

      actionButton(ns("generate"), "\u25b6  Generate", class = "btn-primary w-100 mt-2"),

      div(class = "rd-sidebar-note",
          tags$small("N must be divisible by the sum of allocation ratios to guarantee
                      exact balance. N will be rounded up if necessary."))
    ),

    # ── Main panel ────────────────────────────────────────────────────────
    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),

      h2(class = "rd-method-title", "Random Allocation Rule"),
      span(class = "rd-method-badge badge-green", "2 \u2013 6 arms"),

      p(class = "rd-description",
        "The Random Allocation Rule (Lachin 1988) samples uniformly at random from
        all permutations of a sequence that contains exactly N\u1d62 patients for each
        treatment i, where the N\u1d62 are determined by the allocation ratios. This
        guarantees exact balance at the end of the trial by construction. Unlike
        Complete Randomization, the final group sizes are fixed in advance; unlike
        Permuted Block Randomization, the balance is only guaranteed at the trial
        endpoint rather than after each block."),

      p(class = "rd-reference",
        "Lachin JM (1988). Statistical properties of randomization in clinical trials.
        Controlled Clinical Trials, 9(4), 289\u2013311.
        doi:10.1016/0197-2456(88)90045-5"),

      uiOutput(ns("results_ui"))
    )
  )
}


mod_rar_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    seq_obj <- reactiveVal(NULL)

    # ── Dynamic sidebar inputs ─────────────────────────────────────────────
    output$ratio_inputs <- renderUI({
      k <- as.integer(req(input$k))
      lapply(seq_len(k), function(i) {
        numericInput(
          session$ns(paste0("ratio", i)),
          paste0("Factor of treatment ", i, ":"),
          value = 1, min = 1, step = 1
        )
      })
    })

    output$name_inputs <- renderUI({
      k <- as.integer(req(input$k))
      lapply(seq_len(k), function(i) {
        textInput(
          session$ns(paste0("name", i)),
          paste0("Name of treatment ", i, ":"),
          value = LETTERS[i]
        )
      })
    })

    # ── Generate sequence ──────────────────────────────────────────────────
    observeEvent(input$generate, {
      n <- min(input$n, 10000)
      k <- as.integer(input$k)

      ratio  <- get_ratio(input, "ratio", k)
      groups <- get_groups(input, "name", k)

      # Round N up so it is divisible by sum(ratio)
      ratio_sum <- sum(ratio)
      if (n %% ratio_sum != 0) {
        n <- ceiling(n / ratio_sum) * ratio_sum
      }

      par <- rarPar(N = n, K = k, ratio = ratio, groups = groups)
      seq <- genSeq(par, seed = input$seed)
      seq_obj(seq)
    })

    # ── Outputs ────────────────────────────────────────────────────────────
    output$results_ui <- renderUI({
      req(seq_obj())
      show_plot <- as.integer(input$k) == 2
      tagList(
        div(class = "rd-section-label", tags$span("Randomization Sequence")),
        div(
          class = "rd-readout-wrap",
          div(class = "rd-readout-header",
              span(class = "rd-readout-title-bar", "Sequence")),
          verbatimTextOutput(session$ns("sequence"))
        ),
        if (show_plot) tagList(
          div(class = "rd-section-label", tags$span("Randomization Walk")),
          div(class = "rd-plot-wrap",
              plotOutput(session$ns("walk_plot"), height = "280px"))
        ),
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
      if (as.integer(input$k) != 2) return(NULL)
      rand_walk_plot(seq_obj()@M)
    })

    output$download <- downloadHandler(
      filename = function() "rar_randomization.html",
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
