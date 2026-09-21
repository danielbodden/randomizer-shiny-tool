# =============================================================================
# Module: Efron's Biased Coin Design (EBC)
#
# ebcPar(N, p, groups) — 2-arm only
# genSeq(par, seed = seed)
# =============================================================================

mod_ebc_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Parameters"),

      rd_n_input(ns("n"), "Total sample size:", 10),

      numericInput(ns("p"), "Biasing probability (p ∈ [0.5, 1]):",
                   value = 0.75, min = 0.5, max = 1, step = 0.05),

      textInput(ns("name1"), "Name of treatment 1:", value = "A"),
      textInput(ns("name2"), "Name of treatment 2:", value = "B"),

      hr(),
      rd_seed_input(ns("seed")),

      actionButton(ns("generate"), "\u25b6  Generate", class = "btn-primary w-100 mt-2")
    ),

    # ── Main panel ────────────────────────────────────────────────────────
    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),
      rand_steps_ui(),

      h2(class = "rd-method-title", "Efron's Biased Coin Design"),
      span(class = "rd-method-badge badge-coral", "2-arm only"),

      p(class = "rd-description",
        "Efron's Biased Coin Design (1971) uses an adaptive coin whose bias depends
        on the current imbalance. When both groups are equal, a fair coin is tossed.
        When one group leads, the next patient is assigned to the under-represented
        treatment with probability p. The biasing probability must lie in
        [0.5, 1]: setting p = 0.5 gives Complete Randomization, p = 1 gives
        Permuted Block Randomization with blocks of size 2."),

      p(class = "rd-reference",
        "Efron B (1971). Forcing a sequential experiment to be balanced.
        Biometrika, 58(3), 403\u2013417. doi:10.1093/biomet/58.3.403"),

      uiOutput(ns("error_ui")),
      uiOutput(ns("results_ui"))
    )
  )
}


mod_ebc_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    seq_obj <- reactiveVal(NULL)
    errors  <- reactiveVal(NULL)

    observeEvent(input$generate, {
      # p was previously clamped silently: p = 0.4 produced a list for p = 0.5
      # and p = 1.5 a list for p = 1, without the user noticing.
      msgs <- c(
        rd_check_int(input$n, "Total sample size", min = 2, max = RD_MAX_N),
        rd_check_num(input$p, "Biasing probability (p)", min = 0.5, max = 1),
        rd_check_seed(input$seed)
      )
      if (length(msgs) > 0) {
        errors(msgs); seq_obj(NULL); return()
      }
      errors(NULL)

      n  <- as.integer(input$n)
      p  <- input$p
      g1 <- if (nzchar(trimws(input$name1))) trimws(input$name1) else "A"
      g2 <- if (nzchar(trimws(input$name2))) trimws(input$name2) else "B"

      res <- tryCatch({
        par <- ebcPar(N = n, p = p, groups = c(g1, g2))
        genSeq(par, seed = as.integer(input$seed))
      }, error = function(e) e)

      if (inherits(res, "error")) {
        errors(conditionMessage(res)); seq_obj(NULL); return()
      }
      seq_obj(res)
    })

    output$error_ui <- renderUI({ rd_error_ui(errors()) })

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
      filename = function() "ebc_randomization.html",
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
