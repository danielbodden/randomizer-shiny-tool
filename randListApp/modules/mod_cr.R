# =============================================================================
# Module: Complete Randomization (CR)
#
# crPar(N, K, ratio, groups) — supports 2–6 arms
# genSeq(par, seed = seed)
# Walk plot is only shown for 2-arm designs.
# =============================================================================

mod_cr_ui <- function(id) {
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

      actionButton(ns("generate"), "\u25b6  Generate", class = "btn-primary w-100 mt-2")
    ),

    # ── Main panel ────────────────────────────────────────────────────────
    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),

      h2(class = "rd-method-title", "Complete Randomization"),
      span(class = "rd-method-badge badge-green", "2 – 6 arms"),

      p(class = "rd-description",
        "Complete Randomization (2 arms, ratio 1:1) is equivalent to tossing a fair
        coin for every patient. With multiple arms or unequal ratios, the allocation
        probability is proportional to each treatment's factor — a ratio of 1:2 means
        the second treatment is allocated twice as often."),

      p(class = "rd-reference",
        "Rosenberger WF, Lachin JM (2016). Randomization in Clinical Trials: Theory
        and Practice. 2nd ed. Hoboken, NJ: John Wiley & Sons."),

      uiOutput(ns("results_ui"))
    )
  )
}


mod_cr_server <- function(id) {
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

      par <- crPar(N = n, K = k, ratio = ratio, groups = groups)
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
      filename = function() "cr_randomization.html",
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
