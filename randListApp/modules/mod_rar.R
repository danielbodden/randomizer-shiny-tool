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

      rd_n_input(ns("n"), "Total sample size:", 10),

      selectInput(ns("k"), "Number of treatment arms:",
                  choices = as.character(2:RD_MAX_ARMS), selected = "2",
                  selectize = FALSE),

      # Dynamic ratio and name inputs (rendered server-side)
      uiOutput(ns("ratio_inputs")),
      uiOutput(ns("name_inputs")),

      hr(),
      rd_seed_input(ns("seed")),

      actionButton(ns("generate"), "\u25b6  Generate", class = "btn-primary w-100 mt-2"),

      div(class = "rd-sidebar-note",
          tags$small("N must be divisible by the sum of allocation ratios to guarantee
                      exact balance. N will be rounded up if necessary."))
    ),

    # ── Main panel ────────────────────────────────────────────────────────
    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),
      rand_steps_ui(),

      h2(class = "rd-method-title", "Random Allocation Rule"),
      span(class = "rd-method-badge badge-green", "2 \u2013 6 arms"),

      p(class = "rd-description",
        "The Random Allocation Rule (Lachin, 1988) samples uniformly at random from
        all possible permutations of a sequence that assigns exactly N\u1d62 patients
        to each treatment group i, where the N\u1d62 are determined by the predefined
        allocation ratios. By construction, this ensures exact balance according to
        the allocation ratio at the end of the trial."),

      p(class = "rd-reference",
        "Lachin JM (1988). Statistical properties of randomization in clinical trials.
        Controlled Clinical Trials, 9(4), 289\u2013311.
        doi:10.1016/0197-2456(88)90045-5"),

      uiOutput(ns("error_ui")),
      uiOutput(ns("results_ui"))
    )
  )
}


mod_rar_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    seq_obj <- reactiveVal(NULL)
    errors  <- reactiveVal(NULL)
    notes   <- reactiveVal(NULL)

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
      # the arm selector is always present, but the dependent ratio and
      # name inputs are rendered server-side
      req(input$k)
      k <- as.integer(input$k)

      msgs <- c(
        rd_check_int(input$n, "Total sample size", min = 2, max = RD_MAX_N),
        rd_check_ratio(input, "ratio", k),
        rd_check_seed(input$seed)
      )
      if (length(msgs) > 0) {
        errors(msgs); seq_obj(NULL); notes(NULL); return()
      }
      errors(NULL)

      n      <- as.integer(input$n)
      ratio  <- get_ratio(input, "ratio", k)
      groups <- get_groups(input, "name", k)

      # rarPar() requires N to be a multiple of sum(ratio) and aborts otherwise,
      # so N is rounded up and the user is told about it.
      info      <- character(0)
      ratio_sum <- sum(ratio)
      if (n %% ratio_sum != 0) {
        n_new <- ceiling(n / ratio_sum) * ratio_sum
        info  <- c(info, sprintf("N must be a multiple of the sum of the
                                  allocation ratios (%d) for exact terminal
                                  balance. The sample size was rounded up from
                                  %d to %d.", ratio_sum, n, n_new))
        n <- n_new
      }

      res <- tryCatch({
        par <- rarPar(N = n, K = k, ratio = ratio, groups = groups)
        genSeq(par, seed = as.integer(input$seed))
      }, error = function(e) e)

      if (inherits(res, "error")) {
        errors(conditionMessage(res)); seq_obj(NULL); notes(NULL); return()
      }
      notes(gsub("\\s+", " ", info))
      seq_obj(res)
    })

    output$error_ui <- renderUI({ rd_error_ui(errors()) })

    # ── Outputs ────────────────────────────────────────────────────────────
    output$results_ui <- renderUI({
      req(seq_obj())
      show_plot <- as.integer(input$k) == 2
      tagList(
        div(class = "rd-section-label", tags$span("Randomization Sequence")),
        if (length(notes()) > 0)
          rd_note_ui(tags$strong("Note"), tags$br(),
                     tags$ul(style = "margin:4px 0 0 0; padding-left:18px;",
                             lapply(notes(), function(m) tags$li(m)))),
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
