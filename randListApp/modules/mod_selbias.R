# =============================================================================
# Module: Selection Bias Assessment
#
# Workflow:
#   exact: getAllSeq(par) -> assess(seqs, selBias(...), endp=normEndp(...))
#   sim:   genSeq(par, r, seed) -> assess(seqs, selBias(...), endp=normEndp(...))
# =============================================================================

mod_selbias_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,

      h5("Randomization Procedure"),
      selectInput(ns("procedure"), NULL,
                  choices = c(
                    "Big Stick Design"          = "bsd",
                    "Complete Randomization"    = "cr",
                    "Efron's Biased Coin"       = "ebc",
                    "Permuted Block"            = "pbr",
                    "Random Permuted Block"     = "rpbr",
                    "Random Allocation Rule"    = "rar",
                    "Chen's Design"             = "chen"
                  ), selectize = FALSE),

      uiOutput(ns("proc_params")),

      hr(),
      h5("Selection Bias Model"),

      radioButtons(ns("type"), "Strategy:",
                   choices  = c("Convergent (CS)" = "CS",
                                "Directional (DS)" = "DS"),
                   selected = "CS"),

      numericInput(ns("eta"), "Selection effect (\u03b7):",
                   value = 1, min = 0, step = 0.1),

      numericInput(ns("alpha"), "Significance level (\u03b1):",
                   value = 0.05, min = 0.001, max = 0.5, step = 0.01),

      hr(),
      h5("Endpoint"),

      numericInput(ns("mu1"), "Expected response \u2014 treatment 1 (H\u2080):",
                   value = 0, step = 0.1),
      numericInput(ns("mu2"), "Expected response \u2014 treatment 2 (H\u2080):",
                   value = 0, step = 0.1),
      numericInput(ns("sigma"), "Standard deviation:",
                   value = 1, min = 0.01, step = 0.1),

      hr(),
      h5("Computation"),

      uiOutput(ns("method_ui")),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'sim' || input['%s'] == 'rpbr'",
                            ns("method"), ns("procedure")),
        numericInput(ns("r"), "Number of simulated sequences:",
                     value = 1000, min = 100, max = 10000, step = 100)
      ),

      numericInput(ns("seed"), "Seed:", value = sample.int(2^31 - 1, 1)),

      actionButton(ns("run"), "\u25b6  Assess", class = "btn-primary w-100 mt-2")
    ),

    div(
      class = "rd-main-inner",

      div(class = "rd-pattern-strip"),

      h2(class = "rd-method-title", "Selection Bias Assessment"),

      p(class = "rd-description",
        "Assesses the type I error inflation from selection bias (Convergent or
        Directional Strategy) under the chosen randomization procedure. See references
        for the underlying model."),

      div(class = "rd-reference-block",
        p(class = "rd-reference",
          "Blackwell D, Hodges JL (1957). Design for the control of selection bias.",
          tags$em("Annals of Mathematical Statistics,"), " 28(2), 449\u2013460."),
        p(class = "rd-reference",
          "Proschan M (1994). Influence of selection bias on type I error rate under
          random permuted block designs.", tags$em("Statistica Sinica,"), " 4, 219\u2013231."),
        p(class = "rd-reference",
          "Tamm M, Hilgers RD (2014). Chronological bias in randomized clinical trials
          arising from different types of unobserved time trends.",
          tags$em("Methods of Information in Medicine,"), " 53(6), 501\u2013510."),
        p(class = "rd-reference",
          HTML("Uschner D, Schindler D, Hilgers RD, Heussen N (2018). randomizeR: An R
          package for the assessment and implementation of randomization in clinical trials.
          <em>Journal of Statistical Software,</em> 85(8), 1\u201333.
          doi:10.18637/jss.v085.i08"))
      ),

      uiOutput(ns("results_ui"))
    )
  )
}


mod_selbias_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    result_obj <- reactiveVal(NULL)
    r_code_str <- reactiveVal(NULL)

    # ── Method selector (hide exact for RPBR) ─────────────────────────────────
    output$method_ui <- renderUI({
      if (input$procedure == "rpbr") {
        div(
          tags$small(class = "text-muted",
                     "Random Permuted Block always uses simulation."),
          tags$input(type = "hidden", id = session$ns("method"), value = "sim")
        )
      } else {
        selectInput(session$ns("method"), "Method:",
                    choices  = c("Simulation" = "sim", "Exact" = "exact"),
                    selected = "sim", selectize = FALSE)
      }
    })

    # ── Dynamic procedure parameters ──────────────────────────────────────────
    output$proc_params <- renderUI({
      switch(input$procedure,
        "bsd" = tagList(
          numericInput(session$ns("n"),   "Sample size:", value = 10, min = 2, max = 1000),
          numericInput(session$ns("mti"), "MTI:",         value = 3,  min = 1)
        ),
        "cr" = tagList(
          numericInput(session$ns("n"), "Sample size:", value = 10, min = 2, max = 1000)
        ),
        "ebc" = tagList(
          numericInput(session$ns("n"), "Sample size:", value = 10, min = 2, max = 1000),
          numericInput(session$ns("p"), "Biasing probability (p):",
                       value = 0.75, min = 0.5, max = 1, step = 0.05)
        ),
        "pbr" = tagList(
          numericInput(session$ns("n"),          "Target sample size:", value = 12, min = 2, max = 1000),
          numericInput(session$ns("block_size"), "Block size:",         value = 4,  min = 2, step = 2)
        ),
        "rpbr" = tagList(
          numericInput(session$ns("n"),  "Sample size:", value = 20, min = 2, max = 1000),
          textInput(session$ns("rb"), "Block sizes (comma-separated):", value = "2, 4, 6")
        ),
        "rar" = tagList(
          numericInput(session$ns("n"), "Sample size:", value = 10, min = 2, max = 1000)
        ),
        "chen" = tagList(
          numericInput(session$ns("n"),   "Sample size:", value = 10, min = 2, max = 1000),
          numericInput(session$ns("mti"), "MTI:",         value = 3,  min = 1),
          numericInput(session$ns("p"),   "Biasing probability (p):",
                       value = 0.75, min = 0.5, max = 1, step = 0.05)
        )
      )
    })

    # ── Build randomization parameter object ──────────────────────────────────
    make_par <- function() {
      n    <- max(2, min(input$n, 1000))
      proc <- input$procedure
      switch(proc,
        "bsd"  = bsdPar(N = n, mti = max(1, input$mti)),
        "cr"   = crPar(N = n),
        "ebc"  = ebcPar(N = n, p = input$p),
        "pbr"  = {
          bs <- max(2, input$block_size)
          if (bs %% 2 != 0) bs <- bs + 1
          pbrPar(bc = rep(bs, ceiling(n / bs)))
        },
        "rpbr" = {
          rb <- suppressWarnings(
            as.integer(strsplit(trimws(input$rb), "[, ]+")[[1]])
          )
          rb <- rb[!is.na(rb) & rb >= 2 & rb %% 2 == 0]
          if (length(rb) == 0) rb <- c(2L, 4L)
          rpbrPar(N = n, rb = rb)
        },
        "rar"  = rarPar(N = n),
        "chen" = chenPar(N = n, mti = max(1, input$mti), p = input$p)
      )
    }

    # ── Run assessment ─────────────────────────────────────────────────────────
    observeEvent(input$run, {
      result_obj(NULL)
      r_code_str(NULL)
      req(input$n)

      par    <- tryCatch(make_par(), error = function(e) NULL)
      req(par)

      method <- if (input$procedure == "rpbr") "sim" else isolate(input$method)
      endp   <- normEndp(
        mu    = c(input$mu1, input$mu2),
        sigma = c(input$sigma, input$sigma)
      )
      bias <- selBias(
        type   = input$type,
        eta    = input$eta,
        method = method,
        alpha  = input$alpha
      )

      seq_obj <- tryCatch({
        if (method == "exact") {
          getAllSeq(par)
        } else {
          genSeq(par, r = max(100, input$r), seed = input$seed)
        }
      }, error = function(e) e)

      if (inherits(seq_obj, "error")) {
        result_obj(seq_obj)
        return()
      }

      res <- tryCatch(
        assess(seq_obj, bias, endp = endp),
        error = function(e) e
      )
      result_obj(res)

      # ── Build R code string ──────────────────────────────────────────────────
      n    <- max(2, min(isolate(input$n), 1000))
      proc <- isolate(input$procedure)
      par_code <- switch(proc,
        "bsd"  = sprintf("par <- bsdPar(N = %d, mti = %d)", n, max(1, isolate(input$mti))),
        "cr"   = sprintf("par <- crPar(N = %d)", n),
        "ebc"  = sprintf("par <- ebcPar(N = %d, p = %.2f)", n, isolate(input$p)),
        "pbr"  = {
          bs <- max(2, isolate(input$block_size)); if (bs %% 2 != 0) bs <- bs + 1
          sprintf("par <- pbrPar(bc = rep(%d, %d))", bs, ceiling(n / bs))
        },
        "rpbr" = {
          rb <- suppressWarnings(as.integer(strsplit(trimws(isolate(input$rb)), "[, ]+")[[1]]))
          rb <- rb[!is.na(rb) & rb >= 2 & rb %% 2 == 0]
          if (length(rb) == 0) rb <- c(2L, 4L)
          sprintf("par <- rpbrPar(N = %d, rb = c(%s))", n, paste(rb, collapse = ", "))
        },
        "rar"  = sprintf("par <- rarPar(N = %d)", n),
        "chen" = sprintf("par <- chenPar(N = %d, mti = %d, p = %.2f)",
                         n, max(1, isolate(input$mti)), isolate(input$p))
      )
      seq_code <- if (method == "exact") {
        "seqs <- getAllSeq(par)"
      } else {
        sprintf("seqs <- genSeq(par, r = %d, seed = %d)",
                max(100, isolate(input$r)), isolate(input$seed))
      }
      type_str <- isolate(input$type)
      eta_str  <- isolate(input$eta)
      alpha_str <- isolate(input$alpha)
      mu1_str  <- isolate(input$mu1); mu2_str <- isolate(input$mu2)
      sig_str  <- isolate(input$sigma)
      bias_code <- sprintf(
        'bias <- selBias(type = "%s", eta = %s, method = "%s", alpha = %s)',
        type_str, eta_str, method, alpha_str
      )
      endp_code <- sprintf(
        "endp <- normEndp(mu = c(%s, %s), sigma = c(%s, %s))",
        mu1_str, mu2_str, sig_str, sig_str
      )
      r_code_str(paste(
        "library(randomizeR)",
        "",
        par_code,
        seq_code,
        bias_code,
        endp_code,
        "res  <- assess(seqs, bias, endp = endp)",
        "summary(res)",
        sep = "\n"
      ))
    })

    # ── UI ────────────────────────────────────────────────────────────────────
    output$results_ui <- renderUI({
      req(result_obj())
      tagList(
        div(class = "rd-section-label", tags$span("Rejection Probability Distribution")),
        div(class = "rd-plot-wrap",
            plotOutput(session$ns("bias_plot"), height = "300px")),
        div(class = "rd-section-label", tags$span("Summary Statistics")),
        uiOutput(session$ns("bias_summary")),
        div(class = "rd-section-label", tags$span("R Code")),
        div(class = "rd-code-wrap",
            verbatimTextOutput(session$ns("r_code")))
      )
    })

    output$bias_plot <- renderPlot({
      req(result_obj())
      if (inherits(result_obj(), "error")) return(NULL)
      print(bias_assessment_plot(result_obj(), alpha = input$alpha))
    })

    output$bias_summary <- renderUI({
      req(result_obj())
      bias_summary_ui(result_obj(), alpha = input$alpha)
    })

    output$r_code <- renderText({
      req(r_code_str())
      r_code_str()
    })
  })
}
