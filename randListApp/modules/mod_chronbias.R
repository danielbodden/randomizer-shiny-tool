# =============================================================================
# Module: Chronological Bias Assessment
#
# Workflow:
#   exact: getAllSeq(par) -> assess(seqs, chronBias(...), endp=normEndp(...))
#   sim:   genSeq(par, r, seed) -> assess(seqs, chronBias(...), endp=normEndp(...))
# =============================================================================

mod_chronbias_ui <- function(id) {
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
      h5("Chronological Bias Model"),

      selectInput(ns("type"), "Time trend type:",
                  choices  = c("Linear trend"      = "linT",
                               "Step trend"         = "stepT",
                               "Logarithmic trend"  = "logT"),
                  selected = "linT", selectize = FALSE),

      numericInput(ns("theta"), "Trend strength (\u03b8):",
                   value = 1, step = 0.1),

      numericInput(ns("saltus"), "Step size (saltus):",
                   value = 1, step = 0.1),

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

      h2(class = "rd-method-title", "Chronological Bias Assessment"),

      p(class = "rd-description",
        "Chronological bias arises when there is a time trend in patient outcomes
        or prognostic factors over the course of the trial. If the treatment
        allocation is correlated with time, this trend can distort the treatment
        comparison and inflate the type I error rate. The assessment computes the
        rejection probability P(reject H\u2080) across allocation sequences under a
        specified time trend model (linear, step, or logarithmic)."),

      div(class = "rd-model-box",
        h5(class = "rd-model-title", "Statistical Model"),
        p(class = "rd-model-text",
          "Each patient\u2019s response is modelled as:"),
        p(class = "rd-model-eq",
          HTML("y<sub>i</sub> = \u03bc<sub>E</sub> T<sub>i</sub> + \u03bc<sub>C</sub>(1 \u2212 T<sub>i</sub>) + \u03c4<sub>i</sub> + \u03b5<sub>i</sub>")),
        p(class = "rd-model-text",
          HTML("where T<sub>i</sub> \u2208 {0, 1} is the treatment indicator,
          \u03bc<sub>E</sub> and \u03bc<sub>C</sub> are the expected responses under H\u2080,
          and \u03b5<sub>i</sub> \u223c N(0, \u03c3\u00b2) is the random error.
          The chronological bias effect \u03c4<sub>i</sub> depends on the chosen trend type:")),
        tags$ul(class = "rd-model-list",
          tags$li(HTML("<strong>Linear:</strong> \u03c4<sub>i</sub> = \u03b8 \u00b7 i/N")),
          tags$li(HTML("<strong>Step:</strong> \u03c4<sub>i</sub> = \u03b8 \u00b7 \u230ai/k\u230b
                       &nbsp;(saltus k = block size)")),
          tags$li(HTML("<strong>Logarithmic:</strong> \u03c4<sub>i</sub> = \u03b8 \u00b7 log(i)"))
        ),
        p(class = "rd-model-text",
          HTML("Here \u03b8 is the trend strength parameter and N is the total sample size.
          The standardised trend effect is \u03b3 = \u03b8/\u03c3.")),
        h5(class = "rd-model-title", "Hypothesis Test"),
        p(class = "rd-model-text",
          HTML("A two-sided t-test is applied to compare the two groups. Under chronological
          bias, the test statistic follows a <strong>doubly non-central t-distribution</strong>
          with non-centrality parameters \u03b4 (shift) and \u03bb (spread), derived from
          the specific allocation sequence and trend model. The type I error for a given
          sequence is:")),
        p(class = "rd-model-eq",
          HTML("P(|S| > t<sub>crit</sub>) = F(\u2212t<sub>crit</sub>; \u03b4, \u03bb) + 1 \u2212 F(t<sub>crit</sub>; \u03b4, \u03bb)")),
        p(class = "rd-model-text",
          "The rejection probability is averaged (weighted by sequence probability for
          exact assessment, or by relative frequency for simulation) to obtain the
          overall type I error estimate.")
      ),

      div(class = "rd-reference-block",
        p(class = "rd-reference",
          "Tamm M, Hilgers RD (2014). Chronological bias in randomized clinical trials
          arising from different types of unobserved time trends.",
          tags$em("Methods of Information in Medicine,"), " 53(6), 501\u2013510."),
        p(class = "rd-reference",
          "Berger VW (2005).", tags$em("Selection Bias and Covariate Imbalances in
          Randomized Clinical Trials."), " John Wiley & Sons, Chichester."),
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


mod_chronbias_server <- function(id) {
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
      bias <- chronBias(
        type   = input$type,
        theta  = input$theta,
        method = method,
        saltus = input$saltus,
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
      type_str   <- isolate(input$type)
      theta_str  <- isolate(input$theta)
      saltus_str <- isolate(input$saltus)
      alpha_str  <- isolate(input$alpha)
      mu1_str    <- isolate(input$mu1); mu2_str <- isolate(input$mu2)
      sig_str    <- isolate(input$sigma)
      bias_code <- sprintf(
        'bias <- chronBias(type = "%s", theta = %s, method = "%s", saltus = %s, alpha = %s)',
        type_str, theta_str, method, saltus_str, alpha_str
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
