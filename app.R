library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(expm)
library(ggrepel)
library(writexl)
library(openxlsx)

# ---- Helper Functions ----

NUM_AQ <- 3

sysmat <- function(kD, c) {
  n <- length(kD)
  a <- 1 / (kD * c)
  b <- 1 / (kD * c(c[2:n], Inf))
  diag(a + b) - rbind(0, cbind(diag(a[-1]), 0)) - cbind(0, rbind(diag(b[-n]), 0))
}

stijgh0 <- function(A, p, n) {
  sqA <- sqrtm(A)
  m <- max(dim(A))
  h_known <- rep(p, n)
  h_unknown <- -solve(sqA[(n+1):m, (n+1):m]) %*% (sqA[(n+1):m, 1:n] %*% h_known)
  c(h_known, h_unknown)
}

get_phi <- function(x, h0, sqrtmA) expm(-x * sqrtmA) %*% h0

err_sum <- function(obs, h0, sqrtmA) {
  if (is.null(obs) || nrow(obs) == 0) return(NA_real_)
  apply(obs[, c('aquifer', 'x', 'Head')], 1, function(row)
    (row[3] - get_phi(row[2], h0, sqrtmA)[as.integer(row[1])])^2) %>%
    sqrt() %>% sum()
}

seq_log <- function(minv, maxv, n) 10^(seq(log10(minv), log10(maxv), length.out=n))

to_wide <- function(df) {
  df %>%
    filter(!observation) %>%
    mutate(AqNum = as.integer(gsub("Aq", "", Aq))) %>%
    select(x, AqNum, Head) %>%
    tidyr::pivot_wider(names_from = AqNum, names_prefix = "Head_L", values_from = Head) %>%
    rename(`Distance (m)` = x) %>%
    arrange(`Distance (m)`)
}

# ---- UI ----

ui <- fluidPage(
  titlePanel("MultiLayer3"),
  tabsetPanel(
    tabPanel("Model",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("riverlevel", "Rivierlevel (m+ref).", -5, 5, -0.52, 0.01),
                 sliderInput("polderlevel", "Polderlevel (m+ref).", -5, 5, -2.2, 0.01),
                 radioButtons("n", "Nr of aquifers in direct contact with the river", c(1, 2, 3), 2),
                 lapply(c("kD", "c"), function(param)
                   lapply(1:3, function(i) {
                     sliderInput(paste0(param, i), paste0(param, i, if (param == "kD") " (m2/d):" else " (d):"),
                                 100, 1000, 250, if (param == "kD") 0.01 else 1)
                   })
                 ),
                 checkboxInput("logscale_xaxis", "Log scale", FALSE),
                 sliderInput("xmax", "x-axis (max value)", 10, 10000, 4000, 10),
                 numericInput("num_points_x", "Number of points in plot:", 100, 1),
                 conditionalPanel("output.err != ''",
                                  checkboxInput("Labels", "Labels", FALSE),
                                  actionButton("Optimize", "Optimize")
                 ),
                 downloadButton("downloadData", "Download input data"), br(), br(),
                 fileInput("uploadData", "Upload input data", accept = ".csv"),
                 tags$a(href = "https://github.com/KeesVanImmerzeel/MultiLayer3/tree/master", "Documentation")
               ),
               mainPanel(plotOutput("plot"), textOutput("err"))
             )
    ),
    tabPanel("Ranges",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("riverlevelrange", "Range riverlevel (m+ref)", -10, 10, c(-5, 1), 0.1),
                 sliderInput("polderlevelrange", "Range polderlevel (m+ref)", -10, 10, c(-5, 1), 0.1),
                 lapply(c("kD", "c"), function(param)
                   lapply(1:3, function(i) {
                     sliderInput(paste0(param, i, "range"), paste0("Range ", param, i, if (param == "kD") " (m2/d)" else " (d)"),
                                 0.01, 10000, c(100, 1000), if (param == "kD") 0.01 else 1)
                   })
                 ),
                 tags$hr(),
                 fileInput("file2", "Choose CSV File with ranges", accept = ".csv"),
                 radioButtons("sep2", "Separator", c(Comma = ",", Semicolon = ";", Tab = "\t"), ";"),
                 radioButtons("quote2", "Quote", c(None = "", "Double Quote" = '"', "Single Quote" = "'"), "")
               ),
               mainPanel(tableOutput("ranges"))
             )
    ),
    tabPanel("Results",
             sidebarLayout(
               sidebarPanel(downloadButton("downloadResultsXLSX", "Download as Excel")),
               mainPanel(h4("Output Results"), tableOutput("calculationResults"))
             )
    ),
    tabPanel("Measurements",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Choose CSV File with measurements", accept = ".csv"),
                 tags$hr(),
                 radioButtons("sep", "Separator", c(Comma = ",", Semicolon = ";", Tab = "\t"), ";"),
                 radioButtons("quote", "Quote", c(None = "", "Double Quote" = '"', "Single Quote" = "'"), ""),
                 tags$hr(),
                 radioButtons("disp", "Display", c(Head = "head", All = "all"), "head")
               ),
               mainPanel(tableOutput("contents"))
             )
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {
  rv <- reactiveValues(obs = NULL, ranges = NULL)
  param_vec <- function(p) sapply(1:3, function(i) input[[paste0(p,i)]])
  param_range_vec <- function(p) lapply(1:3, function(i) input[[paste0(p,i,"range")]])
  
  observeEvent(input$file1, {
    req(input$file1)
    df <- read.csv2(input$file1$datapath, header = TRUE, sep = input$sep, quote = input$quote)
    if (!"Aq" %in% names(df)) df$Aq <- paste0("Aq", df$aquifer)
    df$observation <- TRUE
    rv$obs <- df
  })
  
  observeEvent(input$file2, {
    req(input$file2)
    df <- read.csv2(input$file2$datapath, header = TRUE, sep = input$sep2, quote = input$quote2)
    rv$ranges <- df
    for (i in 1:3) {
      updateSliderInput(session, paste0("kD", i, "range"), value = c(df[i,2], df[i,4]))
      updateSliderInput(session, paste0("c", i, "range"), value = c(df[i+3,2], df[i+3,4]))
    }
  })
  
  observeEvent(input$riverlevelrange, { updateSliderInput(session, "riverlevel", min = input$riverlevelrange[1], max = input$riverlevelrange[2]) })
  observeEvent(input$polderlevelrange, { updateSliderInput(session, "polderlevel", min = input$polderlevelrange[1], max = input$polderlevelrange[2]) })
  for (param in c("kD", "c"))
    for (i in 1:3)
      observeEvent(input[[paste0(param, i, "range")]], {
        updateSliderInput(session, paste0(param, i),
                          min = input[[paste0(param, i, "range")]][1],
                          max = input[[paste0(param, i, "range")]][2]
        )
      })
  
  output$contents <- renderTable({ d <- rv$obs; if(is.null(d)) return(); if (input$disp == "head") head(d) else d })
  output$ranges <- renderTable({ rv$ranges })
  
  kD <- reactive(param_vec("kD"))
  c_ <- reactive(param_vec("c"))
  n_contact <- reactive(as.integer(input$n))
  A <- reactive(sysmat(kD(), c_()))
  sqrt_A <- reactive(sqrtm(A()))
  
  h0 <- reactive({
    if (n_contact() == NUM_AQ) rep(input$riverlevel - input$polderlevel, NUM_AQ)
    else stijgh0(A(), input$riverlevel - input$polderlevel, n_contact())
  })
  xvals <- reactive(seq(0, input$xmax, length.out = input$num_points_x))
  
  model_melted <- reactive({
    mmd <- sapply(xvals(), get_phi, h0(), sqrt_A()) %>% t() %>% as.data.frame()
    names(mmd) <- c("phi1","phi2","phi3")
    mmd$x <- xvals()
    mmd %>%
      melt(id.vars = "x") %>%
      mutate(
        Aq = paste0("Aq", as.integer(sub("phi", "", variable))),
        Head = input$polderlevel + value,
        observation = FALSE, location = "model"
      ) %>%
      select(x, Head, Aq, observation, location) 
  })
  
  melted_data <- reactive({
    md <- model_melted()
    if(!is.null(rv$obs)) {
      obs <- rv$obs %>% select(names(md))
      md <- bind_rows(md, obs)
    }
    md$Aq <- factor(md$Aq, levels = c("Aq1", "Aq2", "Aq3"))
    md$aquifer <- as.integer(gsub("Aq", "", md$Aq))
    md
  })
  
  av_err <- reactive({
    obs <- melted_data() %>% filter(observation)
    err_sum(obs, h0(), sqrt_A()) / max(1,nrow(obs))
  })
  output$err <- renderText({
    if(!is.null(rv$obs))
      paste("Average of absolute error:", round(av_err(),2), "(m)")
    else ""
  })
  
  output$plot <- renderPlot({
    pal <- setNames(brewer.pal(3, 'Set1'), paste0("Aq", 1:3))
    md <- melted_data()
    p <- ggplot(md, aes(x=x, y=Head, colour=Aq)) +
      geom_line(data=filter(md, !observation), linewidth=1, linetype="dashed") +
      scale_colour_manual(values = pal) +
      xlab("Distance (m)") + ylab("Head (m+ref)") + theme(legend.title=element_blank())
    if(input$logscale_xaxis) p <- p + scale_x_continuous(trans='log10')
    if(!is.null(rv$obs)) {
      p <- p + geom_point(data=filter(md, observation), size=3)
      if(input$Labels)
        p <- p + ggrepel::geom_label_repel(
          aes(label=ifelse(observation, location, "")),
          box.padding=0.35, point.padding=0.5, segment.color='grey50', show.legend=FALSE
        )
    }
    p
  })
  
  observeEvent(input$Optimize, {
    n_seq <- 5
    grid <- expand.grid(
      kD1=seq_log(input$kD1range[1], input$kD1range[2], n_seq),
      kD2=seq_log(input$kD2range[1], input$kD2range[2], n_seq),
      kD3=seq_log(input$kD3range[1], input$kD3range[2], n_seq),
      c1 =seq_log(input$c1range[1], input$c1range[2], n_seq),
      c2 =seq_log(input$c2range[1], input$c2range[2], n_seq),
      c3 =seq_log(input$c3range[1], input$c3range[2], n_seq)
    )
    obs <- melted_data() %>% filter(observation)
    withProgress(message = 'Optimizing', min=0, max = nrow(grid), value=0, {
      errors <- sapply(seq_len(nrow(grid)), function(i) {
        incProgress(1)
        par <- grid[i,]
        Am <- sysmat(as.numeric(par[1:3]), as.numeric(par[4:6]))
        h0m <- if (n_contact() == NUM_AQ) rep(h0()[1], NUM_AQ)
        else stijgh0(Am, h0()[1], n_contact())
        err_sum(obs, h0m, sqrtm(Am)) / max(1,nrow(obs))
      })
      best <- which.min(errors); par <- grid[best,]
      mapply(function(parname, val) updateSliderInput(session, parname, value=val),
             parname=c("kD1","kD2","kD3","c1","c2","c3"), val=as.numeric(par)
      )
    })
  })
  
  get_all_params <- reactive({
    as.data.frame(t(c(
      riverlevel  = input$riverlevel,
      polderlevel = input$polderlevel,
      n           = input$n,
      kD1         = input$kD1, kD2 = input$kD2, kD3 = input$kD3,
      c1          = input$c1, c2 = input$c2, c3 = input$c3,
      logscale_xaxis = input$logscale_xaxis,
      xmax        = input$xmax,
      num_points_x = input$num_points_x,
      riverlevel_range_min = input$riverlevelrange[1],
      riverlevel_range_max = input$riverlevelrange[2],
      polderlevel_range_min = input$polderlevelrange[1],
      polderlevel_range_max = input$polderlevelrange[2],
      kD1_range_min = input$kD1range[1], kD1_range_max = input$kD1range[2],
      kD2_range_min = input$kD2range[1], kD2_range_max = input$kD2range[2],
      kD3_range_min = input$kD3range[1], kD3_range_max = input$kD3range[2],
      c1_range_min = input$c1range[1], c1_range_max = input$c1range[2],
      c2_range_min = input$c2range[1], c2_range_max = input$c2range[2],
      c3_range_min = input$c3range[1], c3_range_max = input$c3range[2]
    )), stringsAsFactors = FALSE)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() paste0("MultiLayer3_input_data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(get_all_params(), file, row.names = FALSE)
  )
  
  observeEvent(input$uploadData, {
    req(input$uploadData)
    params <- read.csv(input$uploadData$datapath)
    # Just to match the right fields:
    paramNames <- c("kD1","kD2","kD3","c1","c2","c3","polderlevel")
    for (nm in paramNames) {
      rng <- c(params[[paste0(nm,"_range_min")]], params[[paste0(nm,"_range_max")]])
      updateSliderInput(session, paste0(nm, "range"), value = rng)
      updateSliderInput(session, nm, min = rng[1], max = rng[2], value = params[[nm]])
    }
    updateSliderInput(session, "riverlevel", value = params$riverlevel)
    updateRadioButtons(session, "n", selected = as.character(params$n))
    updateCheckboxInput(session, "logscale_xaxis", value = as.logical(params$logscale_xaxis))
    updateSliderInput(session, "xmax", value = params$xmax)
    updateNumericInput(session, "num_points_x", value = params$num_points_x)
  })
  
  output$calculationResults <- renderTable({ to_wide(model_melted()) })
  
  output$downloadResultsXLSX <- downloadHandler(
    filename = function() paste0("model_results_", Sys.Date(), ".xlsx"),
    content = function(file) {
      mmd <- model_melted() %>% filter(!observation) %>%
        mutate(Aquifer = gsub("^Aq", "", Aq)) %>%
        select(x, Head, Aquifer) %>%
        rename(`Distance (m)` = x, `Head (m+ref)` = Head)
      img_file <- tempfile(fileext = ".png")
      plot_obj <- ggplot(mmd, aes(x = `Distance (m)`, y = `Head (m+ref)`, color = factor(Aquifer))) +
        geom_line(linewidth = 1) +
        labs(color = "Aquifer") +
        theme_minimal()
      ggsave(img_file, plot = plot_obj, width = 7, height = 5, dpi = 150, bg = "white")
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Results Table")
      openxlsx::writeData(wb, "Results Table", to_wide(model_melted()))
      openxlsx::addWorksheet(wb, "Graph")
      openxlsx::insertImage(wb, "Graph", img_file, startRow=1, startCol=1, width=7, height=5, units="in", dpi=150)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)