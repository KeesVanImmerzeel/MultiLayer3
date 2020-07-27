library(shiny)
library(dplyr)
library(ggplot2)
theme_update(text = element_text(size=15))
library(reshape2)
library(RColorBrewer)
library(expm)
library(magrittr)
library(ggrepel)

ui <- fluidPage(# Application title
    #theme = "sweco-bootstrap.scss",
    titlePanel("MultiLayer3"),
    tabsetPanel(
        tabPanel(title = "Model",
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput(
                             inputId = "h0",
                             label = "Rivierlevel relative to polderlevel (m).",
                             min = -5,
                             max = 5,
                             value = 2.43,
                             step = 0.05,
                             width = '100%'
                         ),
                         radioButtons(
                             inputId = "n",
                             label = "Nr of aquifers in direct contact with the river",
                             choices = c(1, 2, 3),
                             selected = 2
                         ),
                         sliderInput(
                             inputId = "kD1",
                             label = "kD1 (m2/d):",
                             min = 100,
                             max = 1000,
                             value = 250,
                             step = 1,
                             width = '100%'
                         ),
                         sliderInput(
                             inputId = "kD2",
                             label = "kD2 (m2/d):",
                             min = 100,
                             max = 1000,
                             value = 250,
                             step = 1,
                             width = '100%'
                         ),
                         sliderInput(
                             inputId = "kD3",
                             label = "kD3 (m2/d):",
                             min = 100,
                             max = 1000,
                             value = 250,
                             step = 1,
                             width = '100%'
                         ),
                         sliderInput(
                             inputId = "c1",
                             label = "c1 (d):",
                             min = 100,
                             max = 1000,
                             value = 250,
                             step = 1,
                             width = '100%'
                         ),
                         sliderInput(
                             inputId = "c2",
                             label = "c2 (d):",
                             min = 100,
                             max = 1000,
                             value = 250,
                             step = 1,
                             width = '100%'
                         ),
                         sliderInput(
                             inputId = "c3",
                             label = "c3 (d):",
                             min = 100,
                             max = 1000,
                             value = 250,
                             step = 1,
                             width = '100%'
                         ),
                         checkboxInput("logscale_xaxis", "Log scale", value = FALSE),
                         sliderInput(
                             inputId = "xmax",
                             label = "x-axis (max value)",
                             min = 100,
                             max = 10000,
                             value = 4000,
                             step = 10,
                             width = '100%'
                         ),                         
                         conditionalPanel("output.err != ''",
                                          checkboxInput("Labels", "Labels", value = FALSE),
                                          actionButton("Optimize", "Optimize"))
                     ),
                     mainPanel(
                         plotOutput("plot"),
                         textOutput("err")
                         #conditionalPanel(
                         #    condition = "input.plotMeasurements == true",
                         #    uiOutput("SelectGroup")
                         #   checkboxGroupInput(
                         #       "improve",
                         #       "Adjust these parameters to improve fit:",
                         #       c("kD1", "kD2", "KD3", "c1", "c2", "c3")
                         #   ),
                         #   actionButton("improve", "Improve")
                         # )
                     )
                 )),
        tabPanel(title = "Ranges",
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput(
                             "kD1range",
                             label = "Range kD1 (m2/d)",
                             min = 0.1,
                             max = 10000,
                             value = c(100, 1000),
                             step = 1,
                             width = '100%'
                         ),
                         sliderInput(
                             "kD2range",
                             label = "Range kD2 (m2/d)",
                             min = 0.1,
                             max = 10000,
                             value = c(100, 1000),
                             step = 1,
                             width = '100%'
                         ),
                         sliderInput(
                             "kD3range",
                             label = "Range kD3 (m2/d)",
                             min = 0.1,
                             max = 10000,
                             value = c(100, 1000),
                             step = 1,
                             width = '100%'
                         ),
                         sliderInput(
                             "c1range",
                             label = "Range c1 (d)",
                             min = 0.1,
                             max = 10000,
                             value = c(100, 1000),
                             step = 1,
                             width = '100%'
                         ),
                         sliderInput(
                             "c2range",
                             label = "Range c2 (d)",
                             min = 0.1,
                             max = 10000,
                             value = c(100, 1000),
                             step = 1,
                             width = '100%'
                         ),
                         sliderInput(
                             "c3range",
                             label = "Range c3 (d)",
                             min = 0.1,
                             max = 10000,
                             value = c(100, 1000),
                             step = 1,
                             width = '100%'
                         ),
                         
                         # Horizontal line ----
                         tags$hr(),
                         fileInput(
                             "file2",
                             "Choose CSV File with ranges",
                             multiple = TRUE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                         ),
                         
                         # Input: Select separator ----
                         radioButtons(
                             "sep2",
                             "Separator",
                             choices = c(
                                 Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"
                             ),
                             selected = ";"
                         ),
                         
                         # Input: Select quotes ----
                         radioButtons(
                             "quote2",
                             "Quote",
                             choices = c(
                                 None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"
                             ),
                             selected = ''
                         )
                     ),
                     mainPanel(tableOutput("ranges"))
                 )),
        tabPanel(title = "Measurements",
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                     # Sidebar panel for inputs ----
                     sidebarPanel(
                         # Input: Select a file ----
                         fileInput(
                             "file1",
                             "Choose CSV File with measurements",
                             multiple = TRUE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                         ),
                         # Horizontal line ----
                         tags$hr(),
                         # Input: Select separator ----
                         radioButtons(
                             "sep",
                             "Separator",
                             choices = c(
                                 Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"
                             ),
                             selected = ";"
                         ),
                         
                         # Input: Select quotes ----
                         radioButtons(
                             "quote",
                             "Quote",
                             choices = c(
                                 None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"
                             ),
                             selected = ''
                         ),
                         
                         # Horizontal line ----
                         tags$hr(),
                         
                         # Input: Select number of rows to display ----
                         radioButtons(
                             "disp",
                             "Display",
                             choices = c(Head = "head",
                                         All = "all"),
                             selected = "head"
                         )
                         
                     ),
                     # Main panel for displaying outputs ----
                     mainPanel(# Output: Data file ----
                               tableOutput("contents"))
                 )),
        tabPanel(title = "Documentation", 
                 includeHTML("www/MultiLayer3.html"))
    ))

server <- function(input, output, session) {
    m <- 3         # Number of aquifers
    
    # Create system matrix from kD and c values
    sysmat <- function(kD, c) {
        n <- length(kD)
        a <- 1 / (kD * c)
        b <- 1 / (kD * c(c[2:n], Inf))
        di_a <- rbind(0, cbind(diag(a[2:n]), 0))
        di_b <- cbind(0, rbind(diag(b[1:(n - 1)]), 0))
        A <- diag(a + b) - di_a - di_b
        return(A)
    }
    
    # The function 'stijgh0' calculates the heads at the river (x=0) in aquifers that are not in direct
    # contact with the sandy river bed.
    # A = Systeemmatrix (ref. function 'sysmat')
    # p = Riverlevel relative to polderlevel (m)
    # n = The number of aquifers that are in direct contact with the river
    stijgh0 <- function(A, p, n) {
        A <- sqrtm(A)    # Systeemmatrix
        m <- max(dim(A)) # Aantal aquifers
        B3 <- A[(n + 1):m, (1:n)] # submatrix van A
        B4 <- A[(n + 1):m, (n + 1):m]  # submatrix van A
        h_bekend <-
            matrix(data = p, nrow = n) #   stijghoogten in de aangesneden aquifers
        h_onbekend <- -solve(B4) %*% (B3 %*% h_bekend)
        h <- c(h_bekend, h_onbekend)
        return(h)
    }
    
    get_phi <- function(x, h0, sqrtmA) {
        if ((length(x) == 1) & (length(h0) == 3)) {
            return(expm::expm(-x * sqrtmA) %*% h0)
        } else {
            return(NA)
        }
    }
    
    # Calculate error = difference between observation (Aq, x, Head) and model (m)
    err <- function(aquifer, x, Head, h0, sqrtmA) {
        return(Head - get_phi(x, h0, sqrtmA)[aquifer])
    }
    
    # Calculate sum of absolute errors
    err_sum <- function(observations, h0, sqrtmA) {
        apply(observations[, c('aquifer', 'x', 'Head')], 1, function(x)
            {err(x[1], x[2], x[3], h0, sqrtmA) ^ 2}) %>% sqrt(.) %>% sum(.)
    }
    
    # Define parameter values to be used in optimization 
    sample_parameter_values <- function( minvalue, maxvalue, nr_of_values ) {
        x <- seq( log10(minvalue), log10(maxvalue), length.out=nr_of_values )
        return(10^x)
    }

    rv <- reactiveValues(observations = NULL, ranges = NULL )
    
    # Upload measurements
    observeEvent(input$file1, {
        req(input$file1)
        df <- read.csv2(
            input$file1$datapath,
            header = TRUE,
            sep = input$sep,
            quote = input$quote
        )
        df$Aq <-  #for plotting
            df$aquifer %>% as.character() %>% paste0("Aq", .) #for plotting
        df$aquifer <- NULL
        df$observation <- TRUE #for plotting
        rv$observations <- df
    })    
    
    # Upload ranges
    observeEvent(input$file2, {
        req(input$file2)
        df <- read.csv2(
            input$file2$datapath,
            header = TRUE,
            sep = input$sep2,
            quote = input$quote2
        )
        rv$ranges <- df
        updateSliderInput(session,
                          "kD1range",
                          value = c(df[1, 1], df[1, 3]))
        updateSliderInput(session,
                          "kD2range",
                          value = c(df[2, 1], df[2, 3]))
        updateSliderInput(session,
                          "kD3range",
                          value = c(df[3, 1], df[3, 3]))
        updateSliderInput(session,
                          "c1range",
                          value = c(df[4, 1], df[4, 3]))
        updateSliderInput(session,
                          "c2range",
                          value = c(df[5, 1], df[5, 3]))
        updateSliderInput(session,
                          "c3range",
                          value = c(df[6, 1], df[6, 3]))
    })

    observeEvent(input$kD1range, {
        updateSliderInput(session,
                          "kD1",
                          min = input$kD1range[1],
                          max = input$kD1range[2],
                          value= rv$ranges[1,2])
        
    })
    observeEvent(input$kD2range, {
        updateSliderInput(session,
                          "kD2",
                          min = input$kD2range[1],
                          max = input$kD2range[2],
                          value= rv$ranges[2,2])
    })
    observeEvent(input$kD3range, {
        updateSliderInput(session,
                          "kD3",
                          min = input$kD3range[1],
                          max = input$kD3range[2],
                          value= rv$ranges[3,2])
    })

    observeEvent(input$c1range, {
        updateSliderInput(session,
                          "c1",
                          min = input$c1range[1],
                          max = input$c1range[2],
                          value= rv$ranges[4,2])
    })
    observeEvent(input$c2range, {
        updateSliderInput(session,
                          "c2",
                          min = input$c2range[1],
                          max = input$c2range[2],
                          value= rv$ranges[5,2])
    })
    observeEvent(input$c3range, {
        updateSliderInput(session,
                          "c3",
                          min = input$c3range[1],
                          max = input$c3range[2],
                          value= rv$ranges[6,2])
    })
    
        
    output$contents <- renderTable({
        if (input$disp == "head") {
            return(head(rv$observations))
        }
        else {
            return(rv$observations)
        }
    })
    
    output$ranges <- renderTable({
        rv$ranges
    })
    
    kD <- eventReactive(c(input$kD1,
                          input$kD2,
                          input$kD3), {
                              c(input$kD1, input$kD2, input$kD3) # (m2/d)
                          })
    
    c_ <- eventReactive(c(input$c1,
                          input$c2,
                          input$c3), {
                              c(input$c1, input$c2, input$c3) # (d)
                          })
    
    A <- reactive({
        sysmat(kD(), c_())
    })
    
    sqrtmA <- reactive({
        expm::sqrtm(A())
    })
    
    n_ <- eventReactive(input$n, {
        input$n %>% as.integer()
    })
    
    h0 <- reactive({
        if (n_() == m) {
            rep(input$h0, m) # River is in contact with all aquifers
        } else {
            stijgh0(A(), p = input$h0, n = n_()) # River is in direct contact with less then rv$m aquifers
        }
    })
    
    xmin <- reactive({
        10
    })
    xmax <- reactive({
        input$xmax
    })
    xstep <- reactive({
        25
    })
    x <- reactive({
        seq(xmin(), xmax(), xstep())
    })
    
    melted_model_data <- reactive({
        mmd <-
            sapply(x(), get_phi, h0(), sqrtmA())  %>% t() %>% as.data.frame()
        names(mmd) <- c("phi1", "phi2", "phi3")
        mmd$x <- x() %>% as.character()
        mmd %<>%
            reshape2::melt(id.vars = c("x"))
        mmd$x %<>%  as.numeric(.)
        mmd$Aq <- mmd$variable
        mmd$variable <- NULL
        levels(mmd$Aq) <- c("1", "2", "3")
        mmd$Aq %<>% as.character() %>% paste0("Aq", .)
        mmd$observation <- FALSE
        mmd$location <- "model"
        names(mmd)[2] <- c("Head")
        return(mmd)
    })
    
    observations_are_available <- reactive({
        !is.null(rv$observations)
    })
    
    ranges_are_read <- reactive({
        !is.null(rv$ranges)
    })
    
    nr_of_observations <- reactive({
        if (observations_are_available()) {
            nrow(rv$observations)
        } else {
            0
        }
    })
    
    observation_filename <- reactive({
        if (observations_are_available()) {
            input$file1[[1]]
        } else {
            ""
        }
    })
    
    graph_title <- reactive({
        observation_filename() %>% sub(pattern = "(.*?)\\..*$",
                                       replacement = "\\1",
                                       basename(.))
    })
    
    melted_data <- reactive({
        md <- melted_model_data()
        if (observations_are_available()) {
            rv$observations <-
                select(rv$observations, names(md))
            md <-
                rbind(md, rv$observations) #Combine observations with model output for plot
        }
        md$Aq %<>% as.factor()
        md$aquifer <- md$Aq %>% as.numeric()
        return(md)
    })
    
    av_err <- reactive({
        if (observations_are_available()) {
            .data <- melted_data() %>% filter(observation == TRUE)
            err_sum(.data, h0(), sqrtmA()) / nr_of_observations()
        } else {
            NULL
        }
    })
    
    output$err <- renderText({
        if (observations_are_available()) {
            paste("Average of absolute error:",
                  round(av_err(), 2) %>% as.character(),
                  "(m)")
        } else {
            ""
        }
    })
    
    output$plot <- renderPlot({
        cbr <- brewer.pal(n = 3, name = 'Set1')
        MyPalette <-
            c("Aq1" = cbr[1],
              "Aq2" = cbr[2],
              "Aq3" = cbr[3])
        plt <- ggplot(melted_data(),
                      aes(
                          x = x,
                          y = Head,
                          colour = Aq
                      )) +
            geom_line(
                data = filter(.data = melted_data(), observation == FALSE),
                size = 1,
                linetype = "dashed"
            ) +
            scale_colour_manual(values = MyPalette) +
            xlab("Distance (m)") +
            ylab("Head (m+ref)") + theme(legend.title = element_blank())
        
        if (input$logscale_xaxis) {
            plt <- plt + scale_x_continuous(trans = 'log10')
        }
        
        if (observations_are_available()) {
            .data <- melted_data() %>% filter(observation == TRUE)
            plt <- plt + geom_point(data = .data, size = 3) +
                ggtitle(graph_title())
            if (input$Labels) {
                plt <-
                    plt + ggrepel::geom_label_repel(
                        aes(label = ifelse(observation == TRUE, location, "")),
                        box.padding   = 0.35,
                        point.padding = 0.5,
                        segment.color = 'grey50',
                        show.legend = FALSE
                    )
            }
        }
        return(plt)
    })
    
    observeEvent(input$Optimize,
                 {
                     n <-
                         5 # Number of sample points per parameter to be used in optimization
                     xkD1 <-
                         sample_parameter_values(input$kD1range[1], input$kD1range[2], n)
                     xkD2 <-
                         sample_parameter_values(input$kD2range[1], input$kD2range[2], n)
                     xkD3 <-
                         sample_parameter_values(input$kD3range[1], input$kD3range[2], n)
                     xc1 <-
                         sample_parameter_values(input$c1range[1], input$c1range[2], n)
                     xc2 <-
                         sample_parameter_values(input$c2range[1], input$c2range[2], n)
                     xc3 <-
                         sample_parameter_values(input$c3range[1], input$c3range[2], n)
                     
                     .data <-
                         melted_data() %>% filter(observation == TRUE)
                     i <- 0
                     
                     j <- 0
                     
                     
                     withProgress(
                         message = 'Optimizing',
                         min = 0,
                         max = n ^ 6,
                         value = 0,
                         {
                             for (ikD1 in (1:n)) {
                                 for (ikD2 in (1:n)) {
                                     for (ikD3 in (1:n)) {
                                         for (ic1 in (1:n)) {
                                             for (ic2 in (1:n)) {
                                                 for (ic3 in (1:n)) {
                                                     j <- j + 1
                                                     
                                                     incProgress(1)
                                                     A <-
                                                         sysmat(c(xkD1[ikD1], xkD2[ikD2], xkD3[ikD3]),
                                                                c(xc1[ic1], xc2[ic2], xc3[ic3]))
                                                     h0 <-
                                                         if (n_() == m) {
                                                             rep(input$h0, m) # River is in contact with all aquifers
                                                         } else {
                                                             stijgh0(A,
                                                                     p = input$h0,
                                                                     n = n_()) # River is in direct contact with less then rv$m aquifers
                                                         }
                                                     sqrtmA <- sqrtm(A)
                                                     e <-
                                                         err_sum(.data, h0, sqrtmA) / nr_of_observations()
                                                     if (i == 0) {
                                                         min_err <- e
                                                         opt_indx <-
                                                             c(1, 1, 1, 1, 1, 1)
                                                         i <- i + 1
                                                     } else {
                                                         if (e < min_err) {
                                                             min_err <- e
                                                             opt_indx <-
                                                                 c(ikD1,
                                                                   ikD2,
                                                                   ikD3,
                                                                   ic1,
                                                                   ic2,
                                                                   ic3)
                                                         }
                                                     }
                                                 }
                                             }
                                         }
                                     }
                                 }
                             }
                             
                         }
                     )
                     updateSliderInput(session,
                                       "kD1",
                                       value = xkD1[opt_indx[1]])
                     updateSliderInput(session,
                                       "kD2",
                                       value = xkD2[opt_indx[2]])
                     updateSliderInput(session,
                                       "kD3",
                                       value = xkD3[opt_indx[3]])
                     updateSliderInput(session,
                                       "c1",
                                       value = xc1[opt_indx[4]])
                     updateSliderInput(session,
                                       "c2",
                                       value = xc2[opt_indx[5]])
                     updateSliderInput(session,
                                       "c3",
                                       value = xc3[opt_indx[6]])
                 })
}

# Run the application
shinyApp(ui = ui, server = server)
