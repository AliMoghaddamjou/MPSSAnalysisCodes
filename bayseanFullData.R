#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rstanarm)
library(bayestestR)
library(insight)
library(ggplot2)
library(logspline)
library(BayesFactor)
library(dplyr)
library(see)
library(modelbased)
library(performance)
library(parameters)

## Defining Choices
dataChoices <- c("Raw Data", "Exclusions (GCS<12,Age<15 or >75, Baseline Total Misisng or 100 ")
OutcomeChoices<- c("UEM 12Months", "LEM 12Months", "Total Motor 12Months", "UEM 6Months", "LEM 6Months", "Total Motor 6Months" )
ASIAChoices<- c("All", "A", "B", "C", "D")
InjuryRegionChoices <-c("All", "Cervical+Thoracic", "C1-L2")
drugC<- c( "No", "Yes")
drugC2<- c( "No", "Yes")

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Merged Sygen/Nascis MPSS BAYSEAN Analysis"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            ## Selecting the Data
            selectInput("dataType", "Which Data Type:", dataChoices) ,
            ##Selecting Injury REgion
            selectInput("injuryregion", "Which Injury Region:", InjuryRegionChoices),

            ## Selecting ASIA TYPE
            selectInput("ASIAT", "Which ASIA at Baseline:", ASIAChoices) ,
            selectInput("sygenI", "Exclude Sygen", drugC2),
            selectInput("naloxoneI", "Exclude Naloxone", drugC),

            ## Selecting Outcome Measure
            selectInput("OutcomeM", "Which Outcome Measure:", OutcomeChoices),

            ## Selecting ROPE MIN
            numericInput("ropeMin", "Minimum of Region of Practical Equivalence ", -2),
            ## Selecting ROPE MAX
            numericInput("ropeMax", "Maximum of Region of Practical Equivalence ", 2),

        ),

        # Show a plot of the generated distribution


        mainPanel(
            verbatimTextOutput("warning"),
            verbatimTextOutput("sumTable"),
            plotOutput("summary"),
            plotOutput("posterior"),
            tableOutput("bayStats"),
            verbatimTextOutput("baysCor"),
            plotOutput("baysCorGraphs"),
            plotOutput("ropeGraph"),
            tableOutput("ropeTable"),
            tableOutput("pdTable")

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    library(ggplot2)
    library(see)
    library(jtools)
    library(rstanarm)
    library(bayestestR)
    library(insight)
    library(logspline)
    library(BayesFactor)
    library(dplyr)
    library(see)
    library(modelbased)
    library(performance)
    library(parameters)


    ## Loading base data
    base_data <- reactive({
        if(input$dataType =="Raw Data"){
            baseData<-read.csv('mergeddata.csv')
        } else if (input$dataType =="Exclusions (GCS<12,Age<15 or >75, Baseline Total Misisng or 100 "){
            baseData<-read.csv('mergeddata.csv')
            baseData<- subset(baseData, exclude1 == "No")
        }
    })

    ## Selecting for injury region

    data_injury <- reactive({

        baseData <-   base_data()

        if (input$injuryregion=="All" ){
            dataIR<-baseData
        } else if (input$injuryregion=="Cervical+Thoracic") {
            dataIR<-subset(baseData, injury_regionCT == 1)

        } else if (input$injuryregion=="C1-L2") {
            dataIR <- subset(baseData, injury_regionCTL2 == 1)
        }
    })

    # All", "Cervical+Thoracic", "C1-L2")
    ##ACtive Data based on ASIA
    active_dataASIA <- reactive({

        dataIR <-   data_injury()
        dataASIA <- split(dataIR, f = dataIR$miss_asia_0)
        if(input$ASIAT == "All"){
            bdata <-dataIR
        } else if ( input$ASIAT == "A"){
            bdata <-dataASIA[[1]]
        } else if (input$ASIAT == "B"){
            bdata <-dataASIA[[2]]
        } else if ( input$ASIAT == "C"){
            bdata <-dataASIA[[3]]
        } else if ( input$ASIAT == "D"){
            bdata <-dataASIA[[4]]
        }
    })
    ## Sygen

    active_sygen <- reactive({
        bdata<-active_dataASIA()
        if(input$sygenI=="No"){
            bdata2 <- bdata
        } else if (input$sygenI=="Yes") {
            bdata2 <-subset(bdata, sygendrug ==0)
        }
    })
    ## Naloxone
    active_data <- reactive({
        bdata2 <-active_sygen ()
        if(input$naloxoneI=="No"){
            data <- bdata2
        } else if (input$naloxoneI=="Yes") {
            data <-subset(bdata2, naloxonedrug ==0)
        }

    })


    ##Outcome
    active_outcome <- reactive ({
        data <- active_data ()
        if(input$OutcomeM == "UEM 12Months"){
            Outcome <- data$UEMDiff12m

        } else if (input$OutcomeM == "LEM 12Months"){
            Outcome <- data$LEMDiff12m

        } else if (input$OutcomeM == "Total Motor 12Months"){
            Outcome <- data$TOMDiff12m

        } else if (input$OutcomeM == "UEM 6Months"){
            Outcome <- data$UEMDiff6m

        }else if (input$OutcomeM == "Total Motor 6Months"){
            Outcome <- data$TOMDiff6m

        } else if (input$OutcomeM == "LEM 6Months"){
            Outcome <- data$LEMDiff6m

        }
    })
    active_ropeMin <-reactive ({
        minRope <- input$ropeMin
    })
    active_ropeMax <-reactive ({
        maxRope <- input$ropeMax
    })
    ## Warning Msg

    output$warning <-renderPrint({

        print("Missing Carry Forward Method Was used for Missing Data Handling")

    })

    ## Summary table

    output$sumTable <- renderPrint({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment<- data$steroid

        table1 <- table( Treatment, Outcome )
        rowSums(table1)

    })


    ## Summary PLot
    output$summary <- renderPlot({
        data <-active_data()
        Outcome <- active_outcome ()

        OutcomeNA <- na.omit(Outcome)
        dataNA<- data[!is.na(Outcome), ]
        TreatmentNA <- dataNA$steroid


        dataNA %>%
            ggplot(aes(x = TreatmentNA, y = OutcomeNA, fill = TreatmentNA)) +
            geom_violindot(fill_dots = "black", size_dots = 1) +
            scale_fill_material() +
            ggtitle("Summary Plot")+
            xlab("Treatment") +
            ylab (input$OutcomeM) +
            annotate("text",
                     x = 1:length(table(TreatmentNA)),
                     y = aggregate(OutcomeNA ~ TreatmentNA, dataNA, median)[ , 2],
                     label = table(TreatmentNA),
                     col = "red",
                     vjust = - 20)+
            theme_modern()
    })

    ## Posterior Distribution Plot
    output$posterior <- renderPlot({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment <- data$steroid
        model <- reactive({
            if (is.null(input$modelVars) == TRUE){
                stan_glm(Outcome ~ Treatment , data = data)
            } else if (is.null(input$modelVars) == FALSE){
                vars <- as.matrix(data[, input$modelVars])
                stan_glm(Outcome ~ Treatment + vars, data = data)
            }
        })


        ##Extracting Posterior

        posteriors <- insight::get_parameters(model())

        ## Graph with mean median and mode
        ggplot(posteriors, aes(x = TreatmentSteroid)) +
            ggtitle("Posterior Plot")+
            geom_density(fill = "orange") +
            # The mean in blue
            geom_vline(xintercept = mean(posteriors$TreatmentSteroid), color = "blue", size = 1) +
            # The median in red
            geom_vline(xintercept = median(posteriors$TreatmentSteroid), color = "red", size = 1) +
            # The MAP in purple
            geom_vline(xintercept = map_estimate(posteriors$TreatmentSteroid), color = "purple", size = 1)
    })

    ## Important Baysean Statistics
    output$bayStats <- renderTable({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment <- data$steroid
        model <- reactive({
            if (is.null(input$modelVars) == TRUE){
                stan_glm(Outcome ~ Treatment , data = data)
            } else if (is.null(input$modelVars) == FALSE){
                vars <- as.matrix(data[, input$modelVars])
                stan_glm(Outcome ~ Treatment + vars, data = data)
            }
        })
        posteriors <- insight::get_parameters(model())

        describe_posterior(model(), test = c("p_direction", "rope", "bayesfactor"))
    })

    ## Baysean Correlation
    output$baysCor <- renderPrint({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment <- data$steroid
        Outcome<- as.numeric(Outcome)
        TreatmentN <- ifelse(Treatment == "Steroid", 0, 1)
        result <- correlationBF(Outcome, TreatmentN)
        describe_posterior(result)
        bayesfactor_models(result)

    })

    ## Visualizing Baysean Correlation
    output$baysCorGraphs<- renderPlot({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment <- data$steroid
        Outcome<- as.numeric(Outcome)
        TreatmentN <- ifelse(Treatment == "Steroid", 0, 1)
        result <- correlationBF(Outcome, TreatmentN)
        plot(bayesfactor_models(result)) +
            ggtitle("Bayes Factor")+
            scale_fill_pizza()
    })

    output$ropeGraph <-renderPlot({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment <- data$steroid
        maxRope <-  active_ropeMax ()
        minRope <- active_ropeMin ()
        model <- reactive({
            if (is.null(input$modelVars) == TRUE){
                stan_glm(Outcome ~ Treatment , data = data)
            } else if (is.null(input$modelVars) == FALSE){
                vars <- as.matrix(data[, input$modelVars])
                stan_glm(Outcome ~ Treatment + vars, data = data)
            }
        })
        posteriors <- insight::get_parameters(model())
        percentage_in_rope <- rope(posteriors$TreatmentSteroid, range = c(minRope, maxRope), ci = 0.89)
        plot(percentage_in_rope)
    })


    ## Table Custom ROPE
    output$ropeTable <- renderTable({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment <- data$steroid
        maxRope <-  active_ropeMax ()
        minRope <- active_ropeMin ()
        model <- reactive({
            if (is.null(input$modelVars) == TRUE){
                stan_glm(Outcome ~ Treatment , data = data)
            } else if (is.null(input$modelVars) == FALSE){
                vars <- as.matrix(data[, input$modelVars])
                stan_glm(Outcome ~ Treatment + vars, data = data)
            }
        })
        posteriors <- insight::get_parameters(model())
        percentage_in_rope <- rope(posteriors$TreatmentSteroid, range = c(minRope, maxRope), ci = 0.89)
        percentage_in_rope

    })
    ## Table PD
    output$pdTable <- renderTable ({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment <- data$steroid
        model <-  stan_glm(Outcome ~ Treatment , data = data)

        pd <- p_direction(model)
        pd
    })


}

# Run the application
shinyApp(ui = ui, server = server)
