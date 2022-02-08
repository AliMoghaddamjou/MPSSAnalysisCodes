#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(ggpubr)
library(ggplot2)
library(naniar)
library(Epi)
library(fragility)
library(dplyr)
library(tidyverse)
library(rstatix)
## Defining Choices

OutcomeChoices<- c("UEM 6Months", "LEM 6Months", "Total Motor 6Months" )
ASIAChoices<- c("All", "A", "B", "C", "D")
InjuryRegionChoices <-c( "Cervical+Thoracic", "C1-L2")



# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Merged Sygen/Nascis MPSS Analysis- FG Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

            ##Selecting Injury REgion
            selectInput("injuryregion", "Which Injury Region:", InjuryRegionChoices),

            ## Selecting ASIA TYPE
            selectInput("ASIAT", "Which ASIA at Baseline:", ASIAChoices) ,


            ## Selecting Outcome Measure
            selectInput("OutcomeM", "Which Outcome Measure:", OutcomeChoices),

            ## Selecting Binary Cutoff
            numericInput("cutoffInput", "Select Cutoff Value for the Binary Outcome  ", 2),



        ),

        # Show a plot of the generated distribution


        mainPanel(

            verbatimTextOutput("warning"),
            verbatimTextOutput("sumTable"),
            plotOutput("AllPlot"),
            tableOutput("AllTtest"),
            plotOutput("summary"),
            plotOutput("ttestPlot"),
            verbatimTextOutput("ttest"),
            plotOutput("BinaryPlot"),
            verbatimTextOutput("BinaryTable"),
            verbatimTextOutput("Fragility")



        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    library(ggplot2)
    library(dplyr)
    library(see)
    library(jtools)



    ## Loading base data


    ## Selecting for injury region

    data_injury <- reactive({

    baseData<-read.csv('fgdata.csv')

        if (input$injuryregion=="Cervical+Thoracic") {
            dataIR<-subset(baseData, injury_regionCT == 1)

        } else if (input$injuryregion=="C1-L2") {
            dataIR <- subset(baseData, injury_regionCTL2 == 1)
        }
    })

    # All", "Cervical+Thoracic", "C1-L2")
    ##ACtive Data based on ASIA
    active_data <- reactive({

        dataIR <-   data_injury()
        dataASIA <- split(dataIR, f = dataIR$miss_asia_0)
        if(input$ASIAT == "All"){
            data <-dataIR
        } else if ( input$ASIAT == "A"){
            data <-dataASIA[[1]]
        } else if (input$ASIAT == "B"){
            data <-dataASIA[[2]]
        } else if ( input$ASIAT == "C"){
            data <-dataASIA[[3]]
        } else if ( input$ASIAT == "D"){
            data <-dataASIA[[4]]
        }
    })






    ##Outomce
    active_outcome <- reactive ({
        data <- active_data ()
        if(input$OutcomeM == "UEM 6Months"){
            Outcome <- data$UEMDiff6m

        } else if (input$OutcomeM == "LEM 6Months"){
            Outcome <- data$LEMDiff6m

        } else if (input$OutcomeM == "Total Motor 6Months"){
            Outcome <- data$TOMDiff6m

        }
    })


    ## Data for the spread graphs

    graph_data <- reactive({

        data <- active_data ()


        mydata <- data[c("steroid", "UEMDiff6m", "LEMDiff6m", "TOMDiff6m")]


    })

    active_cutoff <- reactive({
        CutOff <- input$cutoffInput
    })


    ## Warning Msg

    output$warning <-renderPrint({

        print("Exclusions already applied. Carry back method was used  for Missing Data Handling")

    })

    ## Summary table

    output$sumTable <- renderPrint({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment<- data$steroid

        table1 <- table( Treatment, Outcome )
        rowSums(table1)

    })

    ## Graph All
    output$AllPlot <- renderPlot({

        data <-active_data()
        mydata <- graph_data ()
        mydata.long <- mydata %>%
            pivot_longer(-steroid, names_to = "variables", values_to = "value")

        # Create the plot
        myplot <- ggboxplot(
            mydata.long, x = "steroid", y = "value",
            fill = "steroid", palette = "npg", legend = "none",
            ggtheme = theme_pubr(border = TRUE)
        )+
            facet_wrap(~variables)


        myplot

    })

    ## All Ttest Analysis
    output$AllTtest <- renderTable({

        data <-active_data()
        mydata <- graph_data ()
        mydata.long <- mydata %>%
            pivot_longer(-steroid, names_to = "variables", values_to = "value")
        stat.test <- mydata.long %>%
            group_by(variables) %>%
            t_test(value ~ steroid) %>%
            adjust_pvalue(method = "BH") %>%
            add_significance()
        stat.test
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



    ##Ttest Plot
    output$ttestPlot<- renderPlot({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment<- data$steroid

        box_plot <- ggplot(data, aes(x = Treatment, y = Outcome))
        box_plot +
            ggtitle("Boxplot")+
            geom_boxplot() +
            geom_point(shape = 5,
                       color = "steelblue") +
            stat_summary(fun = mean, geom = "point", col = "red") +  # Add points to plot
            stat_summary(fun = mean, geom = "text", col = "red",     # Add text to plot
                         vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1))))+
            stat_summary(fun = median, geom = "point", col = "blue") +  # Add points to plot
            stat_summary(fun = median, geom = "text", col = "blue",     # Add text to plot
                         vjust = 2, aes(label = paste("Median:", round(..y.., digits = 1))))+

            theme_classic()
    })
    ## T-Test
    output$ttest <- renderPrint({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment<- data$steroid
        ttest<- t.test(Outcome~Treatment, data=data)
        print(ttest)
    })

    #

    ## Binary Mosaic
    output$BinaryPlot <- renderPlot({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment<- data$steroid
        CutOff<- active_cutoff ()

        OutcomeBinary <-ifelse(Outcome<CutOff,"Negative","Positive")
        table1 <- table( Treatment, OutcomeBinary )
        ##PLotting
        mosaicplot(table1, color = c("red", "green"), main = "Mosaic Plot for Binary Outcome",
                   xlab = "Treatment", ylab = "Outcome  ")

    })

    ## Two by 2
    output$BinaryTable <- renderPrint({

        data <-active_data()
        Outcome <- active_outcome ()
        Treatment<- data$steroid
        CutOff<- active_cutoff ()

        OutcomeBinary <-ifelse(Outcome<CutOff,"Negative","Positive")
        table2<-twoby2(Treatment, OutcomeBinary)
        table2
    })


    ## Fragility Index


    output$Fragility <- renderPrint({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment <- data$steroid
        CutOff<- active_cutoff ()

        OutcomeBinary <-ifelse(Outcome<CutOff,"Negative","Positive")
        table1 <- table( Treatment, OutcomeBinary )

        n0 <- table1["No Steroid", "Negative"] + table1["No Steroid", "Positive"]
        n1 <- table1["Steroid", "Negative"] + table1["Steroid", "Positive"]

        e0 <- table1["No Steroid", "Positive"]
        e1 <- table1["Steroid", "Positive"]

        Frag1<- frag.study(e0, n0, e1, n1)
        Frag1
    })

}



# Run the application
shinyApp(ui = ui, server = server)
