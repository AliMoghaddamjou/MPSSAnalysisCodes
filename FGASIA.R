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

## Defining Choices

OutcomeChoices<- c("ASIA Change >0 (6months)", "ASIA Change >1 (6months)" )
ASIAChoices<- c("All", "A", "B", "C", "D")



# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Merged Sygen/Nascis MPSS Analysis-ASIA - FG DATA"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

            ## Selecting ASIA TYPE
            selectInput("ASIAT", "Which ASIA Choice:", ASIAChoices) ,
            ## Selecting Outcome Measure
            selectInput("OutcomeM", "Which Outcome Measure:", OutcomeChoices),



        ),

        # Show a plot of the generated distribution


        mainPanel(

            verbatimTextOutput("warning"),
            verbatimTextOutput("sumTable"),
            plotOutput("BinaryPlot"),
            verbatimTextOutput("BinaryTable"),

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
    base_data <- reactive({

            baseData<-read.csv('fgdata.csv')


    })




    # All", "Cervical+Thoracic", "C1-L2")
    ##ACtive Data based on ASIA
    active_data <- reactive({

        dataIR <-   base_data()
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




    ##Outcome
    active_outcome <- reactive ({
        data <- active_data ()
        if(input$OutcomeM == "ASIA Change >0 (6months)"){
            Outcome <- data$ASIADiffI1

        } else if (input$OutcomeM == "ASIA Change >1 (6months)"){
            Outcome <- data$ASIADiffI2
        }
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

    output$BinaryPlot <- renderPlot({
        data <-active_data()
        Outcome <- active_outcome ()
        OutcomeNA <- na.omit(Outcome)

        Treatment<- data$steroid
        TreatmentNA<-na.omit(Treatment)


        table1 <- table( Treatment, Outcome )
        ##PLotting
        mosaicplot(table1, color = c("red", "green"), main = "Mosaic Plot for Binary Outcome",
                   xlab = "Treatment", ylab = "Outcome  ")

    })

    ## Two by 2
    output$BinaryTable <- renderPrint({

        data <-active_data()
        Outcome <- active_outcome ()
        OutcomeNA <- na.omit(Outcome)

        Treatment<- data$steroid
        TreatmentNA<-na.omit(Treatment)

        table2<-twoby2(Treatment, Outcome)
        table2
    })



}

# Run the application
shinyApp(ui = ui, server = server)
