library(shiny)

calc <- function(sens,spec,prev)
    {
        p00 <- (100-prev)*spec
        p10 <- (100-prev)*(100-spec)
        p01 <- prev*(100-sens)
        p11 <- prev*sens
        
        x <- matrix(c(p00,p01,p10,p11),nrow=2,byrow=TRUE)
        x <- as.table(x)
        dimnames(x) <- list(test=c("T-","T+"),disease=c("D-","D+"))
        
        npv <- p00/(p00+p01)
        ppv <- p11/(p11+p10)
        pv <- list(npv=npv,ppv=ppv)
        list(x=x,pv=pv)
    }

ui <- fluidPage(

  titlePanel("Anatomy of a diagnostic test"),

    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "sensitivity",
                        label = "Sensitivity",
                        min = 0,
                        max = 100,
                        value = 80),
            
            sliderInput(inputId = "specificity",
                        label = "Specificity",
                        min = 0,
                        max = 100,
                        value = 90),
            
            sliderInput(inputId = "prevalence",
                        label = "Disease prevalence",
                        min = 0,
                        max = 100,
                        value = 20),
            sliderInput(inputId = "ppv",
                        label = "Positive predictive value",
                        min = 0,
                        max = 100,
                        value = round(100*(calc(80,90,10)$pv)$ppv)),
            sliderInput(inputId = "npv",
                        label = "Negative predictive value",
                        min = 0,
                        max = 100,
                        value = round(100*(calc(80,90,10)$pv)$npv)),
            
            width=4),
        mainPanel(plotOutput(outputId = "mosaicplot"),width=8)
        )
    )


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

    
    output$mosaicplot <- renderPlot({

    sens <- input$sensitivity
    spec <- input$specificity
    prev <- input$prevalence

    out <- calc(sens,spec,prev)
        par(cex=2,mar=c(3,3,1,1),cex.lab=1)
        mosaicplot(t(out$x),main="",color=c("orange","maroon"))
      
    },height=600,width=600)
  
    output$pvtable <- renderDataTable({
        sens <- input$sensitivity
        spec <- input$specificity
        prev <- input$prevalence
        out <- calc(sens,spec,prev)
        out$pv})

    observe({
        sens <- input$sensitivity
        spec <- input$specificity
        prev <- input$prevalence
        out <- calc(sens,spec,prev)
        
        updateSliderInput(session, "ppv", min=0,max=100,
                          value=100*out$pv$ppv)
        updateSliderInput(session, "npv", min=0,max=100,
                          value=100*out$pv$npv)
    })
}



shinyApp(ui = ui, server = server)


