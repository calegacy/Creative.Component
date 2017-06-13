#combine ui and server

#Libraries

library(markdown)
library(ggplot2)
library(shinydashboard)
library(ggmosaic)
library(datasets)
library(shinythemes)

library(shiny)
library(MASS)
library(RColorBrewer)

library(grid)
library(gridExtra)

library(shiny)
library(shinydashboard)
library(LaplacesDemon)

shinyApp(
  # Create the UI with a dashboard for the project.
  # The dashboard will provide a space to add and delete desired panels.
  ui = dashboardPage(
    dashboardHeader(),
    
    #The sidebar will be where the selection of the groups will occur.
    # Using checkboxGroupInput will simpify the code for choosing which tabs we want.
    dashboardSidebar(
      
      checkboxGroupInput("checkGroup", label = h3("Topic Selction"),
                         choices = list("One Proportion" = 1, "Confidence Interval" = 2, "One Mean" = 3,
                                        "Two Proportions" = 4, "Two Means" = 5, "Correlation" = 6, "Outliers" = 7,
                                        "Equation" = 8, "ANOVA Means" = 9, "ANOVA St Dev" = 10, "ANOVA Sample Size" = 11))
    ),
    # This code will allow the tab selections we create to appear in the output.
    dashboardBody(
      uiOutput('mytabs')
    )
  ),
  
  
  server = function(input, output, session){
    
    # Here we build the UI for each tab panel. Since we are building the UI based on input,
    # this infomration goes into the renderUI() function
    output$mytabs = renderUI({
      
      # Create a list of all possible tabs to be selected. We will select from this list of panels
      # based on out checkBoxInput. The selected will correspond to these panels,
      # giving us the new collection of panels to build out UI output.
      newTabPanels <- list(
        # One Proportion Sampling Distribution
        tabPanel(h6("One Proportion"),
                 fixedPage(responsive = NULL, 
                 column(width = 2,
                        
                        numericInput("popProp", "Population Proportion",
                                     value = 0.3,min = 0, max = 1, step = 0.01),
                        numericInput("sampleSize", "Sample Size", value = 100, min = 1),
                        radioButtons("numSamp", label = "Number of Samples",
                                     choices = list("1 " = 1, "100" = 100, "Many" = 1000), 
                                     selected = 1),
                
                        actionButton("goProp", "Draw",class="btn btn-success btn")
                        
                 ),
                 
                 column(width = 9,
                        verbatimTextOutput("sdist"),
                        tableOutput("sampSumDat"),
                        plotOutput("sampleDist", width = "auto", height = 200),
                        verbatimTextOutput("singdist"),
                        tableOutput("popSumDat"),
                        plotOutput("samplingDist", width = "auto", height = 200)
                        
                 )
              
        )),
        #Confidence interval
        tabPanel(h6("Confidence Interval"),
                 fluidRow(
                   box(width = 3, title = NULL, status = "primary", 
                       sliderInput("cIDemoCL", "Confidence Level %", value = 80, min = 80, max = 99, step = 5)
                   ),
                   box(width = 3, title = NULL, status = "primary",
                       sliderInput("cIDemoSampSize", "Sample Size", value = 25, min = 25, max = 500, step = 5)
                   ),
                   box(width = 3, title = NULL, status = "primary",
                       sliderInput("cIDemoNumSamp", "Number of Samples", value = 25, min = 1, max = 100, step = 24)
                       
                       
                   ),
                   box(width = 3, status = "primary",
                   actionButton("redrawCI", h5("Draw"), width = '100%',class="btn btn-success btn")
                   )
                 ),
                 fluidRow(
                   box(width = 12, title = NULL, status = "primary",
                       plotOutput("sampCLPlot"))
                   
                   
                 )
        ),
        # Inference for one Mean
        tabPanel( h6("One Mean"),
                  fixedPage(responsive = NULL,
                            
                  column(width = 2,
                         
                         numericInput("popMeanOM", "Population Mean",
                                      value = 10,min = 0, max = 1000, step = 1),
                         numericInput( "sigma", "Standard Deviation", value = 1, min = 0.01),
                         numericInput("sampleSizeOM", "Sample Size", value = 10, min = 1),
                    
                         radioButtons("numSampOM", label = "Number of Samples",
                                      choices = list("1 " = 1, "100" = 100, "Many" = 1000), 
                                      selected = 1),
                         actionButton("goMean", "Draw",class="btn btn-success btn")
                         
                  ),
                  
                  column(width=8,
                         verbatimTextOutput("OneMeanDist"),
                         tableOutput("sampSumDatOM"),
                         verbatimTextOutput("ManyMeansDist"),
                         plotOutput("samplingDistOM", width = "auto", height = 200),
                         tableOutput("popSumDatOM"),
                         plotOutput("sampleDistOM", width = "auto", height = 200)
                        
                  )
                 
                  
        )),
        
        # Inference for two Proportions
        tabPanel( h6("Two Proportions"),
                  fixedPage( responsive = NULL,
                  column(width = 3, 
                         box( width ='100%',  status = "primary", title = "Group 1",
                         numericInput("popPropG1P", h6("Population Proportion "),
                                      value = 0.3, min = 0, max = 1, step = 0.01),
                         numericInput("sampleSizeGP1", h6("Sample Size"), value = 100, min = 1)
                         
                         ),
                         box(width = "100%", status = "primary", title = "Group 2",
                         numericInput("popPropG2P",h6("Population Proportion "),
                                      value = 0.3, min = 0, max = 1, step = 0.01),
                         
                         numericInput("sampleSizeGP2", h6("Sample Size "), value = 100, min = 1)
                         ),
                         radioButtons("numSampGP", label = "Number of Samples",
                                      choices = list("1 " = 1, "100" = 100, "Many" = 1000), 
                                      selected = 1),
                         actionButton("go2Prop", "Draw",class="btn btn-success btn")
                  ),
                  
                  column(width = 9,
                         
                         tableOutput("sampSumDatP2"),
                         verbatimTextOutput("twoPropSampleDist"),
                         plotOutput("sampleDistG1", width = 'auto', height = 200),
                         tableOutput("popSumDat2P"),
                         verbatimTextOutput("twoPropSampDist"),
                         plotOutput("samplingDistTP", width = 'auto', height = 200)
                         
                         
                  )
                  
                  
               
        )),
        
        # Inference for two Means
        tabPanel( h6("Two Means"),
                  fixedPage( responsive = NULL,
                  column(width = 2,
                         box(width = 2, title = "Group 1",
                         numericInput("popMeanM1", h6("Group 1"),
                                      value = 5,min = 0, max = 1, step = 0.1),
                         numericInput("sampleSizeTM1", h6("Group 1 Size"), value = 10, min = 1),
                         numericInput( "sigmaTM1", h6("Group 1 St Dev"), value = 1, min = 0.01)
                         ),
                         
                         
                         box(width = 2, title = "Group 2",
                        numericInput("popMeanM2", h6("Group 2"), value = 5,min = 0, max = 1, step = 0.1),
                         numericInput( "sigmaTM2", h6("Group 2 St Dev"), value = 1, min = 0.01),
                         numericInput("sampleSizeTM2", h6("Group 2 Size"), value = 10, min = 1)
                         ),
                         radioButtons("numSampTM", label = "Number of Samples",
                                      choices = list("1 " = 1, "100" = 100, "Many" = 1000), 
                                      selected = 1),
                      
                         actionButton("go2Mean", "Draw",class="btn btn-success btn")
                  
                  ),
                  column(width = 8,
                         verbatimTextOutput("twoMeansSampDist"),
                         tableOutput("sampSumDatTM"),
                         plotOutput("sampleDistM1", width = 'auto', height = 300),
                         
                         verbatimTextOutput("twoMeansSamplingDist"),
                         tableOutput("popSumDatTM"),
                         plotOutput("samplingDistTM", width = '100%', height = 200)
                         
                  )
                  
                  )    
        ),
        
        # Correlation
        tabPanel( h6("Correlation"),
                  column(width = 12,
                         box(width = 12,
                             title = NULL,  status= "primary",
                             sliderInput("correlation", "Correlation", value = 0.1, min = -1, max =1, step = 0.01)
                             
                         ),
                         box( width = 10, status = "primary", align = "center",
                           plotOutput("corrPlot", width = '350', height = '350')
                           
                         ))
        ),
        
        # Outliers
        tabPanel(h6("Outliers"),
                 
                 column(
                   width = 4, status= "primary",
                   actionButton("outData", "Plot Data"),
                   checkboxInput("fitLine", label = "Fit Line", value = FALSE),
                   
                   verbatimTextOutput("lineSum"),
                   tableOutput("lineEq"),
                   sliderInput("outX", "Outlier X",
                                value = 0.3,min = 0, max = 1, step = 0.01),
                   sliderInput("outY", "Outlier Y",
                                value = 0.3,min = 0, max = 1, step = 0.01),
                   actionButton("outPoint", "Plot Point"),
                  
                   checkboxInput("fitLineNoPt", label = "Fit Line Without Point", value = FALSE),
                   verbatimTextOutput("lineSumNoPt"),
                   tableOutput("lineEqNoPt")
                   
                 ),
                 column(
                   width = 5, status = "primary",
                   box(
                     width = "100%", height = "100%",
                     title = "Linear Regression",  status = "primary",
                     plotOutput("outlierPlot" )
                   )
                   
                 )
        ),
        
        
       #Regression tab
        tabPanel(h6("Equation"), value = "eqBd",
                 fixedPage(responsive= NULL,
                 column(width = 4, status = "primary",
                       
                       
                        sliderInput("ssx", "St dev X", value = 15, min = 0, max =40),
                        sliderInput("ssy", "St dev y", value = 15, max =40, min = 0),
                       
                        sliderInput("meanx", "Mean x", value = 10, max =20, min = -20),
                        sliderInput("meany", "Mean y", value = 10, min = -20, max =20),
                        sliderInput("correlationLM", "Correlation", value = 0.5, min = -1, max =1, step = 0.01)
                       # checkboxInput("fitPoints", "Fit Line",value = FALSE)
    
                 ),
                 
                 column(
                   checkboxInput("fitPoints", "Fit Line",value = FALSE),
                   width = 7, status = "primary",
                   box(
                     width = "100%", height = "100%",
                     title = "Linear Regression",  status = "primary",
                     plotOutput("linreg" ),
                     tableOutput("eqPointsTable")
                   )
                   
                 )
                 
        )),
        
        
        #ANOVA Mean
        tabPanel(h6("ANOVA Means"),
                 fluidRow(
                   column(width = 4,
                          
                          sliderInput("anovaMean1", h6("Mean 1"), value = 30, min = 10, max =50, step = 1)
                   ),
                   column(width = 4,
                          sliderInput("anovaMean2", h6("Mean 2"), value = 30, min = 10, max =50, step = 1)
                         
                   ),
                   column(width = 4,
                          sliderInput("anovaMean3", h6("Mean 3"), value = 30, min = 10, max =50, step = 1)
                   )
                 ),
                 actionButton("goAnovaMean", "Draw",class="btn btn-success btn"),
                 fluidRow(
                   column(width = 7,
                          plotOutput("anovaPlotMean", width = "auto", height = 500)
                   ),
                   column( width = 5,
                           tableOutput("anovaDatSumMean"),
                           tableOutput("anovaTableMean")
                   )
                 )
        ),
       
       #ANOVA Standard deviation
        tabPanel(h6("ANOVA St Dev"),
                 
                 
                 fluidRow(
                   column(width = 4,
                          
                          sliderInput("anovaSD1", h6("St Dev 1"), value = 10, min = 1, max =50, step = 1)
                          
                   ),
                   column(width = 4,
                        
                          sliderInput("anovaSD2", h6("St Dev 2"), value = 10, min = 1, max =50, step = 1)
                         
                   ),
                   column(width = 4,
                          sliderInput("anovaSD3", h6("St Dev 3"), value = 10, min = 1, max =50, step = 1)
                   )
                 ),
                 actionButton("goAnovaSD", "Draw",class="btn btn-success btn"),
                 fluidRow(
                   column(width = 7,
                          plotOutput("anovaPlotSD", width = "auto", height = 500)
                   ),
                   column( width = 5,
                           tableOutput("anovaDatSumSD"),
                           tableOutput("anovaTableSD")
                   )
                 )
        ) ,
       #ANOVA Sample Size
        tabPanel(h6("ANOVA Sample Size"),
                 
                 
                 fluidRow(
                   column(width = 8,
                          sliderInput("sampSizeANOVA", h6("Sample Size"), value = 50, min = 10, max =100, step = 5)
                   ),
                   column(width = 4,
                          actionButton("goAnovaSS", "Draw",class="btn btn-success btn", width = '100%')
                 ),
                 fluidRow(
                   column(width = 7,
                          plotOutput("anovaPlotSS", width = "auto", height = 500)
                   ),
                   column( width = 5,
                           tableOutput("anovaDatSumSS"),
                           tableOutput("anovaTableSS")
                   )
                 )
        )
        
      )
      )
      
      
      # Choose the new tabs based on the input from the user.
      collectNewTabs = newTabPanels[c(as.numeric(input$checkGroup))]
      
      # doCall creates the layout for the tabsetPanel based on the selection above
      do.call(tabsetPanel, collectNewTabs)
    })
    
    # ------------ This section will contain all the server code that goes with each tab. ------------- #
    
    
    # -------------------------------------------- Inference for One Proportion Sampling Dist. ----------------------------#
    # Create a single sample distribution
    # Get a sample from a binomial distribution
    pickVect = eventReactive(input$goProp,{
      pickf = binomProb(input$sampleSize,input$popProp)
      return(pickf)
    })
    
    #Sample Distribution bar graph
    output$sampleDist = renderPlot({
      qplot(c(rep("Success", input$sampleSize*pickVect()),rep("Failure", input$sampleSize*(1-pickVect()))), xlab = "Category", ylab = "Count")
    })
    
    #Sample summary information to be put into a table
    samplesum = eventReactive(input$goProp,{data.frame(
      Category = c( "Success", "Failure"),
      Prop = c(pickVect(), 1-pickVect())
    )
    })
    
    #Sample summary table output
    output$sampSumDat  = renderTable(caption = "Sample Summary Statistics",caption.placement = getOption("xtable.caption.placement", "top"),{
      samplesum()
    })
    
    #Calculations to make a sampling distribution
    samples = eventReactive(input$goProp,{
      # If one sample is needed use only the one p_hat from above
      if(input$numSamp == 1){
        return(pickVect())
      }
      # If many samples are needed, generate a vector of p_hats by the above method
      if(input$numSamp > 1){
        v = c(pickVect())
        for(i in 2:input$numSamp){
          # Generate a sample
          npick = reactive({
            newprop = binomProb(input$sampleSize, input$popProp)
            return(newprop)
          })
          # Store the generated sample in a vector
          v = c(v,npick())
        }}
      # Return to
      return(v)
    })
    
    bins = reactive({
      if(input$numSamp>1){
      bin = (3.5*sd(samples()) )/(input$sampleSize^(1/3))}
      else{ bin = 0.1}
      return(bin)
    })

    # Histogram of the sampling distribution
    output$samplingDist =renderPlot({
      ggplot(data = data.frame(samples()),aes(samples()))+ geom_histogram(binwidth = bins()) +xlab("Sample Proportion") +ylab("Number of Samples")
    })
    
    # Population summary info to be displayed in a table
    popSum = eventReactive(input$goProp,{data.frame(
      Mean = mean(samples()),
      "Standard Deviation" = sd(samples())
    )
    })
    
    #Population summary table output
    output$popSumDat  = renderTable(caption = "Sampling Distribution Summary Statistics",caption.placement = getOption("xtable.caption.placement", "top"),{
      popSum()
    })
    
    # Add the text for "Sample/Sampling Distribution" to the Shiny display
    output$sdist <- renderText({ "Data Distribution" })
    output$singdist <- renderText({ "Sampling Distribution" })
    #--------------------------------------------- One proportion CIs ---------------------------------------------------- #
    
    # Generate 20 sample p_hats to create confidence intervals around
    twSamps = eventReactive(input$redrawCI,{
      tsampCL = NULL
      for(i in 1:input$cIDemoNumSamp){
        
        # Get the proportion of "successes" for the generated sample
        countPropCL = binomProb(input$cIDemoSampSize, 0.5)
        tsampCL = c(tsampCL,countPropCL)
      }
      return(tsampCL)
    })
    
    # Create a sample vector of y values so that the confidence intervals can be stacked in the plot
    ysCLDemo = reactive({
      yy = seq(from = 1, by = 0.5, length.out = length(twSamps()))
      return(yy)
    })
    
    # Set the z-value to be the value input by the user
    zlev = reactive({
      lev = qnorm(((100 -  (100-input$cIDemoCL)/2)/100), 0,1)
      return(lev)
    })
    
    # Calculate the upper bounds for the confidence intervals
    upCLDemo = reactive({
      usd = twSamps() + zlev()*(sqrt(twSamps()*(1-twSamps())/input$cIDemoSampSize))
      return(usd)
    })
    
    # Calculate the lower bounds for the confidence intervals
    lowCLDemo = reactive({
      lsd =  twSamps() - zlev()*(sqrt(twSamps()*(1-twSamps())/input$cIDemoSampSize))
      return(lsd)
    })
    
    # Color the interval based on whether or not they capture the true population proportion
    colorCLDemo = reactive({
      coldemo = NULL
      for(i in 1:input$cIDemoNumSamp){
        # Color the intervals blue if they contain 0.4 (the set value for the population parameter)
        if(lowCLDemo()[i] <= 0.5 && 0.5  <= upCLDemo()[i]   ){
          
          colne = "steelblue1"
          coldemo = c(coldemo,colne)
          
        }
        # Color the intervals orange if they do not contain 0.4 (the set value for the population parameter)
        else{
          
          colne = "tan1"
          coldemo = c(coldemo,colne)
          
        }
      }
      # Return the vector of colors to be added to the plot
      return(coldemo)
    })
    
    # Graph the confidence intervals stacked on one plot to display how many captured the population proportion
    output$sampCLPlot = renderPlot({
      qplot(x = twSamps(), y = ysCLDemo(), xlab = "Confidence Intervals")+xlim(0,1)+
        geom_segment(aes(x = lowCLDemo(), xend = upCLDemo(), y = ysCLDemo(), yend = ysCLDemo()), colour = colorCLDemo())+
        theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+geom_vline(xintercept = 0.5, color = "slategray4")
    })
    
    
    # --------------------------------------------- Inference for One Mean ------------------------------------#
    
    # Calculations to get a sample distribution
    
    
    # Generate a sample of means from a population with the input mean and the sample standard deveation
    pickOM = eventReactive(input$goMean,{
      pickfOM = c(rnorm(input$sampleSizeOM, input$popMeanOM, input$sigma))
      return(pickfOM)
    })
    
    # Sample Distribution output
    output$sampleDistOM = renderPlot({
      qplot(pickOM(), xlab = "x", ylab = "Count")
    })
    
    #Sample summary information to be displayed in a table
    samplesumOM = reactive({data.frame(
      Mean = mean(pickOM()),
      "Standard Deviation" = sd(pickOM())
    )
    })
    
    # Output for the sample summary information
    output$sampSumDatOM  = renderTable(caption = "Sample Summary Statistics",caption.placement = getOption("xtable.caption.placement", "top"),{
      samplesumOM()
    })
    
    #Calculations to make a sampling distribution
    
    # Generate many samples
    samplesOM = eventReactive(input$goMean,{
      # If the number of samples is only one just keep the one sample generated above
      if(input$numSampOM == 1){
        return(mean(pickOM()))
      }
      # If the user wants many samples generate them as follows
      if(input$numSampOM > 1){
        v = c(mean(pickOM()))
        for(i in 2:input$numSampOM){
          
          nnpick = reactive({
            newpick = c(rnorm(input$sampleSizeOM, input$popMeanOM, input$sigma))
            # Calculate the mean for your new sample
            meanSample = mean(newpick)
            return(meanSample)
          })
          # Store the mean for the sample in a vector
          v = c(v,nnpick())
        }
      }
      # Return the vector of the means of each sample generated
      return(v)
    })
    
    binsOM = reactive({
      if(input$numSampOM > 1){
      bin = (3.5*sd(samplesOM()) )/(input$sampleSizeOM^(1/3))}
      else{ bin = 0.5}
      return(bin)
    })
    # Output a histogram of the sampling distribution of the sample means
    output$samplingDistOM =renderPlot({
      ggplot(data = data.frame(samplesOM()),aes(x = samplesOM())) + geom_histogram(binwidth = binsOM())+xlab("Sample Means") +ylab("Number of Samples")
      
    })
    
    # Population summary information to be put in a table
    popSumOM = reactive({data.frame(
      
      Mean = mean(samplesOM()),
      "Standard Deviation" = sd(samplesOM())
    )
    })
    
    # Output the table of the population summary information
    output$popSumDatOM  = renderTable(caption = "Summary of Sampling Distribution",caption.placement = getOption("xtable.caption.placement", "top"),{
      popSumOM()
    })
    
    # Put text on the shiny app to distinguish the Sample Distribution from the Sampling Distribution
    output$OneMeanDist <- renderText({ "Data Distribution" })
    output$ManyMeansDist <- renderText({ "Sampling Distribution" })
    
    #--------------------------------------------- Inference in Two Proportions ---------------------------------------#
    
    # Calculations to get a sample distribution for group 1
    pickP1Prop = eventReactive(input$go2Prop,{
      pickfp1 = binomProb(input$sampleSizeGP1, input$popPropG1P)
      return(pickfp1)
    })
    
    # Generate a binomial sample for group 2
    pickP2Prop = eventReactive(input$go2Prop,{
      pickfp2 =binomProb(input$sampleSizeGP2, input$popPropG2P)
      return(pickfp2)
    })
    
    propDat = reactive({data.frame(
        sf = c(rep("Success", input$sampleSizeGP1*pickP1Prop()),rep("Failure", input$sampleSizeGP1*(1-pickP1Prop())),
        rep("Success", input$sampleSizeGP2*pickP2Prop()),rep("Failure", input$sampleSizeGP2*(1-pickP2Prop()))),
        grp = c(rep("Group 1", input$sampleSizeGP1), rep("Group 2", input$sampleSizeGP2) )
      )
    
    })

    # Sample Distribution output for group 1 sample
    output$sampleDistG1 = renderPlot({
      ggplot(data = propDat()) +  geom_mosaic(aes(x = product(sf,grp), fill = as.factor(sf)))+xlab("Group")+
        guides(fill=guide_legend(title="Category"))
    })
    
    #Sample summary information for two proportions to be displyed in a table
    samplesumP2 = eventReactive(input$go2Prop,{data.frame(
      "Proportion 1" = pickP1Prop(),
      "Proporiton 2" = pickP2Prop(),
      Difference =pickP1Prop()-pickP2Prop()
    )
    })
    
    # Table output for the difference in the sample distributions
    output$sampSumDatP2  = renderTable(caption = "Summary of Samples",caption.placement = getOption("xtable.caption.placement", "top"),{
      samplesumP2()
    })
    
    #### Sampling Dist for two proportions ####
    
    # Calculations to make a sampling distribution of the difference in proportions
    samplesP2 = eventReactive(input$go2Prop,{
      # If there is only one sample needed, use the one created above
      if(input$numSampGP == 1){
        return(pickP1Prop()-pickP2Prop())
      }
      # If many samples are needed, draw many samples for two groups
      if(input$numSampGP > 1){
        vp2 = c(pickP1Prop()-pickP2Prop())
        for(i in 2:input$numSampGP){
          
          npickdiffp = reactive({
           
            # Calculate the difference in the two samples
            newpropdiff  =  binomProb(input$sampleSizeGP1, input$popPropG1P) -  binomProb(input$sampleSizeGP2, input$popPropG2P)
            # Return the difference of the two samples
            return(newpropdiff)
          })
          # Store differences in a vector
          vp2 = c(vp2,npickdiffp())
          
        }}
      # Return the vector of differences
      return(vp2)
    })
    
    binsTwoProp = reactive({
      if(input$numSampGP > 1){
      bin = (3.5*sd(samplesP2()) )/(input$sampleSizeGP1^(1/3))}
      else{ bin  = 0.1}
      return(bin)
    })
    # Output the plot for the sampling distribution for the difference in proportions
    output$samplingDistTP =renderPlot({
      qplot(samplesP2(), xlab = "Difference in Proportions", ylab = "Number of Samples", binwidth = binsTwoProp())
    })
    
    
    # Population summary info and p-values to be displayed in a table
    popSumTP = reactive({data.frame(
      Mean = mean(samplesP2()),
      "Standard Deviation" = sd(samplesP2())
    )
    })
    
    # Output the table of summary information and test statistics
    output$popSumDat2P = renderTable(caption = "Samping Distribution Summary",caption.placement = getOption("xtable.caption.placement", "top"),{
      popSumTP()
    })
    
    # Output the text labels for the Distributions and test options
    output$twoPropSampleDist <- renderText({ "Data Distribution" })
    output$twoPropSampDist <- renderText({ "Sampling Distribution" })
    
    # --------------------------------------------- Two Means Code -----------------------------------------------------#
    
    # Generate a sample for group one
    pickM1 = eventReactive(input$go2Mean,{
      pickfM1 = c(rnorm(input$sampleSizeTM1, input$popMeanM1, input$sigmaTM1))
      return(pickfM1)
    })
    
    # Generate a sample for group two
    pickM2 = eventReactive(input$go2Mean,{
      pickfM2 = c(rnorm(input$sampleSizeTM2, input$popMeanM2, input$sigmaTM2))
      return(pickfM2)
    })
    
    # Make data frame
    datTM = reactive({data.frame(
      dataTM = c(pickM1(), pickM2()),
      groupTM = c(rep("Group 1", input$sampleSizeTM1), rep("Group 2", input$sampleSizeTM2))
    )
      
    })
    
    #Sample Distribution plot
    output$sampleDistM1 = renderPlot({

      ggplot(datTM(), aes(x=groupTM, y=dataTM, fill = groupTM)) + 
        geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.5, binwidth = binsTM())+
        stat_summary(fun.y=mean, geom="point", shape=5,size=6, color="black")+
        labs(x = "Group", y = "Value")+
        guides(fill=guide_legend(title="Group"))+scale_y_continuous(NULL, breaks = NULL)

    })
    
    # Table summary information for groups one and two
    samplesumTM = reactive({data.frame(
      Mean1 = mean(pickM1()),
      Mean2 = mean(pickM2()),
      Difference= mean(pickM1())-mean(pickM2())
    )
    })
    
    # Output the table information for groups one and two
    output$sampSumDatTM  = renderTable(caption = "Summary of Samples",caption.placement = getOption("xtable.caption.placement", "top"),{
      samplesumTM()
    })
    
    # Generate many samples to make a sampling distribution of differences in means
    samplesTM = eventReactive(input$go2Mean,{
      # For only one sample keep the difference from above
      if(input$numSampTM == 1){
        return(mean(pickM1())-mean(pickM2()))
      }
      # For many samples, generate many samples, calculate the means, and get the difference in the means
      if(input$numSampTM > 1){
        vm = c(mean(pickM1())-mean(pickM2()))
        for(i in 2:input$numSampTM){
          nmpick = reactive({
       diffmean = mean(c(rnorm(input$sampleSizeTM1, input$popMeanM1, input$sigmaTM1))) - mean(c(rnorm(input$sampleSizeTM2, input$popMeanM2, input$sigmaTM2)))
            return(diffmean)
          })
          # Hold all the differences in means in a vector
          vm = c(vm,nmpick())
        }
      }
      # Return the differences in means vector to be plot as the sampling distribution
      return(vm)
    })
    
    binsTM = reactive({
      if(input$numSampTM > 1){
      bin = (3.5*sd(samplesTM()) )/(input$sampleSizeTM1^(1/3))}
      else{ bin = 0.5}
      return(bin)
    })
    # Plot the sampling distribution for the differences in means
    output$samplingDistTM =renderPlot({
      qplot(samplesTM(), xlab = "Value", ylab = "Count", binwidth = binsTM())
    
    })
    
    
    # Sampling distribution and test statistics information to be displayed in a table
    popSumTM = reactive({data.frame(
      Mean = mean(samplesTM()),
      "Standard Deviation" = sd(samplesTM())
    )
    })
    
    # Table output of the above information
    output$popSumDatTM  = renderTable(caption = "Summary Sampling Distribution",caption.placement = getOption("xtable.caption.placement", "top"),{
      popSumTM()
    })
    
    # Text output to label the distributions and test options
    
    output$twoMeansSampDist <- renderText({ "Data Distribution" })
    output$twoMeansSamplingDist <- renderText({ "Sampling Distribution" })
    # --------------------------------------------- Linear Regression Code ----------------------------------------------#
    #---------------------------------------------- Correlation Tab ---------------------------------------------------#
    
    # Make the dataset with a set correlation for the scatterplot
    xycorr =reactive({
      datxy = as.data.frame(mvrnorm(100, mu = c(0,0), Sigma = matrix(c(1,input$correlation,input$correlation,1),, ncol = 2),empirical = TRUE))
     
      return(datxy)
    })

    #Output the scatterplot of the data created above
    output$corrPlot = renderPlot({
      qplot(xycorr()$V1,xycorr()$V2, xlab = "x", ylab = "y")

    })
    
    #----------------------------------------------- Outliers tab ----------------------------------------------------#

    getData = reactive({
      datxy = as.data.frame(mvrnorm(20, mu = c(0,0),
                                    Sigma = matrix(c(1,0.99,0.99,1),, ncol = 2),
                                    empirical = TRUE))
      datnew = data.frame(dataX = c(datxy$V1), dataY = c(datxy$V2))
      return(datnew)
    })
    
    output$outlierPlot = renderPlot({
      # Print the plot with just the data
         print(ggplot(data = getData(), aes(x = dataX, dataY)) + geom_point()) 
      
      # Fit the line to the plot 
      if(input$fitLine == TRUE){
            # Make a linear model if the dataset
            hL1 = lm(getData()$dataY~getData()$dataX)
            # Print the plot with the fitted line

            print(ggplot(data = getData(), aes(x = dataX, dataY)) + geom_point() +
                    geom_abline(slope = hL1$coefficients[2],intercept = hL1$coefficients[1], colour = "navyblue", size = 0.75))

            # Collect the information about the line to be displayed in a table
            hL1Line = reactive({data.frame(
              intercept = hL1$coefficients[1],
              slope = hL1$coefficients[2],
              Rsquared = summary(hL1)$r.squared
            )
            })
            # Output the table of information about the line
            output$lineEq  = renderTable({
              hL1Line()
            })
          }
     
    })
    
    # # Print the plot for the high leverage data
    # output$outlierPlot = renderPlot({
    #   # Print the plot with all the data
    #   print(ggplot(data = getData(), aes(x = dataX, dataY)) + geom_point() +
    #           geom_point(aes(x = dataX[21], y = dataY[21]), color= "firebrick2", size = 2))
    #   # Print the plot with the line fit to the data
    #   if(input$fitLine == TRUE){
    #     # Make a linear model if the dataset
    #     hL1 = lm(getData()$dataY~getData()$dataX)
    #     # Print the plot with the fitted line
    #     
    #     print(ggplot(data = getData(), aes(x = dataX, dataY)) + geom_point() +
    #             geom_point(aes(x = dataX[21], y = dataY[21]), colour= "firebrick2", size = 2) +
    #             geom_abline(slope = hL1$coefficients[2],intercept = hL1$coefficients[1], colour = "navyblue", size = 0.75))
    #     
    #     # Collect the information about the line to be displayed in a table
    #     hL1Line = reactive({data.frame(
    #       intercept = hL1$coefficients[1],
    #       slope = hL1$coefficients[2],
    #       Rsquared = summary(hL1)$r.squared
    #     )
    #     })
    #     # Output the table of information about the line
    #     output$lineEq  = renderTable({
    #       hL1Line()
    #     })
    #   }
    #   # If the Fit with no red point box is selected
    #   if(input$fitLineNoPt == TRUE ){
    #     # Calculate the line of best fit without the point of interest
    #     hL1NoPt = lm(getData()$dataY[-21]~getData()$dataX[-21])
    #     # Print the plot with the line of fit for the data without the point of interest
    #     
    #     print(ggplot(data = getData(), aes(x = dataX, dataY)) + geom_point() +
    #             geom_point(aes(x = dataX[21], y = dataY[21]), colour= "firebrick2", size = 2) +
    #             geom_abline(slope = hL1NoPt$coefficients[2],intercept = hL1NoPt$coefficients[1], linetype = "dashed", colour="springgreen2", size = 1))
    #     
    #     # Create a table of information about the line fit to the data without the point of interest
    #     hL1LineNoPt = reactive({data.frame(
    #       intercept = hL1NoPt$coefficients[1],
    #       slope = hL1NoPt$coefficients[2],
    #       Rsquared = summary(hL1NoPt)$r.squared
    #     )
    #     })
    #     # Output this table to the user
    #     output$lineEqNoPt  = renderTable({
    #       hL1LineNoPt()
    #     })
    #     # If the "Fit Line" is also selected, then the plot will display both lines of best fit
    #     if(input$fitLine == TRUE){
    #       
    #       print(ggplot(data = getData(), aes(x = dataX, dataY)) + geom_point() +
    #               geom_point(aes(x = dataX[21], y = dataY[21]), colour= "firebrick2", size = 2) +
    #               geom_abline(slope = hL1$coefficients[2],intercept = hL1$coefficients[1], colour = "navyblue", linetype ="F1", size = 0.75) +
    #               geom_abline(slope = hL1NoPt$coefficients[2],intercept = hL1NoPt$coefficients[1], linetype = "dashed", colour="springgreen2", size = 1)+
    #               scale_colour_manual(name="Legend",values=c("fitted" = "navyblue", "noPoint" = "springgreen2"))+
    #               scale_linetype_manual(name = "Legend", values = c("fitted" = "F1", "noPoint"= "dashed"))+
    #               theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
    #               guides(fill=guide_legend(title="Legend")))
    #     }
    #   }
    # })
    
    # Add text to label the tables for the line with the point of interest and without it
    output$lineSum <- renderText({ "Line Summary" })
    output$lineSumNoPt <- renderText({ "Line Summary Without Point" })
    # ------------------------------------------------ Regression Line -----------------------------------------------#
    
    
    # Create a data frame with the desired slope
    xy = reactive({
      datXY = as.data.frame(mvrnorm(100, mu = c(0,0), Sigma = matrix(c((input$ssx)^2,input$correlationLM*input$ssx*input$ssy,input$correlationLM*input$ssx*input$ssy,(input$ssy)^2),, ncol = 2),empirical = TRUE))
      normY = (datXY$V2 - mean(datXY$V2) / sd(datXY$V2)) + input$meany
      normX = (datXY$V1 - mean(datXY$V1) / sd(datXY$V1)) + input$meanx
      normdat = data.frame( xn = normX, yn = normY)
      return(normdat)
    })
    # Change the intercept to match the one desired
    linexy =reactive({
      return(lm(xy()$xn~xy()$yn))
    })
   
  
    # Plot the points on the graph
    output$linreg = renderPlot({
      print(ggplot(data = xy(),aes(x =  xy()$xn, y = xy()$yn)) + geom_point() + xlab("Explanatory") + ylab("Response")+
              scale_x_continuous(limits = c(-100, 100))+scale_y_continuous(limits = c(-100, 100)))
      
      
      # Add the line of best fit and display the equation in table form
      if(input$fitPoints == TRUE){
        pointsLine = lm(xy()$xn~xy()$yn)

        print(ggplot(data = xy(),aes(x =  xy()$xn, y = xy()$yn)) + geom_point()+xlab("Explanatory")+ylab("Response")+
                geom_abline(intercept =pointsLine$coefficients[1], slope = pointsLine$coefficients[2])+
                scale_x_continuous(limits = c(-100, 100))+scale_y_continuous(limits = c(-100, 100)))

        # Make a table with the equation information
        eqPoints= reactive({data.frame(
          intercept = pointsLine$coefficients[1],
          slope = pointsLine$coefficients[2] )

        })
        output$eqPointsTable = renderTable({
          eqPoints()
        })
      }
    })

    
    
    # ----------------------------------------- ANOVA Mean ------------------------------------------------- #
    
    # Generate data for 3 different groups based on the input means, standard deviations, and sample sizes
    
    group1Data = eventReactive(input$goAnovaMean, {
      dat1 = rnorm(50, input$anovaMean1,10)
      return(dat1)
    })
    
    group2Data = eventReactive(input$goAnovaMean, {
      dat2 = rnorm(50, input$anovaMean2,10)
      return(dat2)
    })
    
    group3Data = eventReactive(input$goAnovaMean,{
      dat3 = rnorm(50, input$anovaMean3, 10)
      return(dat3)
    })
    
    # Combine all the data for the 3 groups
    g123Data = eventReactive(input$goAnovaMean, {
      alData = c(group1Data(), group2Data(), group3Data())
      return(alData)
    })
    
    # Make a vector of the categories that each data point is in
    groupList = eventReactive(input$goAnovaMean, {
      grpList = c(rep("Group 1", 50), rep("Group 2", 50), rep("Group 3", 50))
      return(grpList)
    })
    
    # Create a data frame all the data and the corresponding groups the data point goes in
    dframe = reactive( {
      newframe = data.frame(g123Data(), groupList())
      return(newframe)
    })
    
    # Dotplot of the threee groups of data colored and faceted by group
    output$anovaPlotMean = renderPlot( {
      ggplot(dframe(), aes(x=groupList(), y=g123Data(), fill = groupList())) + 
        geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.5,binwidth = 1)+
        stat_summary(fun.y=mean, geom="point", shape=5,size=6, color="black")+
        labs(x = "Group", y = "Value")+
        guides(fill=guide_legend(title="Group"))+scale_y_continuous(NULL, breaks = NULL)
     
    })
    
    # Make a linear model based on the data in order to do an ANOVA analysis
    mod=reactive({
      ll = lm(g123Data()~groupList())
      return(ll)
    })
    
    # Get ANOVA test information
    an =reactive({
      model = anova(mod())
      return(model)
    })
    
    # Output an ANOVA table form above
    output$anovaTableMean = renderTable({
      an()
    })
    
    # Give the actual means & standard deviations of each group generated from the input from the user
    anovaSumMean = reactive({data.frame(
      Group = c("Group1", "Group2", "Group3"),
      Means = c(mean(group1Data()), mean(group2Data()), mean(group3Data())),
      StDev = c(sd(group1Data()),sd(group2Data()), sd(group3Data()))
    )
    })
    
    # Output the summary information about the three groups
    output$anovaDatSumMean = renderTable({
      anovaSumMean()
    })
    
  # ------------------------------------ ANOVA SD -------------------------------------#    
    
    
    # Generate data for 3 different groups based on the input means, standard deviations, and sample sizes
    
    group1DataSD = eventReactive(input$goAnovaSD, {
      dat1 = rnorm(50, 20,input$anovaSD1)
      return(dat1)
    })
    
    group2DataSD = eventReactive(input$goAnovaSD, {
      dat2 = rnorm(50, 20,input$anovaSD2)
      return(dat2)
    })
    
    group3DataSD = eventReactive(input$goAnovaSD,{
      dat3 = rnorm(50, 20, input$anovaSD3)
      return(dat3)
    })
    
    # Combine all the data for the 3 groups
    g123DataSD = eventReactive(input$goAnovaSD, {
      alDataSD = c(group1DataSD(), group2DataSD(), group3DataSD())
      return(alDataSD)
    })
    
    # Make a vector of the categories that each data point is in
    groupListSD = eventReactive(input$goAnovaSD, {
      grpList = c(rep("Group 1", 50), rep("Group 2", 50), rep("Group 3", 50))
      return(grpList)
    })
    
    # Create a data frame all the data and the corresponding groups the data point goes in
    dframeSD = reactive( {
      newframe = data.frame(g123DataSD(), groupListSD())
      return(newframe)
    })
    
    # Dotplot of the threee groups of data colored and faceted by group
    output$anovaPlotSD = renderPlot( {
      ggplot(dframeSD(), aes(x=groupListSD(), y=g123DataSD(), fill = groupListSD())) + 
        geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.5, binwidth = 1)+
        stat_summary(fun.y=mean, geom="point", shape=5,size=6, color="black")+
        labs(x = "Group", y = "Value")+
        guides(fill=guide_legend(title="Group"))+scale_y_continuous(NULL, breaks = NULL)
      
    })
    
    # Make a linear model based on the data in order to do an ANOVA analysis
    modSD=reactive({
      ll = lm(g123DataSD()~groupListSD())
      return(ll)
    })
    
    # Get ANOVA test information
    anSD =reactive({
      model = anova(modSD())
      return(model)
    })
    
    # Output an ANOVA table form above
    output$anovaTableSD = renderTable({
      anSD()
    })
    
    # Give the actual means & standard deviations of each group generated from the input from the user
    anovaSumSD = reactive({data.frame(
      Group = c("Group 1", "Group 2", "Group 3"),
      Means = c(mean(group1DataSD()), mean(group2DataSD()), mean(group3DataSD())),
      StDev = c(sd(group1DataSD()),sd(group2DataSD()), sd(group3DataSD()))
    )
    })
    
    # Output the summary information about the three groups
    output$anovaDatSumSD = renderTable({
      anovaSumSD()
    })   
    
    #------------------------------------ ANOVA SS ------------------------------------ #
    
    
    # Generate data for 3 different groups based on the input means, standard deviations, and sample sizes
    
    group1DataSS = eventReactive(input$goAnovaSS, {
      dat1 = rnorm(input$sampSizeANOVA, 20,5)
      return(dat1)
    })
    
    group2DataSS = eventReactive(input$goAnovaSS, {
      dat2 = rnorm(input$sampSizeANOVA, 20,5)
      return(dat2)
    })
    
    group3DataSS = eventReactive(input$goAnovaSS,{
      dat3 = rnorm(input$sampSizeANOVA, 20, 5)
      return(dat3)
    })
    
    # Combine all the data for the 3 groups
    g123DataSS = eventReactive(input$goAnovaSS, {
      alDataSS = c(group1DataSS(), group2DataSS(), group3DataSS())
      return(alDataSS)
    })
    
    # Make a vector of the categories that each data point is in
    groupListSS = eventReactive(input$goAnovaSS, {
      grpList = c(rep("Group 1", input$sampSizeANOVA), rep("Group 2", input$sampSizeANOVA), rep("Group 3", input$sampSizeANOVA))
      return(grpList)
    })
    
    # Create a data frame all the data and the corresponding groups the data point goes in
    dframeSS = reactive( {
      newframe = data.frame(g123DataSS(), groupListSS())
      return(newframe)
    })
    
    # Dotplot of the threee groups of data colored and faceted by group
    output$anovaPlotSS = renderPlot( {
      ggplot(dframeSS(), aes(x=groupListSS(), y=g123DataSS(), fill = groupListSS())) + 
        geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.5, binwidth = 1)+
        stat_summary(fun.y=mean, geom="point", shape=5,size=6, color="black")+
        labs(x = "Group", y = "Value")+
        guides(fill=guide_legend(title="Group"))+scale_y_continuous(NULL, breaks = NULL)
      
    })
      
      
     
    # Make a linear model based on the data in order to do an ANOVA analysis
    modSS=reactive({
      ll = lm(g123DataSS()~groupListSS())
      return(ll)
    })
    
    # Get ANOVA test information
    anSS =reactive({
      model = anova(modSS())
      return(model)
    })
    
    # Output an ANOVA table form above
    output$anovaTableSS = renderTable({
      anSS()
    })
    
    # Give the actual means & standard deviations of each group generated from the input from the user
    anovaSumSS = reactive({data.frame(
      Group = c("Group1", "Group2", "Group3"),
      Means = c(mean(group1DataSS()), mean(group2DataSS()), mean(group3DataSS())),
      StDev = c(sd(group1DataSS()),sd(group2DataSS()), sd(group3DataSS()))
    )
    })
    
    # Output the summary information about the three groups
    output$anovaDatSumSS = renderTable({
      anovaSumSS()
    })      
    
    
    
#Ends everything
    
  }
)