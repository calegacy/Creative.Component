#combine ui and server

# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(markdown)
library(ggplot2)
library(shinydashboard)

library(datasets)
library(shinythemes)

library(shiny)
library(MASS)
library(RColorBrewer)

library(grid)
library(gridExtra)

shinyApp(
ui <- navbarPage(h6("Statistics"),theme = shinytheme("flatly"),
                 #Inference for Proportions tab code
                 
                 navbarMenu(h6("One Proportion"),
                            tabPanel("Sampling Distribution",
                                     column(width = 2,
                                            
                                            numericInput("popProp", "Population Proportion",
                                                         value = 0.3,min = 0, max = 1, step = 0.01),
                                            numericInput("sampleSize", "Sample Size", value = 100, min = 1),
                                            numericInput( "numSamp", "Number of Samples", value = 100, min = 1),
                                            actionButton("goProp", "Draw",class="btn btn-success btn")
                                            
                                     ),
                                     
                                     column(width = 5,
                                            
                                            tableOutput("sampSumDat"),
                                            tableOutput("popSumDat")
                                            
                                     ),
                                     
                                     column(width = 5, 
                                            verbatimTextOutput("sdist"),
                                            plotOutput("sampleDist", width = "auto", height = 200),
                                            verbatimTextOutput("singdist"),
                                            plotOutput("samplingDist", width = "auto", height = 200)
                                     )
                            ),
                            
                            
                            
                            
                            # Confidence Level CI Demo
                            tabPanel("Confidence Interval",
                                     fluidRow(
                                       box(width = 4, title = NULL, status = "primary",
                                           sliderInput("cIDemoCL", "Confidence Level %", value = 80, min = 80, max = 99, step = 5)
                                       ),
                                       box(width = 4, title = NULL, status = "primary",
                                           sliderInput("cIDemoSampSize", "Sample Size", value = 25, min = 25, max = 500, step = 5)
                                       )
                                     ),
                                     fluidRow(
                                       box(width = 10, title = NULL, status = "primary",
                                           plotOutput("sampCLPlot")
                                       )
                                       
                                     )
                            )
                 ),
                 
                 
                 
                 # Inference for one Mean
                 tabPanel( h6("One Mean"),
                           column(width = 2,
                                  
                                  numericInput("popMeanOM", "Population Mean",
                                               value = 10,min = 0, max = 1000, step = 1),
                                  numericInput( "sigma", "Standard Deviation", value = 1, min = 0.01),
                                  numericInput("sampleSizeOM", "Sample Size", value = 10, min = 1),
                                  numericInput( "numSampOM", "Number of Samples", value = 100, min = 1),
                                  actionButton("goMean", "Draw",class="btn btn-success btn")
                                  
                           ),
                           
                           
                           
                           column(width=4,
                                  
                                  tableOutput("sampSumDatOM"),
                                  tableOutput("popSumDatOM")
                           ),
                           column(width = 6,
                                  verbatimTextOutput("OneMeanDist"),
                                  plotOutput("sampleDistOM", width = "auto", height = 200),
                                  verbatimTextOutput("ManyMeansDist"),
                                  plotOutput("samplingDistOM", width = "auto", height = 200)
                                  
                           )
                           
                 ),
                 
                 # Inference for two Proportions
                 tabPanel( h6("Two Proportions"),
                           column(width = 2,
                                  numericInput("popPropG1P", h6("Proportion Group 1"),
                                               value = 0.3, min = 0, max = 1, step = 0.01),
                                  numericInput("popPropG2P", h6("Proportion Group 2"),
                                               value = 0.3, min = 0, max = 1, step = 0.01),
                                  numericInput("sampleSizeGP1", h6("Sample Size Group 1"), value = 100, min = 1),
                                  numericInput("sampleSizeGP2", h6("Sample Size Group 2"), value = 100, min = 1),
                                  numericInput( "numSampGP",h6("Number of Samples"), value = 100, min = 1),
                                  actionButton("go2Prop", "Draw",class="btn btn-success btn")
                           ),
                           
                           column(width = 4,
                                  
                                  tableOutput("sampSumDatP2"),
                                  tableOutput("popSumDat2P")
                           ),
                           
                           
                           column( width = 6,
                                   verbatimTextOutput("twoPropSampleDist"),
                                   fluidRow(
                                     plotOutput("sampleDistG1", width = 'auto', height = 150),
                                     plotOutput("sampleDistG2", width = 'auto', height = 150)
                                   ),
                                   verbatimTextOutput("twoPropSampDist"),
                                   plotOutput("samplingDistTP", width = 'auto', height = 200)
                                   
                           )
                           
                           
                           
                 ),
                 
                 # Inference for two Means
                 tabPanel( h6("Two Means"),
                           column(width = 2,
                                  numericInput("popMeanM1", h6("Group 1"),
                                               value = 5,min = 0, max = 1, step = 0.1),
                                  numericInput("popMeanM2", h6("Group 2"),
                                               value = 5,min = 0, max = 1, step = 0.1),
                                  numericInput( "sigmaTM1", h6("Group 1 St Dev"), value = 1, min = 0.01),
                                  numericInput( "sigmaTM2", h6("Group 2 St Dev"), value = 1, min = 0.01),
                                  numericInput("sampleSizeTM1", h6("Group 1 Size"), value = 10, min = 1),
                                  numericInput("sampleSizeTM2", h6("Group 2 Size"), value = 10, min = 1),
                                  numericInput( "numSampTM",h6( "Number of Samples"), value = 100, min = 1),
                                  actionButton("go2Mean", "Draw",class="btn btn-success btn")
                                  
                           ),
                           column(width = 4,
                                  tableOutput("sampSumDatTM"),
                                  tableOutput("popSumDatTM")
                                  
                           ),
                           column(width = 6,
                                  verbatimTextOutput("twoMeansSampleDist"),
                                  plotOutput("sampleDistM1", width = '100%', height = 150),
                                  plotOutput("sampleDistM2", width = '100%', height = 150),
                                  verbatimTextOutput("twoMeansSampDist"),
                                  plotOutput("samplingDistTM", width = '100%', height = 200)
                                  
                           )
                           
                           
                 ),
                 
                 
                 # Correlation
                 navbarMenu(h6("Regression"),
                            tabPanel( "Correlation",
                                      column(width = 12,
                                             box(width = 12,
                                                 title = NULL,  status= "primary",
                                                 sliderInput("correlation", "Correlation", value = 0.1, min = -1, max =1, step = 0.01)
                                                 
                                             ),
                                             box(
                                               width =12, status = "primary",
                                               
                                               plotOutput("corrPlot")
                                               
                                             ))
                                      
                            ),
                            
                            # Outliers
                            tabPanel("Outliers",
                                     
                                     column(
                                       width = 4, status= "primary",
                                       
                                       
                                       selectInput("dataset", "Choose Data Set:", choices = c("Dataset 1","Dataset 2", "Dataset 3", "Dataset 4")),
                                       checkboxInput("fitLine", label = "Fit Line", value = FALSE),
                                       checkboxInput("fitLineNoPt", label = "Fit Line Without Point", value = FALSE),
                                       verbatimTextOutput("lineSum"),
                                       tableOutput("lineEq"),
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
                            
                            
                            #NEW REGRESSION TAB
                            tabPanel("Equation", value = "eqBd",
                                     column(width = 5,
                                            actionButton("plotPoints", "Plot",class="btn btn-success btn"),
                                            sliderInput("intercept", "Intercept", value = 1, min = -25, max =25),
                                            sliderInput("slope", "Slope", value = 1, max =25, min = -25),
                                            checkboxInput("fitPoints", "Fit Line",value = FALSE),
                                            tableOutput("eqPointsTable"),
                                            checkboxInput("interceptPoints", "Intercept",value = FALSE),
                                            verbatimTextOutput("intercept"),
                                            checkboxInput("slopePoints", "Slope",value = FALSE),
                                            verbatimTextOutput("slope")
                                     ),
                                     column(
                                       width = 7, status = "primary",
                                       box(
                                         width = "100%", height = "100%",
                                         title = "Linear Regression",  status = "primary",
                                         plotOutput("linreg" )
                                       )
                                       
                                     )
                            )      
                 ),
                 
                 
                 #ANOVA
                 tabPanel(h6("ANOVA"),
                          
                          
                          fluidRow(
                            
                            
                            column(width = 4,
                                   
                                   sliderInput("anovaMean1", h6("Mean 1"), value = 30, min = 10, max =50, step = 1), 
                                   sliderInput("anovaSD1", h6("St Dev 1"), value = 1, min = 1, max =50, step = 1),
                                   sliderInput("sampSizeANOVA", h6("Sample Size"), value = 50, min = 10, max =100, step = 5)     
                            ),
                            column(width = 4,
                                   sliderInput("anovaMean2", h6("Mean 2"), value = 30, min = 10, max =50, step = 1), 
                                   sliderInput("anovaSD2", h6("St Dev 2"), value = 1, min = 1, max =50, step = 1),
                                   actionButton("goAnova", "Draw",class="btn btn-success btn")
                            ),
                            column(width = 4, 
                                   sliderInput("anovaMean3", h6("Mean 3"), value = 30, min = 10, max =50, step = 1),
                                   sliderInput("anovaSD3", h6("St Dev 3"), value = 1, min = 1, max =50, step = 1)
                            )
                          ),
                          fluidRow(
                            column(width = 8,
                                   plotOutput("anovaPlot", width = "auto", height = 500)
                            ),
                            column( width = 3,
                                    tableOutput("anovaDatSum"),
                                    tableOutput("anovaTable")
                            )
                          )
                          
                          
                 )),









server = function(input, output, session) {
  
  # -------------------------------------------- Inference for One Proportion Sampling Dist. ----------------------------#  
  # Create a single sample distribution 
  # Get a sample from a binomial distribution
  pickVect = eventReactive(input$goProp,{
    pickf = rbinom(input$sampleSize,1, input$popProp)
    return(pickf)
  })
  
  # Count the number of "successes" and put it over the sample size to make p_hat
  pickProp = eventReactive(input$goProp,{
    pickprop = length(which(pickVect() == 1))/input$sampleSize
    return(pickprop)
  })
  
  
  #Sample Distribution bar graph
  output$sampleDist = renderPlot({
    qplot(as.character(pickVect()), xlab = "Category", ylab = "Count")
  })
  
  #Sample summary information to be put into a table
  samplesum = eventReactive(input$goProp,{data.frame(
    Category = c( "Yes = 1", "No = 0"),
    Count =c( length(which(pickVect() == 1)), length(which(pickVect() == 0))),
    Prop = c(format(round(length(which(pickVect() == 1))/input$sampleSize,digits = 4),4), format(round(length(which(pickVect() == 0))/input$sampleSize,4),4))
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
      return(pickProp())
    }
    # If many samples are needed, generate a vector of p_hats by the above method
    if(input$numSamp > 1){
      v = c(pickProp())
      for(i in 2:input$numSamp){
        # Generate a sample
        npick = reactive({
          newpick = rbinom(input$sampleSize, 1,input$popProp)
          newprop = length(which(newpick == 1))/input$sampleSize
          return(newprop)
          
        })
        # Store the generated sample in a vector
        v = c(v,npick())
        
      }}
    # Return to 
    return(v)
  })
  
  # Histogram of the sampling distribution
  output$samplingDist =renderPlot({
    ggplot(data = data.frame(samples()),aes(samples()))+ geom_histogram(binwidth = 0.01) +xlab("Sample Proportion") +ylab("Number of Samples")
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
  output$sdist <- renderText({ "Sample Distribution" })
  output$singdist <- renderText({ "Sampling Distribution" })
  #--------------------------------------------- One proportion CIs ---------------------------------------------------- #
  
  # Generate 20 sample p_hats to create confidence intervals around
  twSamps = eventReactive(input$cIDemoSampSize,{
    tsampCL = NULL
    for(i in 1:20){
      tsCL = rbinom(input$cIDemoSampSize, 1, 0.5)
      # Get the proportion of "successes" for the generated sample
      countPropCL = length(which(tsCL == 1 ))/ input$cIDemoSampSize
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
    if(input$cIDemoCL  == 80){
      zlev = 1.282
    }
    if(input$cIDemoCL == 85){
      zlev = 1.44
    }
    if(input$cIDemoCL == 90){
      zlev = 1.645
    }
    if(input$cIDemoCL == 95){
      zlev = 1.96
    }
    if(input$cIDemoCL == 99){
      zlev = 2.576
    }
    return(zlev)
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
    for(i in 1:20){
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
  
  # Output a histogram of the sampling distribution of the sample means
  output$samplingDistOM =renderPlot({
    ggplot(data = data.frame(samplesOM()),aes(x = samplesOM())) + geom_histogram()+xlab("Sample Means") +ylab("Number of Samples")
    
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
  output$OneMeanDist <- renderText({ "Sample Distribution" })
  output$ManyMeansDist <- renderText({ "Sampling Distribution" })
  
  #--------------------------------------------- Inference in Two Proportions ---------------------------------------#
  
  # Calculations to get a sample distribution for group 1
  pickP1Vect = eventReactive(input$go2Prop,{
    pickfp1 = rbinom(input$sampleSizeGP1,1, input$popPropG1P)
    return(pickfp1)
  })
  # Get count of "successes" in the sample for group 1
  npickP1Prop = eventReactive(input$go2Prop,{
    npickp1f = length(which(pickP1Vect() == 1))/input$sampleSizeGP1
    return(npickp1f)
  })
  
  # Sample Distribution output for group 1 sample
  output$sampleDistG1 = renderPlot({
    qplot(as.factor(pickP1Vect()),xlab = "Category", ylab = "Count") + labs(title = "Group 1")
  })
  
  ####  Group 2 Calculations ####
  
  # Generate a binomial sample for group 2
  pickP2Vect = eventReactive(input$go2Prop,{
    pickfp2 = rbinom(input$sampleSizeGP2,1, input$popPropG2P)
    return(pickfp2)
  })
  
  npickP2Prop = eventReactive(input$go2Prop,{
    npickp2f = length(which(pickP2Vect() ==1))/input$sampleSizeGP2
    return(npickp2f)
  })
  
  #Sample Distribution output plot for group 2
  output$sampleDistG2 = renderPlot({
    qplot(as.factor(pickP2Vect()), xlab = "Category", ylab = "Count") + labs(title = "Group 2")
  })
  
  ####
  #Sample summary information for two proportions to be displyed in a table
  samplesumP2 = eventReactive(input$go2Prop,{data.frame(
    Prop1 = npickP1Prop(),
    Prop2 = npickP2Prop(),
    Diff =npickP1Prop()-npickP2Prop()
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
      return(npickP1Prop()-npickP2Prop())
    }
    # If many samples are needed, draw many samples for two groups
    if(input$numSampGP > 1){
      vp2 = c(npickP1Prop()-npickP2Prop())
      for(i in 2:input$numSampGP){
        
        npickdiffp = reactive({
          newpickp1 = rbinom(input$sampleSizeGP1,1, input$popPropG1P)
          npSamp1 = length(which(newpickp1 == 1))/input$sampleSizeGP1
          newpickp2 = rbinom(input$sampleSizeGP2,1, input$popPropG1P)
          npSamp2 = length(which(newpickp2 == 1))/input$sampleSizeGP2
          # Calculate the difference in the two samples
          newpropdiff  = npSamp1 - npSamp2
          # Return the difference of the two samples
          return(newpropdiff)
        })
        # Store differences in a vector
        vp2 = c(vp2,npickdiffp())
        
      }}
    # Return the vector of differences
    return(vp2)
  })
  
  # Output the plot for the sampling distribution for the difference in proportions
  output$samplingDistTP =renderPlot({
    qplot(samplesP2(), xlab = "Difference in Proportions", ylab = "Number of Samples", binwidth = 0.01)
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
  output$twoPropSampleDist <- renderText({ "Sample Distributions" }) 
  
  output$twoPropSampDist <- renderText({ "Sampling Distribution" }) 
  
  # --------------------------------------------- Two Means Code -----------------------------------------------------#
  
  # Generate a sample for group one 
  pickM1 = eventReactive(input$go2Mean,{
    pickfM1 = c(rnorm(input$sampleSizeTM1, input$popMeanM1, input$sigmaTM1))
    return(pickfM1)
  })
  
  #Sample Distribution plot for group one
  output$sampleDistM1 = renderPlot({
    qplot(pickM1(), xlab = "Value", ylab = "Count")+ labs( title = "Group 1")
  })
  
  # Generate a sample for group two 
  pickM2 = eventReactive(input$go2Mean,{
    pickfM2 = c(rnorm(input$sampleSizeTM2, input$popMeanM2, input$sigmaTM2))
    return(pickfM2)
  })
  
  
  #Sample Distribution plot for group two
  output$sampleDistM2 = renderPlot({
    qplot(pickM2(), xlab = "Value", ylab = "Count") + labs(title = "Group 2")
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
          newpick1 = c(rnorm(input$sampleSizeTM1, input$popMeanM1, input$sigmaTM1))
          meanSample1 = mean(newpick1)
          newpick2 = c(rnorm(input$sampleSizeTM2, input$popMeanM2, input$sigmaTM2))
          meanSample2 = mean(newpick2)
          diffmean = meanSample1 - meanSample2
          return(diffmean)
        })
        # Hold all the differences in means in a vector
        vm = c(vm,nmpick())
      }
    }
    # Return the differences in means vector to be plot as the sampling distribution
    return(vm)
  })
  
  # Plot the sampling distribution for the differences in means
  output$samplingDistTM =renderPlot({
    qplot(samplesTM(), xlab = "Value", ylab = "Count") 
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
  output$twoMeansHyp <- renderText({ "Alternative Hypothesis" })
  output$twoMeansSampDist <- renderText({ "Sampling Distribution" })
  output$twoMeansSampleDist <- renderText({ "Sampling Distribution" })
  # --------------------------------------------- Linear Regression Code ----------------------------------------------#
  #Correlation Tab
  
  # Make the dataset with a set correlation for the scatterplot
  xycorr =reactive({
    datxy = as.data.frame(mvrnorm(100, mu = c(0,0), Sigma = matrix(c(1,input$correlation,input$correlation,1),, ncol = 2),empirical = TRUE))
    return(datxy)
  })
  
  #Output the scatterplot of the data created above
  output$corrPlot = renderPlot({
    qplot(xycorr()$V1,xycorr()$V2, xlab = "x", ylab = "y")
  })
  
  # Outliers tab
  # Select the proper dataset to analyze based on the user input
  getData = reactive({
    if(input$dataset == "Dataset 1"){
      dataX = c(seq(1,20, 1), 30)
      dataY =  c(0.05, 0.40, 0.94, 1.69, 1.83, 3.06, 3.86, 4.14, 5.63, 7.69, 9.65, 10.16,
                 11.72, 12.69, 13.05, 14.38, 16.06, 17.75, 18.52, 19.55, 30)
    }
    if(input$dataset == "Dataset 2"){
      dataX = c(seq(1,20, 1), 20)
      dataY =  c(0.05, 0.40, 0.94, 1.69, 1.83, 3.06, 3.86, 4.14, 5.63, 7.69, 9.65, 10.16,
                 11.72, 12.69, 13.05, 14.38, 16.06, 17.75, 18.52, 19.55, 50)
    }
    if(input$dataset == "Dataset 3"){
      dataX = c(seq(1,20, 1), 20)
      dataY =  c(0.05, 0.40, 0.94, 1.69, 1.83, 3.06, 3.86, 4.14, 5.63, 7.69, 9.65, 10.16,
                 11.72, 12.69, 13.05, 14.38, 16.06, 17.75, 18.52, 19.55, 0)
    }
    if(input$dataset == "Dataset 4"){
      dataX = c(seq(1,20, 1), 7)
      dataY =  c(0.05, 0.40, 0.94, 1.69, 1.83, 3.06, 3.86, 4.14, 5.63, 7.69, 9.65, 10.16,
                 11.72, 12.69, 13.05, 14.38, 16.06, 17.75, 18.52, 19.55, 10)
    }
    dataXY = data.frame(dataX,dataY)
    return(dataXY)
    
  })
  
  # Print the plot for the high leverage data
  output$outlierPlot = renderPlot({
    # Print the plot with all the data
    print(ggplot(data = getData(), aes(x = dataX, dataY)) + geom_point() +
            geom_point(aes(x = dataX[21], y = dataY[21]), color= "firebrick2", size = 2))
    # Print the plot with the line fit to the data
    if(input$fitLine == TRUE){
      # Make a linear model if the dataset
      hL1 = lm(getData()$dataY~getData()$dataX)
      # Print the plot with the fitted line
      
      print(ggplot(data = getData(), aes(x = dataX, dataY)) + geom_point() +
              geom_point(aes(x = dataX[21], y = dataY[21]), colour= "firebrick2", size = 2) +
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
    # If the Fit with no red point box is selected
    if(input$fitLineNoPt == TRUE ){
      # Calculate the line of best fit without the point of interest
      hL1NoPt = lm(getData()$dataY[-21]~getData()$dataX[-21])
      # Print the plot with the line of fit for the data without the point of interest
      
      print(ggplot(data = getData(), aes(x = dataX, dataY)) + geom_point() +
              geom_point(aes(x = dataX[21], y = dataY[21]), colour= "firebrick2", size = 2) +
              geom_abline(slope = hL1NoPt$coefficients[2],intercept = hL1NoPt$coefficients[1], linetype = "dashed", colour="springgreen2", size = 1))
      
      # Create a table of information about the line fit to the data without the point of interest
      hL1LineNoPt = reactive({data.frame(
        intercept = hL1NoPt$coefficients[1],
        slope = hL1NoPt$coefficients[2],
        Rsquared = summary(hL1NoPt)$r.squared
      )
      })
      # Output this table to the user
      output$lineEqNoPt  = renderTable({
        hL1LineNoPt()
      })
      # If the "Fit Line" is also selected, then the plot will display both lines of best fit
      if(input$fitLine == TRUE){
        
        print(ggplot(data = getData(), aes(x = dataX, dataY)) + geom_point() +
                geom_point(aes(x = dataX[21], y = dataY[21]), colour= "firebrick2", size = 2) +
                geom_abline(slope = hL1$coefficients[2],intercept = hL1$coefficients[1], colour = "navyblue", linetype ="F1", size = 0.75) +
                geom_abline(slope = hL1NoPt$coefficients[2],intercept = hL1NoPt$coefficients[1], linetype = "dashed", colour="springgreen2", size = 1)+
                scale_colour_manual(name="Legend",values=c("fitted" = "navyblue", "noPoint" = "springgreen2"))+
                scale_linetype_manual(name = "Legend", values = c("fitted" = "F1", "noPoint"= "dashed"))+
                theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
                guides(fill=guide_legend(title="Legend")))
        
      }
    }
  })
  
  # Add text to label the tables for the line with the point of interest and without it
  output$lineSum <- renderText({ "Line Summary" })
  output$lineSumNoPt <- renderText({ "Line Summary Without Point" })
  
  
  # Regression Line 
  
  
  # Create a data frame with the desired slope
  xy = reactive({
    datXY = as.data.frame(mvrnorm(100, mu = c(0,0), Sigma = matrix(c(1,input$slope,input$slope,1000),, ncol = 2),empirical = TRUE))
    return(datXY)
  })
  # Change the intercept to match the one desired
  linexy =reactive({
    return(lm(xy()$V2~xy()$V1))
    
  })
  # Adjust the interecept to match the desired intercept amount
  intdelt = reactive({
    rintdelt= linexy()$coefficients[1] - input$intercept
    return(rintdelt)
  })
  
  # Plot the points on the graph
  output$linreg = renderPlot({
    print(ggplot(data = xy(),aes(x = V1, y = V2-intdelt()))+ geom_point()+xlab("Explanatory")+ylab("Response"))
    
    
    # Add the line of best fit and display the equation in table form
    if(input$fitPoints == TRUE){
      pointsLine = lm(xy()$V2-intdelt()~xy()$V1)
      
      print(ggplot(data = xy(),aes(x = V1, y = V2-intdelt()))+ geom_point()+xlab("Explanatory")+ylab("Response")+
              geom_abline(intercept =pointsLine$coefficients[1], slope = pointsLine$coefficients[2]))
      
      # Make a table with the equation information
      eqPoints= reactive({data.frame(
        intercept = pointsLine$coefficients[1],
        slope = pointsLine$coefficients[2] )
        
      })
      output$eqPointsTable = renderTable({
        eqPoints()
      })
    }
    
    
    
    
    # Add an "X" for the intercept and display the intercept interpretation if the intercept box is selected
    if(input$interceptPoints == TRUE){
      pointsLine = lm(xy()$V2-intdelt()~xy()$V1)
      
      print(ggplot(data = xy(),aes(x = V1, y = V2-intdelt()))+ geom_point()+xlab("Explantatory")+ylab("Response")+
              geom_abline(intercept =pointsLine$coefficients[1], slope = pointsLine$coefficients[2])+
              geom_point(aes(x = 0, y = pointsLine$coefficients[1], color = "intercept"), shape = 8))
      
      
      # Display the intercept interpretation
      intText = reactive({
        int = as.character(round(pointsLine$coefficients[1], 2))
        text = paste("If the explantory variable is zero, the predicted response value is ", int, ".")
        return(text)
      })
      output$intercept <- renderText({ intText() })
    }
    
    
    # If the slope box is selected add colored lines to the plot to help explain the slope
    if(input$slopePoints == TRUE){
      pointsLine = lm(xy()$V2-intdelt()~xy()$V1)
      
      print(ggplot(data = xy(),aes(x = V1, y = V2-intdelt()))+ geom_point()+xlab("Explanatory")+ylab("Response")+
              geom_abline(intercept =pointsLine$coefficients[1], slope = pointsLine$coefficients[2])+
              geom_point(aes(x = 0, y = pointsLine$coefficients[1], color = "intercept"), shape = 8)+
              geom_segment(aes(x = 1, xend = 2, y = (pointsLine$coefficients[1]+ pointsLine$coefficients[2]), yend = (pointsLine$coefficients[1]+ pointsLine$coefficients[2]), color = "one unit"))+
              geom_segment(aes(x = 2, xend = 2, y = (pointsLine$coefficients[1]+ pointsLine$coefficients[2]), yend =(pointsLine$coefficients[1]+ 2*pointsLine$coefficients[2]) , color = "slope amount")))
      
      # Display the text of the slope interpretation
      slopeText = reactive({
        # Find the sign of the slope in order to determine whether to use "increase" or "decrease"
        if(sign(pointsLine$coefficients[2]) == -1){
          signSlope = "decrease"
        }
        if(sign(pointsLine$coefficients[2]) == 1){
          signSlope = "increase"
        }
        # Get the absolute value of the slope amount
        sl = as.character(abs(round(pointsLine$coefficients[2], 2)))
        
        slText = paste("If the explantaory variable inceases by one unit, we expect the predicted response variable to", signSlope,"by",sl, "units")
        return(slText)
      })
      output$slope <- renderText({slopeText()})
    }
  })
  
  
  
  # ----------------------------------------- ANOVA -------------------- #
  
  # Generate data for 3 different groups based on the input means, standard deviations, and sample sizes
  
  group1Data = eventReactive(input$goAnova, {
    dat1 = rnorm(input$sampSizeANOVA, input$anovaMean1, input$anovaSD1)
    return(dat1)
  })
  
  group2Data = eventReactive(input$goAnova, {
    dat2 = rnorm(input$sampSizeANOVA, input$anovaMean2, input$anovaSD2)
    return(dat2)
  })
  
  group3Data = eventReactive(input$goAnova,{
    dat3 = rnorm(input$sampSizeANOVA, input$anovaMean3, input$anovaSD3)
    return(dat3)
  })
  
  # Combine all the data for the 3 groups
  g123Data = eventReactive(input$goAnova, {
    alData = c(group1Data(), group2Data(), group3Data())
    return(alData)
  })
  
  # Make a vector of the categories that each data point is in 
  groupList = eventReactive(input$goAnova, {
    grpList = c(rep("group1", input$sampSizeANOVA), rep("group2", input$sampSizeANOVA), rep("group3", input$sampSizeANOVA))
    return(grpList)
  }) 
  
  # Create a data frame all the data and the corresponding groups the data point goes in
  dframe = reactive( {
    newframe = data.frame(g123Data(), groupList())
    return(newframe)
  })
  
  # Dotplot of the threee groups of data colored and faceted by group
  output$anovaPlot = renderPlot( {
    qplot(data = dframe(), x =g123Data(), geom = "dotplot", fill = groupList(), xlab = "x")+facet_grid(facets = groupList()~., scales = "free")+
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
  output$anovaTable = renderTable({
    an()
  })
  
  # Give the actual means & standard deviations of each group generated from the input from the user
  anovaSum = reactive({data.frame(
    Group = c("Group1", "Group2", "Group3"),
    Means = c(mean(group1Data()), mean(group2Data()), mean(group3Data())),
    StDev = c(sd(group1Data()),sd(group2Data()), sd(group3Data()))
  )
  })
  
  # Output the summary information about the three groups
  output$anovaDatSum = renderTable({
    anovaSum()
  })
  
  
  # Must be within this bracket
}

)