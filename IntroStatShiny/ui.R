#
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

ui <- navbarPage(h6("Stats"),theme = shinytheme("flatly"),
                 #Inference for Proportions tab code
                 
                 navbarMenu(h6("One Proportion"),
                            tabPanel("Sampling Distribution",
                                     column(width = 2,
                                       
                                         numericInput("popProp", "Population Proportion",
                                                      value = 0.3,min = 0, max = 1, step = 0.01),
                                         numericInput("sampleSize", "Sample Size", value = 100, min = 1),
                                         numericInput( "numSamp", "Number of Samples", value = 10, min = 1),
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
                                       ),
                                      
                                       box(width = 3, title = NULL, status = "primary",
                                           tableOutput("Clinfo"))
                                       
                                   
                                       
                                       
                                       
                                     ),
                                     fluidRow(
                                       box(width = 12, title = NULL, status = "primary",
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
                               numericInput( "numSampOM", "Number of Samples", value = 1, min = 1),
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
                               numericInput( "numSampGP",h6("Number of Samples"), value = 10, min = 1),
                               actionButton("go2Prop", "Draw",class="btn btn-success btn")
                           ),
                           
                           column(width = 3,
                                  verbatimTextOutput("twoPropSampleDist"),
                                 plotOutput("sampleDistG1", width = 200, height = 200),
                                 plotOutput("sampleDistG2", width = 200, height = 200),
                                 tableOutput("sampSumDatP2")
                             ),
                             
                      
                             column( width = 7,
                                     withMathJax(),
                                     verbatimTextOutput("twoPropHyp"),
                                  radioButtons("altHypR", "", choices = c( "<","!=", ">"), inline = TRUE,
                                               width = '400px'),
                                  verbatimTextOutput("twoPropSampDist"),
                                  plotOutput("samplingDistTP", width = '100%', height = 200),
                                  tableOutput("popSumDat2P")
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
                               numericInput( "numSampTM",h6( "Number of Samples"), value = 1, min = 1),
                               actionButton("go2Mean", "Draw",class="btn btn-success btn")
                      
                          ),
                           column(width = 3,
                                  verbatimTextOutput("twoMeansSampleDist"),
                               plotOutput("sampleDistM1", width = '100%', height = 200),
                               plotOutput("sampleDistM2", width = '100%', height = 200),
                               tableOutput("sampSumDatTM")
                             
                           ),
                           column(width = 6,
                                  verbatimTextOutput("twoMeansHyp"),
                                 radioButtons("altHypRMeans", "", choices = c("<", "!=", ">"), selected ="<", inline = TRUE,
                                              width = '400px'),
                                 verbatimTextOutput("twoMeansSampDist"),
                                 plotOutput("samplingDistTM", width = '100%', height = 200),
                                 tableOutput("popSumDatTM")
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
                            
                            # Regression
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
                      
                            
                 ))

