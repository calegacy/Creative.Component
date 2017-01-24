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
                                     fluidRow(
                                       box(
                                         width = 4, status= "primary", 
                                         numericInput("popProp", "Population Proportion",
                                                      value = 0.3,min = 0, max = 1, step = 0.01)
                                       ),
                                       
                                       box(
                                         width = 3, status= "primary", 
                                         numericInput("sampleSize", "Sample Size", value = 100, min = 1)
                                       ),
                                       
                                       box(
                                         width = 3, status= "primary",
                                         numericInput( "numSamp", "Number of Samples", value = 10, min = 1)
                                       ),
                                       
                                       actionButton("goProp", "Draw",class="btn btn-success btn")
                                       
                                     ),
                                     
                                     fluidRow(
                                       box(
                                         width = 4, 
                                         title = h4("Sample Summary"),  status= "primary", 
                                         tableOutput("sampSumDat")
                                       ),
                                       box(
                                         width = 6, 
                                         title = h4("Sample Distribution"),  status = "primary",
                                         plotOutput("sampleDist", width = 300, height = 200)
                                       )
                                     ),
                                     
                                     fluidRow(
                                       box(width = 4,
                                           title = h4("Population Summary"),  status= "primary", background = "light-blue",
                                           tableOutput("popSumDat")
                                       ),
                                       box( width = 6,  
                                            title = h4("Sampling Distribution"), status = "primary",
                                            plotOutput("samplingDist", width = 300, height = 200)
                                       )
                                     )
                                     
                                     
                            ),
                            
                            
                            
                            
                            # One Proportion CI
                            tabPanel( "Confidence Interval",
                                      
                                      fluidRow(
                                        box(
                                          width = 3, status= "primary",
                                          numericInput("popPropCI", "Population Proportion",
                                                       value = 0.3,min = 0, max = 1, step = 0.01)
                                        ),
                                        
                                        box(
                                          width = 3, status= "primary",
                                          numericInput("sampleSizeCI", "Sample Size", value = 100, min = 1)
                                        ),
                                        
                                        box(
                                          width = 3, status= "primary",
                                          numericInput( "numSampCI", "Number of Samples", value = 10, min = 1)
                                        ),
                                        box( width = 2, status = "primary",
                                             radioButtons("CLProp", "Confidence level", choices = c("90%", "95%", "99%"), selected ="90%", inline = FALSE,
                                                          width = NULL)
                                        ),
                                        
                                        actionButton("goCI", "Draw", class="btn btn-success btn")
                                        
                                        
                                      ),
                                      
                                      fluidRow(
                                        box(
                                          width = 5,
                                          title = h4("Confidence Interval Summary"),  status= "primary", background = "light-blue",
                                          tableOutput("sPropSumDat")
                                        ),
                                        box(
                                          width = 6,
                                          title =  h4("Sample Confidence Interval"),  status = "primary",
                                          plotOutput("samplePropCI", width = 300, height = 200)
                                        )
                                      ),
                                      
                                      fluidRow(
                                        box(width = 5,
                                            title =  h4("Population Summary"),  status= "primary", background = "light-blue",
                                            tableOutput("sPropPopDat")
                                        ),
                                        box( width = 6,
                                             title =  h4("Sampling Distribution"), status = "primary",
                                             plotOutput("sPropPopCI", width = 300, height = 200)
                                        )
                                      )
                            ),
                            
                            
                            
                            
                            # Sample size CI Demo
                            tabPanel( "Sample Size",
                                      fluidRow(
                                        box(width =4, title = NULL, status = "primary",
                                            sliderInput("sampdemo", "Sample Size", value = 10, min = 20, max = 1000, step = 5)
                                        ),
                                        box(width = 6, title = NULL, status = "primary",
                                            tableOutput("CIinfo")
                                            
                                        ),
                                        
                                        actionButton("goSS", "Draw",class="btn btn-success btn")
                                        
                                      ),
                                      fluidRow(
                                        box(width = 12, title = NULL, status = "primary",
                                            plotOutput("sampDemoPlot")
                                        )
                                        
                                      )
                                      
                            ),
                            
                            # Confidence Level CI Demo
                            tabPanel("Confidence Level",
                                     fluidRow(
                                       box(width = 4, title = NULL, status = "primary",
                                           sliderInput("cldemo", "Confidence Level %", value = 80, min = 80, max = 99, step = 5)
                                       ),
                                       box(width = 6, title = NULL, status = "primary",
                                           tableOutput("Clinfo")),
                                       
                                       actionButton("goCL", "Draw",class="btn btn-success btn")
                                       
                                       
                                       
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
                           fluidRow(
                             box(
                               width = 3, status= "primary",
                               numericInput("popMeanOM", "Population Mean",
                                            value = 10,min = 0, max = 1000, step = 1)
                             ),
                             box(
                               width = 2, status= "primary",
                               numericInput( "sigma", "St. Dev.", value = 1, min = 0.01)
                             ),
                             
                             box(
                               width = 2, status= "primary",
                               numericInput("sampleSizeOM", "Samp Size", value = 10, min = 1)
                             ),
                             
                             box(
                               width = 3, status= "primary",
                               numericInput( "numSampOM", "Number of Samples", value = 1, min = 1)
                             ),
                             
                             actionButton("goMean", "Draw",class="btn btn-success btn")
                             
                           ),
                           
                           
                           
                           fluidRow(
                             box(
                               width = 4,
                               title =  h4("Sample Summary"),  status= "primary", background = "light-blue",
                               tableOutput("sampSumDatOM")
                             ),
                             box(
                               width = 6,
                               title =  h4("Sample Distribution"),  status = "primary",
                               plotOutput("sampleDistOM", width = 300, height = 200)
                             )
                           ),
                           
                           fluidRow(
                             box(width = 4,
                                 title =  h4("Population Summary"),  status= "primary", background = "light-blue",
                                 tableOutput("popSumDatOM")
                             ),
                             box( width = 6,
                                  title =  h4("Sampling Distribution"), status = "primary",
                                  plotOutput("samplingDistOM", width = 300, height = 200)
                             )
                           )
                           
                           
                 ),
                 
                 # Inference for two Proportions
                 tabPanel( h6("Two Proportions"),
                           fluidRow(
                             box(
                               width = 3, status= "primary", title=  h4("Population Proportion"),
                               numericInput("popPropG1P", "Group 1",
                                            value = 0.3,min = 0, max = 1, step = 0.01),
                               numericInput("popPropG2P", "Group 2",
                                            value = 0.3,min = 0, max = 1, step = 0.01)
                             ),
                             
                             box(
                               width = 3, status= "primary", title =  h4("Sample Size"),
                               numericInput("sampleSizeGP1", "Group 1", value = 100, min = 1),
                               numericInput("sampleSizeGP2", "Group 2", value = 100, min = 1)
                             ),
                             box(
                               width = 3, status= "primary", title =  h4("Number of Samples"),
                               numericInput( "numSampGP",label = NULL, value = 10, min = 1)
                             ),
                             actionButton("go2Prop", "Draw",class="btn btn-success btn")
                           ),
                           fluidRow(
                             box(
                               width = 4,
                               title =  h4("Sample Summary"),  status= "primary", background = "light-blue",
                               tableOutput("sampSumDatP2")
                               #  )
                             ),
                             #fluidRow(
                             box(
                               width = 4,
                               title =  h4("Sample Distribution 1"),  status = "primary",
                               plotOutput("sampleDistG1", width = 200, height = 200)
                             ),
                             box(
                               width = 4,
                               title =  h4("Sample Distribution 2"), status = "primary",
                               plotOutput("sampleDistG2", width = 200, height = 200)
                               
                             )
                           ),
                           
                           fluidRow(
                             box(width = 6,
                                 title = h4("Population Summary"),  status= "primary", background = "light-blue",
                                 tableOutput("popSumDat2P")
                             ),
                             #),
                             #fluidRow(
                             box( width = 4,
                                  title =  h4("Alternative Hypothesis"), status = "primary",
                                  
                                  radioButtons("altHypR", "", choices = c("<", "=", ">"), selected ="<", inline = TRUE,
                                               width = '400px')
                             )#,
                           ),
                           fluidRow(
                             
                             box( width = 11,
                                  title =  h4("Sampling Distribution"), status = "primary",
                                  plotOutput("samplingDistTP", width = '100%', height = 200)
                             )
                           )
                           
                           
                 ),
                 
                 # Inference for two Means
                 tabPanel( h6("Two Means"),
                           fluidRow(
                             box(
                               width = 2, status= "primary", title =  h4("Population Mean"),
                               numericInput("popMeanM1", "Group 1",
                                            value = 5,min = 0, max = 1, step = 0.1),
                               numericInput("popMeanM2", "Group 2",
                                            value = 5,min = 0, max = 1, step = 0.1)
                               
                             ),
                             
                             box(
                               width = 3, status= "primary",title =  h4("Standard Deviation"),
                               numericInput( "sigmaTM1", "Group 1", value = 1, min = 0.01),
                               numericInput( "sigmaTM2", "Group 2", value = 1, min = 0.01)
                             ),
                             
                             box(
                               width = 3, status= "primary", title =  h4("Sample Size"),
                               numericInput("sampleSizeTM1", "Group 1", value = 10, min = 1),
                               numericInput("sampleSizeTM2", "Group 2", value = 10, min = 1)
                             ),
                             
                             box(
                               width = 3, status= "primary", title =  h4("Number of Samples"),
                               numericInput( "numSampTM", label = NULL, value = 1, min = 1)
                             ),
                             actionButton("go2Mean", "Draw",class="btn btn-success btn")
                           ),
                           
                           
                           fluidRow(
                             
                             box(
                               width = 6,
                               title =  h4("Sample Distribution 1"),  status = "primary",
                               plotOutput("sampleDistM1", width = '100%', height = 200)
                             ),
                             box(
                               width = 6,
                               title = h4( "Sample Distribution 2"),  status = "primary",
                               plotOutput("sampleDistM2", width = '100%', height = 200)
                             )
                           ),
                           
                           fluidRow(
                             
                             box(
                               width = 6,
                               title =  h4("Sample Summary"),  status= "primary", background = "light-blue",
                               tableOutput("sampSumDatTM")
                             )
                           ),
                           fluidRow(
                             box(width = 6,
                                 title =  h4("Population Summary"),  status= "primary", background = "light-blue",
                                 tableOutput("popSumDatTM")
                             ),
                             box( width = 4,
                                  title =  h4("Alternative Hypothesis"), status = "primary",
                                  
                                  radioButtons("altHypRMeans", "", choices = c("<", "=", ">"), selected ="<", inline = TRUE,
                                               width = '400px')
                             )
                           ),
                           fluidRow(
                             box( width = 12,
                                  title =  h4("Sampling Distribution"), status = "primary",
                                  plotOutput("samplingDistTM", width = '100%', height = 200)
                             )
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
                            tabPanel("Linear Regression",
                                     
                                     column(
                                       width = 4, status= "primary",
                                       box(width = "100%",height = "100%",status = "primary",
                                           tabsetPanel(id = "tabs",
                                                       tabPanel("Dataset", value = "seldattab",
                                                                selectInput("dataset", "Choose Data Set:", choices = c("Cars", "Orange")),
                                                                checkboxInput("fitLine", label = "Fit Line", value = FALSE),
                                                                tableOutput("lineEq")
                                                                
                                                       ),
                                                       
                                                       tabPanel("Sample", value = "samptab",
                                                                
                                                                
                                                                column(
                                                                  width =12, status = "primary",
                                                                  
                                                                  
                                                                  
                                                                  sliderInput("intercept", "Intercept", value = 1, min = -25, max =25
                                                                              
                                                                  ),
                                                                  sliderInput("slope", "Slope", value = 1, max =25, min = -25
                                                                              
                                                                  ),
                                                                  checkboxInput("fitLine2", label = "Fit Line", value = FALSE)
                                                                ),
                                                                tableOutput("lineEqSamp")
                                                                
                                                       )
                                           )
                                           
                                       )
                                       
                                     ),
                                     column(
                                       width = 7, status = "primary",
                                       box(
                                         width = "100%", height = "100%",
                                         title = "Linear Regression",  status = "primary",
                                         plotOutput("linreg" )
                                       )
                                       
                                     )
                                     
                                     
                            )),
                 
                 #ANOVA
                 tabPanel(h6("ANOVA"),
                          fluidRow(
                            box(width = 10, status = "primary", title = NULL,
                                sliderInput("sampSizeANOVA", "Sample Size", value = 50, min = 10, max =100, step = 5)),
                            
                            actionButton("goAnova", "Draw",class="btn btn-success btn")
                            
                            
                            
                          ),
                          fluidRow(
                            box(
                              width = 4, status = "primary",title = NULL,
                              
                              sliderInput("anovaMean1", "Mean 1", value = 30, min = 10, max =50, step = 1)),
                            box(
                              width = 4, status = "primary",title = NULL,
                              sliderInput("anovaMean2", "Mean 2", value = 30, min = 10, max =50, step = 1)),
                            box(
                              width = 4, status = "primary",title = NULL,
                              sliderInput("anovaMean3", "Mean 3", value = 30, min = 10, max =50, step = 1)
                              
                            )
                          ),
                          fluidRow(
                            box(width = 4, status = "primary", title = NULL,
                                sliderInput("anovaSD1", "St Dev 1", value = 1, min = 1, max =50, step = 1)),
                            box(
                              width = 4, status = "primary",title = NULL,
                              sliderInput("anovaSD2", "St Dev 2", value = 1, min = 1, max =50, step = 1)),
                            box(
                              width = 4, status = "primary",title = NULL,
                              sliderInput("anovaSD3", "St Dev 3", value = 1, min = 1, max =50, step = 1))
                            
                            
                          ),
                          
                          fluidRow(
                            box(
                              width = 12, status = "primary",title =  h4("Dot Plot"),
                              plotOutput("anovaPlot",width = 500, height = 300)
                              
                            ),
                            fluidRow(
                              box(
                                width = 4, status = "primary",title =  h4("Summary Data"),
                                tableOutput("anovaDatSum")
                              ),
                              box(
                                width = 6, status = "primary",title = h4("ANOVA Table"),
                                tableOutput("anovaTable")
                              )
                              
                            )
                          )
                 ))

