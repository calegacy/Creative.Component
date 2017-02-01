#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#Libraries
library(shiny)
library(datasets)
library(ggplot2)
library(plotrix)
library(MASS)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(gridExtra)
library(plotly)

function(input, output, session) {
  
  # -------------------------------------------- Code for Inference for One Proportion ----------------------------#  
  
  #Calculations to get a sample distribution
  # Get a sample proportion
  pickVect = eventReactive(input$goProp,{
    pickf = rbinom(input$sampleSize,1, input$popProp)
    
    return(pickf)
  })
  pickProp = eventReactive(input$goProp,{
    
    pickprop = length(which(pickVect() == 1))/input$sampleSize
    return(pickprop)
  })
  
  #Make data frame of counts in order to make a bar graph
  dat = eventReactive(input$goProp,{
    datf = c(rep("yes",length(which(pickVect() == 1))), rep("no", length(which(pickVect() == 0))))
    return(datf)
  })
  #Sample Distribution output
  output$sampleDist = renderPlot({
    ggplot(data = data.frame(dat()),aes(dat()))+ geom_histogram( stat = "count")+xlab("Category")
  })
  
  #Sample summary information
  samplesum = eventReactive(input$goProp,{data.frame(
    Category = c( "Yes", "No"),
    Count =c(  length(which(pickVect() == 1)), length(which(pickVect() == 0))),
    Prop = c(length(which(pickVect() == 1))/input$sampleSize, length(which(pickVect() == 0))/input$sampleSize)
  )
    
  })
  
  #Sample summary table output
  output$sampSumDat  = renderTable(caption = "Summary Statistics",caption.placement = getOption("xtable.caption.placement", "top"),{
    samplesum()
  })
  
  #Calculations to make a sampling distribution
  samples = eventReactive(input$goProp,{
    if(input$numSamp == 1){
      return(pickProp())
    }
    if(input$numSamp > 1){
      v = c(pickProp())
      for(i in 2:input$numSamp){
        
        npick = reactive({
          newpick = rbinom(input$sampleSize, 1,input$popProp)
          newprop = length(which(newpick == 1))/input$sampleSize
          return(newprop)
        })
        v = c(v,npick())
        
      }}
    return(v)
  })
  
  # Plot of the sampling distribution
  output$samplingDist =renderPlot({
    ggplot(data = data.frame(samples()),aes(samples()))+ geom_histogram(binwidth = 0.01) +xlab("Proportion") +ylab("Count")
  })
  
  
  # Population summary info
  
  popSum = eventReactive(input$goProp,{data.frame(
    
    Mean = mean(samples()),
    StDev = sd(samples())
    
    
    
  )
    
  })
  
  #Population summary table
  output$popSumDat  = renderTable(caption = "Population Summary",caption.placement = getOption("xtable.caption.placement", "top"),{
    popSum()
  })
  
# Add the text for "Sample/Sampling Distribution"
  output$sdist <- renderText({ "Sample Distribution" })
  output$singdist <- renderText({ "Sampling Distribution" })
  #---------------------------------------------- One proportion CIs ------------------------------------------------ #
  
  
  pickci = eventReactive(input$goCI,{
    pickfci = rbinom(input$sampleSizeCI, 1, input$popPropCI)
    pickCIprop = length(which(pickfci == 1))/input$sampleSizeCI
    return(pickCIprop)
  })
  
  #One plot of a confidence interval
  upProp = reactive({
    upci = pickci() + (sqrt(pickci()*(1-pickci())/input$sampleSizeCI))
    return(upci)
  })
  
  lowProp = reactive({
    loci = pickci() - (sqrt(pickci()*(1-pickci())/input$sampleSizeCI))
    return(loci)
  })
  
  
  #Sample summary information
  samplesumCI = reactive({data.frame(
    Proportion = c( pickci()),
    Lower = c(lowProp()),
    Upper = c( upProp())
  )
    
  })
  output$sPropSumDat  = renderTable(caption = "Summary Statistics",caption.placement = getOption("xtable.caption.placement", "top"),{
    samplesumCI()
  })
  
  # Output graph from the single sample CI
  output$samplePropCI = renderPlot({
    ggplot(x = pickci())+ xlab("Confidence Interval")+geom_segment(aes(x = lowProp(), xend = upProp(), y = 0, yend = 0))+theme(axis.title.y=element_blank(),
                                                                                                                                      axis.text.y=element_blank(),
                                                                                                                                      axis.ticks.y=element_blank())+geom_point()
  })
  #Calculations to make confidence intervals a sampling distribution of 
  samplesCI = eventReactive(input$goCI,{
    if(input$numSampCI == 1){
      return(pickci())
    }
    if(input$numSampCI > 1){
      vci = c(pickci())
      for(i in 2:input$numSampCI){
        
        npickci = reactive({
          newpickci = rbinom(input$sampleSizeCI,1, input$popPropCI)
          newpickciProp =  length(which(newpickci == 1))/input$sampleSizeCI
          return(newpickciProp)
        })
        vci = c(vci,npickci())
        
      }}
    return(vci)
  })
  
  #Make the y axis for all the segments to sit
  ysCI = reactive({
    yy = seq(from = 1, by = 0.5, length.out = length(samplesCI()))
    return(yy)
  })
  
  
  #Set the Z based on the input from the user
  zprop = reactive({
    if(input$CLProp == "90%"){
      zprop = 1.645
    }
    if(input$CLProp == "95%"){
      zprop = 1.96
    }
    if(input$CLProp == "99%"){
      zprop = 2.576
    }
    return(zprop)
  })
  
  #Calculate the upper and lower bounds for the CIs of the sample
  upPropMult = eventReactive(input$goCI,{
    upci2 = samplesCI() + zprop()*(sqrt(samplesCI()*(1-samplesCI())/input$sampleSizeCI))
    return(upci2)
  })
  
  lowPropMult = eventReactive(input$goCI,{
    loci2 = samplesCI() - zprop()*(sqrt(samplesCI()*(1-samplesCI())/input$sampleSizeCI))
    return(loci2)
  })
  
  # Make line segments colored based on whether they capture the true pop prop
  colorPCI = eventReactive(input$goCI,{
    colvect = NULL
    for(i in 1:length(samplesCI())){
      if(lowPropMult()[i] <= input$popPropCI && input$popPropCI  <= upPropMult()[i]   ){
        colnew = "steelblue1"
        colvect = c(colvect,colnew)
        
      }
      else{
        colnew = "tan1"
        colvect = c(colvect,colnew)
        
      }
      
    }
    return(colvect)
  })
  
  # Multiple samples lines
  output$sPropPopCI = renderPlot({
    ggplot(data = data.frame(samplesCI(),ysCI()),aes(x = samplesCI(), y = ysCI()))+ xlab("Confidence Intervals")+geom_segment(aes(x = lowPropMult(), xend = upPropMult(), y = ysCI(), yend = ysCI()), colour =colorPCI())+theme(axis.title.y=element_blank(),
                                                                                                                                                                                     axis.text.y=element_blank(),
                                                                                                                                                                                     axis.ticks.y=element_blank())+ geom_point()
  })
  
  
  
  # Population summary info
  
  popSumCI = eventReactive(input$goCI,{data.frame(
    
    "ProportionCaptured" = length(which(colorPCI() == "steelblue1"))/length(samplesCI())
    
  )
    
  })
  output$sPropPopDat  = renderTable(caption = "Summary of Samples",caption.placement = getOption("xtable.caption.placement", "top"),{
    popSumCI()
  })
  
  
  # Add the text for "Sample/Sampling Distribution"
  output$CISumStat <- renderText({ "One Sample" })
  output$CIManyStat <- renderText({ "Multiple Samples" })
  
  # --------------------------------------------- Confidence Interval Sample Size  -----------------------------------#
  
  # 
  # CIInf = reactive({data.frame(
  #   NumberSamples = 20,
  #   PopProp = 0.4,
  #   Confidence = "95%"
  #   
  # )
  #   
  # })
  # 
  # output$CIinfo = renderTable({
  #   CIInf()
  # })
  # 
  # # Get a plot of the 20 samples
  # 
  # tenSamps = eventReactive(input$goSS,{
  #   tsamp = NULL
  #   for(i in 1:20){
  #   ts = rbinom(input$sampdemo, 1, 0.4)
  #   countProp = length(which(ts ==1 ))/input$sampdemo
  #   tsamp = c(tsamp,countProp)
  #   }
  #   return(tsamp)
  # })
  # 
  # ysCIDemo = reactive({
  #   yy = seq(from = 1, by = 0.5, length.out = length(tenSamps()))
  #   return(yy)
  # })
  # 
  # upSampDemo = reactive({
  #   usd = tenSamps() + 1.96*(sqrt(tenSamps()*(1-tenSamps())/input$sampdemo))
  #   return(usd)
  # })
  # 
  # lowSampDemo = reactive({
  #   lsd =  tenSamps() - 1.96*(sqrt(tenSamps()*(1-tenSamps())/input$sampdemo))
  #   return(lsd)
  # })
  # 
  # colorSampDemo = reactive({
  #   coldemo = NULL
  #   for(i in 1:20){
  #     if(lowSampDemo()[i] <= 0.4 && 0.4  <= upSampDemo()[i]   ){
  #       colne = "steelblue1"
  #       coldemo = c(coldemo,colne)
  #       
  #     }
  #     else{
  #       colne = "tan1"
  #       coldemo = c(coldemo,colne)
  #       
  #     }
  #     
  #   }
  #   return(coldemo)
  # })
  # 
  # 
  # output$sampDemoPlot = renderPlot({
  #   qplot(x = tenSamps(), y = ysCIDemo(), xlab = "Confidence Intervals")+xlim(-0.1,1.1)+geom_segment(aes(x = lowSampDemo(), xend = upSampDemo(), y = ysCIDemo(), yend = ysCIDemo()), colour = colorSampDemo())+theme(axis.title.y=element_blank(),
  #                                                                                                                                                      axis.text.y=element_blank(),
  #                                                                                                                                                      axis.ticks.y=element_blank())
  # })
  
  
  
  
  # Get a plot of the 20 samples
  example = eventReactive(input$goSS,{
    
    ts = rbinom(input$sampdemo, 1, 0.5)
    countProp = length(which(ts ==1 ))/input$sampdemo
    
    return(countProp)
  })
  
  
  ysCIDemo = reactive({
    yy = seq(from = 1, by = 0.5, length.out = length(example()))
    return(yy)
  })
  
  upSampDemo = reactive({
    usd = example() + 1.96*(sqrt(example()*(1-example())/input$sampdemo))
    return(usd)
  })
  
  lowSampDemo = reactive({
    lsd = example() - 1.96*(sqrt(example()*(1-example())/input$sampdemo))
    return(lsd)
  })
  
  output$sampDemoPlot = renderPlot({
    qplot(x = example(), y = ysCIDemo(), xlab = "Confidence Intervals")+xlim(-0.1,1.1)+geom_segment(aes(x = lowSampDemo(), xend = upSampDemo(), y = ysCIDemo(), yend = ysCIDemo()))+theme(axis.title.y=element_blank(),
                                                                                                                                                                                          axis.ticks.y=element_blank())
  })
  
  CIInf = reactive({data.frame(
    PopProp = 0.5,
    Confidence = "95%",
    Lower = lowSampDemo(),
    Upper = upSampDemo()
    
    
  )
    
  })
  
  output$CIinfo = renderTable({
    CIInf()
  })
  
  
  #---------------------------------------------- Code for confidence level demo -------------------------------------#
  #Information
  ClInf = reactive({data.frame(
    SampleSize = 100,
    NumberSamples = 20,
    PopProp = 0.4
  )
  })
  
  # Output information table
  output$Clinfo = renderTable({
    ClInf()
  })
  
  #Get samples for output
  
  twSamps = eventReactive(input$goCL,{
    tsampCL = NULL
    for(i in 1:20){
      tsCL = rbinom(100, 1, 0.4)
      countPropCL = length(which(tsCL == 1 ))/ 100
      tsampCL = c(tsampCL,countPropCL)
    }
    return(tsampCL)
  })
  
  
  ysCLDemo = reactive({
    yy = seq(from = 1, by = 0.5, length.out = length(twSamps()))
    return(yy)
  })
  
  zlev = reactive({
    if(input$cldemo  == 80){
      zlev = 1.282
    }
    if(input$cldemo == 85){
      zlev = 1.44
    }
    if(input$cldemo == 90){
      zlev = 1.645
    }
    if(input$cldemo == 95){
      zlev = 1.96
    }
    if(input$cldemo == 99){
      zlev = 2.576
    }
    return(zlev)
  })
  
  upCLDemo = reactive({
    usd = twSamps() + zlev()*(sqrt(twSamps()*(1-twSamps())/100))
    return(usd)
  })
  
  lowCLDemo = reactive({
    lsd =  twSamps() - zlev()*(sqrt(twSamps()*(1-twSamps())/100))
    return(lsd)
  })
  
  colorCLDemo = reactive({
    coldemo = NULL
    for(i in 1:20){
      if(lowCLDemo()[i] <= 0.4 && 0.4  <= upCLDemo()[i]   ){
        colne = "steelblue1"
        coldemo = c(coldemo,colne)
        
      }
      else{
        colne = "tan1"
        coldemo = c(coldemo,colne)
        
      }
      
    }
    return(coldemo)
  })
  
  
  output$sampCLPlot = renderPlot({
    qplot(x = twSamps(), y = ysCLDemo(), xlab = "Confidence Intervals")+xlim(0,1)+geom_segment(aes(x = lowCLDemo(), xend = upCLDemo(), y = ysCLDemo(), yend = ysCLDemo()), colour = colorCLDemo())+theme(axis.title.y=element_blank(),
                                                                                                                                                                                                         axis.text.y=element_blank(),
                                                                                                                                                                                                         axis.ticks.y=element_blank())
  })
  
  
  # --------------------------------------------- Code for Inference for One Mean ------------------------------------#
  
  
  # Calculations to get a sample distribution
  sdOM = reactive({
    stdevOM = input$sigma/sqrt(input$sampleSizeOM)
    return(stdevOM)
  })
  pickOM = eventReactive(input$goMean,{
    pickfOM = c(rnorm(input$sampleSizeOM, input$popMeanOM, sdOM()))
    return(pickfOM)
  })
  
  
  #Sample Distribution output
  output$sampleDistOM = renderPlot({
    qplot(pickOM(), xlab = "x")
  })
  
  #Sample summary information
  samplesumOM = reactive({data.frame(
    Mean = mean(pickOM()),
    StandardDeviation = input$sigma/sqrt(input$sampleSizeOM)
    
  )
    
  })
  output$sampSumDatOM  = renderTable(caption = "One Sample Summary",caption.placement = getOption("xtable.caption.placement", "top"),{
    samplesumOM()
  })
  
  #Calculations to make a sampling distribution
  samplesOM = eventReactive(input$goMean,{
    if(input$numSampOM == 1){
      return(mean(pickOM()))
    }
    if(input$numSampOM > 1){
      v = c(mean(pickOM()))
      for(i in 2:input$numSampOM){
        
        nnpick = reactive({
          newpick = c(rnorm(input$sampleSizeOM, input$popMeanOM, sdOM()))
          meanSample = mean(newpick)
          return(meanSample)
        })
        v = c(v,nnpick())
      }
    }
    return(v)
  })
  output$samplingDistOM =renderPlot({
    qplot(samplesOM(), xlab = "x")
  })
  
  
  
  
  
  # Population summary info
  
  #Sample summary information
  popSumOM = reactive({data.frame(
    
    Mean = mean(samplesOM()),
    StDev = sd(samplesOM())
    
    
  )
    
  })
  output$popSumDatOM  = renderTable(caption = "Summary of Samples",caption.placement = getOption("xtable.caption.placement", "top"),{
    popSumOM()
  })
  output$OneMeanDist <- renderText({ "Sample Distribution" })
  output$ManyMeansDist <- renderText({ "Sampling Distribution" })
  
  #--------------------------------------------- Inference in Two Proportions ---------------------------------------#
  #Calculations to get a sample distribution for group 1
  
  pickP1Vect = reactive({
    pickfp1 = rbinom(input$sampleSizeGP1,1, input$popPropG1P)
    return(pickfp1)
  })
  # Get counts
  npickP1Prop = reactive({
    npickp1f = length(which(pickP1Vect() == 1))/input$sampleSizeGP1
    return(npickp1f)
  })
  
  #Make data frame of counts in order to make a bar graph
  datp1 = eventReactive(input$go2Prop,{
    datfp1 = c(rep("yes",length(which(pickP1Vect() == 1))), rep("no", length(which(pickP1Vect() == 0))))
    return(datfp1)
  })
  #Sample Distribution output
  output$sampleDistG1 = renderPlot({
    qplot(datp1(),xlab = "Category", ylab = "Count") + labs(title = "Group 1")
  })
  
  ####
  #Calculations to get a sample distribution for group 2
  
  pickP2Vect = reactive({
    pickfp2 = rbinom(input$sampleSizeGP2,1, input$popPropG2P)
    return(pickfp2)
  })
  
  npickP2Prop = reactive({
    npickp2f = length(which(pickP2Vect() ==1))/input$sampleSizeGP2
    return(npickp2f)
  })
  
  #Make data frame of counts in order to make a bar graph
  datp2 = eventReactive(input$go2Prop,{
    datfp2 = c(rep("yes", length(which(pickP2Vect() ==1))), rep("no",  length(which(pickP2Vect() ==0))))
    return(datfp2)
  })
  #Sample Distribution output
  output$sampleDistG2 = renderPlot({
    qplot(datp2(), xlab = "Category", ylab = "Count") + labs(title = "Group 2")
  })
  
  ####
  #Sample summary information for two proportions
  samplesumP2 = eventReactive(input$go2Prop,{data.frame(
    
    Prop1 = npickP1Prop(),
    Prop2 = npickP2Prop(),
    Diff =npickP1Prop()-npickP2Prop()
  )
    
  })
  output$sampSumDatP2  = renderTable(caption = "Summary of Samples",caption.placement = getOption("xtable.caption.placement", "top"),{
    samplesumP2()
  })
  
  ####
  #Sampling Dist for two proportions
  #Calculations to make a sampling distribution
  samplesP2 = eventReactive(input$go2Prop,{
    if(input$numSampGP == 1){
      return(npickP1Prop()-npickP2Prop())
    }
    if(input$numSampGP > 1){
      vp2 = c(npickP1Prop()-npickP2Prop())
      for(i in 2:input$numSampGP){
        
        npickdiffp = reactive({
          newpickp1 = rbinom(input$sampleSizeGP1,1, input$popPropG1P)
          npSamp1 = length(which(newpickp1 == 1))/input$sampleSizeGP1
          newpickp2 = rbinom(input$sampleSizeGP2,1, input$popPropG1P)
          npSamp2 = length(which(newpickp2 == 1))/input$sampleSizeGP2
          newpropdiff  = npSamp1 - npSamp2
          return(newpropdiff)
        })
        vp2 = c(vp2,npickdiffp())
        
      }}
    return(vp2)
  })
  
  #Sampling distribution for two proportions
  
  two.prop.st = reactive({
    tps = sqrt((npickP1Prop()*(1-npickP1Prop())/input$sampleSizeGP1)+(npickP2Prop()*(1-npickP2Prop())/input$sampleSizeGP2))
    return(tps)
  })
  
  
  
  output$samplingDistTP =renderPlot({
    
    
    qplot(samplesP2(), xlab = "Difference in Proportions", ylab = "Count")
  })
  
  
  
  
  # Calculate a simulated p-value
  pvalueR = reactive({
    if(input$altHypR == "<"){
      pvr = length(which(samplesP2() < (npickP1Prop()-npickP2Prop())))/ input$numSampGP
    }
    if(input$altHypR == "="){
      pvr =2*(length(which(samplesP2() < (npickP1Prop()-npickP2Prop()))))/ input$numSampGP
    }
    if(input$altHypR == ">"){
      pvr  = length(which(samplesP2() > (npickP1Prop()-npickP2Prop())))/ input$numSampGP
    }
    return(pvr)
  })
  
  
  # Calculate the z-score and p-value
  pPooled = reactive({
    ppool = (input$sampleSizeGP1*npickP1Prop() + input$sampleSizeGP2*npickP2Prop()) /(input$sampleSizeGP1 + input$sampleSizeGP2)
    return(ppool)
  })
  zval = reactive({
    z =  (npickP1Prop()-npickP2Prop()) / sqrt((pPooled()*(1-pPooled())/input$sampleSizeGP1)+(pPooled()*(1-pPooled())/input$sampleSizeGP2))
    return(z)
  })
  
  pvalueZ = reactive({
    if(input$altHypR == "<"){
      pv = pnorm(zval(),0,1)
    }
    if(input$altHypR == "="){
      pv = 2*(pnorm(zval(),0,1))
    }
    if(input$altHypR == ">"){
      pv = 1 - pnorm(zval(),0,1)
    }
    return(pv)
  })
  
  
  # Population summary info and p-values
  popSumTP = reactive({data.frame(
    
    Mean = mean(samplesP2()),
    StDev =sd(samplesP2()),
    PValue = pvalueR(),
    ZValue = zval(),
    PValueZ = pvalueZ()
    
  )
    
  })
  output$popSumDat2P = renderTable(caption = "Samping Distribution Summary",caption.placement = getOption("xtable.caption.placement", "top"),{
    popSumTP()
  })
  
  # Plot output for the z-score
  xs = seq(-4,4,by = 0.01)
  norm = 1/sqrt(2*pi) * exp(-xs^2/2)
  output$normalPlot = renderPlot({
    qplot(x = xs, y =norm,size=I(0.2))+geom_vline(xintercept = zval(), linetype="F1", 
                                                  color = "blue", size=0.75)
    
  })
  
  output$twoPropSampleDist <- renderText({ "Sample Distributions" }) 
  output$twoPropHyp <- renderText({ "Alternative Hypothesis" })
  output$twoPropSampDist <- renderText({ "Sampling Distribution" }) 
  
  
  
  
  # --------------------------------------------- Two Means Code -----------------------------------------------------#
  # Calculations to get a sample distribution group 1
  
  pickM1 = eventReactive(input$go2Mean,{
    pickfM1 = c(rnorm(input$sampleSizeTM1, input$popMeanM1, input$sigmaTM1))
    return(pickfM1)
  })
  
  
  #Sample Distribution output
  output$sampleDistM1 = renderPlot({
    qplot(pickM1(), xlab = "Value", ylab = "Count")+ labs( title = "Group 1")
  })
  ###
  # Calculations to get a sample distribution group 2
  
  pickM2 = eventReactive(input$go2Mean,{
    pickfM2 = c(rnorm(input$sampleSizeTM2, input$popMeanM2, input$sigmaTM2))
    return(pickfM2)
  })
  
  
  #Sample Distribution output
  output$sampleDistM2 = renderPlot({
    qplot(pickM2(), xlab = "Value", ylab = "Count") + labs(title = "Group 2")
  })
  
  
  
  #Samples summary information
  samplesumTM = reactive({data.frame(
    Mean1 = mean(pickM1()),
    Mean2 = mean(pickM2()),
    DiffMeans = mean(pickM1())- mean(pickM2())
    
    
  )
    
  })
  output$sampSumDatTM  = renderTable(caption = "Summary of Samples",caption.placement = getOption("xtable.caption.placement", "top"),{
    samplesumTM()
  })
  
  #Calculations to make a sampling distribution
  samplesTM = reactive({
    if(input$numSampTM == 1){
      return(mean(pickM1())-mean(pickM2()))
    }
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
        vm = c(vm,nmpick())
      }
    }
    return(vm)
  })
  output$samplingDistTM =renderPlot({
    qplot(samplesTM(), xlab = "Value", ylab = "Count") 
  })
  
  
  # Calculate a simulated p-value for two means
  pvalueR2M = reactive({
    if(input$altHypRMeans == "<"){
      pvr2 = length(which(samplesTM() < (input$popMeanM1-input$popMeanM2)))/ input$numSampTM
    }
    if(input$altHypRMeans == "="){
      pvr2 =2*(length(which(samplesTM() < (input$popMeanM1-input$popMeanM))))/ input$numSampTM
    }
    if(input$altHypRMeans == ">"){
      pvr2  = length(which(samplesTM() > (input$popMeanM1-input$popMeanM2)))/ input$numSampTM
    }
    return(pvr2)
  })
  
  zscore2M = reactive({
    zs2m = input$popMeanM1-input$popMeanM2 / sqrt(input$sigmaTM1^2/input$sampleSizeTM1 +input$sigmaTM2^2/input$sampleSizeTM2  )
    return(zs2m)
  })
  
  pvalueZTM = reactive({
    if(input$altHypRMeans == "<"){
      pv2 = pnorm(zscore2M(),0,1)
    }
    if(input$altHypRMeans == "="){
      pv2 = 2*(pnorm(zscore2M(),0,1))
    }
    if(input$altHypRMeans == ">"){
      pv2 = 1 - pnorm(zscore2M(),0,1)
    }
    return(pv2)
  })
  # Population summary info
  
  
  popSumTM = reactive({data.frame(
    
    Mean = mean(samplesTM()),
    StDev = sd(samplesTM()),
    PValue = pvalueR2M(),
    Zscore = zscore2M(),
    ZPvalue = pvalueZTM()
    
  )
    
  })
  output$popSumDatTM  = renderTable(caption = "Summary Sampling Distribution",caption.placement = getOption("xtable.caption.placement", "top"),{
    popSumTM()
  })
  
  output$twoMeansHyp <- renderText({ "Alternative Hypothesis" })
  output$twoMeansSampDist <- renderText({ "Sampling Distribution" })
  output$twoMeansSampleDist <- renderText({ "Sampling Distribution" })
  
  # --------------------------------------------- Linear Regression Code ----------------------------------------------#
  # Correlation tab code
  
  # 
  xycorr =reactive({
    datxy = as.data.frame(mvrnorm(100, mu = c(0,0), Sigma = matrix(c(1,input$correlation,input$correlation,1),, ncol = 2),empirical = TRUE))
    
    return(datxy)
  })
  output$corrPlot = renderPlot({
    qplot(xycorr()$V1,xycorr()$V2, xlab = "x", ylab = "y")
  })
  
  
  
  
  
  
  
  # Plotting the linear regression
  output$linreg = renderPlot({
    #"Dataset" tab: To plot an existing preloaded dataset
    if(input$tabs == "seldattab"){
      
      if(input$dataset == "Cars"){
        print(qplot( cars$speed, cars$dist, ylab="Distance", xlab="Speed"))
        if(input$fitLine == TRUE){
          carout = lm(cars$dist~cars$speed)
          print(qplot( cars$speed, cars$dist, ylab="Distance", xlab="Speed")+geom_abline(intercept =  carout$coefficients[1],slope = carout$coefficients[2], col = "mediumseagreen"))
          carsline = reactive({data.frame(
            
            intercept = carout$coefficients[1],
            slope = carout$coefficients[2]
            
            
          )
            
          })
          output$lineEq  = renderTable({
            carsline()
          })
        }
      }
      else{ 
        
        print(qplot(Orange$age, Orange$circ, ylab="Circumfrence", xlab="Year"))
        if(input$fitLine == TRUE){
          oranout= lm(Orange$circ~Orange$age)
          print(qplot(Orange$age, Orange$circ, ylab="Circumfrence", xlab="Year")+geom_abline(intercept =  oranout$coefficients[1],slope = oranout$coefficients[2], col = "mediumseagreen"))
          treesline = reactive({data.frame(
            
            intercept = oranout$coefficients[1],
            slope = oranout$coefficients[2]
            
            
          )
            
          })
          output$lineEq  = renderTable({
            treesline()
          })
          
        }
      }
    }
    
    
    # "Sample" tab: To plot a specified sample of data
    if( input$tabs == "samptab"){
      
      # To set slope
      xy = as.data.frame(mvrnorm(100, mu = c(0,0), Sigma = matrix(c(1,input$slope,input$slope,1000),, ncol = 2),empirical = TRUE))
      
      #intercept change
      linexy = line(xy$V1,xy$V2)
      intdelt = reactive({
        rintdelt= linexy$coefficients[1] - input$intercept
        return(rintdelt)
      })
      
      print(qplot(xy$V1,xy$V2-intdelt(),xlab = "Predictor", ylab = "Response"))
      if(input$fitLine2 == TRUE){
        outp = lm(xy$V2-intdelt()~xy$V1)
        print(qplot(xy$V1,xy$V2-intdelt(),xlab = "Predictor", ylab = "Response") + geom_abline(intercept = outp$coefficients[1], slope =outp$coefficients[2], col = "mediumseagreen" ))
        sampline = reactive({data.frame(
          
          intercept = outp$coefficients[1],
          slope = outp$coefficients[2]
          
          
        )
          
        })
        output$lineEqSamp  = renderTable({
          sampline()
        })
      }
    }
    
    
    
    
  })
  
  # ----------------------------------------- ANOVA -------------------- #
  
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
  
  g123Data = eventReactive(input$goAnova, {
    alData = c(group1Data(), group2Data(), group3Data())
    return(alData)
  })
  groupList = eventReactive(input$goAnova, {
    grpList = c(rep("group1", input$sampSizeANOVA), rep("group2", input$sampSizeANOVA), rep("group3", input$sampSizeANOVA))
    return(grpList)
  }) 
  
  dframe = reactive( {
    newframe = data.frame(g123Data(), groupList())
    return(newframe)
    
    
  })
  
  
  output$anovaPlot = renderPlot( {
    
    qplot(data = dframe(), x =g123Data(), geom = "dotplot", fill = groupList(), xlab = "x")+facet_grid(facets = groupList()~.)
    
  })
  mod=reactive({
    
    ll = lm(g123Data()~groupList())
    return(ll)
  })
  an =reactive({
    
    model = anova(mod())
    return(model)
  })
  
  
  output$anovaTable = renderTable({
    an()
  })
  
  anovaSum = reactive({data.frame(
    Group = c("Group1", "Group2", "Group3"),
    Means = c(mean(group1Data()), mean(group2Data()), mean(group3Data())),
    StDev = c(sd(group1Data()),sd(group2Data()), sd(group3Data()))
  )
    
    
  })
  # ???????? ##
  # samplesum = eventReactive(input$goProp,{data.frame(
  #   Category = c( "Yes", "No"),
  #   Count =c(  yes(), no()),
  #   Prop = c(yes()/input$sampleSize, no()/input$sampleSize)
  # )
  #   
  # })
  
  output$anovaDatSum = renderTable({
    anovaSum()
  })
  
  
  
  
  
  
  
  
  
  
  # Must be within this bracket
}


