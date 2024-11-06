library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(shinydashboard)
library(DescTools)
#pull the latest simulation data
latestSimData <- read.csv(url("https://raw.githubusercontent.com/skyeliam/2024ElectionModel/refs/heads/main/Latest_Election%20Simulation%20Data.csv"))
colnames(latestSimData) <- str_replace_all(colnames(latestSimData),"\\."," ")
timeStamp <- StrExtractBetween(read_file(url(
  "https://raw.githubusercontent.com/skyeliam/2024ElectionModel/refs/heads/main/timestamp.txt"))," ","\n")
yaxis <- c(state.name,"District of Columbia","Maine CD 1","Maine CD 2","Nebraska CD 2")
#function for generating histogram of all EV outcomes
histGenerator <- function(){
  breaktest <- c(80,85,90,100,105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200,205,210,215,220,225,230,235,240,
                 245,250,255,260,265,268.01,269.99,275,280,285,290,295,300,305,310,315,320,325,330,335,340,345,350,355,
                 360,365,370,375,380,385,390,395,400,405,410,415,420,425,430,435,440,445,450,455,460,465,470)
  gram <- hist(latestSimData$ElectoralVotes,breaks = breaktest,freq=TRUE)
  colorbreaks <- rep("firebrick1",length(gram$breaks))
  colorbreaks[gram$breaks > 268] <- "grey"
  colorbreaks[gram$breaks > 269] <- "dodgerblue"
  axis(1,at = c(120,195,270,345,420))
  par(mar=c(2,2,1,1))
  hist(latestSimData$ElectoralVotes, col = colorbreaks, breaks = breaktest,freq=TRUE,
       xaxs="i",xlab = NULL,main = NULL,xlim=c(min(latestSimData$ElectoralVotes),max(latestSimData$ElectoralVotes)))
}

#function for generating a dot plot of EV outcomes vs popular vote
dotplotGenerator <- function(bias){
  latestSimData %>% mutate(quadrant = case_when(latestSimData$HarrisMargin>0 & latestSimData$ElectoralVotes >= 270 ~"Harris wins PV and EC",
                                                latestSimData$HarrisMargin<0 & latestSimData$ElectoralVotes >= 270 ~"Harris loses PV and wins EC",
                                                latestSimData$HarrisMargin>0 & latestSimData$ElectoralVotes < 269 ~"Trump loses PV and wins EC",
                                                latestSimData$HarrisMargin<0 & latestSimData$ElectoralVotes < 269 ~"Trump wins PV and wins EC",
                                                TRUE ~ "Electoral College tie")) %>%
    ggplot(aes(x=latestSimData$HarrisMargin,y=latestSimData$ElectoralVotes-270,color = quadrant)) + geom_jitter() + 
    scale_color_manual(values = c(`Harris wins PV and EC` = 'dodgerblue4', `Harris loses PV and wins EC` = 'dodgerblue', 
                                  `Trump loses PV and wins EC` = 'firebrick1', 
                                  `Trump wins PV and wins EC` = 'firebrick',`Electoral College tie` = "darkgrey")) +
    xlab("Harris popular vote margin (%)") +
    ylab("Harris Electoral College vote margin")+
    geom_vline(xintercept=bias,linetype="dotted")
}

#function for generating the range of states as a forestplot
stateRangeGenerator <- function(){
  districts <- c(state.name,"District of Columbia","Maine CD 1","Maine CD 2","Nebraska CD 2")
  mean  <- unlist(lapply(latestSimData[,districts],mean))
  lower <- unlist(lapply(latestSimData[,districts],quantile,probs=1/20))
  upper <- unlist(lapply(latestSimData[,districts],quantile,probs=19/20))
  zeropoint <- ifelse(lower<0 & upper>0,0,NA)
  lineColor <- ifelse(upper<0,"firebrick1",ifelse(lower>0,"dodgerblue1",NA))
  
  df <- data.frame(districts, mean, lower, upper,zeropoint)
  
  #flip the axes
  df$districts <- factor(df$districts, levels=rev(df$districts))
  
  dotcolors <- ifelse(mean>0,"dodgerblue4","firebrick")
  #generate the forest plot
  ggplot(data=df) + geom_segment(aes(x=lower,xend=zeropoint,y=districts,yend=districts),colour="firebrick1") + 
    geom_segment(aes(x=zeropoint,xend=upper,y=districts,yend=districts),colour="dodgerblue1") + 
    geom_segment(aes(x=lower,xend=upper,y=districts,yend=districts),colour=lineColor) +
    geom_point(aes(x = mean, y = districts),color=dotcolors) + xlab("Harris Forecasted Margin (%)") + ylab("State/District") + theme_bw()
}

singleStateLine <- function(state){
  mean  <- mean(latestSimData[,state])
  lower <- quantile(latestSimData[,state],probs=1/20)
  upper <- quantile(latestSimData[,state],probs=19/20)
  zeropoint <- ifelse(lower<0 & upper>0,0,NA)
  lineColor <- ifelse(upper<0,"firebrick1",ifelse(lower>0,"dodgerblue1",NA))
  dotcolors <- ifelse(mean>0,"dodgerblue4","firebrick")
  df <- data.frame(state, mean, lower, upper,zeropoint)
  
  TrumpOrHarris <- function(val){
    ifelse(val<0,paste0("Trump \n+",sprintf("%.2f%%",abs(val))),paste0("Harris \n+",sprintf("%.2f%%",val)))
  }
  if(!is.na(zeropoint)){
    ggplot(data=df) + geom_segment(aes(x=lower,xend=zeropoint,y=state,yend=state),colour="firebrick1",size=3) + 
      geom_segment(aes(x=zeropoint,xend=upper,y=state,yend=state),colour="dodgerblue1",size=3) + 
      geom_segment(aes(x=lower,xend=upper,y=state,yend=state),colour=lineColor,size=3) +
      geom_point(aes(x = mean, y = state),color=dotcolors,size = 3) + xlab(paste("95th Percentile Range in",state)) +
      geom_text(aes(x = mean, y = state, label = TrumpOrHarris(mean),vjust=2)) + 
      geom_text(aes(x = lower, y = state, label = TrumpOrHarris(lower),vjust=-1)) +
      geom_text(aes(x = upper, y = state, label = TrumpOrHarris(upper),vjust=-1)) +
      ylab("")+ theme_bw() + theme(axis.text.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.ticks.x=element_blank()) + 
      xlim(lower -5,upper +5)
  }else{
    ggplot(data=df) + 
      geom_segment(aes(x=lower,xend=upper,y=state,yend=state),colour=lineColor,size=3) +
      geom_point(aes(x = mean, y = state),color=dotcolors,size = 3) + xlab(paste("95th Percentile Range in",state)) +
      geom_text(aes(x = mean, y = state, label = TrumpOrHarris(mean),vjust=2)) + 
      geom_text(aes(x = lower, y = state, label = TrumpOrHarris(lower),vjust=-1)) +
      geom_text(aes(x = upper, y = state, label = TrumpOrHarris(upper),vjust=-1)) +
      ylab("")+ theme_bw() + theme(axis.text.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.ticks.x=element_blank())+
      xlim(ifelse(lower>0,-5,lower-5),ifelse(upper<0,5,upper+5))
  }
}

#function that calculates the margin for each tipping point state
tippingPointMargin <- function(simData){
  simData[simData["TippingPointState"]]
}

#generating a plot comparing two states data
comparisonPlot <- function(state1, state2){
  frameplot <- data.frame(state1,state2)
  frameplot$dotColor <- ifelse(state1 > 0 & state2 > 0, "dodgerblue4",ifelse(state1 <0 & state2<0, "firebrick","plum2"))
  ggplot(frameplot) + geom_point(aes(x=state1,y=state2),color=frameplot$dotColor) + theme_bw()
}

server <- function(input, output, session) {
  #calculate the electoral college bias
  bias <- mean(latestSimData$HarrisMargin-as.numeric(apply(latestSimData, 1, tippingPointMargin)),na.rm=TRUE)
  
  #header for page + info about when data was last updated
  output$header <- renderText({
    paste0("<h1>Welcome to Liam's 2024 Presidential Election Nowcast</h1><p1>Data last updated on ",
           paste(format(as.Date(timeStamp),format="%B %d, %Y"),"at",StrExtractBetween(timeStamp," ","\\.")),
           "</p>")
  })
  
  #get the state selection
  state <- reactive({
    latestSimData[,input$statePick]
  })
  
  #get the comparison state selection
  state2 <- reactive({
    latestSimData[,input$stateComparePick]
  })
  
  #plot the histogram of election outcomes
  output$ECHist <- renderPlot({
    histGenerator()
  }, res = 96)
  
  #plot the popular vote versus electoral college
  output$PopVsEC <- renderPlot({
    dotplotGenerator(bias)
  }, res = 96)
  
  #plot the ranges for every state
  output$forestPlot <- renderPlot({
    stateRangeGenerator()
  }, res = 96)
  
  #plot the selected state's range of outcomes
  output$stateForest <- renderPlot({
    singleStateLine(input$statePick)
  }, res = 96)
  
  #print a summary of the selected state's data
  output$summary <- renderText({
    #find odds of winning selected state
    winProb <- sum(state()>0)/4000
    
    #find odds state is the tipping point state
    tpoint <- sum(latestSimData$TippingPointState == input$statePick,na.rm=TRUE)/4000
    
    #calculate odds of winning overall based on winning state
    harrisConditionalWin <- sum(latestSimData[,input$statePick]>0 & latestSimData[,"ElectoralVotes"] > 269)/sum(latestSimData[,input$statePick]>0)
    trumpConditionalWin <- sum(latestSimData[,input$statePick]<0 & latestSimData[,"ElectoralVotes"] < 269)/sum(latestSimData[,input$statePick]<0)
    
    if(winProb < .5){
      outputString <- paste0("<br>Trump has a ",sprintf("%.1f%%",100-winProb*100)," chance of winning ",input$statePick,".")
    }else{
      outputString <- paste0("Harris has a ",sprintf("%.1f%%",winProb*100)," chance of winning ",input$statePick,".")
    }
    outputString <- paste0(outputString, "<p>",ifelse(winProb < 0.5,"His","Her")," average margin in ",input$statePick, " is ", sprintf("%.2f%%",abs(mean(state()))),".</p>")
    outputString <- paste0(outputString,"<p>",input$statePick, " is the tipping point in ",
                           sprintf("%.1f%%",tpoint*100)," of simulations.")
    if(!is.na(harrisConditionalWin)){
    outputString <- paste0(outputString,"<br>If Harris wins ",input$statePick," she has a ",
                           sprintf("%.1f%%",harrisConditionalWin*100)," chance of winning the election.")
    }
    if(!is.na(trumpConditionalWin)){
    outputString <- paste0(outputString,"<br>If Trump wins ", input$statePick," he has a ",
                           sprintf("%.1f%%",trumpConditionalWin*100),
                           " chance of winning the election.")
    }
    outputString <- paste0(outputString,"</p>")
    outputString
  })
  
  #describe the model at top of the page
  output$description <- renderText({
    HTML("<i>
    A deeper analysis of the model's performance is to come, but overall, the model seems to have performed moderately well
    in predicting the Electoral College (while it slightly favored Harris to win, the modal outcome was a Trump sweep of the swing states),
    but whiffed pretty terribly on the popular vote. Trump is poised to carry the popular vote by roughly 1.5%, and fewer than 2.5% of simulations
    predicted that outcome. While further analysis is needed, certainly one driver of this miss is the lack of discounting of TIPP's
    daily polling of the NPV, which diluted a more diverse set of polls that pointed to a Trump popular vote win. <br><br>
    This dashboard provides figures for the 2024 Presidential Election. Combining polling data from 538's
         database and state demographic data, the model runs 4,000 simulations on a daily or twice daily basis,
         and the results are then loaded into this dashboard in aggregate, providing a look at possible outcomes.
         <br><br> The model works by first taking an average of polls in each state since the start of the race, 
         weighting on the square root of the polls sample size, and discounting older polls by the number of weeks since they
         closed. The model then combines this data with a state's historical lean relative to the national vote polling
         and assigns a range of splits for undecided voters. <br> Unlike some other models, this one does not factor in economic
         fundamentals, nor does it weight based on percieved poll quality, although pollsters excluded by 538 (most
         notably Rasmussen), are not captured in the model. The model is also not forward looking, that is it does
         not project polling forward to forecast what changes might have happened by Election Day. Thus the model better approximates
         a nowcast than a forecast, although the two should converge as Election Day approaches.<br>
         <br>I hope you enjoy perusing the model as much as I have enjoyed constructing it.<br><br>-Liam</i>")
  })
  
  #describe the dot plot at the bottom of the page and give info about EC bias
  output$dotPlotDescription <- renderText({
    HTML(paste0("This chart plots each simulation's margin of lead for Kamala Harris in the popular vote against her 
    lead in the Electoral College. Because each state is run as an independent race, the Electoral College winner
         can often differ from the winner of the popular vote.<br><br> Polling data for this election indicate the
         Electoral College carries an R+",
                sprintf("%1.1f",bias)," bias. This means the model expects Harris needs to win the national popular 
                vote by at least ",sprintf("%.1f%%",bias)," to win the election. For comparison, the 2020 Electoral 
                College had a bias of R+3.8 and the 2016 Electoral College had a bias of R+2.9."))
  })
  
  #footer with credits
  output$footer <- renderText({
    paste0("<footer>Built by Liam C. Stewart, www.liamstewart.com <br>
    Polling data from 538. State partisan index data from Cook Political Report.<br>
           Built in R using Shiny.<br>
           Thanks to virtualstaticvoid for Heroku Buildpack.</footer>")
  })
  
  #plot selected state against comparison state
  output$comparisonPlot <- renderPlot({
    comparisonPlot(state(),state2())+xlab(paste(input$statePick,"margin (%)"))+ylab(paste(input$stateComparePick,"margin (%)"))
  })
  
  #compare first state selected to second state selected
  output$compare <- renderText({
    stateDiff <- mean(state()) - mean(state2())
    odds <- sum(state2()>0 & state()>0)/sum(state()>0)
    odds2 <- sum(state2()<0 & state()<0)/sum(state()<0)
    compareStr <- paste0("<p><br>",input$statePick," is about ",sprintf("%.1f%%",abs(stateDiff))," to the ",
                ifelse(stateDiff < 0,"right ","left "), "of ",input$stateComparePick,".<br>")
    if(!is.na(odds)){
        compareStr <- paste0(compareStr,"If Harris wins ", input$statePick, " her odds of winning ",input$stateComparePick, " 
                are ", sprintf("%.0f%%",abs(odds)*100),".<br>")
    }
    if(!is.na(odds2)){
                compareStr <- paste0(compareStr, "If Trump wins ", input$statePick, " his odds of winning ",
                input$stateComparePick, " are ", sprintf("%.0f%%",abs(odds2)*100),".</p>")
    }
    HTML(compareStr)
  })
  
  #output from clicking on the histogram
  output$histSelection <- renderText({
    xval <- input$histClick$x
    if(is.null(xval)){
      xval <- 268.5
    }
    buckets <- histGenerator()$breaks
    top <- buckets[min(which(xval < buckets))]
    bottom <- buckets[max(which(xval > buckets))]
    simCount <- sum(latestSimData$ElectoralVotes <= top & latestSimData$ElectoralVotes > bottom)
    if(xval > 268 & xval < 270){
      HTML(paste0(sprintf("%.2f%%",simCount/40)," of simulations result in a tie.<br><br>"))
    }else{
      if(round(top)<269){
        HTML(paste0("Trump wins between ", 538-round(top-.50001), " and ", 538 - round(bottom), " electoral votes in ",
                    sprintf("%.2f%%",simCount/40)," of simulations.<br><br>"))
      }else{
        HTML(paste0("Harris wins between ", round(bottom+.50001), " and ", round(top), " electoral votes in ",
                    sprintf("%.2f%%",simCount/40)," of simulations.<br><br>"))
      }
    }
  })
  
  #output from clicking on the forestplot
    output$info <- renderText({
      if(is.null(input$forestClick$y)){
        HTML("")
      }else{
        margin <- mean(latestSimData[,yaxis[ifelse((55-round(input$forestClick$y)<0),0,55-round(input$forestClick$y))]])
        if(is.na(margin)){
          HTML("")
        }else{
        HTML(paste0("The average predicted margin in ",yaxis[55-round(input$forestClick$y+.01)], " is ",
                    ifelse(margin < 0, "Trump +","Harris +"),abs(round(margin,2))))
        }
      }
    })
  
  #code the prevents heroku from timing app out until after 5 mins
  count <- reactiveValues(i = 0)
  autoInvalidate <- reactiveTimer(5000)
  observe({
    if(isolate(count$i<60)){
      autoInvalidate()
    }
    cat(isolate(count$i))
    isolate(count$i <- count$i + 1)
  })
}
