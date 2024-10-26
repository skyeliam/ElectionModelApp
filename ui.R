#a shiny app for displaying the results of the election simulation
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(shinydashboard)
library(DescTools)

latestSimData <- read.csv(url("https://raw.githubusercontent.com/skyeliam/2024ElectionModel/refs/heads/main/Latest_Election%20Simulation%20Data.csv"))
colnames(latestSimData) <- str_replace_all(colnames(latestSimData),"\\."," ")
timeStamp <- StrExtractBetween(read_file(url(
  "https://raw.githubusercontent.com/skyeliam/2024ElectionModel/refs/heads/main/timestamp.txt"))," ","\n")
#for the drop down menu to get state level data
stateOptions <- c(state.name,"Maine CD 2","Maine CD 2","Nebraska CD 2")

#defining UI
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(
                      title = "Liam's 2024 Election Nowcast",
                      titleWidth = "calc(100% - 44px)"),
                    dashboardSidebar(disable=TRUE),
                    dashboardBody(
                      htmlOutput("header"),
                      fluidRow(tabItem(tabName = "Election Plots",
                                       box(htmlOutput("description"),width=12),
                                       box(
                        title = p(strong(paste("Harris wins",sprintf("%.1f%%",sum(latestSimData$ElectoralVotes>=270)/40),"of Electoral College simulations")),
                                  br(),"Harris wins the popular vote in ",
                                  sprintf("%.1f%%",sum(latestSimData$HarrisPopVote>latestSimData$TrumpPopVote)/4000*100),
                                  " of those sims"),
                                  plotOutput("ECHist",click = "histClick",height = "400px"),
                              htmlOutput("histSelection"),
                                  plotOutput("forestPlot",click = "forestClick",
                                              height="800px"),
                              htmlOutput("info")),
                              box(selectInput("statePick", "State/district level statistics:", 
                                              choices = stateOptions,selected="Pennsylvania"),
                                  plotOutput("stateForest",height="200px"),
                                  htmlOutput("summary"),
                                  selectInput("stateComparePick",HTML("<br>Compare to:"),
                                              choices = stateOptions,selected="Michigan"),
                                  plotOutput("comparisonPlot",height="400px",width="100%"),
                                  htmlOutput("compare")),
                              box(plotOutput("PopVsEC"),width=12,
                                  title = p(strong("Popular vote margin vs. Electoral College margin")),
                                  htmlOutput("dotPlotDescription")))),
                      htmlOutput("footer")
                    ))