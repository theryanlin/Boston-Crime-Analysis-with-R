#Descriptive
  #1.time series
  #2.geographic distribution of crimes
  #3Pareto chart for crime types
#inferential statistics 
  #1.Hypothesis Test
  #2.MLA
  #3.Logistics regression
  #4.Chi-Square Test 
  #5.Cluster Analysis

library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(matrixStats)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(MASS)
library(readxl)
library(leaflet)
library(leaflet.extras)
library(GGally)
library(plotly)
library(hrbrthemes)
library(lubridate)


df_by_year <- read_csv("Inferential_Analysis.csv")
df_by_month <- read_csv("Data.csv")

data <- read_csv('crimewithcat2.csv')
data2 <- read_csv('final data 2.csv')
data3 <- data2[,c(7,10,16,17)]%>%
na.omit()
df_ts <- read.csv("timeseries-final.csv")
df_yearly <- read.csv("YearlyBarchart.csv")
df_yearly_cat <- read.csv("CategoricalYearly.csv")
df_shoot <- read.csv("ShootingCount.csv")
df_shoot_cat_yearly <- read.csv("YearlyCategorizedShooting.csv")
df_shoot_assoc <- read.csv("CategoricalShootingRate.csv")
df_year_comp <- read.csv("yearlyComposition.csv")


#-------------------dashboard sidebar------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction" ,tabName = "Introduction", icon=icon("globe")),
    menuItem("Descriptive Statistics" ,tabName = "DescriptiveStatistics", icon=icon("globe")),
    menuItem("Inferential Statistics" ,tabName = "InferentialSatistics", icon=icon("globe"))
  )
)

#-----------------------body---------------
body <- dashboardBody(
  tabItems(
    #-------------Introduction
    tabItem(tabName = 'Introduction',
            fluidPage(
              titlePanel("Crime in Boston"),
              sidebarLayout(
                sidebarPanel( 
                  selectInput("year", "Year:",
                              choices = 
                                list('All' = list(2016,2017,2018,2019,2020))),
                  selectInput("shoot","Shooting or not:",
                              choices = 
                                list('All' = list(0,1))),
                  
                  
                  Position = "top"
                ),
                mainPanel(
                  leafletOutput("map", height = 500)
                )
              ),
              
            )
    ),
    
    #-------------Descriptive Statistics
    tabItem(
      tabName = 'DescriptiveStatistics',
      fluidPage(
        titlePanel("Descriptive Statistics"),
        fluidRow(
          column(
            width = 12,
            tabsetPanel(tabPanel("Time Series",box(plotOutput("time"))),
                        tabPanel("Yearly Report", 
                                 selectInput(inputId = "cat", label =  "Crime categories:",
                                             choices = list("Yearly Crime Report" = "df_yearly$Count", "Yearly Shooting Report" = "df_shoot$Shootings"),
                                             selected = "Yearly Crime Report"),
                                 box(plotOutput("year"))),
                        tabPanel("Total Reported Crimes per category",
                                 selectInput(inputId = "cate", label =  "With/Withour Other:",
                                             choices = list("With Other" = "df_yearly", "Without Other" = "df_yearly_cat"),
                                             selected = "With Other"),
                                             box(plotOutput("cate"))),
                        tabPanel("Yearly Crime Categorical Composition", 
                                 selectInput(inputId = "y", label =  "Year:",
                                             choices = list("2016" = "2016", "2017" = "2017", "2018" = "2018", "2019" = "2019",
                                                            "2020" = "2020"),
                                             selected = "2016"),
                                 box(plotOutput("cate2"))),
                        tabPanel("Shooting Rates", box(plotOutput("sr")))
                           
                
                )
     
                
           )))),
    #-------------Inferential Statistics
    tabItem(
      tabName = "InferentialSatistics",
      fluidPage(
        titlePanel("Inferential Satistics"),
        fluidRow(
          column(
            width=12,
            tabsetPanel(tabPanel("Heatmap",box(plotOutput("heatmap"), box(width = 800, height = 800,verbatimTextOutput("correlationtest")))),
                        tabPanel("Regression1", box(plotOutput("regression1"), box(width = 800, height = 800, verbatimTextOutput("reg1")))),
                        tabPanel("Regression2", box(plotOutput("regression2"), box(width = 800, height = 800, verbatimTextOutput("reg2")))),
                        tabPanel("T Test",
                                 selectInput(inputId = "crimecate", label =  "Crime categories:",
                                             choices = list("Shooting" = "Number_Shooting", "Drugs/alcohol abuse" = "Drugs / Alcohol Abuse","Sex Offence"="Sex Offence",
                                                            "Monetary" = "Monetary", "Motor Vehicle Accident/Violation" = "Motor Vehicle Accident / Violation", "Other" = "Other","Weapons/Firearms" = "Weapons/Firearms",
                                                            "Violence/Risk of Safety" = "Violence / Risk of Safety"),
                                             selected = "Shooting"
                                 ),
                                 box(plotOutput("plotTtest")
                                 ,
                                 box(width = 800, height = 800, verbatimTextOutput("ttest")))
                        )
                                 
                
                                 
                )
                )
            ))
  
    )
    
  ))
#----------------------UI-------------------------

ui <-dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Boston Crime"),
  sidebar,
  body
)
#-----------------------server---------------
server <-function(input, output){

  output$map <- renderLeaflet({
    data3 %>%
      filter(`YEAR` == input$year,`SHOOTING` == input$shoot) %>%
      leaflet() %>%
      setView(lng = -71.07208833, lat = 42.33926764, zoom = 11) %>%
      addTiles() %>%
      addHeatmap(lng = ~Long, lat = ~Lat, radius = 8)
  })  
  
  output$time <- renderPlot({
    df_ts$Date <- ymd(df_ts$Date)
    p_shiny <- ggplot(df_ts, aes(x=df_ts$Date, y=df_ts$Count)) +
      geom_area(fill="#69b3a2", alpha=0.5) +
      geom_line(color="#69b3a2") +
      ylab("Daily Reported Crimes") +
      xlab("Date")+
      theme_ipsum()
    print(p_shiny)
    
  }) 
  
  output$year <- renderPlot({
    if(input$cat=="df_yearly$Count")
    {
    p2_shiny <-ggplot(df_yearly, aes(x=Year,y=Count)) +
      geom_bar(stat='identity',color="blue", fill=rgb(0.1,0.4,0.5,0.7))+
      ylab("Total Reported Crimes")+
      geom_text(aes(label = Count), vjust = -0.2)
    
    p2_shiny}
    else if(input$cat =="df_shoot$Shootings")
    {
     p4 <-ggplot(df_shoot, aes(x=Year,y=Shootings)) +
      geom_bar(stat='identity',color="blue", fill=rgb(0.1,0.4,0.5,0.7))+
      ylab("Total Shootings")
     p4}
    
  }) 
  
  output$cate <- renderPlot({
    if(input$cate=="df_yearly")
      {p3 <- ggplot(df_yearly_cat,aes(fill=Cat,y=Count,x=Year))+
        geom_bar(position='dodge',stat='identity')+
        ylab("Total Reported Crimes")
    p3}
    else if(input$cate =="df_yearly_cat")
      {df_yearly_cat2 <- slice(df_yearly_cat,-(31:35))
      p3b <- ggplot(df_yearly_cat2, aes(fill=Cat, y = Count, x = Year))+
        geom_bar(position = "dodge", stat = 'identity')+
        ylab("Total Reported Crimes")
      p3b}
 
  }) 
  
  output$cate2 <- renderPlot({
    if(input$y=="2016")
    {
      input3 <- 2016
      df_year_comp_filtered <- df_year_comp[df_year_comp$Year==input3,]
      p7_shiny <- ggplot(df_year_comp_filtered, aes(x = Category, y = Percentage)) +
        geom_col() +
        geom_text(aes(label = paste(Percentage,"%",sep="")), vjust = -0.2)
      p7_shiny}
    else if(input$y =="2017")
    {
      input3 <- 2017
      df_year_comp_filtered <- df_year_comp[df_year_comp$Year==input3,]
      p7_shiny <- ggplot(df_year_comp_filtered, aes(x = Category, y = Percentage)) +
        geom_col() +
        geom_text(aes(label = paste(Percentage,"%",sep="")), vjust = -0.2)
      p7_shiny}
    else if(input$y =="2018")
    {
      input3 <- 2018
      df_year_comp_filtered <- df_year_comp[df_year_comp$Year==input3,]
      p7_shiny <- ggplot(df_year_comp_filtered, aes(x = Category, y = Percentage)) +
        geom_col() +
        geom_text(aes(label = paste(Percentage,"%",sep="")), vjust = -0.2)
      p7_shiny}
    else if(input$y =="2019")
    {
      input3 <- 2019
      df_year_comp_filtered <- df_year_comp[df_year_comp$Year==input3,]
      p7_shiny <- ggplot(df_year_comp_filtered, aes(x = Category, y = Percentage)) +
        geom_col() +
        geom_text(aes(label = paste(Percentage,"%",sep="")), vjust = -0.2)
      p7_shiny}
    else if(input$y =="2020")
    {
      input3 <- 2020
      df_year_comp_filtered <- df_year_comp[df_year_comp$Year==input3,]
      p7_shiny <- ggplot(df_year_comp_filtered, aes(x = Category, y = Percentage)) +
        geom_col() +
        geom_text(aes(label = paste(Percentage,"%",sep="")), vjust = -0.2)
      p7_shiny}
    
  })  
  
  output$sr <- renderPlot({
    p6 <-ggplot(df_shoot_assoc, aes(x=Category,y=ShootingRate)) +
      geom_bar(stat='identity',color="blue", fill=rgb(0.1,0.4,0.5,0.7))+
      ylab("Rate of Shootings")+
      geom_text(aes(label = ShootingRate),vjust = -0.2)
    p6
  })
  
  
  output$heatmap <-renderPlot({
    ggcorr(data=df_by_year, palette = "RdYlGn", label = TRUE, 
           hjust = .85, size = 3, label_color = "black")
    
  })  
  
  output$correlationtest <-renderPrint({
    cortest <- cor.test(df_by_year$No_Shooting, df_by_year$Rate_Unemployment, 
             method=c("pearson"))
    print(cortest)

    
  })  
  
  output$regression1 <- renderPlot({
    before_covid <- subset(df_by_month, Number_Covid='NA')
    after_covid <- subset(df_by_month, Number_Covid!='NA')
    scatter.smooth(x=after_covid$Rate_Unemployment , y=after_covid$Number_Shooting, 
                   main="Number_Shooting ~ Rate_Unemployment")
    })
  
  output$reg1 <-renderPrint({
    a = lm(Number_Shooting ~ Rate_Unemployment, data = df_by_month,)
    print(summary(a))
    
    
  })  
  
  output$regression2 <- renderPlot({
    scatter.smooth(x=df_by_month$Rate_Unemployment , y=df_by_month$Number_Shooting, 
                   main="Number_Shooting ~ Rate_Unemployment")
  })
  
  output$reg2 <-renderPrint({
    b = lm(Number_Shooting ~ Rate_Unemployment, data = df_by_month,)
    print(summary(b))
    
    
  })  
  
  output$plotTtest <- renderPlot({
    df_by_month$Date <- as.Date(df_by_month$Date, '%Y/%m/%d')
    before_covid <- subset(df_by_month, Number_Covid='NA')
    after_covid <- subset(df_by_month, Number_Covid!='NA')
    if(input$crimecate =="Number_Shooting")
    {
    plot(df_by_month$Date, df_by_month$Number_Shooting, pch=19, type="o", col="black", xlab="Date", ylab="Number") 
    abline(h=c(mean(before_covid$Number_Shooting), mean(after_covid$Number_Shooting)), col=c("red", "blue"), lty=c(1,2))  
    legend("topleft", legend=c("Mean before Covid","Mean After Covid"), col=c("red", "blue"), lty=1:2, cex=0.8) 
    title(main="Number of Shooting")
    }
    else if(input$crimecate =="Drugs / Alcohol Abuse")
    {
    plot(df_by_month$Date, df_by_month$`Drugs / Alcohol Abuse`, pch=19, type="o", col="black", xlab="Date", ylab="Number") 
    abline(h=c(mean(before_covid$`Drugs / Alcohol Abuse`), mean(after_covid$`Drugs / Alcohol Abuse`)), col=c("red", "blue"), lty=c(1,2))  
    legend("topleft", legend=c("Mean before Covid","Mean After Covid"), col=c("red", "blue"), lty=1:2, cex=0.8) 
    title(main="Drugs / Alcohol Abuse")
    }
    else if(input$crimecate =="Sex Offence")
    {
    plot(df_by_month$Date, df_by_month$`Sex Offence`, pch=19, type="o", col="black", xlab="Date", ylab="Number") 
    abline(h=c(mean(before_covid$`Sex Offence`), mean(after_covid$`Sex Offence`)), col=c("red", "blue"), lty=c(1,2))  
    legend("topleft", legend=c("Mean before Covid","Mean After Covid"), col=c("red", "blue"), lty=1:2, cex=0.8) 
    title(main="Sex Offence")
    }
    else if(input$crimecate =="Monetary")
    {
    plot(df_by_month$Date, df_by_month$Monetary, pch=19, type="o", col="black", xlab="Date", ylab="Number") 
    abline(h=c(mean(before_covid$Monetary), mean(after_covid$Monetary)), col=c("red", "blue"), lty=c(1,2))  
    legend("topleft", legend=c("Mean before Covid","Mean After Covid"), col=c("red", "blue"), lty=1:2, cex=0.8) 
    title(main="Monetary")
    }
    else if(input$crimecate =="Motor Vehicle Accident / Violation")
    {
    plot(df_by_month$Date, df_by_month$`Motor Vehicle Accident / Violation`, pch=19, type="o", col="black", xlab="Date", ylab="Number") 
    abline(h=c(mean(before_covid$`Motor Vehicle Accident / Violation`), mean(after_covid$`Motor Vehicle Accident / Violation`)), col=c("red", "blue"), lty=c(1,2))  
    legend("topleft", legend=c("Mean before Covid","Mean After Covid"), col=c("red", "blue"), lty=1:2, cex=0.8) 
    title(main="Motor Vehicle Accident/Violation")
    }
    else if(input$crimecate =="Other")
    {
    plot(df_by_month$Date, df_by_month$Other, pch=19, type="o", col="black", xlab="Date", ylab="Number") 
    abline(h=c(mean(before_covid$Other), mean(after_covid$Other)), col=c("red", "blue"), lty=c(1,2))  
    legend("topleft", legend=c("Mean before Covid","Mean After Covid"), col=c("red", "blue"), lty=1:2, cex=0.8) 
    title(main="Other")
    }
    else if(input$crimecate =="Weapons/Firearms")
    {
    plot(df_by_month$Date, df_by_month$`Weapons/Firearms`, pch=19, type="o", col="black", xlab="Date", ylab="Number") 
    abline(h=c(mean(before_covid$`Weapons/Firearms`), mean(after_covid$`Weapons/Firearms`)), col=c("red", "blue"), lty=c(1,2))  
    legend("topleft", legend=c("Mean before Covid","Mean After Covid"), col=c("red", "blue"), lty=1:2, cex=0.8) 
    title(main="Weapons/Firearms")
    }
    else if(input$crimecate =="Violence / Risk of Safety")
    {
    plot(df_by_month$Date, df_by_month$`Violence / Risk of Safety`, pch=19, type="o", col="black", xlab="Date", ylab="Number") 
    abline(h=c(mean(before_covid$`Violence / Risk of Safety`), mean(after_covid$`Violence / Risk of Safety`)), col=c("red", "blue"), lty=c(1,2))  
    legend("topleft", legend=c("Mean before Covid","Mean After Covid"), col=c("red", "blue"), lty=1:2, cex=0.8) 
    title(main="Violence/Risk of Safety")
    }
  })
 
  output$ttest <-renderPrint({
    before_covid <- subset(df_by_month, Number_Covid='NA')
    after_covid <- subset(df_by_month, Number_Covid!='NA')
    if(input$crimecate =="Number_Shooting")
    {t1 <- t.test(before_covid$`Number_Shooting`, after_covid$`Number_Shooting`, 
                  "two.sided", var.equal=TRUE, conf.level=0.95)
    print(t1)
    }
    else if(input$crimecate =="Drugs / Alcohol Abuse")
    {t2 <- t.test(before_covid$`Drugs / Alcohol Abuse`, after_covid$`Drugs / Alcohol Abuse`, 
                  "two.sided", var.equal=TRUE, conf.level=0.95)	
     print(t2)
    }
    else if(input$crimecate =="Sex Offence")
    {t3 <- t.test(before_covid$`Sex Offence`, after_covid$`Sex Offence`, 
             "two.sided", var.equal=TRUE, conf.level=0.95)
     print(t3)
    }
    else if(input$crimecate =="Monetary")
    {t4 <- t.test(before_covid$Monetary, after_covid$Monetary, 
                  "two.sided", var.equal=TRUE, conf.level=0.95)
    print(t4)
    }
    else if(input$crimecate =="Motor Vehicle Accident / Violation")
    {t5 <- t.test(before_covid$`Motor Vehicle Accident / Violation`, after_covid$`Motor Vehicle Accident / Violation`, 
                  "two.sided", var.equal=TRUE, conf.level=0.95)
    print(t5)
    }
    else if(input$crimecate =="Other")
    {t6 <- t.test(before_covid$Other, after_covid$Other, 
                  "two.sided", var.equal=TRUE, conf.level=0.95)
    print(t6)
    }
    else if(input$crimecate =="Weapons/Firearms")
    {t7 <- t.test(before_covid$`Weapons/Firearms`, after_covid$`Weapons/Firearms`, 
                  "two.sided", var.equal=TRUE, conf.level=0.95)
    print(t7)
    }
    else if(input$crimecate =="Violence / Risk of Safety")
    {t8 <- t.test(before_covid$`Violence / Risk of Safety`, after_covid$`Violence / Risk of Safety`, 
                  "two.sided", var.equal=TRUE, conf.level=0.95)
    print(t8)
    }
  })  
  
}
  
shinyApp(ui, server)

