rm(list=ls())
library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggpubr)
library(knitr)
library(tidyverse)
options(digits = 22)
setwd("D:\\Git\\FIH")
#------------------------------title-----------------------------------------------#
header <- dashboardHeader(
  title = "DosePredict"
)

#----------------------------siderbar----------------------------------------------#
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("NOAEL", tabName = "noael", icon = icon("dashboard")),
    menuItem("Allometric scaling & CL-FCIM", tabName = "allometricscaling", icon = icon("keyboard")),
    menuItem("CL-TS(rat-dog)", tabName = "clts", icon = icon("comment-dots")),
    menuItem("固定指数法", tabName = "exponent", icon = icon("gear"))
  ), width = 300
)
body <- dashboardBody(tabItems(
  # First tab content
  tabItem(tabName = "noael",                                                                                      #@@补充参考值表，参考指导原则
          fluidRow(
            box(title = "Mouse:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE,
                numericInput("BW_m", "The weight of mouse(kg):", 0.020, min = 0, max = Inf,step = 0.00000000001),
                numericInput("Dose_m", "The dose of mouse(mg/kg):", 15, min = 0, max = Inf,step = 0.00000000001)
            ),
            box(title = "Rat:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE,
                numericInput("BW_r", "The weight of rat(kg):", 0.30, min = 0, max = 10000,step = 0.00000000001),  #@@大鼠默认0.3kg
                numericInput("Dose_r", "The dose of rat(mg/kg):", 50, min = 0, max = Inf,step = 0.00000000001)
            ),
            box(title = "Begale:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE,    
                numericInput("BW_b", "The weight of begale(kg):", 10, min = 0, max = 10000,step = 0.00000000001),
                numericInput("Dose_b", "The dose of begale(mg/kg):", 15, min = 0, max = 10000,step = 0.00000000001)
            ),
            box(title = "Monkey:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE, 
                numericInput("BW_mo", "The weight of monkey(kg):", 3, min = 0, max = 10000,step = 0.00000000001),  
                numericInput("Dose_mo", "The dose of monkey(mg/kg):", 10, min = 0, max = 10000,step = 0.00000000001)
            ),
            box(title = "Human:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE, 
                numericInput("BW_h", "The weight of human(kg):", 60, min = 0, max = 10000,step = 0.00000000001)   
            ),
            box(title = "Safety factor", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE, 
                numericInput("Factor", "factor:", 10, min = 0, max = 10000,step = 0.00000000001)
            )
          ),
          fluidRow(
            column(6, dataTableOutput("table1")
            ),
            column(6, img(src = "www/p1.png", width = "100%")
            )
          )
  ),
  # Second tab content
  tabItem(tabName = "allometricscaling",                     #@@补充参考值表，#食蟹猴5kg#大鼠0.3kg#小鼠0.03kg#人体重建议70kg #@@删除二种属的“Allometric scaling”结果的展示
          tabBox(id = "tabset1", width = "100%",             #@@补充a值、b值、人体CL值   #@@"The AUC of animal(mg·min/ml):"这部分更改描述，直接点描述（"参考AUC"，"预估人体AUC"之类的）或者直接只写个AUC
                 tabPanel("Two animals",
                          tags$style(type='text/css', 
                                     ".nav-tabs {font-size: 20px} "),
                          column(4, br(),
                                 fluidRow(
                                   box(title = "Animal1:", status = "primary", solidHeader = TRUE,width = 12,
                                       collapsible = TRUE,
                                       numericInput("BW21", "The weight of animal1(kg):", 1, min = 0, max = Inf,step = 0.00000000001),
                                       numericInput("PK21", "The CL of animal1(ml/min):", 10, min = 0, max = Inf,step = 0.00000000001)
                                   )),
                                 br(),
                                 fluidRow(
                                   box(title = "Animal2:", status = "primary", solidHeader = TRUE,width = 12,
                                       collapsible = TRUE,
                                       numericInput("BW22", "The weight of animal2(kg):", 10, min = 0, max = Inf,step = 0.00000000001),
                                       numericInput("PK22", "The CL of animal2(ml/min):", 100, min = 0, max = Inf,step = 0.00000000001)
                                   )),
                                 br(),
                                 fluidRow(
                                   box(title = "Human:", status = "primary", solidHeader = TRUE,width = 12, 
                                       collapsible = TRUE,
                                       numericInput("BW2_h", "The weight of human(kg):", 60, min = 0, max = Inf,step = 0.00000000001)
                                   )),
                                 fluidRow(
                                   box(title = "Human:", status = "primary", solidHeader = TRUE,width = 12,
                                       collapsible = TRUE,
                                       numericInput("AUC2", "The AUC of animal(mg·min/ml):", 60, min = 0, max = Inf,step = 0.00000000001)),
                                   box(title = "Fraction unbound in rat", status = "primary", solidHeader = TRUE,width = 12,
                                       collapsible = TRUE,
                                       numericInput("fu_r2", "fu_r:", 0.4, min = 0, max = 1,step = 0.00000000001)
                                   ),
                                   box(title = "Fraction unbound in human", status = "primary", solidHeader = TRUE,width = 12, 
                                       collapsible = TRUE,
                                       numericInput("fu_h2", "fu_h:", 0.6, min = 0, max = 1,step = 0.00000000001))
                                 )
                          ),
                          column(8,br(),
                                 fluidRow(
                                   box(dataTableOutput("table2"), width = 12)
                                 ),
                                 fluidRow(
                                   box(plotOutput("plot2"), width = 12)
                                 )
                          )
                 ),
                 tabPanel("Three animals",
                          tags$style(type='text/css', 
                                     ".nav-tabs {font-size: 20px} "),
                          fluidRow(box(title = "Animal1:", status = "primary", solidHeader = TRUE,width = 3,
                                       collapsible = TRUE,
                                       numericInput("BW31", "The weight of animal1(kg):", 1, min = 0, max = Inf,step = 0.00000000001),
                                       numericInput("PK31", "The CL of animal1(ml/min):", 10, min = 0, max = Inf,step = 0.00000000001)
                          ),
                          box(title = "Animal2:", status = "primary", solidHeader = TRUE,width = 3,
                              collapsible = TRUE,
                              numericInput("BW32", "The weight of animal2(kg):", 10, min = 0, max = Inf,step = 0.00000000001),
                              numericInput("PK32", "The CL of animal2(ml/min):", 100, min = 0, max = Inf,step = 0.00000000001)
                          ),
                          box(title = "Animal3:", status = "primary", solidHeader = TRUE,width = 3,
                              collapsible = TRUE,
                              numericInput("BW33", "The weight of animal3(kg):", 4, min = 0, max = Inf,step = 0.00000000001),
                              numericInput("PK33", "The CL of animal3(ml/min):", 21.2, min = 0, max = Inf,step = 0.00000000001)
                          ),
                          box(title = "Human:", status = "primary", solidHeader = TRUE,width = 3,
                              collapsible = TRUE,
                              numericInput("BW3_h", "The weight of human(kg):", 60, min = 0, max = Inf,step = 0.00000000001)
                          )
                          ),
                          fluidRow(
                            box(title = "The AUC of animal", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("AUC3", "AUC(mg·min/ml):", 60, min = 0, max = Inf,step = 0.00000000001)
                            ),
                            box(title = "Fraction unbound of rat", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("fu_r3", "fu_r:", 0.4, min = 0, max = 1,step = 0.00000000001)
                            ),
                            box(title = "Fraction unbound of human", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("fu_h3", "fu_h:", 0.6, min = 0, max = 1,step = 0.00000000001))
                          ),
                          fluidRow(
                            box(dataTableOutput("table3"), width = 6),
                            box(plotOutput("plot3"), width = 6)
                            
                          )
                 ),
                 tabPanel("Four animals",
                          tags$style(type='text/css', 
                                     ".nav-tabs {font-size: 20px} "),
                          fluidRow(box(title = "Animal1:", status = "primary", solidHeader = TRUE,width = 3,
                                       collapsible = TRUE,
                                       numericInput("BW41", "The weight of animal1(kg):", 1, min = 0, max = Inf,step = 0.00000000001),
                                       numericInput("PK41", "The PK parameter of animal1(kg):", 10, min = 0, max = Inf,step = 0.00000000001)
                          ),
                          box(title = "Animal2:", status = "primary", solidHeader = TRUE,width = 3,
                              collapsible = TRUE,
                              numericInput("BW42", "The weight of animal2(kg):", 10, min = 0, max = Inf,step = 0.00000000001),
                              numericInput("PK42", "The PK parameter of animal2(kg):", 100, min = 0, max = Inf,step = 0.00000000001)
                          ),
                          box(title = "Animal3:", status = "primary", solidHeader = TRUE,width = 3,
                              collapsible = TRUE,
                              numericInput("BW43", "The weight of animal3(kg):", 4, min = 0, max = Inf,step = 0.00000000001),
                              numericInput("PK43", "The PK parameter of animal3(kg):", 21.2, min = 0, max = Inf,step = 0.00000000001)
                          ),
                          box(title = "Animal4:", status = "primary", solidHeader = TRUE,width = 3,
                              collapsible = TRUE,
                              numericInput("BW44", "The weight of animal4(kg):", 4, min = 0, max = Inf,step = 0.00000000001),
                              numericInput("PK44", "The PK parameter of animal4(kg):", 24, min = 0, max = Inf,step = 0.00000000001))
                          ),
                          fluidRow( 
                            box(title = "Human:", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("BW4_h", "The weight of human(kg):", 60, min = 0, max = Inf,step = 0.00000000001)
                            ),
                            box(title = "Fraction unbound of rat", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("fu_r4", "fu_r:", 0.4, min = 0, max = 1,step = 0.00000000001)
                            ),
                            box(title = "Fraction unbound of human", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("fu_h4", "fu_h:", 0.6, min = 0, max = 1,step = 0.00000000001)
                            ),
                            box(title = "The AUC of animal", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("AUC4", "AUC(mg·min/ml):", 60, min = 0, max = Inf,step = 0.00000000001)
                            )
                          ),
                          fluidRow(
                            box(dataTableOutput("table4"), width = 6),
                            box(plotOutput("plot4"), width = 6)
                            
                          )
                 )
          )
  ),
  # Third tab                                                                                                    #@@把参考体重改一改，也列一个参考值表  #@@The PK parameter of rat单位更改  #@@#@@补充a值、b值、人体CL值  #@@更改结果单位显示更清楚   #@@"The AUC of animal(mg·min/ml):"这部分更改描述，直接点描述（"参考AUC"，"预估人体AUC"之类的）或者直接只写个AUC
  tabItem(tabName = "clts",
          fluidRow(
            box(title = "Rat:", status = "primary", solidHeader = TRUE,width = 4,
                collapsible = TRUE,
                numericInput("BW51", "The weight of rat(kg):", 1, min = 0, max = Inf,step = 0.00000000001),
                numericInput("PK51", "The PK parameter of rat(kg):", 10, min = 0, max = Inf,step = 0.00000000001)
            ),
            box(title = "Dog:", status = "primary", solidHeader = TRUE,width = 4,
                collapsible = TRUE,
                numericInput("BW52", "The weight of dog(kg):", 10, min = 0, max = Inf,step = 0.00000000001),
                numericInput("PK52", "The PK parameter of dog(kg):", 100, min = 0, max = Inf,step = 0.00000000001)
            ),
            box(title = "Human:", status = "primary", solidHeader = TRUE,width = 4,
                collapsible = TRUE,
                numericInput("BW5_h", "The weight of human(kg):", 10, min = 0, max = Inf,step = 0.00000000001),
                numericInput("AUC5", "The AUC of animal(mg·min/ml)", 3, min = 0, max =Inf, step = 0.00000000000001)
            )
          ),
          fluidRow(
            box(dataTableOutput("table5"), width = 6),
            box(plotOutput("plot5"), width = 6)
          )
  ),
  #Fourth tab
  tabItem(tabName = "exponent",
          fluidRow(
            box(title = "Animal:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE,
                numericInput("CL6", "CL(ml/min):", 0.020, min = 0, max = Inf,step = 0.00000000001),
                numericInput("BW6", "Weight(kg):", 15, min = 0, max = Inf,step = 0.00000000001),
                numericInput("AUC6", "AUc(mg·min/ml):", 0.15, min = 0, max = 10000,step = 0.00000000001)
            ),
            box(title = "Human:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE,
                numericInput("BW6_h", "The weight of human(kg):", 0.15, min = 0, max = 10000,step = 0.00000000001)
            ),
            box(dataTableOutput("table6")
            )
          )
  )
)
)

ui<-dashboardPage(header, sidebar, body,skin = "red")  
server<-function(input,output){
  #---------------------------------------------------------------------------------------------------------#
  #---------------------------------------------Noael method------------------------------------------------#
  #---------------------------------------------------------------------------------------------------------#
  BW <- reactive(1000*c(input$BW_m, input$BW_r, input$BW_b, input$BW_mo, input$BW_h))  #g                     #@@标注人的HED剂量和MRSD剂量，用括号或其他符号标注清楚【单位】
  Dose <- reactive(c(input$Dose_m, input$Dose_r, input$Dose_b, input$Dose_mo))  #mg/kg
  Km <- reactive(10*BW()/10^(0.698*log(BW(),10)+0.8762))
  Dose_h <- reactive(Dose()/(Km()[5]/Km()[1:4])/input$Factor)
  data <- reactive(tibble("Method" = c("Mouse", "Rat", 
                                       "Begale","Monkey"),
                              "HED_human[mg]" = Dose_h()*input$Factor,
                              "MRSD_human[mg]" = Dose_h()
  )
  )
  #.........@@                                                                                               #@@此段作为参考，表头要标注清楚
  #data <- reactive(data.frame("Method" = c("Mouse", "Rat", 
  #                                         "Begale","Monkey"),
  #                            "HED_human(mg)" = Dose_h()*input$Factor,
  #                            "MRSD_human(mg)" = Dose_h()
  #))
  #.........@
  #---------------------------------------------------------------------------------------------------------#
  #--------------------------------------------AS method&FICM-----------------------------------------------#
  #---------------------------------------------------------------------------------------------------------#  #@@全部代码增加些注释，注释关键的点，方便以后维护
  # two animals
  PK2 <- reactive(c(input$PK21, input$PK22))
  BW2 <- reactive(c(input$BW21, input$BW22))
  model2 <- reactive(lm(log(PK2(), base = 10)~log(BW2(), base = 10)))
  R2 <- reactive(summary(model2())$r.squared)
  coefficients2 <- reactive(coef(model2()))
  a2 <- reactive(10**coefficients2()[[1]])
  b2 <- reactive(coefficients2()[[2]])
  PK_simple <- reactive(a2()*input$BW2_h**b2())
  PK_simple2 <- reactive(33.35*(a2()*input$fu_h2/input$fu_r2)**0.77)
  Dose2 <- reactive(PK_simple()*input$AUC2)
  Dose22 <- reactive(PK_simple2()*input$AUC2)
  data2 <- reactive(data.frame("Method" = c("Allometric scaling", "fu Intercept Correction Method"),
                               "Dose_h(mg)" = c(Dose2(), Dose22())))
  # three animals
  PK3 <- reactive(c(input$PK31, input$PK32, input$PK33))
  BW3 <- reactive(c(input$BW31, input$BW32, input$BW33))
  model3 <- reactive(lm(log(PK3(), base = 10)~log(BW3(), base = 10)))
  R3 <- reactive(summary(model3())$r.squared)
  coefficients3 <- reactive(coef(model3()))
  a3 <- reactive(10**coefficients3()[[1]])
  b3 <- reactive(coefficients3()[[2]])
  PK_simple3 <- reactive(a3()*input$BW3_h**b3())
  Dose3 <- reactive(PK_simple3()*input$AUC3)
  PK_simple33 <- reactive(33.35*(a3()*input$fu_h3/input$fu_r3)**0.77)
  Dose33 <- reactive(PK_simple33()*input$AUC3)
  data3 <- reactive(data.frame("Method" = c("Allometric scaling", "fu Intercept Correction Method"),
                               "Dose_h(mg)" = c(Dose3(), Dose33())))
  
  # four animals
  PK4 <- reactive(c(input$PK41, input$PK42, input$PK43, input$PK44))
  BW4 <- reactive(c(input$BW31, input$BW32, input$BW33, input$BW44))
  model4 <- reactive(lm(log(PK4(), base = 10)~log(BW4(), base = 10)))
  R4 <- reactive(summary(model4())$r.squared)
  coefficients4 <- reactive(coef(model4()))
  a4 <- reactive(10**coefficients4()[[1]])
  b4 <- reactive(coefficients4()[[2]])
  PK_simple4 <- reactive(a4()*input$BW4_h**b4())
  Dose4 <- reactive(PK_simple4()*input$AUC4)
  PK_simple44 <- reactive(33.35*(a4()*input$fu_h4/input$fu_r4)**0.77)
  Dose44 <- reactive(PK_simple44()*input$AUC4)
  data4 <- reactive(data.frame("Method" = c("Allometric scaling", "fu Intercept Correction Method"),
                               "Dose_h(mg)" = c(Dose4(), Dose44())))
  
  
  #---------------------------------------------------------------------------------------------------------#
  #-----------------------------------------CL-TS(rat-dog)--------------------------------------------------#
  #---------------------------------------------------------------------------------------------------------#
  PK5 <- reactive(c(input$PK51, input$PK52))
  BW5 <- reactive(c(input$BW51, input$BW52))
  model5 <- reactive(lm(log(PK5(), base = 10)~log(BW5(), base = 10)))
  R5 <- reactive(summary(model5())$r.squared)
  coefficients5 <- reactive(coef(model5()))
  a5 <- reactive(10**coefficients5()[[1]])
  b5 <- reactive(coefficients5()[[2]])
  PK_simple5 <- reactive(a5()*input$BW5_h**0.628)
  Dose5 <- reactive(PK_simple5()*input$AUC5)
  
  data5 <- reactive(data.frame("Method" = c("CL-TS(rat-dog)"),
                               "Dose_h(mg)" = c(Dose5())))
 
  

  #---------------------------------------------------------------------------------------------------------#
  #-------------------------------------AS model(exponent = 0.75)-------------------------------------------#
  #---------------------------------------------------------------------------------------------------------#
  PK_simple6 <- reactive(input$CL6*(input$BW6_h/input$BW6)^0.75)
  Dose6 <- reactive(PK_simple6()*input$AUC6)
  data6 <- reactive(data.frame("Method" = c("固定指数法"), "Dose_h(mg)" = c(Dose6())))
  
  
  output$plot1 <- renderPlot({
    ggdotchart(data(), x = "Method", y = "Dose_h.mg.",
               color = c("#00AFBB", "#E7B800", "#FC4E07","green"),  # Color by groups 
               sorting = "descending",                       # Sort value in descending order
               add = "segments",                             # 
               rotate = TRUE,                                # Rotate vertically                              
               dot.size = 16,                                 # Large dot size
               label = round(data()$Dose, 4),                 # Add  values as dot labels
               font.label = list(color = "black", size = 9,
                                 vjust = 0.5),               # Adjust label parameters
               ggtheme = theme_pubr()                        # ggplot2 theme
    )
  })
  output$table1 <- renderDataTable({
    data()
  })
  output$table2 <- renderDataTable({
    data2()
  })
  output$plot2 <- renderPlot({
    plot(log(BW2(), base = 10), log(PK2(), base = 10))
    abline(model2(), col = "red")
  })
  output$table3 <- renderDataTable({
    data3()
  })
  output$plot3 <- renderPlot({
    plot(log(BW3(), base = 10), log(PK3(), base = 10))
    abline(model3(), col = "red")
  })
  output$table4 <- renderDataTable({
    data4()
  })
  output$plot4 <- renderPlot({
    plot(log(BW4(), base = 10), log(PK4(), base = 10))
    abline(model4(), col = "red")
  })
  output$table5 <- renderDataTable({
    data5()
  })
  output$plot5 <- renderPlot({
    plot(log(BW5(), base = 10), log(PK5(), base = 10))
    abline(model5(), col = "red")
  })
  output$table6 <- renderDataTable({
    data6()
  })
  
  
}  
shinyApp(ui,server)     





