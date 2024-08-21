rm(list=ls())
library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggpubr)
library(knitr)
library(tidyverse)
options(digits = 4)
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
  tabItem(tabName = "noael",                                                                                    
          fluidRow(
            box(title = "Mouse:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE,
                numericInput("BW_m", "Weight(kg):", 0.020, min = 0, max = Inf,step = 0.0001),
                numericInput("Dose_m", "Dose(mg/kg):", 15, min = 0, max = Inf,step = 0.0001)
            ),
            box(title = "Rat:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE,
                numericInput("BW_r", "Weight(kg):", 0.30, min = 0, max = 10000,step = 0.0001), 
                numericInput("Dose_r", "Dose(mg/kg):", 50, min = 0, max = Inf,step = 0.0001)
            ),
            box(title = "Begale:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE,    
                numericInput("BW_b", "Weight(kg):", 10, min = 0, max = 10000,step = 0.0001),
                numericInput("Dose_b", "Dose(mg/kg):", 15, min = 0, max = 10000,step = 0.0001)
            ),
            box(title = "Monkey:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE, 
                numericInput("BW_mo", "Weight(kg):", 3, min = 0, max = 10000,step = 0.0001),  
                numericInput("Dose_mo", "Dose(mg/kg):", 10, min = 0, max = 10000,step = 0.0001)
            ),
            box(title = "Human:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE, 
                numericInput("BW_h", "Weight(kg):", 60, min = 0, max = 10000,step = 0.0001)   
            ),
            box(title = "Safety factor", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE, 
                numericInput("Factor", "factor:", 10, min = 0, max = 10000,step = 0.0001)
            )
          ),
          fluidRow(
            column(6, dataTableOutput("table1"),
                   br(),
                   tags$p(style = 'color: red;', '注意：若未给出体重数值，按照默认值进行计算')
            ),
            column(6, tags$h3("各种属参考体重:"), 
                   img(src = base64enc::dataURI(file="D:\\Git\\FIH\\www\\noael.png", mime="image/png"),
                          height = 440, width = 750)
            )
          ),
          fluidRow(
            tags$a(href = "https://www.cde.org.cn/zdyz/domesticinfopage?zdyzIdCODE=5a4d762d72643cb695168a8c568aa7e3", 
                   tags$p(style = 'color: red;', '点击这里访问《健康成年志愿者首次临床试验药物最大推荐起始剂量的估算指导原则》'))
          )
  ),
  # Second tab content
  tabItem(tabName = "allometricscaling",                     
          tabBox(id = "tabset1", width = "100%",             
                 tabPanel("Two animals",
                          tags$style(type='text/css', 
                                     ".nav-tabs {font-size: 20px} "),
                          fluidRow( 
                          box(title = "Animal1:", status = "primary", solidHeader = TRUE,width = 4,
                                        collapsible = TRUE,
                                        numericInput("BW21", "Weight(kg):", 0.25, min = 0, max = Inf,step = 0.0001),
                                        numericInput("PK21", "CL(ml/min):", 10, min = 0, max = Inf,step = 0.0001)
                          ),
                          box(title = "Animal2:", status = "primary", solidHeader = TRUE,width = 4,
                              collapsible = TRUE,
                              numericInput("BW22", "Weight(kg):", 15, min = 0, max = Inf,step = 0.0001),
                              numericInput("PK22", "CL(ml/min):", 100, min = 0, max = Inf,step = 0.0001)
                          ),
                          box(title = "Human:", status = "primary", solidHeader = TRUE,width = 4, 
                              collapsible = TRUE,
                              numericInput("BW2_h", "Weight(kg):", 70, min = 0, max = Inf,step = 0.0001))
                          ),
                          fluidRow(
                          box(title = "Fraction unbound in rat", status = "primary", solidHeader = TRUE,width = 4,
                                collapsible = TRUE,
                                numericInput("fu_r2", "fu_r:", 0.4, min = 0, max = 1,step = 0.0001)
                          ),
                          box(title = "Fraction unbound in human", status = "primary", solidHeader = TRUE,width = 4, 
                                collapsible = TRUE,
                                numericInput("fu_h2", "fu_h:", 0.6, min = 0, max = 1,step = 0.0001)),
                          box(title = "预估人体参数", status = "primary", solidHeader = TRUE,width = 4,
                                   collapsible = TRUE,
                                   numericInput("AUC2", "AUC(mg·min/ml):", 0.5, min = 0, max = Inf,step = 0.0001),
                                   numericInput("F2", "F:", 0.5, min = 0, max = 1,step = 0.0001))
                          ),
                          fluidRow(
                            box(dataTableOutput("table2"), width = 12),
                          ),
                          fluidRow(
                            box(plotOutput("plot2"), width = 5,
                                tags$p(style = 'color: red;', '注意：(1)本研究采用了简单异速放大法，未考虑根据指数对公式进行矫正'),
                                tags$p(style = 'color: red;', '(2)采用CL-FCIM法计算，若未给出体重数值则按照默认值进行计算'),
                                tags$a(href = "https://www.tesble.com/10.1007/bf01062336", 
                                       tags$p(style = 'color: red;', '(3)点击这里访问异速放大法的参考文献')),
                                tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/15958605/", 
                                       tags$p(style = 'color: red;', '(4)点击这里访问CL-FCIM法的参考文献'))
                                
                                ),
                            box(tags$h3("采用异速放大法计算，若未给出体重可参考下表:"), 
                                img(src = base64enc::dataURI(file="D:\\Git\\FIH\\www\\AS model.png", mime="image/png"),
                                    height = 440, width = 750))
                          )
                 ),
                 tabPanel("Three animals",
                          tags$style(type='text/css', 
                                     ".nav-tabs {font-size: 20px} "),
                          fluidRow(box(title = "Mouse:", status = "primary", solidHeader = TRUE,width = 3,
                                       collapsible = TRUE,
                                       numericInput("BW31", "Weight(kg):", 0.03, min = 0, max = Inf,step = 0.0001),
                                       numericInput("PK31", "CL(ml/min):", 10, min = 0, max = Inf,step = 0.0001)
                          ),
                          box(title = "Rat:", status = "primary", solidHeader = TRUE,width = 3,
                              collapsible = TRUE,
                              numericInput("BW32", "Weight(kg):", 0.25, min = 0, max = Inf,step = 0.0001),
                              numericInput("PK32", "CL(ml/min):", 100, min = 0, max = Inf,step = 0.0001)
                          ),
                          box(title = "Dog:", status = "primary", solidHeader = TRUE,width = 3,
                              collapsible = TRUE,
                              numericInput("BW33", "Weight(kg):", 15, min = 0, max = Inf,step = 0.0001),
                              numericInput("PK33", "CL(ml/min):", 21.2, min = 0, max = Inf,step = 0.0001)
                          ),
                          box(title = "Human:", status = "primary", solidHeader = TRUE,width = 3,
                              collapsible = TRUE,
                              numericInput("BW3_h", "Weight(kg):", 70, min = 0, max = Inf,step = 0.0001)
                          )
                          ),
                          fluidRow(
                            box(title = "Fraction unbound in rat", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("fu_r3", "fu_r:", 0.4, min = 0, max = 1,step = 0.0001)
                            ),
                            box(title = "Fraction unbound in human", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("fu_h3", "fu_h:", 0.6, min = 0, max = 1,step = 0.0001)),
                            box(title = "预估人体AUC", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("AUC3", "AUC(mg·min/ml):", 60, min = 0, max = Inf,step = 0.0001)
                            ),
                            box(title = "预估人体F", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("F3", "F:", 0.5, min = 0, max = 1,step = 0.0001)
                            )
                          ),
                          fluidRow(
                            box(dataTableOutput("table3"), width = 10,
                                br(),
                                tags$p(style = 'color: red;', '注意：同前述基于两种属计算时注意事项')),
                            box(plotOutput("plot3"), width = 2)
                          )
                 ),
                 tabPanel("Four animals",
                          tags$style(type='text/css', 
                                     ".nav-tabs {font-size: 20px} "),
                          fluidRow(box(title = "Mouse:", status = "primary", solidHeader = TRUE,width = 3,
                                       collapsible = TRUE,
                                       numericInput("BW41", "Weight(kg):", 0.03, min = 0, max = Inf,step = 0.0001),
                                       numericInput("PK41", "CL(ml/min):", 10, min = 0, max = Inf,step = 0.0001)
                          ),
                          box(title = "Rat:", status = "primary", solidHeader = TRUE,width = 3,
                              collapsible = TRUE,
                              numericInput("BW42", "Weight(kg):", 0.25, min = 0, max = Inf,step = 0.0001),
                              numericInput("PK42", "CL(ml/min):", 100, min = 0, max = Inf,step = 0.0001)
                          ),
                          box(title = "Dog:", status = "primary", solidHeader = TRUE,width = 3,
                              collapsible = TRUE,
                              numericInput("BW43", "Weight(kg):", 15, min = 0, max = Inf,step = 0.0001),
                              numericInput("PK43", "CL(ml/min):", 21.2, min = 0, max = Inf,step = 0.0001)
                          ),
                          box(title = "Monkey:", status = "primary", solidHeader = TRUE,width = 3,
                              collapsible = TRUE,
                              numericInput("BW44", "Weight(kg):", 3, min = 0, max = Inf,step = 0.0001),
                              numericInput("PK44", "CL(ml/min):", 24, min = 0, max = Inf,step = 0.0001))
                          ),
                          fluidRow( 
                            box(title = "Human:", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("BW4_h", "Weight(kg):", 70, min = 0, max = Inf,step = 0.0001)
                            ),
                            box(title = "Fraction unbound in rat", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("fu_r4", "fu_r:", 0.4, min = 0, max = 1,step = 0.0001)
                            ),
                            box(title = "Fraction unbound in human", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("fu_h4", "fu_h:", 0.6, min = 0, max = 1,step = 0.0001)
                            ),
                            box(title = "预估人体参数", status = "primary", solidHeader = TRUE,width = 3,
                                collapsible = TRUE,
                                numericInput("AUC4", "预估人体AUC(mg·min/ml):", 60, min = 0, max = Inf,step = 0.0001),
                                numericInput("F4", "预估人体F:", 0.5, min = 0, max = 1,step = 0.0001)
                            )
                          ),
                          fluidRow(
                            box(dataTableOutput("table4"), width = 10,
                                br(),
                                tags$p(style = 'color: red;', '注意：同前述基于两种属计算时注意事项')),
                            box(plotOutput("plot4"), width = 2)
                            
                          )
                 )
          )
  ),
  # Third tab                                                                                                   
  tabItem(tabName = "clts",
          fluidRow(
            box(title = "Rat:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE,
                numericInput("BW51", "Weight(kg):", 0.3, min = 0, max = Inf,step = 0.0001),
                numericInput("PK51", "CL(ml/min):", 10, min = 0, max = Inf,step = 0.0001)
            ),
            box(title = "Dog:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE,
                numericInput("BW52", "Weight(kg):", 10, min = 0, max = Inf,step = 0.0001),
                numericInput("PK52", "CL(ml/min):", 100, min = 0, max = Inf,step = 0.0001)
            ),
            box(title = "Human:", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE,
                numericInput("BW5_h", "Weight(kg):", 70, min = 0, max = Inf,step = 0.0001)
            ),
            box(title = "人体预估参数", status = "primary", solidHeader = TRUE,width = 3,
                collapsible = TRUE,
                numericInput("AUC5", "AUC(mg·min/ml):", 3, min = 0, max =Inf, step = 0.0001),
                numericInput("F5", "F:", 1, min = 0, max = 1,step = 0.0001)
            )
          ),
          fluidRow(
            box(dataTableOutput("table5"), width = 10,
                br(),
                tags$p(style = 'color: red;', '注意：若未给出体重数值，按照默认值进行计算'))
          ),
          fluidRow(
            box(plotOutput("plot5"), width = 6),
            tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/17646280/", 
                   tags$p(style = 'color: red;', '点击这里访问CL-TS(rat-dog)法的参考文献'))
          )
  ),
  #Fourth tab
  tabItem(tabName = "exponent",
          fluidRow(
            box(title = "Animal:", status = "primary", solidHeader = TRUE,width = 4,
                collapsible = TRUE,
                numericInput("CL6", "CL(ml/min):", 0.020, min = 0, max = Inf,step = 0.0001),
                numericInput("BW6", "Weight(kg):", 15, min = 0, max = Inf,step = 0.0001)
            ),
            box(title = "Human:", status = "primary", solidHeader = TRUE,width = 4,
                collapsible = TRUE,
                numericInput("BW6_h", "Weight(kg):", 70, min = 0, max = Inf,step = 0.0001)
            ),
            box(title = "人体预估参数", status = "primary", solidHeader = TRUE,width = 4,
                collapsible = TRUE,
                numericInput("AUC6", "AUC(mg·min/ml):", 70, min = 0, max = Inf,step = 0.0001),
                numericInput("F6", "F:", 0.15, min = 0, max = 1,step = 0.0001)
            )
          ),
          fluidRow(
            box(dataTableOutput("table6"), width = 6),
            box(tags$h3("若未给出体重可参考下表:"),
              img(src = base64enc::dataURI(file="D:\\Git\\FIH\\www\\AS model.png", mime="image/png"),
                            height = 440, width = 750))),
          fluidRow(
            tags$a(href = "https://kns.cnki.net/kns8s/defaultresult/index?crossids=YSTT4HG0%2CLSTPFY1C%2CJUP3MUPD%2CMPMFIG1A%2CWQ0UVIAA%2CBLZOG7CK%2CPWFIRAGL%2CEMRPGLPA%2CNLBO1Z6R%2CNN3FJMUV&korder=SU&kw=%E8%8D%AF%E4%BB%A3%E5%8A%A8%E5%8A%9B%E5%AD%A6%E5%9C%A8%E5%88%9B%E6%96%B0%E8%8D%AF%E7%89%A9%E9%A6%96%E6%AC%A1%E4%BA%BA%E4%BD%93%E8%AF%95%E9%AA%8C%E8%B5%B7%E5%A7%8B%E5%89%82%E9%87%8F%20%E8%AE%A1%E7%AE%97%E4%B8%AD%E7%9A%84%E6%84%8F%E4%B9%89", 
                   tags$p(style = 'color: red;', '点击这里访问固定指数法的参考文献')))
  )
)
)

ui<-dashboardPage(header, sidebar, body,skin = "red")  
server<-function(input,output){
  #---------------------------------------------------------------------------------------------------------#
  #---------------------------------------------Noael method------------------------------------------------#
  #---------------------------------------------------------------------------------------------------------#
  BW <- reactive(1000*c(input$BW_m, input$BW_r, input$BW_b, input$BW_mo, input$BW_h))  #g                    
  Dose <- reactive(c(input$Dose_m, input$Dose_r, input$Dose_b, input$Dose_mo))  #mg/kg
  Km <- reactive(10*BW()/10^(0.698*log(BW(),10)+0.8762))
  Dose_h <- reactive(Dose()/(Km()[5]/Km()[1:4])/input$Factor)
  data <- reactive(tibble("Method" = c("Mouse", "Rat", 
                                       "Begale","Monkey"),
                          "HED_human[mg]" = Dose_h()*input$Factor,
                          "MRSD_human[mg]" = Dose_h()
  )
  )
  #.........@@                                                                                               
  #data <- reactive(data.frame("Method" = c("Mouse", "Rat", 
  #                                         "Begale","Monkey"),
  #                            "HED_human(mg)" = Dose_h()*input$Factor,
  #                            "MRSD_human(mg)" = Dose_h()
  #))
  #.........@
  #---------------------------------------------------------------------------------------------------------#
  #--------------------------------------------AS method&FICM-----------------------------------------------#
  #---------------------------------------------------------------------------------------------------------#  
  # two animals
  PK2 <- reactive(c(input$PK21, input$PK22))
  BW2 <- reactive(c(input$BW21, input$BW22))
  model2 <- reactive(lm(log(PK2(), base = 10)~log(BW2(), base = 10)))
  R2 <- reactive(summary(model2())$r.squared)
  coefficients2 <- reactive(coef(model2()))
  a2 <- reactive(10**coefficients2()[[1]])
  b2 <- reactive(coefficients2()[[2]])
  PK_simple2 <- reactive(a2()*input$BW2_h**b2())
  PK_simple22 <- reactive(33.35*(a2()*input$fu_h2/input$fu_r2)**0.77)
  Dose2 <- reactive(PK_simple2()*input$AUC2)
  Dose22 <- reactive(PK_simple22()*input$AUC2)
  data2 <- reactive(tibble("Method" = c("Allometric scaling(不推荐使用)", "fu Intercept Correction Method"),
                               "a" = a2(),
                               "b" = b2(),
                               "CL[ml/min](F=1)" = c(PK_simple2(), PK_simple22()),
                               "CL[ml/min](F<1)" = c(PK_simple2()/input$F2, PK_simple22()/input$F2),
                               "Dose_human[mg](F=1)" = c(Dose2(), Dose22()),
                               "Dose_human[mg](F<1)"= c(Dose2()/input$F2, Dose22()/input$F2)))
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
  data3 <- reactive(tibble("Method" = c("Allometric scaling", "fu Intercept Correction Method"),
                           "a" = a3(),
                           "b" = b3(),
                           "CL[ml/min](F=1)" = c(PK_simple3(), PK_simple33()),
                           "CL[ml/min](F<1)" = c(PK_simple3()/input$F3, PK_simple33()/input$F3),
                           "Dose_human[mg](F=1)" = c(Dose3(), Dose33()),
                           "Dose_human[mg](F<1)"= c(Dose3()/input$F3, Dose33()/input$F3)))
  
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
  data4 <- reactive(tibble("Method" = c("Allometric scaling", "fu Intercept Correction Method"),
                               "a" = a4(),
                               "b" = b4(),
                               "CL[ml/min](F=1)" = c(PK_simple4(), PK_simple44()),
                               "CL[ml/min](F<1)" = c(PK_simple4()/input$F4, PK_simple44()/input$F4),
                               "Dose_human[mg](F=1)" = c(Dose4(), Dose44()),
                               "Dose_human[mg](F<1)"= c(Dose4()/input$F4, Dose44()/input$F4)))
  
  
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
  
  data5 <- reactive(tibble("Method" = c("CL-TS(rat-dog)"),
                            "a" = a5(),
                            "b" = b5(),
                            "CL[ml/min](F=1)" = PK_simple5(),
                            "CL[ml/min](F<1)" = PK_simple5()/input$F5,
                            "Dose_human[mg](F=1)" = Dose5(),
                            "Dose_human[mg](F<1)" = Dose5()/input$F5
                               ))
  
  
  
  #---------------------------------------------------------------------------------------------------------#
  #-------------------------------------AS model(exponent = 0.75)-------------------------------------------#
  #---------------------------------------------------------------------------------------------------------#
  PK_simple6 <- reactive(input$CL6*(input$BW6_h/input$BW6)^0.75)
  Dose6 <- reactive(PK_simple6()*input$AUC6)
  data6 <- reactive(tibble("Method" = c("固定指数法"), 
                           "Dose_human[mg](F=1)" = c(Dose6()), "Dose_human[mg](F<1)" = c(Dose6()/input$F6)
                           ))
  
  
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
    plot(log(BW3(), base = 10), log(PK3(), base = 10), xlab = "log BW", ylab = "log CL")
    abline(model3(), col = "red")
  })
  output$table4 <- renderDataTable({
    data4()
  })
  output$plot4 <- renderPlot({
    plot(log(BW4(), base = 10), log(PK4(), base = 10), xlab = "log BW", ylab = "log CL")
    abline(model4(), col = "red")
  })
  output$table5 <- renderDataTable({
    data5()
  })
  output$plot5 <- renderPlot({
    plot(log(BW5(), base = 10), log(PK5(), base = 10), xlab = "log BW", ylab = "log CL")
    abline(model5(), col = "red")
  })
  output$table6 <- renderDataTable({
    data6()
  })
  
  
}  
shinyApp(ui,server)     



