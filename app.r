library(tidyverse)
library(googledrive)
library(googlesheets4)
library(gargle)
library(shiny)
library(DT)
library(lubridate)
#old auth
#options(gargle_oauth_cache = "test")
#gargle::gargle_oauth_cache()
#gs4_auth(cache = "test")
#https://stackoverflow.com/questions/63535190/connect-to-googlesheets-via-shiny-in-r-with-googlesheets4
#https://stackoverflow.com/questions/63699558/authorizing-non-interactive-use-of-googlesheets-through-shiny-app-using-googlesh?noredirect=1&lq=1
#https://medium.com/@JosiahParry/googlesheets4-authentication-for-deployment-9e994b4c81d6


#Authorization make sure the working directory is correct
#setwd("/Users/andrew.caffrey/Desktop/Schedule App Drafts")

drive_auth(path = "client_secret.json")
gs4_auth(path = "client_secret.json")

# Spreadsheet: 
#https://docs.google.com/spreadsheets/d/1csfs8FjOVuRANGkj28F7FJqLAG2FFV3FA4qvZppgu5A/edit?usp=sharing

URL<-"https://docs.google.com/spreadsheets/d/1csfs8FjOVuRANGkj28F7FJqLAG2FFV3FA4qvZppgu5A/edit?usp=sharing"

week_1<-week(Sys.Date())+1
week_2<-week(Sys.Date())+2
week_3<-week(Sys.Date())+3
week_4<-week(Sys.Date())+4



ui<-fluidPage(
  selectInput("department","Choose Department",c("IR","PVT","SL","Other")),
  actionButton("update","Load Current Schedule and Budget"),
  dataTableOutput("hours"),
  dataTableOutput("requests"),
  textInput("name","Name"),
  textInput("projectname","Project Name"),
  selectInput("product","Choose Product Format",c("Quail Retail/Club/Food Service Brick",
                                                  "Quail Food Service Patties",
                                                  "Quail Retail/Club Patties",
                                                  "Chameleon")),
  selectInput("ingredient","Ingredient Being Tested",c("Solanic",
                                                       "TSPC",
                                                       "Coconut Oil/Fat Emulsion",
                                                       "Methylcellulose",
                                                       "Advantagel-S",
                                                       "Microgard740",
                                                       "Yeast Extracts",
                                                       "NaOH/HOH for MPG Replacement",
                                                       "Heme (Dried/Powder Only)")),
  textInput("lots","Total number of Lots"),
  textInput("lotcodes","Lot Codes (Separated by a comma)"),
  textInput("control","Control-Lot Number"),
  checkboxGroupInput("cul1","Culinary Tests",
                     choices="NA"),
  checkboxGroupInput("cul2","Culinary Tests",
                     choices = "NA"),
  checkboxGroupInput("age","aging",
                     c("D0","D1","D10","D15","D20")),
  selectInput("cookfrom","Cook samples from...",c("Frozen","4C","Frozen and 4C")),
  p(h4("Total hours in this order:")),
  textOutput("orderhours"),
  p(h4("Amount of sample needed per lot each week (grams):")),
  textOutput("sampleneeded"),
  dateInput("dateavailable","Samples Available on"),
  dateInput("datereport","Ideal Day for the Report"),
  textInput("comments","Comments or additional information/instructions"),
  actionButton("add","add"),
  tableOutput("beta")
)

server<-function(input,output,session){
  ################################## Objects ################################################ Objects
  
  Requests<-eventReactive(c(input$update,input$add), {
    Data<-read_sheet(URL)
    Data$Samples_Available<-Data$Samples_Available%>%as.Date.character()
    Data%>%filter(Department=="IR",Completed=="No")%>%
      mutate(.,Weeks=week(Samples_Available))
  })
  Logistics<-eventReactive(input$update,{
    read_sheet(URL,"Logistics")
  })
  
  Tests<-eventReactive(c(input$update,input$add,input$cul1,input$cul2,input$lots,input$cookfrom),{
    Time1<-Logistics()%>%filter(Test%in%input$cul1)%>%pull(.,Time)%>%sum()
    Time2<-Logistics()%>%filter(Test%in%input$cul2)%>%pull(.,Time)%>%sum()
    Frozen<-if(input$cookfrom=="Frozen and 4C"&input$lots>2){2}
    else{1}
    Lots<-if (input$lots>8){3}
    else if(input$lots>4 & input$lots<=8){2}
    else if(input$lots<=4){1}
    Total<-Lots*Frozen*Time1+Lots*Frozen*Time2+1
  })
  
  Sampleneeded<-eventReactive(c(input$update,input$add,input$cul1,input$cul2,input$lots,input$cookfrom),{
    Amount1<-Logistics()%>%filter(Test%in%input$cul1)%>%pull(.,`Sample Amount`)%>%sum()
    Amount2<-Logistics()%>%filter(Test%in%input$cul2)%>%pull(.,`Sample Amount`)%>%sum()
    Frozen<-if(input$cookfrom=="Frozen and 4C"&input$lots>2){2}
    else{1}
    Total<-Frozen*Amount1+Frozen*Amount2
  })
  
  Timepoints<-eventReactive(input$update,{
    data.frame(input$age)%>%nrow()
  })
  
  Hours<-eventReactive(c(input$update,input$add),{
    Budget<-read_sheet(URL,sheet = "Budgets")%>%filter(Department==input$department)%>%
      mutate(.,next_week=
               (pull(.,Hours_Alloted)-
                  Requests()%>%
                  filter(Weeks==week_1,Department==input$department,Completed=="No")%>%
                  pull(.,Culinary)%>%sum()))%>%
      mutate(.,two_weeks_out=
               (pull(.,Hours_Alloted)-
                  Requests()%>%
                  filter(Weeks==week_2,Department==input$department,Completed=="No")%>%
                  pull(.,Culinary)%>%sum()-
                  Requests()%>%
                  filter(Weeks==week_1,Department==input$department,str_detect(Age,"D10"),Completed=="No")%>%
                  pull(.,Culinary)%>%sum()))%>%
      mutate(.,three_weeks_out=
               (pull(.,Hours_Alloted)-
                  Requests()%>%
                  filter(Weeks==week_3,Department==input$department,Completed=="No")%>%
                  pull(.,Culinary)%>%sum()-
                  Requests()%>%
                  filter(Weeks==week_2,Department==input$department,str_detect(Age,"D10"),Completed=="No")%>%
                  pull(.,Culinary)%>%sum())-
               Requests()%>%
               filter(Weeks==week_1,Department==input$department,str_detect(Age,"D15"),Completed=="No")%>%
               pull(.,Culinary)%>%sum())%>%
      mutate(.,four_weeks_out=
               (pull(.,Hours_Alloted)-
                  Requests()%>%
                  filter(Weeks==week_4,Department==input$department,Completed=="No")%>%
                  pull(.,Culinary)%>%sum()-
                  Requests()%>%
                  filter(Weeks==week_3,Department==input$department,str_detect(Age,"D10"),Completed=="No")%>%
                  pull(.,Culinary)%>%sum())-
               Requests()%>%
               filter(Weeks==week_2,Department==input$department,str_detect(Age,"D15"),Completed=="No")%>%
               pull(.,Culinary)%>%sum()-
               Requests()%>%
               filter(Weeks==week_1,Department==input$department,str_detect(Age,"D20"),Completed=="No")%>%
               pull(.,Culinary)%>%sum())
  })
  ################################## UI Updates ################################################# Ui Updates
  
  observeEvent(c(input$product,input$department,input$ingredient),{
    if(input$product=="Quail Retail/Club/Food Service Brick"){
      updateSelectInput(session,"cookfrom","Cook samples from...",c("4C"))
    }
    else if(input$product%in%c("Quail Food Service Patties","Quail Retail/Club Patties")){
      updateSelectInput(session,"cookfrom","Cook samples from...",c("Frozen","4C","Frozen and 4C"))
    }
    if(input$product=="Quail Retail/Club/Food Service Brick" & input$department%in%c("PVT","SL","Other")){
      updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",c("Restaurant Grill",
                                                                        "Stainless Steel Pan (Patty)",
                                                                        "Griddle",
                                                                        "Meatballs in Sauce",
                                                                        "Bolognese",
                                                                        "Meatloaf",
                                                                        "Taco Crumble",
                                                                        "Backyard Grill",
                                                                        "NA"))
      updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",c("Grill High Salt",
                                                                        "Grill Onion",
                                                                        "Boil",
                                                                        "Deep Fry",
                                                                        "NA"))
      
    }
    else if(input$product=="Quail Food Service Patties" & input$department%in%c("PVT","SL","Other")){
      updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",c("Restaurant Grill",
                                                                        "Stainless Steel Pan (Patty)",
                                                                        "Griddle",
                                                                        "Cast Iron (Patty)",
                                                                        "Cast Iron Grill Pan",
                                                                        "NA"))
      updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",c("NA"))
    }
    else if(input$product=="Quail Retail/Club Patties" & input$department%in%c("PVT","SL","Other")){
      updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",c("Backyard Grill",
                                                                        "Stainless Steel Pan (Patty)"))
      updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",c("Cast Iron (Patty)",
                                                                        "Cast Iron Grill Pan",
                                                                        "Bake",
                                                                        "NA"))
    }
    else if(input$product=="Chameleon" & input$department%in%c("IR","PVT","SL","Other")){
      updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",c("BOH",
                                                                        "Stainless Steel Pan (Patty)",
                                                                        "Stainless Steel Pan (Crumble)",
                                                                        "Non Stick (Patty)",
                                                                        "Non Stick (Crumble)",
                                                                        "Cast Iron Pan (Patty)",
                                                                        "Cast Iron Pan (Crumble)"))
      updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",c("Meatballs in Sauce",
                                                                        "Simmer in Broth (MeatBalls)",
                                                                        "Simmer in Broth (Crumble)",
                                                                        "Bake",
                                                                        "Meatloaf",
                                                                        "NA"))
    }
    else if(input$product%in%c("Quail Retail/Club/Food Service Brick","Quail Food Service Patties","Quail Retail/Club Patties") & input$department=="IR"){
      if(input$ingredient%in%c("Solanic","TSPC","Methylcellulose","Advantagel-S")){
        updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",c("BOH",
                                                                          "Stainless Steel Pan (Patty)",
                                                                          "Meatballs in Sauce",
                                                                          "Taco Crumble",
                                                                          "Backyard Grill",
                                                                          "NA"))
        updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",c("Grill High Salt",
                                                                          "Boil",
                                                                          "NA"))
      }
      else if(input$ingredient%in%c("Coconut Oil/Fat Emulsion","NaOH/HOH for MPG Replacement")){
        updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",c("BOH",
                                                                          "Backyard Grill",
                                                                          "Stainless Steel Pan (Patty)",
                                                                          "Taco Crumble",
                                                                          "NA"))
        updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",choices=c("NA"))
      }
      else if(input$ingredient%in%c("Microgard740","Yeast Extracts","Heme (Dried/Powder Only)")){
        updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",c("BOH",
                                                                          "Backyard Grill",
                                                                          "Stainless Steel Pan (Patty)",
                                                                          "Boil",
                                                                          "Taco Crumble",
                                                                          "NA"))
        updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",choices=c("NA"))
      }}
    
  })
  
  
  ################################## INPUTS ################################################# Inputs 
  
  observeEvent(input$add,{
    
    lots<-as.numeric(input$lots)
    Cul1<-pull(data.frame(input$cul1)[1])%>%toString()
    Cul2<-pull(data.frame(input$cul2)[1])%>%toString()
    Cul<-paste(Cul1,Cul2,sep = ", ")
    Aging<-pull(data.frame(input$age)[1])%>%toString()
    sheet_append(URL,data.frame(input$name,
                                input$projectname,
                                input$department,
                                as.character(input$dateavailable),
                                as.character(input$datereport),
                                input$product,
                                input$cookfrom,
                                "No",
                                Cul,
                                Aging,
                                Sampleneeded(),
                                input$lotcodes,
                                input$control,
                                input$comments,
                                lots,
                                Timepoints(),
                                Tests()
    ))
  })
  
  ################################# OUTPUTS ################################################# Outputs
  
  #Table 1 
  
  observeEvent(input$update|input$add,ignoreInit = T,{
    output$requests<-renderDataTable(
      Requests()[c(1:14)]%>%
        print()
    )
  })
  
  #Order Hours
  output$orderhours<-renderText(
    Tests()
  )
  
  #Sample Needed
  output$sampleneeded<-renderText(
    Sampleneeded()
  )
  #Budgets Table
  
  observeEvent(input$update|input$add,ignoreInit = T,{
    output$hours<-renderDataTable(
      Hours()%>%
        print()
    )
  })
  #beta Table
  
}



shinyApp(ui,server)









