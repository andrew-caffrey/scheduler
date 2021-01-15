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

week_1<-week(Sys.Date())
week_2<-week(Sys.Date())+1
week_3<-week(Sys.Date())+2
week_4<-week(Sys.Date())+3



ui<-fluidPage(
  selectInput("department","Choose Department",c("IR","PVT","SL","Other")),
  actionButton("update","Submit"),
  dataTableOutput("hours"),
  dataTableOutput("requests"),
  textInput("name","Name"),
  textInput("projectname","Project Name"),
  selectInput("product","Choose Product Format",c("Quail Retail/Club/Food Service Brick",
                                                  "Quail Food Service Patties",
                                                  "Quail Retail/Club Patties")),
  textInput("lots","Total number of Lots"),
  textInput("lotcodes","Lot Codes (Separated by a comma)"),
 checkboxGroupInput("cul","Culinary Tests",
                     c("stainless steel","meatball","bake")),
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
  actionButton("delete","delete last entry")
  )

server<-function(input,output,session){
################################## Objects ################################################ Objects
  
  Requests<-eventReactive(c(input$update,input$add,input$delete), {
    Data<-read_sheet(URL)
    Data$Samples_Available<-Data$Samples_Available%>%as.Date.character()
    Data%>%filter(Department=="IR",Completed=="No")%>%
      mutate(.,Weeks=week(Samples_Available))
  })
  Logistics<-eventReactive(input$update,{
    read_sheet(URL,"Logistics")
  })
  Tests<-eventReactive(c(input$update,input$add,input$delete,input$cul,input$lots,input$cookfrom),{
    Time<-Logistics()%>%filter(Test%in%c(input$cul))%>%pull(.,Time)%>%sum()
    Frozen<-if(input$cookfrom=="Frozen and 4C"&input$lots>2){2}
            else{1}
    Lots<-if (input$lots>8){3}
      else if(input$lots>4 & input$lots<=8){2}
      else if(input$lots<=4){1}
    Total<-Lots*Frozen*Time+1
  })
  Sampleneeded<-eventReactive(c(input$update,input$add,input$delete,input$cul,input$lots,input$cookfrom),{
    Amount<-Logistics()%>%filter(Test%in%c(input$cul))%>%pull(.,`Sample Amount`)%>%sum()
    Frozen<-if(input$cookfrom=="Frozen and 4C"&input$lots>2){2}
    else{1}
    Total<-Frozen*Amount
  })
  
  Timepoints<-eventReactive(input$update,{
    data.frame(input$age)%>%nrow()
  })
  
  Hours<-eventReactive(c(input$update,input$add,input$delete),{
    Budget<-read_sheet(URL,sheet = "Budgets")%>%filter(Department==input$department)%>%
      mutate(.,week1=
               (pull(.,Hours_Alloted)-
                  Requests()%>%
                  filter(Weeks==week_1,Department==input$department,Completed=="No")%>%
                  pull(.,Culinary)%>%sum()))%>%
      mutate(.,week2=
               (pull(.,Hours_Alloted)-
                  Requests()%>%
                  filter(Weeks==week_2,Department==input$department,Completed=="No")%>%
                  pull(.,Culinary)%>%sum()-
                  Requests()%>%
                  filter(Weeks==week_1,Department==input$department,str_detect(Age,"D10"),Completed=="No")%>%
                  pull(.,Culinary)%>%sum()))%>%
      mutate(.,week3=
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
      mutate(.,week4=
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
week(Sys.Date())
  observeEvent(input$product,{
    if(input$product=="Quail Retail/Club/Food Service Brick"){
      updateCheckboxGroupInput(session,"cul","Culinary Tests",c("BOH",
                                                                "Restaurant Grill",
                                                                "Stainless Steel Pan",
                                                                "Griddle",
                                                                "Meatballs in Sauce",
                                                                "Bolognese",
                                                                "Meatloaf",
                                                                "Taco Crumble",
                                                                "Backyard Grill"))
      updateSelectInput(session,"cookfrom","Cook samples from...",c("4C"))
    }
    else if(input$product=="Quail Food Service Patties"){
      updateCheckboxGroupInput(session,"cul","Culinary Tests",c("BOH",
                                                                "Restaurant Grill",
                                                                "Stainless Steel Pan",
                                                                "Griddle",
                                                                "Cast Iron",
                                                                "Cast Iron Grill Pan"))
      updateSelectInput(session,"cookfrom","Cook samples from...",c("Frozen","4C","Frozen and 4C"))
      
    }
    else if(input$product=="Quail Retail/Club Patties"){
      updateCheckboxGroupInput(session,"cul","Culinary Tests",c("BOH",
                                                                "Backyard Grill",
                                                                "Stainless Steel Pan",
                                                                "Cast Iron",
                                                                "Cast Iron Grill Pan",
                                                                "Bake",
                                                                "Griddle"))
      updateSelectInput(session,"cookfrom","Cook samples from...",c("Frozen","4C","Frozen and 4C"))
    }
  })

################################## INPUTS ################################################# Inputs 
  
  observeEvent(input$add,{
    lots<-as.numeric(input$lots)
    Cul<-pull(data.frame(input$cul)[1])%>%toString()
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
                                input$lotcodes,
                                input$comments,
                                lots,
                                Timepoints(),
                                Sampleneeded(),
                                Tests()
                                ))
  })
  observeEvent(input$delete,{
    bottom<-as.character.numeric_version(nrow(Requests())+1)
    range_delete(URL,sheet=NULL,bottom)
  })
################################# OUTPUTS ################################################# Outputs
  
#Table 1 

  observeEvent(input$update|input$add|input$delete,ignoreInit = T,{
    output$requests<-renderDataTable(
      Requests()[c(1:12)]%>%
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
    
    observeEvent(input$update|input$budget|input$add|input$delete,ignoreInit = T,{
      output$hours<-renderDataTable(
        Hours()%>%
          print()
      )
    })

}


shinyApp(ui,server)



