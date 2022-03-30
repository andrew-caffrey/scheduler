library(tidyverse)
library(googledrive)
library(googlesheets4)
library(gargle)
library(shiny)
library(DT)
library(lubridate)
library(shinyjs)
#old auth
#options(gargle_oauth_cache = "test")
#gargle::gargle_oauth_cache()
#gs4_auth(cache = "test")
#https://stackoverflow.com/questions/63535190/connect-to-googlesheets-via-shiny-in-r-with-googlesheets4
#https://stackoverflow.com/questions/63699558/authorizing-non-interactive-use-of-googlesheets-through-shiny-app-using-googlesh?noredirect=1&lq=1
#https://medium.com/@JosiahParry/googlesheets4-authentication-for-deployment-9e994b4c81d6

#Authorization make sure the working directory is correct
##setwd("/Users/andrew.caffrey/Desktop/Schedule App Drafts")

drive_auth(path = "client_secret.json")
gs4_auth(path = "client_secret.json")

# Spreadsheet: 
#https://docs.google.com/spreadsheets/d/1csfs8FjOVuRANGkj28F7FJqLAG2FFV3FA4qvZppgu5A/edit?usp=sharing

URL<-"https://docs.google.com/spreadsheets/d/1csfs8FjOVuRANGkj28F7FJqLAG2FFV3FA4qvZppgu5A/edit?usp=sharing"

#IR Ingredients
IR_page<-read_sheet(URL,sheet="IR Tests")
Ingredients<-IR_page%>%select(.,colnames(.[1:4]))%>%pivot_longer(.,colnames(.[1:4]))%>%select(`value`)%>%na.omit()

#IR Tests
G1<-IR_page%>%select(.,`Group 1`)%>%na.omit()%>%pull()
G2<-IR_page%>%select(.,`Group 2`)%>%na.omit()%>%pull()
G3<-IR_page%>%select(.,`Group 3`)%>%na.omit()%>%pull()

G1T1<-IR_page%>%select(.,`Group1_T1_Tests`)%>%na.omit()%>%pull()
G1T2<-IR_page%>%select(.,`Group1_T2_Tests`)%>%na.omit()%>%pull()

G2T1<-IR_page%>%select(.,`Group2_T1_Tests`)%>%na.omit()%>%pull()
G2T2<-IR_page%>%select(.,`Group2_T2_Tests`)%>%na.omit()%>%pull()

G3T1<-IR_page%>%select(.,`Group3_T1_Tests`)%>%na.omit()%>%pull()
G3T2<-IR_page%>%select(.,`Group3_T2_Tests`)%>%na.omit()%>%pull()

G4T1<-IR_page%>%select(.,`Group4_T1_Tests`)%>%na.omit()%>%pull()
G4T2<-IR_page%>%select(.,`Group4_T2_Tests`)%>%na.omit()%>%pull()

#General Tests

Tests_page<-read_sheet(URL,sheet="General Tests")
All_Tests<-Tests_page%>%select(.,colnames(.[1:ncol(.)]))%>%pivot_longer(.,colnames(.[1:ncol(.)]))%>%na.omit()

T1QBrick<-All_Tests%>%filter(name=="Tier 1 Quail Retail/Club/Food Service Brick")%>%select(`value`)%>%na.omit()%>%pull()
T2QBrick<-All_Tests%>%filter(name=="Tier 2 Quail Retail/Club/Food Service Brick")%>%select(`value`)%>%na.omit()%>%pull()
T3QBrick<-All_Tests%>%filter(name=="Tier 3 Quail Retail/Club/Food Service Brick")%>%select(`value`)%>%na.omit()%>%pull()

T1QFSP<-All_Tests%>%filter(name=="Tier 1 Quail Food Service Patties")%>%select(`value`)%>%na.omit()%>%pull()
T2QFSP<-All_Tests%>%filter(name=="Tier 2 Quail Food Service Patties")%>%select(`value`)%>%na.omit()%>%pull()
T3QFSP<-All_Tests%>%filter(name=="Tier 3 Quail Food Service Patties")%>%select(`value`)%>%na.omit()%>%pull()

T1QRCP<-All_Tests%>%filter(name=="Tier 1 Quail Retail/Club Patties")%>%select(`value`)%>%na.omit()%>%pull()
T2QRCP<-All_Tests%>%filter(name=="Tier 2 Quail Retail/Club Patties")%>%select(`value`)%>%na.omit()%>%pull()
T3QRCP<-All_Tests%>%filter(name=="Tier 3 Quail Retail/Club Patties")%>%select(`value`)%>%na.omit()%>%pull()

T1C<-All_Tests%>%filter(name=="Tier 1 Chameleon")%>%select(`value`)%>%na.omit()%>%pull()
T2C<-All_Tests%>%filter(name=="Tier 2 Chameleon")%>%select(`value`)%>%na.omit()%>%pull()
T3C<-All_Tests%>%filter(name=="Tier 3 Chameleon")%>%select(`value`)%>%na.omit()%>%pull()

T1E<-All_Tests%>%filter(name=="Tier 1 Eschen")%>%select(`value`)%>%na.omit()%>%pull()
T2E<-All_Tests%>%filter(name=="Tier 2 Eschen")%>%select(`value`)%>%na.omit()%>%pull()
T3E<-All_Tests%>%filter(name=="Tier 3 Eschen")%>%select(`value`)%>%na.omit()%>%pull()

T1D<-All_Tests%>%filter(name=="Tier 1 Dragon")%>%select(`value`)%>%na.omit()%>%pull()
T2D<-All_Tests%>%filter(name=="Tier 2 Dragon")%>%select(`value`)%>%na.omit()%>%pull()
T3D<-All_Tests%>%filter(name=="Tier 3 Dragon")%>%select(`value`)%>%na.omit()%>%pull()

T1CS<-All_Tests%>%filter(name=="Tier 1 Cased Sausage")%>%select(`value`)%>%na.omit()%>%pull()
T2CS<-All_Tests%>%filter(name=="Tier 2 Cased Sausage")%>%select(`value`)%>%na.omit()%>%pull()
T3CS<-All_Tests%>%filter(name=="Tier 3 Cased Sausage")%>%select(`value`)%>%na.omit()%>%pull()
T4CS<-All_Tests%>%filter(name=="Tier 4 Cased Sausage")%>%select(`value`)%>%na.omit()%>%pull()

T1RTECS<-All_Tests%>%filter(name=="Tier 1 RTE Cased Sausage")%>%select(`value`)%>%na.omit()%>%pull()
T2RTECS<-All_Tests%>%filter(name=="Tier 2 RTE Cased Sausage")%>%select(`value`)%>%na.omit()%>%pull()
T3RTECS<-All_Tests%>%filter(name=="Tier 3 RTE Cased Sausage")%>%select(`value`)%>%na.omit()%>%pull()
T4RTECS<-All_Tests%>%filter(name=="Tier 4 RTE Cased Sausage")%>%select(`value`)%>%na.omit()%>%pull()

#Timings
week_0<-week(Sys.Date())-1
week_1<-week(Sys.Date())
week_2<-week(Sys.Date())+1
week_3<-week(Sys.Date())+2
week_4<-week(Sys.Date())+3
week_5<-week(Sys.Date())+4

week_2s<- paste("Calendar Week #",week_2, "(Next week)")
week_3s<- paste("Calendar Week #",week_3)
week_4s<- paste("Calendar Week #",week_4)
week_5s<- paste("Calendar Week #",week_5)

#Aging
Ages<-c("D0","D10","D14","D20","NA")

ui<-fluidPage(
  useShinyjs(),
  selectInput("department","Choose Department",c("IR","PVT","SL","Other")),
  actionButton("update","Press this button to make the app work"),
  dataTableOutput("hours"),
  dataTableOutput("requests"),
  textInput("name","Name"),
  textInput("projectname","Project Name"),
  selectInput("product","Choose Product Format",c("Quail Retail/Club/Food Service Brick",
                                                  "Quail Food Service Patties",
                                                  "Quail Retail/Club Patties",
                                                  "Chameleon",
                                                  "Eschen",
                                                  "Dragon",
                                                  "Cased Sausage",
                                                  "RTE Cased Sausage")),
  selectInput("ingredient","Ingredient Being Tested",Ingredients),
  textInput("samplename", "Sample name for the report"),
  textInput("projectpurpose","Purpose for the report"),
  textInput("lots","Total number of Lots"),
  textInput("lotcodes","Lot Codes (Separated by a comma)"),
  textInput("control","Control-Lot Number"),
  checkboxGroupInput("cul1","Culinary Tests",
                     choices="NA"),
  checkboxGroupInput("cul2","Culinary Tests",
                     choices = "NA"),
  checkboxGroupInput("cul3","Culinary Tests Tier 3",
                     choices = "NA", selected = "NA"),
  checkboxGroupInput("cul4","Culinary Tests Tier 4",
                     choices = "NA", selected = "NA"),
  checkboxGroupInput("age","aging",
                     Ages,selected = "NA"),
  selectInput("cookfrom","Cook samples from...",c("Frozen","4C","Frozen and 4C")),
  p(h4("Total hours in this order:")),
  textOutput("orderhours"),
  p(h4("Amount of sample needed per lot each timepoint:")),
  textOutput("sampleneeded"),
  dateInput("dateavailable","Samples Available on"),
  dateInput("datereport","Ideal Day for the Report"),
  textInput("location","Sample location (Please label boxes Culinary/BOH)"),
  textInput("comments","Comments or additional information/instructions"),
  p(h4("If you cannot add the test, you may be out of time in your alloted budget and need to select a different date")),
  actionButton("add","add"),
  tableOutput("beta"),
  textOutput("test")
)

server<-function(input,output,session){
  ################################## Objects ################################################ Objects
  
  Requests<-eventReactive(c(input$update,input$add), {
    Data<-read_sheet(URL)
    Data$Samples_Available<-Data$Samples_Available%>%as.Date.character()
    Data$Report_By<-Data$Report_By%>%as.Date.character()
    Data%>%filter(Department==input$department,Completed=="No")%>%
      mutate(.,Weeks=week(.$Report_By))
  })
  Logistics<-eventReactive(c(input$update,input$product),{
    L<-read_sheet(URL,"Logistics")%>%pivot_longer(.,colnames(.[4:ncol(.)]))%>%filter(name==input$product)%>%na.omit()
    L$value<-as.numeric(L$value)
    L
  })
  
  Tests<-eventReactive(c(input$update,input$add,input$cul1,input$cul2,input$cul3,input$cul4,input$lots,input$cookfrom,input$age),{
    Time1<-Logistics()%>%filter(Test%in%input$cul1)%>%pull(.,`Time (hours)`)%>%sum()
    Time2<-Logistics()%>%filter(Test%in%input$cul2)%>%pull(.,`Time (hours)`)%>%sum()
    Time3<-Logistics()%>%filter(Test%in%input$cul3)%>%pull(.,`Time (hours)`)%>%sum()
    Time4<-Logistics()%>%filter(Test%in%input$cul4)%>%pull(.,`Time (hours)`)%>%sum()
    Frozen<-if(input$cookfrom=="Frozen and 4C"&input$lots>2){2}
    else{1}
    Age<-select(data.frame(input$age)[1])%>%nrow()%>%as.numeric()
    Lots<-if (input$lots>8){3}
    else if(input$lots>4 & input$lots<=8){2}
    else if(input$lots<=4){1}
    BOH<-if("BOH"%in%input$cul1){Age*2*Lots-2}
    else{0}
    Total<-Lots*Frozen*Time1*Age+Lots*Frozen*Time2*Age+Lots*Frozen*Time3*Age+Lots*Frozen*Time4*Age+1-BOH
  })
  
  Sampleneeded<-eventReactive(c(input$update,input$add,input$cul1,input$cul2,input$cul3,input$cul4,input$lots,input$cookfrom),{
    Amount1<-Logistics()%>%filter(Test%in%input$cul1)%>%pull(.,value)%>%sum()
    Amount2<-Logistics()%>%filter(Test%in%input$cul2)%>%pull(.,value)%>%sum()
    Amount3<-Logistics()%>%filter(Test%in%input$cul3)%>%pull(.,value)%>%sum()
    Amount4<-Logistics()%>%filter(Test%in%input$cul4)%>%pull(.,value)%>%sum()
    Frozen<-if(input$cookfrom=="Frozen and 4C"&input$lots>2){2}
    else{1}
    Total<-Frozen*Amount1+Frozen*Amount2+Frozen*Amount3+Frozen*Amount4
  })
  
  Timepoints<-eventReactive(input$update,{
    data.frame(input$age)%>%nrow()
  })
  
  Hours<-eventReactive(c(input$update,input$add),{
    Budget<-read_sheet(URL,sheet = "Budgets")%>%filter(Department==input$department)%>%
      mutate(.,!!week_2s :=
               (pull(.,Hours_Alloted)-
                  Requests()%>%
                  filter(Weeks==week_2,Department==input$department,Completed=="No")%>%
                  pull(.,Culinary)%>%sum()))%>%
      mutate(.,!!week_3s :=
               (pull(.,Hours_Alloted)-
                  Requests()%>%
                  filter(Weeks==week_3,Department==input$department,Completed=="No")%>%
                  pull(.,Culinary)%>%sum()))%>%
      mutate(.,!!week_4s :=
               (pull(.,Hours_Alloted)-
                  Requests()%>%
                  filter(Weeks==week_4,Department==input$department,Completed=="No")%>%
                  pull(.,Culinary)%>%sum()))%>%
      mutate(.,!!week_5s :=
               (pull(.,Hours_Alloted)-
                  Requests()%>%
                  filter(Weeks==week_5,Department==input$department,Completed=="No")%>%
                  pull(.,Culinary)%>%sum()))
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
      updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",T1QBrick,selected = "NA")
      updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",T2QBrick,selected = "NA")
      updateCheckboxGroupInput(session,"cul3","Culinary Tests Tier 3",T3QBrick,selected = "NA")
      
    }
    else if(input$product=="Quail Food Service Patties" & input$department%in%c("PVT","SL","Other")){
      updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",T1QFSP,selected = "NA")
      updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",T2QFSP,selected = "NA")
      updateCheckboxGroupInput(session,"cul3","Culinary Tests Tier 3",T3QFSP,selected = "NA")
    }
    else if(input$product=="Quail Retail/Club Patties" & input$department%in%c("PVT","SL","Other")){
      updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",T1QRCP,selected = "NA")
      updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",T2QRCP,selected = "NA")
      updateCheckboxGroupInput(session,"cul3","Culinary Tests Tier 3",T3QRCP,selected = "NA")
    }
    else if(input$product=="Chameleon" & input$department%in%c("IR","PVT","SL","Other")){
      updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",T1C,selected = "NA")
      updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",T2C,selected = "NA")
      updateCheckboxGroupInput(session,"cul3","Culinary Tests Tier 3",T3C,selected = "NA")
    }
    else if(input$product=="Eschen" & input$department%in%c("IR","PVT","SL","Other")){
      updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",T1E,selected = "NA")
      updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",T2E,selected = "NA")
      updateCheckboxGroupInput(session,"cul3","Culinary Tests Tier 3",T3E,selected = "NA")
    }
    else if(input$product=="Dragon" & input$department%in%c("IR","PVT","SL","Other")){
      updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",T1D,selected = "NA")
      updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",T2D,selected = "NA")
      updateCheckboxGroupInput(session,"cul3","Culinary Tests Tier 3",T3D,selected = "NA")
    }
    else if(input$product=="Cased Sausage" & input$department%in%c("IR","PVT","SL","Other")){
      updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",T1CS,selected = "NA")
      updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",T2CS,selected = "NA")
      updateCheckboxGroupInput(session,"cul3","Culinary Tests Tier 3",T3CS,selected = "NA")
      updateCheckboxGroupInput(session,"cul4","Culinary Tests Tier 4",T4CS,selected = "NA")
    }
    else if(input$product=="RTE Cased Sausage" & input$department%in%c("IR","PVT","SL","Other")){
      updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",T1RTECS,selected = "NA")
      updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",T2RTECS,selected = "NA")
      updateCheckboxGroupInput(session,"cul3","Culinary Tests Tier 3",T3RTECS,selected = "NA")
      updateCheckboxGroupInput(session,"cul4","Culinary Tests Tier 4",T4RTECS,selected = "NA")
    }
    else if(input$product%in%c("Quail Retail/Club/Food Service Brick","Quail Food Service Patties","Quail Retail/Club Patties") & input$department=="IR"){
      if(input$ingredient%in%G1){
        updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",G1T1,selected = "NA")
        updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",G1T2,selected = "NA")
      }
      else if(input$ingredient%in%G2){
        updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",G2T1,selected = "NA")
        updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",G2T2,selected = "NA")
      }
      else if(input$ingredient%in%G3){
        updateCheckboxGroupInput(session,"cul1","Culinary Tests Tier 1",G3T1,selected = "NA")
        updateCheckboxGroupInput(session,"cul2","Culinary Tests Tier 2",G3T2,selected = "NA")
      }}
    
  })
  
  
  #####################################Disable Orders#############################################Disable Orders
  
  observeEvent(c(input$update,input$datereport,input$cul1,input$cul2,input$cul3,input$lots,input$cul),ignoreNULL = T,{
    if(week(as.character.Date(input$datereport))==week_1 ){
      if(Tests()>pull(Hours(),colnames(Hours()[3]))){
        disable("add")
      }
      else{
        enable("add")
      }
    }
    
    else if(week(as.character.Date(input$datereport))==week_2){
      if(Tests()>pull(Hours(),colnames(Hours()[4]))){
        disable("add")
      }
      else{
        enable("add")
      }
    }
    
    else if(week(as.character.Date(input$datereport))==week_3){
      if(Tests()>pull(Hours(),colnames(Hours()[5]))){
        disable("add")
      }
      else{
        enable("add")
      }
    }
    
    else if(week(as.character.Date(input$datereport))==week_4){
      if(Tests()>pull(Hours(),colnames(Hours()[6]))){
        disable("add")
      }
      else{
        enable("add")
      }
    }
    
    else{
      enable("add")
    }
  })
  ################################## INPUTS ################################################# Inputs 
  
  observeEvent(input$add,{
    
    lots<-as.numeric(input$lots)
    Cul1<-pull(data.frame(input$cul1)[1])%>%toString()
    Cul2<-pull(data.frame(input$cul2)[1])%>%toString()
    Cul3<-pull(data.frame(input$cul3)[1])%>%toString()
    Cul4<-pull(data.frame(input$cul4)[1])%>%toString()
    Cul<-paste(Cul1,Cul2,Cul3,Cul4,sep = ", ")
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
                                input$samplename,
                                input$projectpurpose,
                                input$lotcodes,
                                input$control,
                                input$location,
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
      Requests()[c(1:17)]%>%
        print()
    )
  })
  
  #Order Hours
  output$orderhours<-renderText(
    Tests()
  )
  
  #Sample Needed
  output$sampleneeded<-renderText(
    if(input$product=="Quail Retail/Club/Food Service Brick"){
    paste0(Sampleneeded()," 12 oz Bricks")
      }
    else if(input$product=="Quail Food Service Patties"){
      paste0(Sampleneeded()," Patties")
    }
    else if(input$product=="Quail Retail/Club Patties"){
      paste0(Sampleneeded()," Patties")
    }
    else if(input$product=="Eschen"){
      paste0(Sampleneeded()," Bricks")
    }
    else if(input$product=="Dragon"){
      paste0(Sampleneeded()," Nuggets")
    }
    else if(input$product=="Cased Sausage"){
      paste0(Sampleneeded()," Sausages")
    }
    else if(input$product=="RTE Cased Sausage"){
      paste0(Sampleneeded()," Sausages")
    }
    else(paste0(Sampleneeded()," Chubs")
  ))
  
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







