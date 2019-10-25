# Number of Studies Value Boxes
frow1 <- fluidRow(
  #making a drop-down of the different Years
  selectInput("date_input", label = "Select Year", choices = levels(factor(Fig1$Year)),
              selected="2018-2019"
  ), #selectInput 
  HTML ('</br>'),
  valueBoxOutput("value1", width=4)  #Active Studies
  ,valueBoxOutput("value2", width=4) #Completed Studies
  ,valueBoxOutput("value3", width=4) #Total Studies
  ,HTML ('</br>')
)#fluidRow


# STUDENT ENGAGEMENT
frow2 <- fluidRow(
  #making a drop-down of the different Years
  selectInput("date_input1", label = "Select Year", choices = levels(factor(Fig2$Year)),
              selected="2018-2019"
  ), #selectInput 
  HTML ('</br>'),
  valueBoxOutput("value4", width=4)  #Undergrad
  ,valueBoxOutput("value5", width=4) #Master
  ,valueBoxOutput("value6", width=4) #PhD
  #,valueBoxOutput("value7", width=3) #Post Doc
  ,HTML ('</br>')
)#fluidRow

#Map
frow3 <- fluidRow(
  #making a drop-down of the different Years
  selectInput("date_input2", label = "Select Year", choices = levels(factor(Map$Year)),
              selected="2018-2019"
  ), #selectInput 
  HTML ('</br>')
)#fluidRow

dashboardPage(title = 'SSRL Annual Report',   
              dashboardHeader(title= "2018-2019 Report"), #Insert the Main Title
              
              
              dashboardSidebar(  
                sidebarMenu(
                  #NUMBER OF STUDIES
                  menuItem("# of Studies", tabName= "first", icon= icon("info-circle")),
                  
                  #COLLABORATIONS
                  menuItem("Collaborations", tabName="second", icon=icon("group")),
                  
                  #RESEARCHER INVOLVEMENT
                  menuItem("Researcher Involvement", tabName="third", icon=icon("institution")),
                  
                  #STUDENT ENGAGEMENT
                  menuItem("Student Engagement", tabName="fourth", icon=icon("graduation-cap")),
                  
                  #RESEARCH OUTPUTS
                  menuItem("Research Outputs", tabName="fifth", icon=icon("sign-out-alt")),
                  
                  #MAP
                  menuItem("Collaboration Map", tabName="ninth", icon=icon("globe")),
                  
                  #NETWORK ANALYSIS
                  menuItem("Network Analysis",icon=icon("sitemap"),
                 
                  menuSubItem("2012-2013", tabName="tenth"),

                  menuSubItem("2013-2014", tabName="eleventh"),
                  
                  menuSubItem("2014-2015", tabName="twelveth"),

                  menuSubItem("2015-2016", tabName="thirteenth"),
                  
                  menuSubItem("2016-2017", tabName="sixth"),
                  
                  menuSubItem("2017-2018", tabName = "seventh"),
                  
                  menuSubItem("2018-2019", tabName = "eighth")),
                              
                  #menuSubItem('Animation', icon=icon("film"), href ="http://ssrl.usask.ca/documents/ssrl_sna_2012-13_to_2018-19.html")
                #),#menuItem
                  
                  #PAST REPORTS
                  menuItem("Past Annual Reports", icon = icon("folder"), 
                           
                  menuSubItem('2017-2018',href = "http://ssrl.usask.ca/ssrl-2017-2018-annual-report.php"),                           
                           
                  menuSubItem('2016-2017',href = "http://ssrl.usask.ca/ssrl-2016-2017-annual-report.php"),
                  
                  menuSubItem('2015-2016',href = "https://ssrl.usask.ca/documents/SSRL_2015-2016_Annual_Report_-_Building_Bridges.pdf"),
                  
                  menuSubItem('2014-2015',href = "https://ssrl.usask.ca/documents/SSRL%202014-2015%20Annual%20Report%20-%20Enhancing%20Capacity.pdf"),
                  
                  menuSubItem('2013-2014',href = "https://ssrl.usask.ca/documents/SSRL%202013-2014%20Annual%20Report%20-%20Enhancing%20Capacity.pdf"),
                  
                  menuSubItem('2012-2013',href = "https://ssrl.usask.ca/documents/SSRL%202012-2013%20Annual%20Report%20-%20Enhancing%20Capacity.pdf")
                  ) #menuItem
                ) #sidebarMenu
                ), #dashboardSidebar
##################################################################################################              
              dashboardBody(  
                
                
                
                tags$head(tags$style(HTML('

                    /* use css to set custom colors */           

                                /* navbar the part with the text on left */
                                .skin-blue .main-header .logo {
                                            background-color: #1B3346;
                                }

                                .skin-blue .main-header .logo:hover {         
                                            background-color: #1B3346;
                                }


                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                            background-color: #1B3346;
                                }        

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                            background-color: #1B3346;
                                }
                        '))) ,              
                
                
                tabItems(
                  tabItem(tabName= "first",
                           frow1
                          ,HTML ('</br>')
                          ,h3("Total Research Studies"),
                          plotlyOutput("Fig1",width = "100%", height = "100%") #Total Research Studies
                          ), #tabItem
                  
##################################################################################################                  
                  tabItem(tabName="second",
                          tabsetPanel(
                            tabPanel("2012-2013 to 2018-2019",
                                     h3("Number of Research Studies by Lab"),
                                     HTML ('</br>'),
                                     #Inserting Radio Buttons
                                     #radioButtons("radio_TABLE1", label= h5 ("Show Hyperlinks:"),
                                                  #choices= c ("Yes", "No"), selected="No", inline=TRUE),
                                     dataTableOutput('table1'), #Table 1- Number of Research Studies by Lab

                                     h6("* Blank cells indicates the lab was not fully operable at any time during the fiscal year"),
                                     dataTableOutput("Table1a") #Table (totals by year)- TOTAL RESEARCH STUDIES
                            ), #tabPanel
                            tabPanel("2018-2019 Collaborative Research Studies by Lab",
                                     h3("2018-19: Engagement in Collaborative Research Studies by SSRL Laboratory"),
                                     plotlyOutput("Fig1a",width = "100%", height = "100%") #Figure 2- 2018-19: Engagement in Collaborative Research Studies by SSRL Laboratory
                          ) #tabPanel
                          ) #tabsetPanel
                          ), #tabItem
##################################################################################################                  
                  tabItem(tabName="third",
                          tabsetPanel(
                            tabPanel("Overview",
                          h3("Number of Researchers by Origin"),
                          dataTableOutput("Table3a") #Table 2- Number of Researchers by Origin
                          ), #tabPanel
                          
                          tabPanel("2012-2013 to 2018-2019",
                          dataTableOutput('table3'), #Table- Studies by U of S Colleges
                          h6("*Blank cells indicates college, school or supporting unit was not affiliated with the SSRL at any time during the fiscal year. Previous years' data for these colleges, schools or supporting units are reported in the row 'Other University of Saskatchewan' in the 'Overview' tab")
                          ) #tabPanel
                          ) #tabsetPanel
                          ), #tabItem
##################################################################################################
                  tabItem(tabName="fourth"
                          ,frow2
                          ,HTML ('</br>')
                          ,h3("Number of Students Trained or Employed"),
                          plotlyOutput("Fig2" ,width = "100%", height = "100%") #Figure 3- Number of Students Trained or Employed
                          ), #tabItem
##################################################################################################
                  tabItem(tabName="fifth",
                          tabsetPanel(
                            tabPanel("Graph",
                          h3("Number of Research Outputs per Year"), 
                          HTML ('</br>'),
                          #Inserting Drop-Down Menu
                          selectInput("yvar", "Choose a Research Output:", choices= c("Books", "Book Chapters", "Conference Papers and Presentations",
                                                                                      "Media Articles", "Non-Refereed Journal Articles",
                                                                                      "Peer-Reviewed Journal Articles", "Review Articles", "Technical Reports",
                                                                                      "Theses", "Websites")),
                          plotlyOutput("Fig3a",width = "100%", height = "100%") #Figure 4- Number of Research Outputs per Year
                          ), #tabPanel
                          
                          tabPanel("Summary Table",
                                   HTML ('</br>'),
                                   checkboxGroupInput('show_vars', 'Columns to show in the table:',
                                                      choices= c("Year","Books", "Book Chapters", "Conference Papers and Presentations",
                                                                 "Media Articles", "Non-Refereed Journal Articles",
                                                                 "Peer-Reviewed Journal Articles", "Review Articles", "Technical Reports",
                                                                 "Theses", "Websites"), selected = c("Year","Peer-Reviewed Journal Articles","Books",
                                                                                                             "Book Chapters","Conference Papers and Presentations"), inline=TRUE),
                                   DT::dataTableOutput('table4') #Table- Type of Products Produces by SSRL
                          )#tabPanel
                          )#tabsetPanel
                          ),#tabItem
##################################################################################################
#MAP!!


        
tabItem(tabName="ninth"
        ,frow3
        ,HTML ('</br>')
        ,h3("Collaboration Map"),
        leafletOutput("mymap", height="650px")

), #tabItem

######################################################################################################

#Social networks

tabItem(tabName="sixth",
  visNetworkOutput("network1",width="100%", height="1000px")
),#tabItem, 2016-2017


tabItem(tabName="seventh",
        visNetworkOutput("network2",width="100%", height="1000px")
),#tabItem, 2017-2018

tabItem(tabName="eighth",
        visNetworkOutput("network3",width="100%", height="1000px")
),#tabItem, 2018-2019

tabItem(tabName="tenth",
        visNetworkOutput("network4",width="100%", height="1000px")
),#tabItem, 2012-2013

tabItem(tabName="eleventh",
        visNetworkOutput("network5",width="100%", height="1000px")
),#tabItem, 2013-2014

tabItem(tabName="twelveth",
        visNetworkOutput("network6",width="100%", height="1000px")
),#tabItem, 2014-2015

tabItem(tabName="thirteenth",
        visNetworkOutput("network7",width="100%", height="1000px")
)#tabItem, 2015-2016

)#tabItems
) #dashboardBody
) #dashboardPage