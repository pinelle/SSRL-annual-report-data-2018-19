# Define server logic required to draw a histogram
shinyServer(function(input, output) {
###########################################  
  #GRAPH 1: TOTAL RESEARCH STUDIES  
  output$Fig1 <- renderPlotly({
    ggplotly(test) 
  }) 
  
#Number of Studies VALUE BOXES
  
#Reactive Practice Dataset 
  reactive_df1 <- reactive({
    Fig1 %>% 
      filter(Year==input$date_input) %>% 
      filter(Status=="Active") %>%
      arrange(desc(Year))
    })
 #creating the valueBoxOutput content (based on the reactive_df above)
  output$value1 <- renderValueBox({
    valueBox(
      paste0(reactive_df1()$Count),
      'Total Number of Active Studies'
      ,icon = icon("book")
      ,color = "light-blue")  
  })
  
  
  #Reactive Practice Dataset 
  reactive_df2 <- reactive({
    Fig1 %>% 
      filter(Year==input$date_input) %>% 
      filter(Status=="Completed") %>%
      arrange(desc(Year))
  })  
  #Value box based on reactive values above
  output$value2 <- renderValueBox({
    valueBox(
      paste0(reactive_df2()$Count),
      'Total Number of Completed Studies'
      ,icon = icon("star",lib='glyphicon')
      ,color = "light-blue")   
  })  
  
#Reactive Practice Dataset 
  reactive_df3 <- reactive({
    Fig1 %>% 
      filter(Year==input$date_input) %>% 
      summarise(Total=sum(Count)) 
  })  
  #Value box based on reactive values above
  output$value3 <- renderValueBox({
    valueBox(
      paste0(reactive_df3()$Total),
      'Total Number of Studies'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")   
  })  
  

  
######################################  
  #DP had to increment the index by 1 to hide the hyperlinks
  #TABLE 1- Research Studies By Lab
  #Displaying Hyperlinks
  TABLE1_Input<- reactive({
    #switch(input$radio_TABLE1, 
           #"No"= Table1[,-9],
           #"Yes"= Table1)
  Table1[,-9]
  })  
  

  output$table1<-renderDataTable({
    datatable(TABLE1_Input(), options=list(paging=FALSE, searching = FALSE), rownames=FALSE, escape=FALSE)%>% 
       
      
    #tableFooter(TRUE)%>% 
    formatStyle('2012-13', `text-align` = 'center')%>%
    formatStyle('2013-14', `text-align` = 'center')%>%
    formatStyle('2014-15', `text-align` = 'center')%>%
    formatStyle('2015-16', `text-align` = 'center')%>%
    formatStyle('2016-17', `text-align` = 'center')%>%
    formatStyle('2017-18', `text-align` = 'center')%>%
    formatStyle('2018-19', `text-align` = 'center')%>%

    formatStyle(
      'Lab',
      target = 'row',
      backgroundColor = styleEqual('TOTAL RESEARCH STUDIES','#354377'), color=styleEqual('TOTAL RESEARCH STUDIES', "white")
    )      
        							
  })
  

###########Map

  filteredMap <- reactive({
    Map %>% 
      filter(Year==input$date_input2) 
  })  
  
  initial_lat = 45.029157
  initial_lng = -93.316257
  initial_zoom = 4

  
  output$mymap <- renderLeaflet({
    #
    Map = filter(Map, Year == '2018-2019')
    #leaflet(data = Map) %>% 
    leaflet(data = Map , options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {
          L.control.zoom({ position: 'topright' }).addTo(this)
      }") %>%    
    
      addTiles() %>% 
      addCircleMarkers(lng = ~Longitude , lat = ~Latitude, radius=4 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Red", label =  paste0(Map$Location,', ',Map$Year)   )  
      })


  observe({
    proxy <- leafletProxy("mymap", data = filteredMap())  %>%
      clearMarkers()  %>%
      setView(lat = initial_lat, lng = initial_lng, initial_zoom, zoom = initial_zoom) %>%       
      addCircleMarkers(lng = ~Longitude , lat = ~Latitude, radius=4 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Red", label = paste0(filteredMap()$Location,', ',filteredMap()$Year) )  
  })  

      
######################################################  
  #STUDENT ENGAGMENT 
  output$Fig2 <- renderPlotly({
    ggplotly(test1) #%>%
      #layout(autosize = F, margin = m)
  })   

  
  #Number of Students Value Box
  
  #Reactive Practice Dataset: Undergraduates
  reactive_df4 <- reactive({
    Fig2 %>% 
      filter(Year==input$date_input1) %>% 
      filter(Student=="Undergraduate Students") %>%
      arrange(desc(Year))
  })
  #creating the valueBoxOutput content (based on the reactive_df above)
  output$value4 <- renderValueBox({
    valueBox(
      paste0(reactive_df4()$Count),
      '# of Undergraduate Students'
      ,icon = icon("users")
      ,color = "light-blue")  
  })
  
  #Reactive Practice Dataset 
  reactive_df5 <- reactive({
    Fig2 %>% 
      filter(Year==input$date_input1) %>% 
      filter(Student=="Master's Students") %>%
      arrange(desc(Year))
  })  
  #Value box based on reactive values above
  output$value5 <- renderValueBox({
    valueBox(
      paste0(reactive_df5()$Count),
      '# of Master Students'
      ,icon = icon("graduation-cap")
      ,color = "light-blue")   
  })  
  
  #Reactive Practice Dataset 
  reactive_df6 <-  reactive({
    Fig2 %>% 
      filter(Year==input$date_input1) %>% 
      filter(Student=="Doctoral Students") %>%
      arrange(desc(Year))
  })  
  #Value box based on reactive values above
  output$value6 <- renderValueBox({
    valueBox(
      paste0(reactive_df6()$Count),
      '# of PhD Students'
      ,icon = icon("institution")
      ,color = "light-blue")   
  })      
  

  
  #Reactive Practice Dataset 
  reactive_df7 <-  reactive({
    Fig2 %>% 
      filter(Year==input$date_input1) %>% 
      filter(Student=="Post Doctoral Fellows") %>%
      arrange(desc(Year))
  })  
  #Value box based on reactive values above
  output$value7 <- renderValueBox({
    valueBox(
      paste0(reactive_df7()$Count),
      '# of Post-Doctoral Fellows'
      ,icon = icon("book")
      ,color = "light-blue")   
  })        
  
########################################################  
  #GRAPH 2: 2017-18: Engagement in Collaborative Research Studies by SSRL Laboratory 
  output$Fig1a <- renderPlotly({
    plot_ly(CollabStudies20182019, x=~Count, y=~Unit, type='bar', text=~Lab, orientation='h',
            marker=list(color='#354377')) %>% 
     layout(xaxis=x, yaxis=y)  
  })   
 
#TABLE 3- Number of Researchers by Origin
  output$table3<-renderDataTable({
   datatable(Table3, options=list(paging=FALSE, searching=FALSE, ordering=TRUE,
                                  columnDefs = list(list(
                                    className = 'dt-center', targets=1:6))),rownames=FALSE)%>% 
      formatStyle('2012-13', 'text-align' = 'center') %>%  
      formatStyle('2013-14', 'text-align' = 'center') %>% 
      formatStyle('2014-15', 'text-align' = 'center') %>% 
      formatStyle('2015-16', 'text-align' = 'center') %>% 
      formatStyle('2016-17', 'text-align' = 'center') %>% 
      formatStyle('2017-18', 'text-align' = 'center') %>%
      formatStyle('2018-19', 'text-align' = 'center')    
  })
  
  #TABLE 3a- SUM of TABLE3
  output$Table3a<-renderDataTable({
    datatable(Table3a, options=list(paging=FALSE, searching=FALSE, ordering=FALSE,
                                    columnDefs = list(list(
                                      className = 'dt-center', targets=1:6))), rownames=FALSE)%>% 
      formatStyle('Summary', color='white', backgroundColor='#354377') %>% 
      formatStyle('2012-13', color='white', backgroundColor='#354377', 'text-align' = 'center') %>%  
      formatStyle('2013-14', color='white', backgroundColor='#354377', 'text-align' = 'center') %>% 
      formatStyle('2014-15', color='white', backgroundColor='#354377', 'text-align' = 'center') %>% 
      formatStyle('2015-16', color='white', backgroundColor='#354377', 'text-align' = 'center') %>% 
      formatStyle('2016-17', color='white', backgroundColor='#354377', 'text-align' = 'center') %>% 
      formatStyle('2017-18', color='white', backgroundColor='#354377', 'text-align' = 'center') %>%
      formatStyle('2018-19', color='white', backgroundColor='#354377', 'text-align' = 'center')
  })

######
#Research Outputs GRAPH

  output$Fig3a <- renderPlotly({
    plot_ly(Fig3, x=~Year, y=~get(input$yvar), type='bar', text="Total Research Outputs", 
            marker=list(color='#354377')) %>% 
      layout(xaxis=y, yaxis=y)  
  })    
  #TABLE 4- Number of Researchers by Origin
  output$table4<-DT::renderDataTable({
    DT::datatable(options = list(paging = FALSE, searching= FALSE, 
                                 autoWidth = TRUE,
                                 columnDefs = list(list(width = '500px', targets = "_all"))),
                  Fig3[,input$show_vars, drop = FALSE]
                  )  
                 })   
###########################################################################################NETWORK ANALYSIS
#2016-2017 Network Analysis 
output$network1 <- renderVisNetwork({
    visNetwork(nodes, links, width="100%", height="1000px", main="The Social Sciences Research Laboratories Collaborators") %>%
    visOptions(highlightNearest = TRUE, selectedBy = "Laboratories.and.Collaborators")
  })    

#2017-2018 Network Analysis 
output$network2 <- renderVisNetwork({
  visNetwork(nodes1, links1, width="100%", height="1000px", main="The Social Sciences Research Laboratories Collaborators") %>%
    visOptions(highlightNearest = TRUE, selectedBy = "Laboratories.and.Collaborators")
}) 
  
#2018-2019 Network Analysis 
output$network3 <- renderVisNetwork({
  visNetwork(nodes2, links2, width="100%", height="1000px", main="The Social Sciences Research Laboratories Collaborators") %>%
    visOptions(highlightNearest = TRUE, selectedBy = "Laboratories.and.Collaborators")
}) 

#2012-2013 Network Analysis
output$network4 <- renderVisNetwork({
  visNetwork(nodes3, links3, width="100%", height="1000px", main="The Social Sciences Research Laboratories Collaborators") %>%
    visOptions(highlightNearest = TRUE, selectedBy = "Laboratories.and.Collaborators")
}) 

#2013-2014 Network Analysis
output$network5 <- renderVisNetwork({
  visNetwork(nodes4, links4, width="100%", height="1000px", main="The Social Sciences Research Laboratories Collaborators") %>%
    visOptions(highlightNearest = TRUE, selectedBy = "Laboratories.and.Collaborators")
}) 

#2014-2015 Network Analysis
output$network6 <- renderVisNetwork({
  visNetwork(nodes5, links5, width="100%", height="1000px", main="The Social Sciences Research Laboratories Collaborators") %>%
    visOptions(highlightNearest = TRUE, selectedBy = "Laboratories.and.Collaborators")
}) 

#2015-2016 Network Analysis
output$network7 <- renderVisNetwork({
  visNetwork(nodes6, links6, width="100%", height="1000px", main="The Social Sciences Research Laboratories Collaborators") %>%
    visOptions(highlightNearest = TRUE, selectedBy = "Laboratories.and.Collaborators")
}) 


}) #last line

