library(shiny)
library(shinydashboard)
library(RCurl) #package for the get URL function 
library(plotly)
library(RColorBrewer) #colors for plot_ly
require(ggplot2)
library(dplyr)
library(grid) #adding text to ggplots 
library(DT) #datatables
library(ggvis)
library(visNetwork) # Package needed for the interactive SNA
library(leaflet) #adding a map
library(dplyr)

##### TOTAL RESEARCH STUDIES
Fig1<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/TotalProjects20182019.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")


#Changing the levels so most recent year is displayed first
Fig1$Year<-as.factor(Fig1$Year)

test<- ggplot(Fig1, aes(x= Year, y=Count, fill= Status)) +
  geom_bar(stat= "identity", position="stack", width= 0.5)+
  scale_fill_manual(values=c('#420c00', '#354377'))+
  #labs(caption= "Total Research Studies")+
  #ggtitle("Total Research Studies") + #MAIN TITLE
  #theme(plot.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=20, hjust=1.0, vjust=-0.5))+
  xlab(NULL)+ #X-axis label 
  theme (axis.title.x=element_text (angle=0, size=18, face="bold", color="black"))+  #Formating title of x-axis
  theme (axis.text.x=element_text(angle=0, size=8, vjust=90))+ #YEAR LABELS ON X AXIS WAS 12
  theme (axis.text.y=element_text(angle=0, size=10))+
  ylab (NULL) + #no title for y-axis
  #geom_text(aes(Year, total, label = total, fill = NULL, vjust=-0.5,  size=20), data = totals)+
  #annotate("text", label= "39", fontface="bold", x=1, y=39)+
  #annotate("text", label= "92", x=2, y=92)+
  #annotate("text", label= "156", x=3, y=156)+
  #annotate("text", label= "208", x=4, y=208)+
  #annotate("text", label= "299", x=5, y=299)+
  theme (panel.grid.minor=element_blank(), #Hiding the minor gridlines
         panel.grid.major=element_blank()) + #Hiding the major gridlines
  theme (plot.background=element_rect(fill='white'))+ #changes the plot background (not the panel) colour
  theme (panel.background=element_rect (fill='white'))+
  theme (axis.line.x=element_line(color="black", size=1))+ #black line on x-axis
  theme (axis.line.y=element_line(color="black", size=1))+
  scale_y_continuous (breaks=seq(0,350, by=50), expand=c(0,0))+ #setting custom y-axis breaks & also expand=c removes space btwn bars and x-axis
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0,350))+ #zooms into the y-axis at 0 to 600 
  theme(legend.title=element_blank(), legend.position="bottom") #removes title from legend
#####

##### STUDENT ENGAGEMENT

Fig2<- read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/StudentEngagement20182019.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
#removing Post Docs
Fig2<- Fig2 %>%  filter(Student!="Post Doctoral Fellows")
#ordering Factor levels
Fig2$Student<- as.factor(Fig2$Student)
Fig2$Student<- factor(Fig2$Student,
                      levels=c(#"Post Doctoral Fellows",
                               "Doctoral Students",
                               "Master's Students",
                               "Undergraduate Students"))

#Changing the levels so most recent year is displayed first
Fig2$Year<-as.factor(Fig2$Year)



test1<- ggplot(Fig2, aes(x= Year, y=Count, fill= Student)) +
  geom_bar(stat= "identity", position="stack", width= 0.5)+
  scale_fill_manual(values=c(#'#050000',#black: Post Docs
                             '#a01d00',#yellow PhD
    # '#d8d652',#yellow PhD
    
                             '#420c00',#red Master's
                             '#354377'))+#blue Undergrad
  #'#354377'))+#blue Undergrad
                   
  #labs(caption= "Total Research Studies")+
  #ggtitle("Total Research Studies") + #MAIN TITLE
  #theme(plot.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=20, hjust=1.0, vjust=-0.5))+
  xlab(NULL)+ #X-axis label 
  theme (axis.title.x=element_text (angle=0, size=18, face="bold", color="black"))+  #Formating title of x-axiS. 
  theme (axis.text.x=element_text(angle=0, size=7, vjust=90))+  #YEAR LABELS ON X AXIS WAS 12
  theme (axis.text.y=element_text(angle=0, size=8))+
  ylab (NULL) + #no title for y-axis
  #geom_text(aes(Year, total, label = total, fill = NULL, vjust=-0.5,  size=20), data = totals)+
  #annotate("text", label= "328", x=1, y=328)+
  #annotate("text", label= "372", x=2, y=372)+
  #annotate("text", label= "274", x=3, y=274)+
  #annotate("text", label= "638", x=4, y=638)+
  #annotate("text", label= "753", x=5, y=753)+
  theme (panel.grid.minor=element_blank(), #Hiding the minor gridlines
         panel.grid.major=element_blank()) + #Hiding the major gridlines
  theme (plot.background=element_rect(fill='white'))+ #changes the plot background (not the panel) colour
  theme (panel.background=element_rect (fill='white'))+
  theme (axis.line.x=element_line(color="black", size=1))+ #black line on x-axis
  theme (axis.line.y=element_line(color="black", size=1))+
  scale_y_continuous (breaks=seq(0,800, by=50), expand=c(0,0))+ #setting custom y-axis breaks & also expand=c removes space btwn bars and x-axis
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0,800))+ #zooms into the y-axis at 0 to 800 
  theme(legend.title=element_blank(), legend.position="bottom")

#margins for plotly graph
m <- list(
  l = 10
  ,r = 50
  ,b = 100
  ,t = 100
  ,pad = 4
)

##### 
#COLABORATIONS
#Table 1- Number of Research Studies By Lab
Table1<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/SingleLabProjects20182019.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

#Renaming Columns
colnames(Table1)[2]<- "2012-13"
colnames(Table1)[3]<- "2013-14"
colnames(Table1)[4]<- "2014-15"
colnames(Table1)[5]<- "2015-16"
colnames(Table1)[6]<- "2016-17"
colnames(Table1)[7]<- "2017-18"
colnames(Table1)[8]<- "2018-19"

#Creating Hyperlinks
#Links<- c("ssrl.usask.ca/laboratories/col.php", # COL
#          "ssrl.usask.ca/laboratories/crl.php",#CRL
#          "ssrl.usask.ca/laboratories/edl.php", #EDL
#          "ssrl.usask.ca/laboratories/ehl.php", #EHL
#          "ssrl.usask.ca/laboratories/mmrl.php",#MMRL
#          "ssrl.usask.ca/laboratories/qrl.php", #QRL
#          "ssrl.usask.ca/laboratories/tsl.php", #Spatial
#          "ssrl.usask.ca/laboratories/sgal.php", #SGAL
#          "ssrl.usask.ca/laboratories/snl.php", #SNL
#          "ssrl.usask.ca/laboratories/vital.php", #Vital
#          "-" #Multi Lab Collab 
#)
#Table1$Links<-Links
#making hyperlinks in data table
#Table1$Links <- sapply(Table1$Links, function(x) 
#  toString(tags$a(href=paste0("http://", x), x)))

#Arraging Labs by alphabetically
#Table1<- Table1 %>% arrange(Lab)

#TABLE 1 A- Total number of research studies
#Year_1<-39
#Year_2<-92
#Year_3<-156
#Year_4<-208
#Year_5<-299
#Year_6<-339
#Year_7<-346
#Lab<-"TOTAL RESEARCH STUDIES"

#Table1a<-cbind(Lab, 
#               Year_1, 
#               Year_2, 
#               Year_3, 
#               Year_4, 
#               Year_5, 
#               Year_6,
#               Year_7)
#Table1a<- as.data.frame(Table1a)
#Renaming columns
#colnames(Table1a)[1]<- "Summary"
#colnames(Table1a)[2]<- "2012-2013"
#colnames(Table1a)[3]<- "2013-2014"
#colnames(Table1a)[4]<- "2014-2015"
#colnames(Table1a)[5]<- "2015-2016"
#colnames(Table1a)[6]<- "2016-2017"
#colnames(Table1a)[7]<- "2017-2018"
#colnames(Table1a)[8]<- "2018-2019"

#####
#Figure 2017-2018: Collaborative Studies
CollabStudies20182019<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/CollabStudies20182019.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")


#Renaming Columns
colnames(CollabStudies20182019)[2]<- "Count"
#FOR PLOT_LY GRAPH
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#070707"
)
x <- list(
  title = "Number of Studies",
  titlefont = f
)
y <- list(
  title = "",
  titlefont = f
)
#####
#Table 3- Number of Research By Origin
Table3<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/AffiliatedResearcherOrigin20182019.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

Table3<- Table3 %>%  arrange(Lab)

Table3$X2012.13<- as.numeric(Table3$X2012.13)
Table3$X2013.14<- as.numeric(Table3$X2013.14)
Table3$X2014.15<- as.numeric(Table3$X2014.15)
Table3$X2015.16<- as.numeric(Table3$X2015.16)
Table3$X2016.17<- as.numeric(Table3$X2016.17)
Table3$X2017.18<- as.numeric(Table3$X2017.18)
Table3$X2018.19<- as.numeric(Table3$X2018.19)

#Renaming Columns
colnames(Table3)[1]<-"Affiliated University of Saskatchewan Colleges, Schools and Supporting Units"
colnames(Table3)[2]<- "2012-13"
colnames(Table3)[3]<- "2013-14"
colnames(Table3)[4]<- "2014-15"
colnames(Table3)[5]<- "2015-16"
colnames(Table3)[6]<- "2016-17"
colnames(Table3)[7]<- "2017-18"
colnames(Table3)[8]<- "2018-19"

#Replacing "NA to blanks
Table3[is.na(Table3)] <- ""

#SUMMARY TABLE OF TABLE 3
Table3a<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/AffiliatedOtherExternal20182019.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

colnames(Table3a)[2]<- "2012-13"
colnames(Table3a)[3]<- "2013-14"
colnames(Table3a)[4]<- "2014-15"
colnames(Table3a)[5]<- "2015-16"
colnames(Table3a)[6]<- "2016-17"
colnames(Table3a)[7]<- "2017-18"
colnames(Table3a)[8]<- "2018-19"

#####

#Research Outputs

Fig3<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/BooksArticlesReports20182019.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

colnames(Fig3)[1]<- "Year"
colnames(Fig3)[2]<- "Review Articles"
colnames(Fig3)[3]<- "Non-Refereed Journal Articles"
colnames(Fig3)[4]<- "Books"
colnames(Fig3)[5]<- "Book Chapters"
colnames(Fig3)[6]<- "Theses"
colnames(Fig3)[7]<- "Websites"
colnames(Fig3)[8]<- "Technical Reports"
colnames(Fig3)[9]<- "Media Articles"
colnames(Fig3)[10]<- "Peer-Reviewed Journal Articles"
colnames(Fig3)[11]<- "Conference Papers and Presentations"
#########################################################################################################################

#Map
Map<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/MapData20182019.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")


Map$Longitude<- as.numeric(Map$Longitude) #convert to numeric
Map$Latitude<- as.numeric(Map$Latitude)
Map$Location <- gsub("'", '', Map$Location) #deal with single quotes (apostrophes)
Map$Location<- iconv(Map$Location, from = 'UTF-8', to = 'ASCII//TRANSLIT')
Map$Year<-as.factor(Map$Year) #Changing the levels so most recent year is displayed first

#name columns
colnames(Map)[1]<- "Location"
colnames(Map)[2]<- "Latitude"
colnames(Map)[3]<- "Longitude"
colnames(Map)[4]<- "Year"

########################################################################################################################

#2016-2017 NETWORK ANALYSIS


nodes<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/ssrlNODES1617.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
links<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/ssrlEDGES1617.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Define nodes properties
nodes$shape <- "dot"                           # Node shape
nodes$shadow <- TRUE                           # Nodes casting shadow boolean
nodes$title <- nodes$name                      # Nodes title
nodes$label <- nodes$labs                      # Nodes labels   
nodes$color.label<- c ("black")                # Nodes label color
nodes$borderWidth <- 1                         # Nodes border size   
nodes$color.border <- "black"                  # Nodes border colour
nodes$color.background <- c("blueviolet", "blue", "red", "midnightblue")[nodes$group] # Nodes colour by group
nodes$size <- nodes$size                       # Nodes size
nodes$color.highlight.background <- "blue"     # Nodes colour when clicked on
nodes$color.highlight.border <- "darkred"      # Nodes border colour when cliked on  

# Define links/edges properties
links$width <- 1+links$value/8 # line width
links$color <- "gray"          # line color  
links$smooth <- FALSE          # should the edges be curved?
links$shadow <- FALSE          # edge shadow

###########################################################################################

#2017-2018 NETWORK ANALYSIS


nodes1<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/ssrlNODES1718.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
links1<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/ssrlEDGES1718.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Define nodes properties
nodes1$shape <- "dot"                           # Node shape
nodes1$shadow <- TRUE                           # Nodes casting shadow boolean
nodes1$title <- nodes1$name                      # Nodes title
nodes1$label <- nodes1$labs                      # Nodes labels   
nodes1$color.label<- c ("black")                # Nodes label color
nodes1$borderWidth <- 1                         # Nodes border size   
nodes1$color.border <- "black"                  # Nodes border colour
nodes1$color.background <- c("blueviolet", "blue", "red", "midnightblue")[nodes1$group] # Nodes colour by group
nodes1$size <- nodes1$size                       # Nodes size
nodes1$color.highlight.background <- "blue"     # Nodes colour when clicked on
nodes1$color.highlight.border <- "darkred"      # Nodes border colour when cliked on  

# Define links/edges properties
links1$width <- 1+links1$value/8 # line width
links1$color <- "gray"          # line color  
links1$smooth <- FALSE          # should the edges be curved?
links1$shadow <- FALSE          # edge shadow

###########################################################################################

#2018-2019 NETWORK ANALYSIS


nodes2<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/ssrlNODES1819.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
links2<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/ssrlEDGES1819.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Define nodes properties
nodes2$shape <- "dot"                           # Node shape
nodes2$shadow <- TRUE                           # Nodes casting shadow boolean
nodes2$title <- nodes2$name                      # Nodes title
nodes2$label <- nodes2$labs                      # Nodes labels   
nodes2$color.label<- c ("black")                # Nodes label color
nodes2$borderWidth <- 1                         # Nodes border size   
nodes2$color.border <- "black"                  # Nodes border colour
nodes2$color.background <- c("blueviolet", "blue", "red", "midnightblue")[nodes2$group] # Nodes colour by group
nodes2$size <- nodes2$size                       # Nodes size
nodes2$color.highlight.background <- "blue"     # Nodes colour when clicked on
nodes2$color.highlight.border <- "darkred"      # Nodes border colour when cliked on  

# Define links/edges properties
links2$width <- 1+links2$value/8 # line width
links2$color <- "gray"          # line color  
links2$smooth <- FALSE          # should the edges be curved?
links2$shadow <- FALSE          # edge shadow

#########################################################################################

#2012-2013 NETWORK ANALYSIS

nodes3<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/ssrlNODES1213.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
links3<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/ssrlEDGES1213.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Define nodes properties
nodes3$shape <- "dot"                           # Node shape
nodes3$shadow <- TRUE                           # Nodes casting shadow boolean
nodes3$title <- nodes3$name                      # Nodes title
nodes3$label <- nodes3$labs                      # Nodes labels   
nodes3$color.label<- c ("black")                # Nodes label color
nodes3$borderWidth <- 1                         # Nodes border size   
nodes3$color.border <- "black"                  # Nodes border colour
nodes3$color.background <- c("blueviolet", "blue", "red", "midnightblue")[nodes3$group] # Nodes colour by group
nodes3$size <- nodes3$size                       # Nodes size
nodes3$color.highlight.background <- "blue"     # Nodes colour when clicked on
nodes3$color.highlight.border <- "darkred"      # Nodes border colour when cliked on  

# Define links/edges properties
links3$width <- 1+links3$value/8 # line width
links3$color <- "gray"          # line color  
links3$smooth <- FALSE          # should the edges be curved?
links3$shadow <- FALSE          # edge shadow

###########################################################################################












#########################################################################################

#2013-2014 NETWORK ANALYSIS

nodes4<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/ssrlNODES1314.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
links4<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/ssrlEDGES1314.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Define nodes properties
nodes4$shape <- "dot"                           # Node shape
nodes4$shadow <- TRUE                           # Nodes casting shadow boolean
nodes4$title <- nodes4$name                      # Nodes title
nodes4$label <- nodes4$labs                      # Nodes labels   
nodes4$color.label<- c ("black")                # Nodes label color
nodes4$borderWidth <- 1                         # Nodes border size   
nodes4$color.border <- "black"                  # Nodes border colour
nodes4$color.background <- c("blueviolet", "blue", "red", "midnightblue")[nodes4$group] # Nodes colour by group
nodes4$size <- nodes4$size                       # Nodes size
nodes4$color.highlight.background <- "blue"     # Nodes colour when clicked on
nodes4$color.highlight.border <- "darkred"      # Nodes border colour when cliked on  

# Define links/edges properties
links4$width <- 1+links4$value/8 # line width
links4$color <- "gray"          # line color  
links4$smooth <- FALSE          # should the edges be curved?
links4$shadow <- FALSE          # edge shadow

###########################################################################################
#########################################################################################

#2014-2015 NETWORK ANALYSIS

nodes5<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/ssrlNODES1415.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
links5<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/ssrlEDGES1415.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Define nodes properties
nodes5$shape <- "dot"                           # Node shape
nodes5$shadow <- TRUE                           # Nodes casting shadow boolean
nodes5$title <- nodes5$name                      # Nodes title
nodes5$label <- nodes5$labs                      # Nodes labels   
nodes5$color.label<- c ("black")                # Nodes label color
nodes5$borderWidth <- 1                         # Nodes border size   
nodes5$color.border <- "black"                  # Nodes border colour
nodes5$color.background <- c("blueviolet", "blue", "red", "midnightblue")[nodes5$group] # Nodes colour by group
nodes5$size <- nodes5$size                       # Nodes size
nodes5$color.highlight.background <- "blue"     # Nodes colour when clicked on
nodes5$color.highlight.border <- "darkred"      # Nodes border colour when cliked on  

# Define links/edges properties
links5$width <- 1+links5$value/8 # line width
links5$color <- "gray"          # line color  
links5$smooth <- FALSE          # should the edges be curved?
links5$shadow <- FALSE          # edge shadow

###########################################################################################
#########################################################################################

#2015-2016 NETWORK ANALYSIS

nodes6<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/ssrlNODES1516.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
links6<-read.csv (text=getURL("https://raw.githubusercontent.com/pinelle/SSRL-annual-report-data-2018-19/master/ssrlEDGES1516.csv"),header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Define nodes properties
nodes6$shape <- "dot"                           # Node shape
nodes6$shadow <- TRUE                           # Nodes casting shadow boolean
nodes6$title <- nodes6$name                      # Nodes title
nodes6$label <- nodes6$labs                      # Nodes labels   
nodes6$color.label<- c ("black")                # Nodes label color
nodes6$borderWidth <- 1                         # Nodes border size   
nodes6$color.border <- "black"                  # Nodes border colour
nodes6$color.background <- c("blueviolet", "blue", "red", "midnightblue")[nodes6$group] # Nodes colour by group
nodes6$size <- nodes6$size                       # Nodes size
nodes6$color.highlight.background <- "blue"     # Nodes colour when clicked on
nodes6$color.highlight.border <- "darkred"      # Nodes border colour when cliked on  

# Define links/edges properties
links6$width <- 1+links6$value/8 # line width
links6$color <- "gray"          # line color  
links6$smooth <- FALSE          # should the edges be curved?
links6$shadow <- FALSE          # edge shadow

###########################################################################################

