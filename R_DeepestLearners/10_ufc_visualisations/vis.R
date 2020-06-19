# ****************
# No License.
# The model is for educational purposes only
# ****************
# 2019 PRATICAL BUSINESS ANALYTICS - University of Surrrey
#
# Group Name   : The Deepest Learners
# November-December 2019
# ****************
# Data Analysis for UFC fight data 1993-2019
# About This Script:   - Generates set of figures in Plots and Viewer which some are interactive visualisations.
#
# ****************

# Set the currect source file as Working Directory
sourceFile<- dirname(parent.frame(2)$ofile)
setwd(sourceFile)

gc() # garbage collection to automatically release memory

# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")

#  clears all objects in "global environment"
rm(list=ls())

print("~~ VISUALISATION STARTED:")

# Global variables - i.e. available to all functions
DATA_FILE <- "../01_ufc_preprocess/UFC_FINAL.csv"

# Define and then load the libraries used in this project
MYLIBRARIES<-c(
  "plotly",
  "countrycode",
  "ggthemes",
  "gridExtra",
  "ggplot2",
  "dplyr"
) 

library(pacman) 
pacman::p_load(char= MYLIBRARIES,install=TRUE,character.only=TRUE) 

print("Please Wait...")


t <- list(family = "sans-serif",size = 14,color = 'black') # Text style
m <- list(l = 8,r = 8,b = 35,t = 35,pad =1) # Magins
# *********************** #
# 1- KNN Accuracy Chart
acc_df<-read.csv("../03_ufc_knn/acc_df.csv",encoding="UTF-8",stringsAsFactors = FALSE) # accuracy table from knn.r
acc_df <- acc_df[,c(1,2,4)] 
names(acc_df) <- c("k_value","avg_accuracy","pca_avg_accuracy")
is.num <- sapply(acc_df, is.numeric) # Format to 3 Decimal Points
acc_df [is.num] <- lapply(acc_df [is.num], round, 3)

x <- acc_df$k_value
y1 <- acc_df$avg_accuracy
y2 <- acc_df$pca_avg_accuracy

t1 <- list(family = "sans-serif",size = 16,color = 'black') # Text style
m1 <- list(l = 50,r = 50,b = 100,t = 100,pad = 4) # Magins
a<-plot_ly(acc_df,x=x, y=y1, type="scatter", mode="line", name="KNN") %>%
  add_trace(y = y2, name = 'PCA-KNN', mode = 'lines') %>%
  layout(
    title="KNN Average Accuracy on 30 runs per K",
    yaxis = list(
      title="Accuracy (%)",
      range=c(55,72)
    ),
    xaxis = list(
      title="K Value",
      range=c(0,105)
    ),
    font = t,
    margin = m
  )


print(a)


# *********************** #
# 2- Weight Class Donut
ufc_data <- read.csv("../data.csv",encoding="UTF-8",stringsAsFactors = TRUE)
is.num <- sapply(ufc_data, is.numeric) # Format to 3 Decimal Points
ufc_data [is.num] <- lapply(ufc_data [is.num], round, 3)

weight_class <- ufc_data$weight_class
weight_class <- na.omit(weight_class) # drop na
weight_class <- as.data.frame(table(ufc_data$weight_class)) # frequency

t2 <- list(family = "sans-serif",size = 16,color = 'black') # Text style
m2 <- list(l = 50,r = 50,b = 100,t = 100,pad = 4) # Magins
b <- plot_ly(weight_class, labels = ~Var1, values = ~Freq)%>%add_pie(hole = 0.6) %>%
  layout(title = "UFC Weight Class 1993 - 2019",
         showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE,showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         font = t,
         margin = m
  )

print(b)



# *********************** #
# 3- Location Map Graph
ufc_data$location <- na.omit(ufc_data$location) # drop na
# Extract Country
countryList <- c()
for(loc in ufc_data[,]$location){
  country <- strsplit(loc,",") # split by ,
  country <- country[[1]][length(country[[1]])] # get country
  countryList <- c(countryList,country)
  
}
countryDF <- data.frame(countryList)
countryDF <- as.data.frame(table(countryDF)) # frequency
codes<-as.data.frame(countrycode(countryDF$countryDF, 'country.name', 'iso3c')) # get contry codes
countryDF<- data.frame(countryDF,codes)
names(countryDF) <- c("country", "fights", "code")

t3 <- list(family = "sans-serif",size = 16,color = 'black') # Text style
m3 <- list(l = 50,r = 50,b = 100,t = 100,pad = 4) # Magins
l <- list(color = toRGB("grey"), width = 0.5) # light grey boundaries
g <- list(showframe = FALSE,showcoastlines = TRUE,projection = list(type = 'Mercator')) # specify map projection/options
c <- plot_geo(countryDF) %>%
  add_trace(
    z = ~fights, color = ~fights, colors = 'Blues',
    text = ~country, locations = ~code, marker = list(line = l)
  ) %>%
  colorbar(title = 'UFC Events') %>%
  layout(
    title = '1993 - 2019 UFC EVENTS WORLDWIDE',
    geo = g,
    font = t,
    margin = m
  )

print(c)



# *********************** #
# 4- Events vs Years BarChart
ufc_data$date <- na.omit(ufc_data$date) # drop na
# Extract Year
yearsList <- c()
for(date in ufc_data[,]$date){
  date <- strsplit(date,"-") # split by -
  date <- date[[1]][1] # get date
  yearsList <- c(yearsList,date)
  
}
yearsDF <- data.frame(yearsList)
yearsDF <- as.data.frame(table(yearsDF)) # frequency
names(yearsDF) <- c("year", "count")
x4 = yearsDF$year
y4 = yearsDF$count

t4 <- list(family = "sans-serif",size = 14,color = 'Black') # Text style
m4 <- list(l = 50,r = 50,b = 100,t = 100,pad = 4) # Magins
bar_color <- rep("#3caef2",27)
bar_color[22] <- '#07466c'
d <- plot_ly(yearsDF, x = ~x4, y = ~y4, type = 'bar',text=y4, textposition="auto",
             marker = list(color = bar_color)) %>%
  layout(title = "Number of UFC matches Over Years",
         xaxis = list(title = "Year"),
         yaxis = list(title = "No. Of matches"),
         font = t,
         margin = m)


print(d)


# *********************** #
# 5- Density plots

fighter_measures <- data.frame(
  "height"  = c(ufc_data$B_Height_cms, ufc_data$B_Height_cms),
  "reach"   = c(ufc_data$B_Reach_cms, ufc_data$R_Reach_cms),
  "weight"  = c(ufc_data$B_Weight_lbs, ufc_data$R_Weight_lbs),
  "age"     = c(ufc_data$B_age, ufc_data$R_age))
fighter_measures <- na.omit(fighter_measures)

p1 <- ggplot(fighter_measures, aes(x=age))+
  geom_density(color="darkblue", fill="lightblue")

p2 <- ggplot(fighter_measures, aes(x=height))+
  geom_density(color="darkblue", fill="lightblue")

p3 <- ggplot(fighter_measures, aes(x=weight))+
  geom_density(color="darkblue", fill="lightblue")

p4 <- ggplot(fighter_measures, aes(x=reach))+
  geom_density(color="darkblue", fill="lightblue")

# Subcharts
grid.arrange(p1, p2, p3, p4,  ncol=2, nrow=2)


# pie charts
fighter_stat <- read.csv("../02_fighter_stat_SQL/master_fighter_recent_stats.csv",encoding="UTF-8",stringsAsFactors = TRUE)
is.num <- sapply(fighter_stat, is.numeric) # Format to 3 Decimal Points
fighter_stat [is.num] <- lapply(fighter_stat [is.num], round, 3)

# 6- Win Type
win_by <- c()
win_by_count <- c()
for (col in names(fighter_stat)[10:15]){
  win_by <- c(win_by, col)
  win_by_count <- c(win_by_count,(sum(fighter_stat[,col])))
}
win_by <- data.frame(win_by,win_by_count)
win_by$win_by <- c("Decision Majority", "Decision Split", "Decision Unanimous", "knock Out", "Submission", "Doctor Stoppage")
f <- plot_ly(win_by, labels = ~win_by, values = ~win_by_count, type = 'pie',
             textposition = 'inside',
             textinfo = 'percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste(win_by_count, ' wins'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1))) %>%
  layout(title = 'Win Type',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         font = t,
         margin = m)
print(f)

# 7- Stance Type
stance <- c()
stance_count <- c()
for (col in names(fighter_stat)[21:24]){
  stance <- c(stance, col)
  stance_count <- c(stance_count,(sum(fighter_stat[,col])))
}
stance <- data.frame(stance,stance_count)
stance$stance <- c("Open Stance", "Orthodox", "SouthPaw", "Switch")
g <- plot_ly(stance, labels = ~stance, values = ~stance_count, type = 'pie',
             textposition = 'inside',
             textinfo = 'percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste(stance_count, stance),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1))) %>%
  layout(title = 'Stance Type',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         font = t,
         margin = m)
print(g)

# 8- Models' Accuracy radar chart
h <- plot_ly(type = 'scatterpolar', fill = 'toself', mode="markers") %>%
  add_trace(
    r = c(71.61, 69.11, 67.94, 67.42),
    theta = c('KNN','SVM','DNN', 'RF'),
    name = 'Normal Data'
  ) %>%
  add_trace(
    r = c(71.27, 68.77, 67.72, 66.67),
    theta = c('KNN','SVM','DNN', 'RF'),
    name = 'PCA-Performed Data'
  ) %>%
  layout(
    margin=c(l=1,r=1,t=2,b=1),
    title = "Models Accuracy Performance",
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(65,72)
      )
    )
  )
print(h)

# 9- Models' Accuracy bar chart
models <- c("KNN", "SVM", "DNN", "RF")
normalt <- c(0.69, 0.692, 0.66, 0.66)
pca_accu <- c(0.68, 0.694, 0.67, 0.65)
data <- data.frame(models, normalt, pca_accu)

i <- plot_ly(data, x = ~models, y = ~normalt, type = 'bar', name = 'Normal Data') %>%
  add_trace(y = ~pca_accu, name = 'PCA-Performed Data') %>%
  layout(yaxis = list(title = 'Accuracy (%)',range=c(0.55,0.72)), barmode = 'group')
print(i)

print("~~ VISUALISATION ENDED:")




