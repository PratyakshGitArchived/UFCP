# ************************************************
# No License.
# The model is for educational purposes only
# ************************************************
# 2019 PRATICAL BUSINESS ANALYTICS - University of Surrrey
#
# Group Name   : The Deepest Learners
# November-December 2019
# ************************************************
# Data Analysis for UFC PPV SALES 2001 - 2017
# ************************************************

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

print("PBA PROJECT STARTED:")

# Global variables - i.e. available to all functions
PPV_FILE <- "ufc_ppv_buys.csv"

# Define and then load the libraries used in this project
MYLIBRARIES<-c(
  "dplyr",
  "ggplot2",
  "reshape2",
  "plotly"
) 


library(pacman) 
pacman::p_load(char= MYLIBRARIES,install=TRUE,character.only=TRUE) 

# ************************************************
# NreadDataset() :
# Read a CSV file from working directory
#
# INPUT: string - csvFilename - CSV filename
#
# OUTPUT : data frame - contents of the headed CSV file
# ************************************************
NreadDataset <- function(csvFilename){
  
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)
  
  # The field names "confuse" some of the library algorithms
  # As they do not like spaces, punctuation, etc.
  names(dataset)<-NPREPROCESSING_removePunctuation(names(dataset))
  
  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}

# ************************************************
# NPREPROCESSING_removePunctuation()
#
# INPUT: String - fieldName - name of field
#
# OUTPUT : String - name of field with punctuation removed
# ************************************************
NPREPROCESSING_removePunctuation<-function(fieldName){
  return(gsub("[[:punct:][:blank:]]+", "", fieldName))
}

# ************************************************
# main code goes below:
# ************************************************


# Load PPV Unproccessed Data
PPV_DATA <- NreadDataset(PPV_FILE)
keeping_cols <- which(colnames(PPV_DATA)=="Year" | colnames(PPV_DATA) == "PPV") # intersting columns
PPV_DATA <- PPV_DATA[,keeping_cols]

# Group PPV sales by year
PPV_Grouped_year <- aggregate(PPV_DATA,
                              by = list(PPV_DATA$Year),
                              FUN = mean)[,-1]
PPV_Grouped_year$PPV <- round(PPV_Grouped_year$PPV)


# Density plot
a <- plot_ly(PPV_DATA, x = ~PPV, type = "histogram", name = "Histogram" ) %>% 
  add_trace(x = density(PPV_DATA$PPV)$x, y = density(PPV_DATA$PPV)$y,type="scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
  layout(yaxis2 = list(overlaying = "y", side = "right"),title="PPV Sales Density & Histogram Plots")
print(a)

# Regression plot
b <- ggplot(PPV_DATA, aes(x=Year, y = PPV/1000), binwidth = 5, ) + geom_point() +
  geom_smooth(method = loess, se = TRUE) +
  ylab("PPV(Thousands)")+
  ggtitle("Average Purchase Plot")
print(b)

# Barchart
y = PPV_Grouped_year$PPV
c <- plot_ly(PPV_Grouped_year,x = ~Year , y = ~PPV, type = 'bar',text = round(y/1000), textposition = 'auto') %>%
  layout(
    title="PPV Sales",
    xaxis = list(
      title="Year",
      dtick = 1, 
      tickangle="-45",
      tickmode = "linear"
    ),
    yaxis = list(
      title = "No. of Sales"
    )
    )

print(c)

# Boxplot
d <- ggplot(PPV_DATA,aes( x = as.factor(Year), y = PPV/1000)) + geom_boxplot()+
  ylab("PPV(Thousands)")+
  ggtitle("PPV Boxplot")+
  xlab("Year")
print(d)





