# Case Study - Extracting Data from the Web with Web Scraping in R

# Web Crawling - "crawling" through a web page or website looking for data
# Web Scraping - "scraping" data from a web page


# Obs: If you have problems with the accentuation, see this link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configuring the working directory
# Enclose the working directory you are using on your computer in quotation marks
# Do not use directories with a space in the name
setwd ("C: / FCD / BigDataRAzure / Cap07")
getwd ()


# R Packages for Web Scraping
# RCurl
# httr
# XML
# rvest

# Rvest package - useful for those who don't know HTML and CSS
install.packages ('rvest')
library (rvest)

library (stringr)
library (dplyr)
library (lubricate)
library (readr)

# Reading the web page - Returns an xml document
# reads an html document and brings a semi structured document in xml
# link q explains xml: https://www.tecmundo.com.br/programacao/1762-o-que-e-xml-.htm
webpage <- read_html ("https://www.nytimes.com/interactive/2017/06/23/opinion/trumps-lies.html")
webpage
str (webpage)
summary (webpage)


# Extracting records
# Each element on the web page above has the following format in html:
# <span class = "short-desc"> <strong> DATE </strong> LIE <span class = "short-truth"> <a href="URL"> EXPLANATION </a> </span> </ span >
? html_nodes
#you have to know a little about html tags
#the function below creates a tag list based on html
results <- webpage%>% html_nodes (". short-desc")
results
# link on html introduction: https://www.youtube.com/watch?v=63Otg81TjqA

# Building the dataset
# we created a dataset with the same length as the file to be built
#in order to receive data from html
records <- vector ("list", length = length (results))
records

# the "for" below has all the logic of what you want to do, a loop
#perform the data according to the file length
#for this we use the function "i in seq_along (results))"
#then search for the text of the tag and the date tag and "strong"
#%>% the concatenation is done, then "html_text (trim = TRUE)"
#where the trim serves to eliminate empty spaces before and after the text
#with that we extract the dates
# lie and explanation is done next
for (i in seq_along (results)) {
  date <- str_c (results [i]%>%
                   html_nodes ("strong")%>%
                   html_text (trim = TRUE), ', 2017')
  
  lie <- str_sub (xml_contents (results [i]) [2]%>% html_text (trim = TRUE), 2, -2)
  
  explanation <- str_sub (results [i]%>%
                            html_nodes (". short-truth")%>%
                            html_text (trim = TRUE), 2, -2)
  
  url <- results [i]%>% html_nodes ("a")%>% html_attr ("href")
  
  records [[i]] <- data_frame (date = date, lie = lie, explanation = explanation, url = url)
}


# Final Dataset
#to join all lines
df <- bind_rows (records)


# Transforming the date field to the Date format in R
#converted the date is in text format for date
df $ date <- mdy (df $ date)


# Exporting to CSV
write_csv (df, "lies_trump.csv")


# Reading the data
df <- read_csv ("lies_trump.csv")
View (df)
