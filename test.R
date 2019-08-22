library(rvest)
url <- "https://www.state.nj.us/treasury/administration/statewide-support/motor-fuel-locations.shtml"

#Reading the HTML code from the website
webpage <- read_html(url)


p_nodes<-webpage%>%
  html_nodes(xpath = '//p')%>%
  html_text()




#replace multiple whitespaces with single space
p_nodes<- gsub('\\s+',' ',p_nodes)
#trim spaces from ends of elements
p_nodes <- trimws(p_nodes)
#drop blank elements
p_nodes <- p_nodes[p_nodes != '']

