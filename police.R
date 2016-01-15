#install text mining packages
needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
install.packages(needed, dependencies=TRUE) 
library(wordcloud)   
library(tm)
library(ggplot2)

#select dataset: Independent Police Review Authority FOIA requests from March 2011 to September 2015
#taken from https://data.cityofchicago.org/browse?q=foia&sortBy=relevance&utf8=%E2%9C%93 
police_foia = file.choose()
requests = read.csv(police_foia,colClasses="character",header=TRUE)

###########
#Determine types of organizations that make FOIA requests
#create wordcloud of organizations
#create histogram of 5 types of organizations by freq (through manual labeling): 
#law firms, educational institutions, legal non-profits/advocacy groups, media, other

#create vector of all requestors' listed organizations, including "Unknown"
all_organizations = as.character(requests$organization)

#create vector of all named_organizations, deleting all "Unknown's" 
named_organizations = all_organizations
remove = c("Unknown")
named_organizations %in% remove
named_organizations = named_organizations[! named_organizations %in% remove]

#create a wordcloud of organizations that request FOIA records
set.seed(142)   
wordcloud(named_organizations, random.order=FALSE, colors=brewer.pal(6, "Dark2"),min.freq=2, scale=c(10,.8),rot.per=.15,max.words=500)

#create a table of occurrences for all named organizations
all_organizations_table = table(all_organizations) 
named_organizations_table = table(named_organizations) 

#write named_organizations_table to a CSV
write.csv(named_organizations_table, file = "named_organizations_table.csv")

#show breakdown of organizations by type
#note that I manually labeled the organizations by type and I combine similar organizations in the CSV I wrote and redownload the data
named_organizations_table = file.choose()
named_organizations_table = read.csv(named_organizations_table,colClasses="character",header=TRUE)

#create data frame of organizations by type and number of requests
organizations_by_requestcount = c(as.numeric(named_organizations_table$law_firms[1]),
                         as.numeric(named_organizations_table$media_outlets[1]),
                         as.numeric(named_organizations_table$publicdefender_legalaid_advocacygroups[1]),
                         as.numeric(named_organizations_table$educational_institutions[1]),
                         as.numeric(named_organizations_table$other[1]))
organizations_by_type = c("Law Firm","Media","Public Defender/\nNon-Profit","Educational\nInstitution","Other")
organizations_by_typeandcount = data.frame(organizations_by_type, organizations_by_requestcount)

#create column chart of organzations by request type
plot = ggplot(organizations_by_typeandcount, aes(x = factor(organizations_by_type, as.character(organizations_by_type)), y = organizations_by_requestcount,  fill=factor(organizations_by_requestcount))) + geom_bar(stat = "identity")
plot = plot + labs(x = "\nType of Organization", y = "Number of Requests\n") + guides(fill=guide_legend(title=NULL)) 
plot = plot + ggtitle(expression(atop("FOIA Requests by Organization Type", atop("March 2011 - September 2015", ""))))
plot = plot + theme(plot.title = element_text(size = 22,face = "bold")) + guides(fill=FALSE) 


###########
#find unaffiliated individuals who make requests
#dataframe is called unknown_organizations
#create array of individuals who made only one request

#select blank CSV with appropriate column headings for writing purposes
request_headings = file.choose()
unknown_organizations = read.csv(request_headings,colClasses="character",header=TRUE)
known_organizations = read.csv(request_headings,colClasses="character",header=TRUE)


for(i in 1:length(requests$name)){
  if(requests$organization[i] == "Unknown"){
    unknown_organizations[nrow(unknown_organizations) + 1, ] <- c(requests[i,])
  }
}

for(i in 1:length(requests$name)){
  if(requests$organization[i] != "Unknown"){
    known_organizations[nrow(known_organizations) + 1, ] <- c(requests[i,])
  }
}

#table of organizations who made requests (sorted by freq) to find the organizations who make most requests
sorted_orgs = sort(table(known_organizations$ORGANIZATION))

#table of unaffiliated individuals who made requests (sorted by freq) to find the individuals who make most requests
sorted_individuals = sort(table(unknown_organizations$REQUESTOR.NAME))

#turn table into a data frame
sorted_individuals = as.data.frame(sorted_individuals)

#create array of individuals with only one request
individuals_one_request = sorted_individuals[!(sorted_individuals$sorted_individuals > 1),]

#find how many individuals made only one request
length(individuals_one_request)

#create CSV of unaffiliated Individual Requests
write.csv(named_organizations_table, file = "unaffiliated_requests.csv")

    
###########
#plot number of organizations that make requests vs. unaffiliated individuals that make requests vs. 
#unaffiliated individuals that make less than one request

organizations_vs_individuals_count = c(length(unknown_organizations$REQUESTOR.NAME), length(individuals_one_request), length(known_organizations$REQUESTOR.NAME))
organizations_vs_individuals_category = c("Unaffiliated Individual", "Unaffiliated Individual\n with Only One Request", "Organization")
organizations_vs_individuals = data.frame(organizations_vs_individuals_category, organizations_vs_individuals_count)

plot = ggplot(organizations_vs_individuals, aes(x = factor(organizations_vs_individuals_category, as.character(organizations_vs_individuals_category)), y = organizations_vs_individuals_count, fill = factor(organizations_vs_individuals_category))) + geom_bar(stat = "identity")
plot = plot + scale_fill_manual(values=c("#619CFF", "#F8766D", "#F8766D"))
plot = plot + labs(x = "\nType of Requestor", y = "Number of Requests\n") + guides(fill=FALSE) 
plot = plot + ggtitle(expression(atop("FOIA Requests by Type", atop("March 2011 - September 2015", ""))))

###########
#make scatterplot of requests from 2011 to 2015 by individual and organization

#upload CSV of all requests
five_year_requests = file.choose()
five_year_requests = read.csv(five_year_requests,colClasses="character",header=TRUE)

#dataframe of all unaffiliated individuals and affiliated individuals
FOIA_headings = file.choose()
FOIA_headings = read.csv(FOIA_headings,colClasses="character",header=TRUE)

#create dataframes for affiliated requests and unaffiliated requests
unaffiliated = FOIA_headings
affiliated = FOIA_headings
master = FOIA_headings

#create variable that contains all entries in "ORGANIZATION" column 
all_requests = as.character(five_year_requests$ORGANIZATION)

#Check for various spellings of Unknown, so you can manually change them in the CSV
#re-upload CSV after manually changing all "Unknown" entries
table(all_requests)

for(i in 1:length(five_year_requests$REQUESTOR)){
  if(five_year_requests$ORGANIZATION[i] == "Unknown"){
    unaffiliated[nrow(unaffiliated) + 1, ] <- c(five_year_requests[i,])
  }
  else{
    five_year_requests$ORGANIZATION[i] = "Organization"
    affiliated[nrow(affiliated) + 1, ] <- c(five_year_requests[i,])
  }
}

#function that pulls last two digits of string from http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#create vectors of FOIA requests by year
affiliated_Date_Received = substrRight(affiliated$DATE.RECEIVED, 2)
affiliated_Date_Received = table(affiliated_Date_Received)
affiliated_Date_Received = data.frame(affiliated_Date_Received)

unaffiliated_Date_Received = substrRight(unaffiliated$DATE.RECEIVED, 2)
unaffiliated_Date_Received = table(unaffiliated_Date_Received)
unaffiliated_Date_Received = data.frame(unaffiliated_Date_Received)

#delete rows without a date
unaffiliated_Date_Received = unaffiliated_Date_Received[-c(1),]
affiliated_Date_Received = affiliated_Date_Received[-c(1),]

#add row to identify organization requests vs. individual requests
unaffiliated_Date_Received$Type = c("Unaffiliated","Unaffiliated","Unaffiliated","Unaffiliated","Unaffiliated")
affiliated_Date_Received$Type = c("Organization","Organization","Organization","Organization","Organization")

#make common column name
names(unaffiliated_Date_Received)[1] <- "Annual.Requests"
names(affiliated_Date_Received)[1] <- "Annual.Requests"

#bind difference data frames
five_year_requests = rbind(unaffiliated_Date_Received,affiliated_Date_Received)


plot = ggplot(data = five_year_requests, aes(x=Annual.Requests, y=Freq, group = Type, colour = Type)) +
  geom_line(aes(),size = 2) +
  geom_point() 

plot = plot + labs(title = "IPRA FOIA Requests\n2011 to 2015", x = "\nYear", y = "Number of Requests\n") 
plot = plot + theme(plot.title = element_text(size = 22, face = "bold"))

###########
#Return gender probabilities for each first name, according to U.S. Social Security Administration Data
#Employs "gender" package from https://cran.r-project.org/web/packages/gender/gender.pdf
#graph breakdown of unaffiliated requestors (according to gender probability) vs. gender of those who file complaints

#write CSV using table of all unaffiliated individuals (from earlier graph)
write.csv(sorted_individuals, file = "unaffiliated_individuals.csv")

# use excel formula (=LEFT(A2,FIND(" ",A2)-1)) to find first names of all individuals and re-upload CSV
unaffiliated = file.choose()
unaffiliated = read.csv(unaffiliated,colClasses="character",header=TRUE)

#create vector of all defendant first names
firstnames = as.character(unaffiliated$firstname)

#find gender probabilities, according to U.S. Social Security Administration baby name data from 1932 to 2012
library(gender)
gender = gender(firstnames, method = "ssa")

#create dataframe of all names   
gender_prob = data.frame(matrix(unlist(gender), nrow=437, byrow=T),stringsAsFactors=FALSE)

#tabulate requestors by gender
tabulated_gender = table(gender_prob$X4)
requestor_gender = data.frame(tabulated_gender)

#turn requestor_gender into percentages
a = requestor_gender[1,2]
b = requestor_gender[2,2]
requestor_gender[1,2] = (a / (a + b)) * 100
requestor_gender[2,2] = (b / (a + b)) * 100
names(requestor_gender)[2] = "Percentage"
names(requestor_gender)[1] = "Gender"

#create vector of complaint gender
#taken from http://cpdb.co/data/AO4o4D/citizens-police-data-project
Gender = c("female","male","transgender")
Percentage = c(54,44,1)
complaint_gender = data.frame(Gender, Percentage)

requestor_gender$Type = c("FOIA","FOIA")
complaint_gender$Type = c("Complaint","Complaint","Complaint")

#combine dataframes
gender_breakdown = rbind(requestor_gender,complaint_gender)

#to make name of legend "Gender"
Gender = gender_breakdown$Gender

#create plot
plot = ggplot(data = gender_breakdown, aes(x = gender_breakdown$Type, y = gender_breakdown$Percentage, fill = Gender)) + 
  geom_bar(stat="identity") 

#annotate plot
plot = plot + labs(x = "\n\n\n Note: Unaffiliated Requestor Breakdown based on U.S. Social Security Administration Baby Name Data", y = "Percent\n")  
plot = plot + ggtitle(expression(atop("Gender Breakdown: Complaints and FOIA Requests ", atop("March 2011 - September 2015", ""))))
plot = plot + theme(plot.title = element_text(size = 22, face = "bold"))
plot = plot + scale_x_discrete(labels = c("Complaint" = "Individuals who Filed Complaints", "FOIA" = "Unaffiliated Requestors"))
plot = plot + theme(axis.text.x = element_text(face="bold", size=14))
plot = plot + theme(axis.title.x = element_text(size=10))

###########
#find trends in request descriptions 

#create vector of request descriptions
request_descriptions = as.character(requests$DESCRIPTION)

#turn vector of request descriptions into a corpus
corp <- Corpus(VectorSource(request_descriptions))
dtm <- DocumentTermMatrix(corp)

