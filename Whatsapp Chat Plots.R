setwd("C:\\Users\\jack-\\Desktop")
getwd()

r <- read.csv("TeamTolandCSV.csv")

str(r)

messages <- subset(r, select=c("Date","Time","Sender","Message"))
levels(messages$Sender)
newmess <- messages[(messages$Sender == "Andrew Morton") |
                       (messages$Sender ==  "Angela Toland")|
                       (messages$Sender ==  "Christine O'Hara")| 
                       (messages$Sender ==  "Emma Steeples")| 
                       (messages$Sender ==  "Fiona Morton")| 
                       (messages$Sender ==  "Fraser McKechnie")| 
                       (messages$Sender ==  "Granny")| 
                       (messages$Sender ==  "Jack McKechnie")| 
                       (messages$Sender ==  "Jaclyn Toland")| 
                       (messages$Sender ==  "Katherine Morton")| 
                       (messages$Sender ==  "Katie Morton")| 
                       (messages$Sender ==  "Liam O'Hara")| 
                       (messages$Sender ==  "Liam Toland")| 
                       (messages$Sender ==  "Marion O'Hara")| 
                       (messages$Sender ==  "Matthew Steeples")| 
                       (messages$Sender ==  "Michael Toland")| 
                       (messages$Sender ==  "Nicola Toland")| 
                       (messages$Sender ==  "Roddy McKechnie")| 
                       (messages$Sender ==  "Sarah O'Hara")| 
                       (messages$Sender ==  "Susan McKechnie")| 
                       (messages$Sender ==  "Tricia Steeples"),] 

sort(table(newmess$Sender))

library(ggplot2)
library(plyr)

#---------------- MESSAGES PER PERSON -------------
senders <- count(newmess,'Sender')
senders <- senders[order(senders$freq,decreasing=T),]

# Plot messages per person 
plot <- ggplot(data = senders, aes(x=reorder(Sender, -freq),y=freq))
messages.per.person <- plot + geom_bar(stat="identity",aes(fill=freq)) +
  scale_fill_gradient2(low='orange', mid='Red', high='Orange') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = c(0.9,0.8)) +
  xlab("Name") +
  ylab("No. of messages sent") +
  geom_text(aes(label=freq,vjust=-0.25)) +
  ggtitle("Number of Messages Sent Per Person")
  
messages.per.person


#------------- MESSAGES OVER THE DAY -------------
#Prep data
times <- newmess
options(max.print=1000000)

times[times$Time] <- substring(time$Time,start=2,stop)
times$Time <- as.factor(substr(newmess$Time,2,3))
times <- count(times,'Time')
times <- times[order(times$Time,decreasing=F),]
colnames(times) <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23")


#Plot data
messages.over.time <- ggplot(data = times, aes(x=Time,y=freq,group=1))
messages.over.time <- messages.over.time + geom_point() + geom_smooth(span=0.3,fill="green",colour="darkred")
messages.over.time <- messages.over.time + ylab("No. of messages sent")+ theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Message Frequency Over The Day")

messages.over.time

#------ Number of messages with length -----
install.packages("stringr")
library("stringr")
messlen <- newmess
messlen$Message <- str_length(messlen$Message)
messlen

head(messlen,n=100)

#Plot the data
u <- ggplot(data = messlen, aes(x = Sender,y = Message, colour = Sender))
u + geom_boxplot()
u + geom_boxplot(size=1.2)
u + geom_boxplot(size=1.2)  + geom_point()
message.length <- u + geom_boxplot(size=1.2)  + geom_jitter() +
  theme(legend.position = "none",axis.text.x = element_text(angle = 90))

message.length


#------------- Who sends messages at what time --------
install.packages("lubridate")
library(lubridate)
#Prepare the data 
time.pop <- messlen

time.pop$Date <- weekdays(as.Date(time.pop$Date,'%d/%m/%Y'))
time.pop$Date

days <- count(time.pop$Date)
days <- days[order(days$freq,decreasing=T),]
days

#Plot the data 
plot <- ggplot(data=days,aes(x=reorder(x,-freq),y=freq))
plot + geom_bar(stat="identity",aes(fill=freq)) + 
  scale_fill_gradient2(mid='red', high='green') +
  theme(axis.text.x = element_text(angle = 90),legend.position = c(0.9,0.88)) +
  xlab("Day of the week") +
  ylab("No. of messages sent") +
  geom_text(aes(label=freq,vjust=-0.25)) +
  ggtitle("Days of the Week")
 


#----------- Most photos sent -----------
photos <- newmess
head(photos$Message)
#photos$Message<- grepl("image omitted",photos$Message,)
tfvect <- as.vector(grepl("image omitted",photos$Message))
photos["is.image"] <- tfvect
head(photos)

photos <- photos[photos$is.image == T, ]
photo.count <- count(photos$Sender)
photo.count <- photo.count[order(photo.count$freq,decreasing=T),]
photo.count
