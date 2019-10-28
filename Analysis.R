install.packages('plyr')
install.packages("dplyr")
library("plyr")
library("dplyr")

#Modified code snippets from https://www.statmethods.net/index.html

setwd("~/Documents/LNU/4ME301-ScientificTheoriesAndMethods /Assignments/03 Data Collection/Submission")
a <- read.csv('DatasetA-Extended.csv', sep=',')
b <- read.csv('DatasetB-Extended.csv', sep=',')

swedish <-a %>% filter(Language == "Swedish")
portuguese <- a %>% filter(Language == "Portuguese")

bswedish <- b %>% filter(Language == "Swedish")
bportuguese <- b %>% filter(Language == "Portuguese")

location <- a
location$Location <- as.character(location$Location)
location$Location[location$Location == "Café Stockholm"] <- "Out"
location$Location[location$Location == "Hostel Stockholm"] <- "Out"
location$Location[location$Location == "Joses"] <- "Out"
location$Location[location$Location == "Rodrigo"] <- "Out"
location$Location[location$Location == "Lisa"] <- "Out"
location$Location[location$Location == "Library"] <- "Out"
location$Location[location$Location == "Bed"] <- "In"
location$Location[location$Location == "Desk"] <- "In"

btime <- b
btime$Duration.in.Minutes[btime$Duration.in.Minutes > 2.563913] <- "Above"
btime$Duration.in.Minutes[btime$Duration.in.Minutes <= 2.563913] <- "Below"
btime$Location <- as.character(btime$Location)
btime$Location[btime$Location == "Café Stockholm"] <- "Out"
btime$Location[btime$Location == "Hostel Stockholm"] <- "Out"
btime$Location[btime$Location == "Joses"] <- "Out"
btime$Location[btime$Location == "Rodrigo"] <- "Out"
btime$Location[btime$Location == "Lisa"] <- "Out"
btime$Location[btime$Location == "Library"] <- "Out"
btime$Location[btime$Location == "Bed"] <- "In"
btime$Location[btime$Location == "Desk"] <- "In"

# 1 How many lessons did I study?
barplot(table(factor(a$Day.of.Month, levels=c(1:25,30)))[c(26,1:25)], col='white', xlab='days of the months', ylab='lessons')


# 2 How many lectures do I study on study days?
par(mfrow=c(1,2))
barplot(table(factor(c(as.data.frame(table(factor(a$Day.of.Month, levels=c(1:25,30))))[['Freq']]), levels = 0:7)), col='white', ylab="frequency", xlab="lessons per day")
boxplot(as.vector(table(a$Date)), boxwex=0.3, ylim=c(2,8), ylab='lessons per day on study days')

# 3 When do I study?
# Which days of the week?
par(mfrow=c(1,1))
barplot(table(a$Day.of.Week)[c(2,6,7,5,1,3,4)], ylim=c(0,20), cex.names=0.9, ylab="lessons",col='white')
# Which time of the day?
barplot(table(a$Time.of.Day)[c(2,1,3)], ylim=c(0,60), ylab="lessons", col='white')

# 4 Where do I study?
# Overall results
barplot(table(a$Location)[order(table(a$Location))], ylim=c(0,30), ylab="lessons",col='white', names.arg=c('Lisa', 'Cafe', 'Rodrigo', 'Jose', 'Hostel', 'Library', 'Desk','Bed'))
# Grouped by Inside or Outside Home
barplot(table(location$Location), ylim=c(0,60), ylab="lessons", col='white')

# 5 Which type of lesson do I study?
barplot(table(a$Type), ylim=c(0,100), ylab="lessons", col='white')

# 6 Which device do I study on?
barplot(table(a$Device)[c(2,1)], ylim=c(0,70), ylab="lessons", col='white')

# 7 How many XP do I make a day?
par(cex=0.7)
helper <- aggregate(a$XP, by=list("day of the month"=a$Day.of.Month), FUN=sum)
helper <- rbind(helper, c(6,0))
helper <- rbind(helper, c(7,0))
helper <- rbind(helper, c(11,0))
helper <- rbind(helper, c(13,0))
helper2 <- helper[-c(22),] 
helper2 <- helper2[order(helper2["day of the month"]),]
plot(helper2, type='o', xaxt = "n", pch=20, ylab="XP")
axis(1, at=c(1:26), labels=c(1:26))
# How many XP per lesson?
par(cex=1)
boxplot(a$XP, ylab='XP per lesson', ylim=c(8,26),boxwex=0.3)

# 8 How many minutes do I spent learning a language per Day?
plot(aggregate(b$Duration.in.Minutes, by=list("day of the month"=b$Day.of.Month), FUN=sum), type='o', ylim=c(5,20), ylab="duration in minutes", pch=20)
axis(1, at=c(1:26), labels=c(1:26))

#Durations per lesson
boxplot(b$Duration.in.Minutes, ylim=c(0,7), boxwex=0.3, ylab='duration in minutes per lesson')

# 9 How much time do I spend per XP?
model <- lm(b$XP~b$Duration.in.Minutes,b)
int <-  model$coefficient["(Intercept)"]
slope <- model$coefficient["b$Duration.in.Minutes"]
plot(b$XP~b$Duration.in.Minutes, pch=20, ylab='XP', xlab='duration in minutes', ylim=c(0,20))
abline(int, slope, lty=1, lwd=2, col="blue") 
cor(b$XP,b$Duration.in.Minutes)

# 10  
table(a$Weekday, a$Time.of.Day)
chisq.test(table(a$Weekday, a$Time.of.Day))
chisq.test(table(a$Day.of.Week, a$Time.of.Day))

#11
table(location$Weekday, location$Location)
chisq.test(table(location$Weekday, location$Location))

#12
table(location$Time.of.Day, location$Location)
chisq.test(table(location$Time.of.Day, location$Location))

#13
table(location$Location, location$Device)
chisq.test(table(location$Location, location$Device))

#14
table(btime$Location, btime$Duration.in.Minutes)
chisq.test(table(btime$Location, btime$Duration.in.Minutes))

#15 How many errors do I make per skill?
skill1 <- subset(b, b$Skill.ID=="34984b608b9c7ea9ecffa7e25c9a593b")
skill2 <- subset(b, b$Skill.ID=="db7c97439e88f3af67b297acf5b564a7")
skill3 <- subset(b, b$Skill.ID=="418290b3544f47f513093fb974dd7167")
skill4 <- subset(b, b$Skill.ID=="84eccf163a429f6b4e1d60f7ac30dd6d")
practice <- subset(b, b$Skill.ID=="")

par(mfrow=c(2,2))
plot(aggregate(skill1$Mistakes, by=list("day of the month"=skill1$Day.of.Month), FUN=mean), type='o', pch=20, ylab='avg number of mistakes', main='Skill 34984b608b9c7ea9ecffa7e25c9a593b')
plot(aggregate(skill2$Mistakes, by=list("day of the month"=skill2$Day.of.Month), FUN=mean), type='o', pch=20, ylab='avg number of mistakes', main='Skill db7c97439e88f3af67b297acf5b564a7')
plot(aggregate(skill3$Mistakes, by=list("day of the month"=skill3$Day.of.Month), FUN=mean), type='o', pch=20, ylab='avg number of mistakes', main='Skill 418290b3544f47f513093fb974dd7167')
plot(aggregate(skill4$Mistakes, by=list("day of the month"=skill4$Day.of.Month), FUN=mean), type='o', pch=20, ylab='avg number of mistakesmistakes', main='Skill 84eccf163a429f6b4e1d60f7ac30dd6d')

plot(aggregate(practice$Mistakes, by=list("day of the month"=practice$Day.of.Month), FUN=mean), type='o', xaxt = "n", pch=20, yaxt="n")

#16 How many errors do I make per lesson?
par(mfrow=c(1,1))
boxplot(b$Mistakes, ylim=c(0,7), boxwex=0.3, ylab='mistakes per lesson')


#17 How many errors do I make if a lesson is hard?
hard <- subset(b, b$XP>=20)
boxplot(hard$Mistakes)

#18 Do the sum of errors per day and the sum of lessons correlate?
aggregate(b$Mistakes, by=list("Day of the Month"=b$Day.of.Month), FUN=sum)
count(b, Day.of.Month)
plot(c(4,5,4,4,4,5,7,4,5,4), c(15,6,10,4,10,4,6,6,7,7), xlab="Sum of Lessons per Day", ylab="Sum of Errors per Day")
cor(c(4,5,4,4,4,5,7,4,5,4), c(15,6,10,4,10,4,6,6,7,7))

#19 Do I make more mistakes if a lesson is shorter?
# with code from https://rcompanion.org/rcompanion/e_01.html
model <- lm(b$Mistakes~b$Duration.in.Minutes,b)
int <-  model$coefficient["(Intercept)"]
slope <- model$coefficient["b$Duration.in.Minutes"]
plot(b$Mistakes~b$Duration.in.Minutes, pch=20, ylab='mistakes', xlab='duration in minutes')
abline(int, slope, lty=1, lwd=2, col="blue") 
cor(b$Mistakes,b$Duration.in.Minutes)

plot(b$Duration.in.Minutes, b$Mistakes, pch=20)
abline(b$Duration.in.Minutes, b$Mistakes,lty=1, lwd=2, col="blue")
cor(b$Duration.in.Minutes, b$Mistakes)

#20 How many lectures do I study in each Language per day?
boxplot(list(Portuguese = as.vector(table(portuguese$Date)), Swedish = as.vector(table(swedish$Date))), boxwex=0.3, ylab='lessons per day' );

#21 Time per Lesson in each language
boxplot(list(Portuguese = bportuguese$Duration.in.Minutes, Swedish = bswedish$Duration.in.Minutes), boxwex=0.3, ylab='duration in minutes per lesson' );

#22 How much time do I take per XP
par(mfrow=c(2,2))
model <- lm(b$XP~b$Duration.in.Minutes,b)
int <-  model$coefficient["(Intercept)"]
slope <- model$coefficient["b$Duration.in.Minutes"]
plot(b$XP~b$Duration.in.Minutes, pch=20, ylab='XP', xlab='duration in minutes', main='Total lessons')
abline(int, slope, lty=1, lwd=2, col="blue")
model <- lm(bswedish$XP~bswedish$Duration.in.Minutes,b)
int <-  model$coefficient["(Intercept)"]
slope <- model$coefficient["bswedish$Duration.in.Minutes"]
plot(bswedish$XP~bswedish$Duration.in.Minutes, pch=20, ylab='XP', xlab='duration in minutes', main='Swedish lessons')
abline(int, slope, lty=1, lwd=2, col="blue")
model <- lm(bportuguese$XP~bportuguese$Duration.in.Minutes,b)
int <-  model$coefficient["(Intercept)"]
slope <- model$coefficient["bportuguese$Duration.in.Minutes"]
plot(bportuguese$XP~bportuguese$Duration.in.Minutes, pch=20, ylab='XP', xlab='duration in minutes', main='Portuguese lessons')
abline(int, slope, lty=1, lwd=2, col="blue")

plot(b$Duration.in.Minutes, b$XP)
plot(bswedish$Duration.in.Minutes, bswedish$XP)
plot(bportuguese$Duration.in.Minutes, bportuguese$XP)
cor(b$Duration.in.Minutes,b$XP)
cor(bswedish$Duration.in.Minutes, bswedish$XP)
cor(bportuguese$Duration.in.Minutes, bportuguese$XP)

#23 Which lesson type do I choose in what language
par(mfrow=c(1,1))
counts <- table(a$Language, a$Type)
barplot(counts,legend = rownames(counts), beside=TRUE, ylab='lessons', ylim=c(0,50))

#24 Which type in which language
table(a$Language, a$Type)
chisq.test(table(a$Language, a$Type))

#25 Which langugae on what day of the week
table(a$Language, a$Weekday)
chisq.test(table(a$Language, a$Weekday))

#26 Which language at what time´
table(a$Language, a$Time.of.Day)
chisq.test(table(a$Language, a$Time.of.Day))

#27 Which language at which location
table(location$Language, location$Location)
chisq.test(table(location$Language, location$Location))

#28 Which language on which device
table(a$Language, a$Device)
chisq.test(table(a$Language, a$Device))

#29 Errors in Swedish or Portuguese per lesson
boxplot(list(Portuguese = bportuguese$Mistakes, Swedish = bswedish$Mistakes), boxwex=0.3, ylab='mistakes per lesson')
