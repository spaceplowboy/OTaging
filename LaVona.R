
# code for LaVona's OT aging pretest of students
library(plyr) # for revalue
library(ggplot2)

library(extrafont)
#font_import()
loadfonts(device = "win")

options(scipen=999)

currentYear<-2020

setwd("C:/data/research/Projects/Health/LaVona_Aging")
dat2018<-read.csv("./OTdata/Class2018.csv")
dat2020<-read.csv("./OTdata/Class2020.csv")
dat2021<-read.csv("./OTdata/Class2021.csv")
dat2022<-read.csv("./OTdata/Class2022.csv")
quizAnswers<-read.csv("./OTdata/quizAnswers.csv")
quizAnswers2<-read.csv("./OTdata/quizAnswers2.csv")
#agingFarmers<-read.csv("./OTdata/agingPopFarmers.csv")
#farmers<-read.csv("./OTdata/agefarmers.csv")
dat2018$class<-2018
dat2020$class<-2020
dat2021$class<-2021
dat2022$class<-2022
dat2018<-subset(dat2018, SC0>1)
dat2020<-subset(dat2020, SC0>1)
dat2021<-subset(dat2021, SC0>1)
dat2022<-subset(dat2022, SC0>1)
dat2018$duration[dat2018$duration >=3600] <- 3600
dat2020$duration[dat2020$duration >=3600] <- 3600
dat2021$duration[dat2021$duration >=3600] <- 3600
dat2022$duration[dat2022$duration >=3600] <- 3600
datAll<-rbind(dat2018,dat2020,dat2021,dat2022)
dim(datAll)
datAll$SC0<-as.numeric(as.character(datAll$SC0))
datAll$Q4<-as.numeric(as.character(datAll$Q4))
datAll$class<-factor(datAll$class)
datAll$percent<-datAll$SC0*2

head(datAll)

# looking at outliers for duration
test<-subset(datAll, duration>1800)
test$minutes<-test$duration/60
test$hours<-test$minutes/60
write.csv(test, "durationProblems.csv")

#

####

# the answers in csv for 2020, 2021, 2022 are not consistent with 2018
#answers<-matrix(data=quizAnswers$correct, nrow=dim(datAll[1]), ncol = 50)
#correct<-as.data.frame(answers)
#dim(answers)
#dim(datAll[18:67])
#results<-datAll[18:67] == correct
#results[1:3,]
#results2<-datAll[18:67] == quizAnswers2
#colnames(results) <- paste("A", colnames(results), sep = "")
#datAll<-cbind(datAll,results)
#datAll$test<-length(datAll[76:125][datAll[76:125] == TRUE])
#datAll$no_calls <- rowSums(datAll[76:125] == "TRUE")
#datAll$SC0
#library(matrixStats)
#rowCounts(datAll[76:125], value = TRUE)
#rowAnys(datAll[76:125], value = "TRUE")

#datAll<-subset(datAll, Q4>1900)
datAll<-subset(datAll, SC0>1)
datAll$gen<-ifelse(datAll$Q4 <= 1945, "silent", 
                   ifelse(datAll$Q4 <=1964, "boomer", 
                          ifelse(datAll$Q4 <=1980, "genX",
                                 ifelse(datAll$Q4<=1996, "millennial", 
                                        ifelse(datAll$Q4<=currentYear, "genZ", "error")))))

datAll$gen2<-revalue(datAll$gen, c("silent"="Silent", 
                                   "boomer"="Baby Boomer",
                                   "genX" = "Generation X",
                                   "millennial" = "Millennial",
                                   "genZ" = "Generation Z"))
datAll$gen2<-factor(datAll$gen2, levels = c("Generation X", "Millennial", "Generation Z"))

factor(datAll$Q3)

datAll$pop<-revalue(datAll$Q3, c("Adult Population"="Adult", 
                                   "All age groups"="All ages",
                                   "Geriatric Population" = "Geriatric",
                                 "Pediatric Population" = "Pediatric",
                                 "Undecided" = "Undecided",
                                 "Specific Population" = "Specific"))
#datAll$pop<-factor(datAll$gen2, levels = c("Generation X", "Millennial", "Generation Z"))



library(janitor)

tabyl(datAll$class) # graduating class
tabyl(datAll$gen2) # generation
tabyl(datAll$Q3) # population of interest
tabyl(datAll$Q61) # traditional

# Table X
genXclass<-datAll %>% tabyl(gen2, class)
genXclass
write.csv(genXclass, "genXclass.csv") 

# Table X
Q3Xclass<-datAll %>% tabyl(Q3, class)
Q3Xclass
write.csv(Q3Xclass, "popXclass.csv")

# Table X
Q61Xclass<-datAll %>% tabyl(Q61, class)
Q61Xclass
write.csv(Q61Xclass, "Q61Xclass.csv")

# Table x on gender
Q1xClass<-datAll %>% tabyl(Q1, class)
Q1xClass
write.csv(Q1xClass, "Q1Xclass.csv")


# not using these right now
library(psych) # describeeBy
describeBy(datAll$percent, datAll$gen)
describeBy(datAll$percent, datAll$Q3)


# describptive statistics
ggplot(datAll, aes(pop)) + geom_bar(aes(fill=class)) + xlab(NULL) + theme_bw() + 
  scale_fill_grey(start = 0.1, end = .8) +
  theme(legend.position="bottom") + theme(legend.title=element_blank()) +
  theme(text=element_text(size=16,  family="serif"))
ggsave("Fig2classXpopBarChart", device = "tiff", dpi = "print")


ggplot(datAll, aes(pop)) + geom_bar(aes(fill=gen2)) + xlab(NULL) + theme_bw() + 
  scale_fill_grey(start = 0.1, end = .8) +
  theme(legend.position="bottom") + theme(legend.title=element_blank()) +
  theme(text=element_text(size=16,  family="serif"))
ggsave("Fig6genXpopBarChart", device = "tiff", dpi = "print")



# test scores
ggplot(datAll, aes(x=percent, fill=factor(class))) + geom_density(alpha=0.2) +
  xlab("Quiz score (%)") + theme_bw() + 
  theme(legend.position="bottom") + 
  scale_fill_grey(start = 0.1, end = .8) +
  theme(legend.title=element_blank()) +
  theme(text=element_text(size=16,  family="serif"))  #xlim(c(20,50)) +  
ggsave("Fig5quizScoresAll4classes", device = "tiff", dpi = "print")

ggplot(datAll, aes(x=duration, fill=factor(class))) + geom_density(alpha=0.2) +
  xlab("duration") + xlim(c(0,1500)) + theme_bw() + 
  scale_fill_grey(start = 0.1, end = .8) +
    theme(legend.position="bottom") + theme(legend.title=element_blank()) +
  theme(text=element_text(size=16,  family="serif")) #xlim(c(20,50)) +  
ggsave("durationAll4classes", device = "tiff", dpi = "print")

ggplot(dat2022, aes(x=SC0)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
               binwidth=1,
               colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  xlab("Quiz score (%)") + theme_bw() +
  theme(text=element_text(size=16,  family="serif")) #xlim(c(20,50)) +  
ggsave("Class2022scores", device = "tiff", dpi = "print")

ggplot(dat2021, aes(x=SC0)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  xlab("Quiz score (%)") + theme_bw() +
  theme(text=element_text(size=16,  family="serif")) #xlim(c(20,50)) +  
ggsave("Class2021scores", device = "tiff", dpi = "print")

ggplot(dat2020, aes(x=SC0)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  xlab("Quiz score (%)") + theme_bw() +
  theme(text=element_text(size=16,  family="serif"))#xlim(c(20,50)) +  
ggsave("Class2020scores", device = "tiff", dpi = "print")

ggplot(dat2018, aes(x=SC0)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  xlab("Quiz score (%)") + theme_bw() +
  theme(text=element_text(size=16,  family="serif"))#xlim(c(20,50)) +  
ggsave("Class2018scores", device = "tiff", dpi = "print")


#quiz duration, does length of time correlate to score or age?
corr.test(datAll$percent,datAll$duration) # no correlation but 5 studens were ouliers for duration

#regression analysis and multiple means comparison
model<-percent~duration+Q4+factor(class)
summary(lm(data=datAll, model))

ggplot(datAll, aes(x=duration, y=percent, color=factor(class))) + geom_point() +
  xlim(c(0,1000)) + xlab("time to complete quiz (seconds)") + ylab("Quiz score (%)") + theme_bw() + theme(legend.position="bottom") +
  theme(text=element_text(size=16,  family="serif"))
ggsave("scoreXdurationXclass", device = "tiff", dpi = "print")

mid<-mean(datAll$Q4)
ggplot(datAll, aes(x=duration, y=percent, color=(Q4))) + geom_point() +
  scale_color_gradientn(colours = rainbow(5)) +
  xlim(c(0,1000)) + xlab("time to complete quiz (seconds)") + ylab("Quiz score (%)") + theme_bw() +
  theme(text=element_text(size=16,  family="serif"))
ggsave("scoreXdurationXage", device = "tiff", dpi = "print")

ggplot(datAll, aes(x=Q4)) + geom_histogram() + 
  geom_histogram(color="black", fill="grey") + xlim(c(1970, 2000)) + xlab(NULL) + 
  theme_bw() +
  theme(text=element_text(size=16,  family="serif"))
ggsave("Fig1histBirthYear", device = "tiff", dpi = "print")

ggplot(datAll, aes(x=duration, y=percent, color=gen2)) + geom_point() + 
  xlim(c(0,1000)) + xlab("time to complete quiz (seconds)") + ylab("Quiz score (%)") + theme_bw() + theme(legend.title=element_blank()) + theme(legend.position="bottom") +
  theme(text=element_text(size=16,  family="serif"))
ggsave("scoreXdurationXgeneration", device = "tiff", dpi = "print")

ggplot(datAll, aes(x=duration, y=percent, color=Q61)) + geom_point() +
  xlim(c(0,1000)) + xlab("time to complete quiz (seconds)") + ylab("Quiz score (%)") + theme_bw()  + theme(legend.title=element_blank()) + theme(legend.position="bottom") +
  theme(text=element_text(size=16,  family="serif"))
ggsave("scoreXdurationXtraditional", device = "tiff", dpi = "print")

ggplot(datAll, aes(x=duration, y=percent, color=Q3)) + geom_point() +
  xlim(c(0,1000)) + xlab("time to complete quiz (seconds)") + ylab("Quiz score (%)") + theme_bw()  + theme(legend.title=element_blank()) + theme(legend.position="bottom") +
  theme(text=element_text(size=16,  family="serif"))
ggsave("scoreXdurationXpopulation", device = "tiff", dpi = "print")

ggplot(datAll, aes(x=pop, y=percent)) + geom_boxplot() + xlab("class") + xlab(NULL) + ylab("Quiz score (%)") + theme_bw() +
  theme(text=element_text(size=16,  family="serif"))
ggsave("Fig7boxplotPopXscore", device = "tiff", dpi = "print")

ggplot(datAll, aes(x=Q1, y=percent)) + geom_boxplot() + xlab("class") + xlab(NULL) + ylab("Quiz score (%)") + theme_bw() +
  theme(text=element_text(size=16,  family="serif"))
ggsave("boxplotGenderXscore", device = "tiff", dpi = "print")

ggplot(datAll, aes(x=factor(class), y=percent)) + geom_boxplot() + xlab(NULL) + ylab("Quiz score (%)") + theme_bw() +
  theme(text=element_text(size=16,  family="serif"))
ggsave("Fig3boxplotClassXscore", device = "tiff", dpi = "print")
#ggsave("boxplotClassXscore", device = "png", dpi = "screen")

ggplot(datAll, aes(x=factor(Q61), y=percent)) + geom_boxplot() + xlab(NULL) + ylab("Quiz score (%)") + theme_bw() +
  theme(text=element_text(size=16,  family="serif"))
ggsave("boxplotTradXscore", device = "tiff", dpi = "print")

ggplot(datAll, aes(x=gen2, y=percent)) + geom_boxplot() + xlab(NULL) + ylab("Quiz score (%)") + theme_bw() +
  theme(text=element_text(size=16,  family="serif"))
ggsave("Fig4boxplotGenXscore", device = "tiff", dpi = "print")

model2<-duration~class
summary(lm(model2,datAll))

# duration means by class
aggregate(data=datAll, duration/60~class, FUN=mean)

#score means by class 
aggregate(data=datAll, percent~class, FUN=mean)


# Tuky HSD multiple means comparisons
model3<-percent~factor(class)
TukeyHSD(aov(model3, datAll))

model4<-percent~Q3
pop.score.aov<-aov(model4, datAll)
TukeyHSD(pop.score.aov)


genderModel<-percent~Q1
TukeyHSD(aov(genderModel, datAll))

genderModel2<-duration~Q1
TukeyHSD(aov(genderModel2, datAll))

model5<-duration~Q3
pop.dur.aov<-aov(model5, datAll)
TukeyHSD(pop.dur.aov)

model6<-duration~gen
gen.dur.aov<-aov(model6, datAll)
TukeyHSD(gen.dur.aov)

model7<-percent~gen
gen.score.aov<-aov(model7, datAll)
TukeyHSD(gen.score.aov)

tradModel<-percent~Q61
trad.score.aov<-aov(tradModel, datAll)
TukeyHSD(trad.score.aov)

tradModeld<-duration~Q61
trad.duration.aov<-aov(tradModeld, datAll)
TukeyHSD(trad.duration.aov)

#################

##################
#########################
##################

head(datAll)
names(datAll)

byQ<-round(colSums(datAll[125:174])/dim(datAll)[1]*100,1)
write.csv(byQ, "byQ.csv.csv")

mean(byQ)
median(byQ)
min(byQ)
max(byQ)

order(byQ)
sort(byQ)
rank(byQ)

#aggregate(data=datAll, byQ~class, FUN=mean)


#colMeans(datAll[125:174])

# update this

##### Grouping
# group questions into groups of common topic
# stat knowledge, normal aging, psycho/social, health


names(datAll)
datAll$statKnow <-rowMeans(datAll[,c(125, 130, 131, 132, 133, 141, 149, 152, 153, 154, 161)]) # 11
datAll$normAging<-rowMeans(datAll[,c(129, 134, 136, 137, 138, 139, 140, 142, 143, 144, 145, 171)]) #12
datAll$psycho   <-rowMeans(datAll[,c(126, 127, 128, 147, 148, 157, 158, 159, 160, 162, 163, 164, 165, 166, 172)]) # 15
datAll$health   <-rowMeans(datAll[,c(135, 146, 150, 151, 155, 156, 167, 168, 169, 170, 173, 174)]) # 12

datAll$statKnowSums <-rowSums(datAll[,c(125, 130, 131, 132, 133, 141, 149, 152, 153, 154, 161)]) # 9
datAll$normAgingSums<-rowSums(datAll[,c(129, 134, 136, 137, 138, 139, 140, 142, 143, 144, 145, 171)]) #12
datAll$psychoSums   <-rowSums(datAll[,c(126, 127, 128, 147, 148, 157, 158, 159, 160, 162, 163, 164, 165, 166, 172)]) # 15
datAll$healthSums   <-rowSums(datAll[,c(135, 146, 150, 151, 155, 156, 167, 168, 169, 170, 173, 174)]) # 14

mean(datAll$statKnow)
mean(datAll$normAging)
mean(datAll$psycho)
mean(datAll$health)

min(datAll$statKnow)
min(datAll$normAging)
min(datAll$psycho)
min(datAll$health)

max(datAll$statKnow)
max(datAll$normAging)
max(datAll$psycho)
max(datAll$health)

sd(datAll$statKnow)
sd(datAll$normAging)
sd(datAll$psycho)
sd(datAll$health)





statKnowmodel<-statKnow~Q3
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-normAging~Q3
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-psycho~Q3
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-health~Q3
TukeyHSD(aov(statKnowmodel, datAll))

#

statKnowmodel<-statKnow~gen2
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-normAging~gen2
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-psycho~gen2
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-health~gen2
TukeyHSD(aov(statKnowmodel, datAll))

#

statKnowmodel<-statKnow~class
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-normAging~class
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-psycho~class
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-health~class
TukeyHSD(aov(statKnowmodel, datAll))

#

statKnowmodel<-statKnow~Q1
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-normAging~Q1
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-psycho~Q1
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-health~Q1
TukeyHSD(aov(statKnowmodel, datAll))

#

statKnowmodel<-statKnow~Q61
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-normAging~Q61
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-psycho~Q61
TukeyHSD(aov(statKnowmodel, datAll))

statKnowmodel<-health~Q61
TukeyHSD(aov(statKnowmodel, datAll))



datAll %>% tabyl(Q1, class) # gender
datAll %>% tabyl(Q2, class) # age
datAll %>% tabyl(Q3, class) # population
datAll %>% tabyl(Q4, class) # birthyear

TukeyHSD(aov(statKnow~Q3, datAll))
TukeyHSD(aov(normAging~Q3, datAll))
TukeyHSD(aov(psycho~Q3, datAll))
TukeyHSD(aov(health~Q3, datAll))

aggregate(datAll$statKnow, list(datAll$Q3), mean)
aggregate(datAll$normAging, list(datAll$Q3), mean)
aggregate(datAll$psycho, list(datAll$Q3), mean)
aggregate(datAll$health, list(datAll$Q3), mean)


t.test(datAll$statKnow,datAll$normAging)
t.test(datAll$statKnow,datAll$psycho)
t.test(datAll$statKnow,datAll$health)
t.test(datAll$normAging,datAll$psycho)
t.test(datAll$normAging,datAll$health)
t.test(datAll$psycho,datAll$health)

mean(datAll$statKnow)
mean(datAll$normAging)
mean(datAll$psycho)
mean(datAll$health)

mean(datAll$testScore)/50
median(datAll$testScore)/50


