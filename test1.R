itc255=read.csv("itc255.csv")
View(itc255)
names(itc255)

View(dfitc255)
#Variable 1 Skill Level
AbsFreq=table(itc255$SkillLevel)
AbsFreq
prop.table(AbsFreq)
RelFreq=round(prop.table(AbsFreq), 2)
RelFreq
CumFreq=cumsum(RelFreq)
CumFreq
FDTSkillLevel=cbind(AbsFreq,RelFreq,CumFreq)

FDTQL=function(x) {
  AbsFreq=table(x)
  RelFreq=round(prop.table(AbsFreq), 2)
  CumFreq=cumsum(RelFreq)
  FDTx= cbind(AbsFreq,RelFreq,CumFreq)
  return(FDTx)
  
}
FDTQL(itc255$SkillLevel)

#Variable 2 Gender
AbsFreq=table(itc255$Gender)
AbsFreq
prop.table(AbsFreq)
RelFreq=round(prop.table(AbsFreq), 2)
RelFreq
CumFreq=cumsum(RelFreq)
CumFreq
FDTSGender=cbind(AbsFreq,RelFreq,CumFreq)

FDTQL=function(x) {
  AbsFreq=table(x)
  RelFreq=round(prop.table(AbsFreq), 2)
  CumFreq=cumsum(RelFreq)
  FDTx= cbind(AbsFreq,RelFreq,CumFreq)
  return(FDTx)
  
}
FDTQL(itc255$Gender)

# FDT of QNT to QUL
summary(itc255$Age)

head(itc255)


#selection + Loop
catitc255=c()  #create an empty vector

for (k in 1:length(itc255$Age)) {
  if(itc255$Age[k]<20){
    catitc255[k]="Youth"
  } else if (itc255$Age[k] >=25  & itc255$Age[k]<30) {
    catitc255[k]="Adult"
  } else {
    catitc255[k]="Senior"
  }
}

head(catitc255)
new=cbind(itc255, catitc255)
View(new)
head(itc255$Age)
#apply the function for FDT of QL
FDTQL(catitc255)
#Univar case 
#Graphs 
#Categorical vars (pie and bar)

#create the FDT 
FDTQL(itc255$Gender)[,2]

fdtGender=FDTQL(itc255$Gender)[,2]
fdtGender

pie(fdtGender, 
    col = rainbow(2), 
    main = 'Gender Distribution')

barplot(fdtGender, 
        col=rainbow(2), 
        main = 'Gender distribution')

fdtAge=FDTQL(catitc255)[,2]
fdtAge

barplot(fdtAge, 
        col=rainbow(3), 
        main = 'Age distribution')

#Descriptive methods
#Univar case 
#Graphs 
#Num vars (hist and density)
head(itc255)

hist(itc255$Age, 
     col='blue', 
     main = 'Age distibution')

plot(density(itc255$Age), 
     col='#0033FF', 
     main='Age distribution')


plot(density(itc255$Age), 
     col='#0033FF', 
     main='Age distribution')

y=read.csv("timeToOffice.csv")
names(y)


hist(y$T)
plot(density(y$T))

#GGPLOT2
library(tidyverse)
itc255<- read.csv("itc255.csv")  
head(itc255)
colnames (itc255)
#1Scatterplot

ggplot(data = itc255, mapping = aes(x = High.cm, y = Age)) +
  geom_point()
ggplot(data = itc255, mapping = aes(x = Gender, y = Age, color = Sport)) +
  geom_point()

#2Smooth line
ggplot(data = itc255, mapping = aes(x = Gender, y = Age)) +
 geom_point(color = "blue") +      
  geom_smooth(color = "red") 
theme_classic() +                      
  ggtitle("Age vs Gender") +             
  xlab("Gender") +                        
  ylab("Age") 
ggplot(data = itc255, mapping = aes(x = Gender, y = SkillLevel)) +
  geom_point() +
  geom_smooth()

#3Bar chart
ggplot(data = itc255, aes(x = SkillLevel)) +
  geom_bar()
ggplot(data = itc255, aes(x = SkillLevel, fill = Sport)) +
  geom_bar(position = "dodge")

#4Histogram
ggplot(data = itc255, aes(x = High.cm)) +
  geom_histogram(bins = 10)

#5Density plot

ggplot(data = itc255, aes(x = Level.of.Satisfaction)) +
  geom_density()

#6Boxplot
ggplot(data = itc255, aes(x = Sport, y = Level.of.Satisfaction)) +
  geom_boxplot()

#7Facets
ggplot(data = itc255, aes(x = Gender, y = Age)) +
  geom_point() +
  facet_wrap(~ Sport)
ggplot(data = itc255, aes(x = Gender, y = Age)) +
  geom_point(color = "Dark Blue") +
  geom_smooth(color = "Green") +
  facet_grid(Gender ~ Age)

p <- ggplot(itc255, aes(x = Gender, y = Age)) +
  geom_point(color = "blue") +      
  geom_smooth(color = "black") +      
  facet_grid(Sport ~ SkillLevel) + 
  ggtitle("Age vs Gender by Sport and SkillLevel") +  # Step 3: Title
  xlab("Sport") + 
  ylab("Age")

# View the plot
p

# Save the plot
ggsave("myplot.png", plot = p)




