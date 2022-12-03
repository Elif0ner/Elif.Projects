setwd("/Users/elifoner/Desktop")

library("readxl")
#install.packages("epiDisplay")
library(productplots)
library(dplyr)
library(CGPfunctions)
library(ggplot2)
library(forcats)
#install.packages("poliscidata")
library(poliscidata)
#install.packages("survey")
library(survey)
library(stats)
library("MASS")
library(dplyr)
#install.packages("gtsummary")
library(gtsummary)
library("devtools")
#install.packages("likert")
library("likert")


survey <- read_excel("SurveyResponses.xlsx")


colnames(survey) <- c("time","age","gender","nationality",
                      "degree","cgpa",
                      "income","income_sat","location",
                      "location_sat","faculty","department","department_sat","aim","reason_dep",
                      "changed_dep","change_plan","is_difficult",
                      "social","online","instructor","job_related",
                      "worry","facilities","club")

survey[] <-lapply(survey, factor)
str(survey)


# Population is 19097 Students. 


description <- data.frame(variable=c("age","gender","nationality",
                                     "degree","cgpa",
                                     "income","income_sat","location",
                                     "location_sat","faculty","department","department_sat","aim","reason_dep",
                                     "changed_dep","change_plan","is_difficult",
                                     "social","online","instructor","job_related",
                                     "worry","facilities","club"), 
                          description=c("Student's Age","Student's Gender",
                                        "Student's Nationality","Student's Degree","Cumulative Grade Point Average", "Student's income",
                                        "Student's income satisfaction","Student's residence","Student's satisfaction with Ankara", "Student's faculty",
                                        "Student's department","Student's department satisfaction",
                                        "Student's aimed department","Reason for choosing department",
                                        "Whether changed department or not","Planning to change department",
                                        "Department is difficult","Free time for social activities",
                                        "Handled online education period","Satisfied with instructor at online period",
                                        "Planning a job related to the department","Worry about finding job",
                                        "University Facilities","Active member of University Club"))
knitr::kable(description)


data <- data.frame(faculty=c("Faculty of Economic and Administrative Sciences",
                             "Faculty of Engineering","Faculty of Architecture",
                             "Faculty of Arts and Sciences","Faculty of Education"),fpc=c(2162,9294,1412,4423,1806))


summary(survey)
survey %>% tbl_summary()



survey$cgpa <- factor(survey$cgpa, levels=c("None", "Below 2.0", "2.0 - 2.49", "2.5 - 2.99","3.0 - 3.49","3.5 - 4.0" ))
survey$department_sat <- factor(survey$department_sat, levels=c("Strongly Disagree", "Disagree", "I neither agree nor disagree", "Agree","Strongly agree" ))

ggplot(survey, aes(fct_rev(fct_infreq(department_sat)))) + geom_bar(fill="#FF9999") + 
  coord_flip() + labs(x="Department Satisfaction", y="Frequency", title="Frequencies Based on Department Satisfaction") + theme_minimal()

ggplot(survey, aes(fct_rev(fct_infreq(faculty)))) + geom_bar(fill="orange") + 
  coord_flip() + labs(x="Faculties", y="Frequency", title="Frequencies Based on Faculties") + theme_minimal()


#PIE CHARTS FOR GENDER, CGPA, CURRENT DEGREE
df <- survey %>% 
  group_by(gender) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))


ggplot(df, aes(x = "", y = perc, fill = gender)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + theme_void()+ labs(title = "Gender of the students who participated in the survey",
                                                subtitle = "Plot of answers by percentage"
  )

df2 <- survey %>% 
  group_by(cgpa) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels.2 = scales::percent(perc))


ggplot(df2, aes(x = "", y = perc, fill = cgpa)) +
  geom_col() +
  geom_text(aes(label = labels.2),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + theme_void()+ labs(title = "CGPA of the students who participated in the survey",
                                              subtitle = "Plot of answers by percentage"
  )



df3 <- survey %>% 
  group_by(facilities) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels.2 = scales::percent(perc))


ggplot(df3, aes(x = "", y = perc, fill = facilities)) +
  geom_col() +
  geom_text(aes(label = labels.2),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + theme_void() + labs(title = "Students' Perspective on School Facilities",
                                                 subtitle = "Plot of answers by percentage"
                                                 )













#Chi Square Degree+gender VS DEP SAT.



new<- left_join(survey,data,on=faculty)

#Normally we should use firstly ordinal(and multinomail) regression we know that, but since we don't know this topic we use directly linear regression.
sat_convert <- data.frame(department_sat = c("Strongly Disagree","Disagree","I neither agree nor disagree", "Agree","Strongly agree"), 
                          dep_scores = c(1,2,3,4,5))
new1<- left_join(new,sat_convert)

cgpa_convert <- data.frame(cgpa = c("None", "Below 2.0", "2.0 - 2.49", "2.5 - 2.99","3.0 - 3.49","3.5 - 4.0"), 
                           cgpa_scores = c(1,2,3,4,5,6))
new77<- left_join(new1,cgpa_convert)

degree_convert <- data.frame(degree = c("1", "2", "3", "4"), 
                             degree_scores = c(1,2,3,4))
new78<- left_join(new77,degree_convert)

adesign <- svydesign(id=~1,strata=~faculty,data=new78,fpc=~fpc)
adesign
svymean(~degree_scores+ dep_scores,adesign)
svytable(~degree+department_sat, adesign)   #neden virg?ll? ??k?yor? -> descriptive statistics
svytable(~gender+department_sat+degree, adesign)
#as.data.frame(svyby(~degree, by=~faculty,adesign,svymean))


g <- ggplot(survey, aes(x = degree, fill = department_sat)) + xlab("Degree") + ylab("Department Satisfaction") + ggtitle("The Relationship of Degree and Department Satisfaction ")

g + geom_bar(position="dodge")



f <- ggplot(survey, aes(x = gender, fill = department_sat)) + xlab("Gender") + ylab("Department Satisfaction") + ggtitle("The Relationship of Gender and Department Satisfaction ")

f + geom_bar(position="dodge")


two_way <- aov(dep_scores ~ degree + gender + degree:gender, data = new78) #for infinite population
summary(two_way)

mod <- svyranktest(dep_scores ~ gender+ degree+ gender:degree, adesign,test= "KruskalWallis")
mod

mode99 <- svyglm(dep_scores ~ gender+ degree+ gender:degree, adesign) 
aov(mode99)


#cross table gender vs degree





# DEP SCORES~CHANGE PLAN
adesign <- svydesign(id=~1,strata=~faculty,data=new1,fpc=~fpc)
model1 <- svyglm(dep_scores ~ change_plan, adesign)
model1
summary(model1)
svytable(~change_plan+department_sat, adesign)

out <- svymean(~department_sat,adesign) #categoric

byby <- svyby(~dep_scores, by=~cgpa,adesign,svymean,T)
dotchart(byby,lcolor = "green",color="blue", xlab = "Department Scores", ylab = "CGPA",
         main = "The relationship Department Scores and CGPA")    #none is problematic
 #lm yapal?m






#Chi Square CGPA VS DEP SAT.
svychisq(~cgpa+department_sat,adesign)

PlotXTabs2(
  data = survey, y =department_sat,x =cgpa,label.fill.alpha = 3,
  title = "Department Satisfaction",xlab = "CGPA",palette = "Pastel1",
  legend.title = "Department Satisfaction Levels"
)
#model y?ntemiyle sonu?
out3 <- svyglm(dep_scores~cgpa,adesign)
summary(out3)








#online ile department sat
new2<- left_join(survey,data,on=faculty)
on_convert <- data.frame(online = c("Strongly disagree","Disagree","I neither agree nor disagree", "Agree","Strongly agree"), 
                          on_scores = c(1,2,3,4,5))
new79<- left_join(new78,on_convert)



cdesign <- svydesign(id=~1,strata=~faculty,data=new79,fpc=~fpc)
svymean(~on_scores,cdesign)
#2.5 , 1e daha yak?n oldu?u i?in mutsuzlu?a daha yak?n.

svychisq(~department_sat+online,adesign)  #we reject the null hypotesis, they are dependent




library(dplyr)
#group_by(new79, online) %>% summarize(mean(dep_scores))
dataa <-svyby(~dep_scores,by=~online, adesign,svymean)  #komple tablo


dataa$online <- factor(dataa$online, levels=c("Strongly disagree", "Disagree", "I neither agree nor disagree", "Agree","Strongly agree"))


ggplot(dataa, aes(x=dataa$online, y=dataa$dep_scores)) +
  geom_segment( aes(x=dataa$online, xend=dataa$online, y=1, yend=dataa$dep_scores) , size=1, color="blue", linetype="dotdash" ) +
  xlab("Online education") + ylab("Department Satisfaction") + ggtitle("The Relationship of Online Education and Department Satisfaction ")
  geom_point()











