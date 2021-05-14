setwd("C:/Users/julli/Downloads/data")
#Load Packages

install.packages("tidyverse")
install.packages("GGally")

#Call packages

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(GGally)
library(modelr)
library(broom)


#1.1 IMPORT DATA
tb1_covid_19_tb <- data.frame(read.csv("Covid19.csv"))
tb2_tests_tb <- data.frame(read.csv("Tests.csv"))
tb3_covid_contries_tb <- data.frame(read.csv("Countries.csv"))
tb4_recovered_tb <- data.frame(read.csv("Recovered.csv"))

#2.	Tidy up the dataframe driven from the file "Recovered.csv" 
  #to be compatible with the dataframe driven from the file "Covid19.csv", 
  #i.e. every observation should have a record of recovered patients in one country in a single day.

tb1_covid_19_tb <- as_tibble(tb1_covid_19_tb)
tb2_tests_tb <- as_tibble(tb2_tests_tb)
tb3_covid_contries_tb <- as_tibble(tb3_covid_contries_tb)
tb4_recovered_tb <- as_tibble(tb4_recovered_tb)

### combine the year columns into a single column with separate rows for each year; assign to new vector
tb4_recovered_tb <- gather(tb4_recovered_tb,Date,Count,-Country.Region)
tb4_recovered_tb$Date<-gsub("X","",as.character(tb4_recovered_tb$Date))
tb4_recovered_tb$Date <- gsub("\\.","-",as.character(tb4_recovered_tb$Date)) # replace all . by _

write.csv(tb4_recovered_tb,"recovered_data.csv")

#3.	Change the column names in the dataframes were loaded from the following files accordingly.
#tb1
tb1_covid_19_tb <- tb1_covid_19_tb %>% rename(Code=iso_code,
                                                Country=location,
                                                Continent=continent,
                                                Date=date,
                                                NewCases=new_cases,
                                                NewDeaths=new_deaths)

#tb2
tb2_tests_tb <- tb2_tests_tb %>% rename(Code=Country.Code,
                                              NewTests=New.Tests)
#tb3
tb3_covid_contries_tb <- tb3_covid_contries_tb %>% rename(Code=countryCode,
                                                            Population=popData2018,
                                                            GDPCapita=GDP.capita)
#tb4
tb4_recovered_tb <- tb4_recovered_tb %>%  rename(Country=Country.Region,
                                                   Recovered=Count)

#4.	Ensure that all dates variables are of date data type and with the same format across the dataframes.
glimpse(tb1_covid_19_tb)
glimpse(tb2_tests_tb)
glimpse(tb3_covid_contries_tb)
glimpse(tb4_recovered_tb)

#5.	Considering the master dataframe is the one loaded from file "Covid19.csv", 
  #add new 5 variables to it from other files (Recovered.csv, Tests.csv, Countries.csv). 
  #The 5 new added variables should be named ("Recovered", "NewTests", "Population", "GDP", "GDPCapita") accordingly.

##Replace this code to the code below

# join_tb1_tb2 <- left_join(tb1_covid_19_tb, tb2_tests_tb, by = c("Code","Date"))
# view(join_tb1_tb2)
# join_tb1_tb2_tb3 <- left_join(join_tb1_tb2,tb3_covid_contries_tb, by= c("Code","Country"))
# join_tb1_tb2_tb3
# join_tb1_tb2_tb3_tb4 <- left_join(join_tb1_tb2_tb3,tb4_recovered_tb, by = c("Country","Date"))
# colnames(join_tb1_tb2_tb3_tb4)
# master_covid <- join_tb1_tb2_tb3_tb4[,c(1,2,3,4,5,6,7,11,8,9,10)]
# view(master_covid)

# Join all the tables
master_covid <- tb1_covid_19_tb %>% left_join(tb2_tests_tb, by = c("Code","Date")) %>% 
                                    left_join(tb3_covid_contries_tb, by= c("Code","Country"))  %>% 
                                    left_join(tb4_recovered_tb, by = c("Country","Date")) 
# Re-order the columns 
master_covid <- master_covid[,c("Code",     "Country", "Continent", "Date", "NewCases", "NewDeaths", "Recovered", "NewTests","Population","GDP", "GDPCapita")]

#6.	Check for Nas in all dataframes and change them to Zero.

colSums(is.na(master_covid)) # count number of missing value in each col

master_covid[is.na(master_covid)] <-  0  # handle missing value

colSums(is.na(master_covid)) # compare the result

#7.	Using existing "Date" variable; add month and week variables to the master dataframe.
  #[Hint: you may use functions from lubridate package]

master_covid <- master_covid %>% mutate(Month=month(Date),
                                          Week=week(Date))

#########   Task 2: Exploratory Data Analysis:#########

#1.	Add four new variables to the master dataframe ("CumCases", "CumDeaths", "CumRecovered", "CumTests") 
#These variables should reflect the cumulative relevant data up to the date of the observation, 
#i.e CumCases for country "X" at Date "Y" should reflect the total number of cases in country "X" 
#since the beginning of recording data till the date "Y".
#[Hint: first arrange by date and country, then for each new variable to be added you need to 
#group by country and mutate the new column using the cumsum function]

master_covid<- master_covid %>% 
            group_by(Country) %>%
            arrange(Date) %>%
            mutate(CumCases= cumsum(NewCases),
                   CumDeaths= cumsum(NewDeaths),
                   CumRecovered= cumsum(Recovered),
                   CumTests= cumsum(NewTests))
write.csv(master_covid,"covid_cum.csv")
#2.	Add two new variables to the master dataframe ("Active", "FatalityRate"). 
  #Active variable should reflect the infected cases that has not been closed yet 
  #(by either recovery or death), and it could be calculated from (CumCases - (CumDeaths + CumRecovered)). 
  #On the other hand, FatalityRate variable should reflect the percentages of death to the infected cases up to date 
  #and it could be calculated from (CumDeaths / CumCases).

master_covid <- master_covid %>% 
             mutate(Active=CumCases- (CumDeaths + CumRecovered),
                   FatalityRate=ifelse(is.na(CumDeaths/CumCases), 0, round(100*CumDeaths/CumCases, digits=2)))



#3.	Add four new variables to the master dataframe 
#("Cases_1M_Pop", "Deaths_1M_Pop", "Recovered_1M_Pop", "Tests_1M_Pop") 
#These variables should reflect the cumulative relevant rate per one million of the corresponding country population, 
#(i.e Cases_1M_Pop for country "X" at Date "Y" 
#should reflect the total number of new cases up to date "Y" per million people of country "X" population)
#[Hint: Cases_1M_Pop = CumCases*(10^6) / Population)]
master_covid <- master_covid %>% 
            mutate(Cases_1M_Pop= round(CumCases*10^6 / Population , digits=2)) %>%  #convert to percentage with 2 digits
            mutate(Deaths_1M_Pop=round(CumDeaths*10^6 / Population , digits=2)) %>% 
            mutate(Recovered_1M_Pop=round(CumRecovered*10^6  / Population, digits=2)) %>% 
            mutate(Tests_1M_Pop=round(CumTests*10^6 / Population, digits=2))



#4.	Find the day with the highest reported death toll across the world. Print the date and the death toll of that day.

index <- which.max(master_covid$CumDeaths)

death_highest <- master_covid$CumDeaths[index]
date_death_highest <- master_covid$Date[index]

print(paste0("the date of ",date_death_highest, " having the highest death toll (", format(death_highest, big.mark=",")," cases) acrossed the world")) 

#5.	Build a graph to show how the cumulative data of (Infected Cases, Deaths, Recovered, Tests) 
  #change over the time for the whole world collectively.
  #[Hint: Use geom_line, use log for Y axis for better presentation, 
  #Use different colour to distinguish between new cases, deaths, and recovered]

world_total <- master_covid %>% 
               group_by(Week) %>% 
               summarize(Infected_Cases =sum(NewCases)/10000, Deaths=sum(NewDeaths)/10000, Recovered=sum(Recovered)/10000,Tests=sum(NewTests)/10000)

ggplot(world_total, aes(x = Week)) +
  geom_line(aes(y = Infected_Cases, color = "Infected_Cases")) + ggtitle("Covid-19 Global Situation by Week") +
  geom_line(aes(y = Deaths, color = "Deaths")) +
  geom_line(aes(y = Recovered, color = "Recovered")) +
  geom_line(aes(y = Tests, color = "Tests")) + xlab("Week") + ylab("Total (x 10,000)") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10")


#6.	Extract the last day (05/05/2020) data and save it in a separate dataframe called "lastDay_data".
  #[Hint: use filter function with Date = "2020-05-05"]
lastDay_data <- master_covid %>% filter(Date == "2020-05-05")
#view(lastDay_data)

#7.	Based on the last day data, extract the whole records of the top 10 countries worldwide that have current 
  #active cases, total confirmed cases, and fatality rate in separate dataframes (i.e. top10activeW, top10casesW, top10fatalityW, top10testsMW).
  #[Hint: you can use head(arranged_data, n=10) to get the top 10 records]
top10activeW <- lastDay_data %>% 
                group_by(Country) %>% 
                mutate(total_actives = sum(Active)) %>%
                arrange(desc(total_actives)) %>% 
                head(n=10)
#view(top10activeW)

top10casesW <- lastDay_data %>% 
               group_by(Country) %>% 
               mutate(total_cases = sum(NewCases)) %>%
               arrange(desc(total_cases)) %>% 
                head(n=10)
top10casesW

top10fatalityW <- lastDay_data %>% 
                  group_by(Country) %>% 
                  mutate(total_fatality = sum(FatalityRate)) %>%
                  arrange(desc(total_fatality)) %>% 
                  head(n=10)
top10fatalityW

top10testsMW <- lastDay_data %>% 
                group_by(Country) %>% 
                mutate(total_tests = sum(NewTests)) %>%
                arrange(desc(total_tests)) %>% 
                head(n=10)
#view(top10testsMW)
#8.	Based on the last day data, print the up to date confirmed, death, recovered cases as well as the tests 
  #for every continent.
continent_data <- lastDay_data %>% 
                  group_by(Continent) %>% 
                  summarise(confirmed=sum(NewCases), death=sum(NewDeaths), recovered=sum(Recovered), test=sum(NewTests))
#view(continent_data)
#9.	Build a graph to show the total number of cases over the time for the top 10 countries that have been obtained in question 7 
#(Use log for Y axis for better presentation).
#[Hint: first you need to get the data of the top-10 countries and then plot their line
top10casesW %>% 
ggplot(aes(x = reorder(`Country`,total_cases), y = total_cases, fill=Country)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(trans="log10") +
  coord_flip() +
  labs(x = "", y = "", title = "Top 10 Countries Confirmed Cases in 05/05/2020") +
  theme_bw()

#10.	Build a graph for the top 10 countries with current highest active cases which was obtained previously in question 7. 
#The graph should have one subgraph (i.e. using facet function) for each of these countries, 
#every subgraph should show how the new cases, new deaths, and new recovered cases were changing over time 
#(Use log for Y axis for better presentation, Use different colour to distinguish between new cases, deaths, and recovered).
#[hint: geom_line function with date on x_axis and each of the values of the variables in y_axis]

colnames(top10activeW)
countries_actives <- top10activeW %>% 
                    select(Country,NewDeaths,NewCases,NewTests,Recovered) %>% 
                    gather(Status, Cases, - Country) 
                    
countries_actives %>% ggplot(aes(x = reorder(Country,Cases), y = Cases,fill = Country  )) +
  geom_bar(stat = "identity")+
  scale_y_continuous(trans="log10") +
  facet_grid(.~Status) +
  labs(x = "", y= "", title = "Top 10 countries active Cases situation in 05/05/2020") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#11.	Build a graph for the top 10 countries with current highest total tests per one million of the population which was obtained previously in question 7. 
  #This graph should present total number of infected cases, total tests so far, and the total tests per million of the population for each country.
  #[hint: you can use bar chart to achieve this task]
countries_test_rate <- lastDay_data %>% 
                        group_by(Country) %>% 
                        mutate(total_tests = max(Tests_1M_Pop)) %>%
                        arrange(desc(total_tests)) %>% 
                        head(n=10)
#view(countries_test_rate)

countries_test_rate %>% 
                      select(Country,NewCases,NewTests,Tests_1M_Pop) %>% 
                      gather(Status, Cases, -Country) %>% 
                      ggplot(aes(x= Country,y= Cases, fill = Status)) +
                      geom_bar(position = "dodge", stat = "identity") +
                      scale_y_continuous(trans="log10",name = "Situation")+
                      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
                      labs(x = "", y ="", title = "Top 10 testing/1M pop in 05/05/2020")

                 
#12.	Build a graph to present the statistics of all continents which was obtained previously in question 8 
#(Use log for Y axis for better presentation, Use Continent in the legend, make sure x-axis labels does not overlap).
  continent_data %>% 
  gather("Type", "Value",-Continent) %>%
  ggplot(aes(Continent, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(trans="log10",name = "Continents Situation")+
  theme_bw()+
  labs(x = "", y ="", title = "Continents in 05/05/2020")
 
  
   #Task 3: Data-Driven Modelling:	(15 marks)
  
#1.	Based on the data of the last day, that you have extracted in the previous task, create a separate dataframe named "cor_data" with the data of these variables 
  #(CumCases, CumTests, Population, GDP, GDPCapita).
  #[Hint: you can use select function on the lastday_data dataframe]
  cor_data <- lastDay_data %>% ungroup() %>% select(CumCases, CumTests, Population, GDP, GDPCapita)
#2.	Compute the correlation matrix between the variables of the "cor_data" and visualise this correlation matrix.
  summary(cor_data)
  
  install.packages("corrplot")
  library(corrplot)

  # Generate the correlation matrix for all of the variables together
  # Correlation Matrix
  cor_matrix <- cor(cor_data)
  # visualise the correlation matrix using ggcorr function
  ggcorr(cor_matrix, label = TRUE, label_alpha = TRUE)
  
#3.	visualise the distribution of the cumulative cases in the cor_data with and without changing 
  #the scale of the x axis to log transformation.
  #[Hint: you can use the geom_histrogram function]
  
  ggplot(cor_data, aes(CumCases))+ geom_histogram(aes(y= ..density..), fill= "aquamarine3")+ geom_density(color= "red")
  
  #4.	Print the outlier values of the cumulative cases in "cor_data".
  # visualising the box plot
  
  cor_data_df <- cor_data %>% select(CumCases) %>% as_tibble()
  ggplot(cor_data_df, aes(x = as.factor("values"), y = CumCases)) + geom_boxplot()
  OutVals = boxplot(cor_data_df, plot=FALSE)$out
  print(paste0("the outlier values is ",OutVals))
  
  #5.	Divide the cor_data into training and testing, where training data represent 65% of the number of rows.
  
  library(caret)
  set.seed(123)
  #sample <- sample(c(TRUE, FALSE), nrow(cor_data), replace = T, prob= c(0.65, 0.35))
  #train <- cor_data[sample,]
  #test <- cor_data[!sample,]
  
  
  
  # Sampling 70% for training set, stratified on the outcome
  train.index <- createDataPartition(cor_data$CumCases,p = 0.65,list = FALSE)
  
  # obtain train and test set
  train <- cor_data[train.index,]
  test <- cor_data[-train.index,]
  dim(train)
  dim(test)
  #6.	Train a linear regression model to predict (y)cumulative cases from the GDP (x) of the countries.
  #Then, evaluate this model on the test data and print the root mean square error value.
  lm1 <- train(CumCases~GDP, data = train, method = "lm", 
               trControl = trainControl(method = "cv", number = 5))
  summary(lm1$finalModel)
  summary(lm1$finalModel)$r.squared

  #7.	Train another linear regression model to predict cumulative cases from all the other variables. 
      #Then, evaluate this model on the test data and print the root mean square error value.
  
  lm2 <- train(CumCases~., data = train, method = "lm", 
               trControl = trainControl(method = "cv", number = 5),
               preProcess = c('scale', 'center')) # adding pre-processing
  summary(lm2$finalModel)
  summary(lm2$finalModel)$r.squared
  
  # see features important for model 2
  ggplot(varImp(lm2))
  
  #predicting with the first model
  test$Predicted_1 <- predict(lm1, test)
  actual <- test$CumCases
  preds <- test$Predicted_1
  sqrt(mean((preds- actual)^2)) #root mean square error
  #predict with the second model
  test$Predicted_2 <- predict(lm2, test)
  actual <- test$CumCases
  preds <- test$Predicted_2
  sqrt(mean((preds- actual)^2)) #root mean square error
  
  
  
  #Task 4: Documentation and Reporting:	(10 marks)
  
  #You are required to build a report (e.g. using MS Word) for the results of Task 2 and Task 3. The report is basically composed of the answers to those questions that asked you to generate or print summaries or asked you to build graphs for some variables. Then you need to export this MS word file into a PDF file to be submitted.
  