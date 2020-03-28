setwd("E:/Papers/Risk Profiling")
library(rio)
library(dplyr)
library(tidyr)
library(stringi)
library(rattle)
library(ggplot2)
library(tidytext) # for NLP
library(stringr) # to deal with strings
library(wordcloud) # to render wordclouds
library(knitr) # for tables
library(DT) # for dynamic tables
library(tidyr)
library(lubridate)

##Load the base load Data
base=import("Data/Kiva/kiva_loans.csv")
base2=base%>%mutate(posted_time=as.Date(substr(as.character(posted_time),1,10)),
                    disbursed_time=as.Date(substr(as.character(disbursed_time),1,10)),
                    funded_time=as.Date(substr(as.character(funded_time),1,10)),
                    Female_Borrowers=stri_count_regex(tolower(borrower_genders),'female'),
                    Male_Borrowers=stri_count_regex(tolower(borrower_genders),'male'))


###Visulaize tags for more data extraction
# tokenize
tokens <- base2 %>% 
  unnest_tokens(word, tags) %>% 
  dplyr::count(word, sort = TRUE) %>% 
  ungroup()


tokens %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "lightblue", colour = "black") +
  xlab('Tags') +
  coord_flip()

base2=base2%>%mutate(Parent=stri_count_regex(tolower(tags),'parent'),
                     Elderly=stri_count_regex(tolower(tags),'elderly'),
                     women_owned_biz=stri_count_regex(tolower(tags),'woman owned'),
                     repeat_borrower=stri_count_regex(tolower(tags),'repeat borrower'))


##Load loan theme info
loan_theme=import("Data/Kiva/loan_theme_ids.csv")
chk=loan_theme%>%group_by(`Loan Theme Type`)%>%summarise(cnt=n())

##Creating table with loan theme added
base3=inner_join(base2,loan_theme%>%select(c(id,`Loan Theme Type`)),by = c("id"="id"))%>%
  mutate(Year=year(posted_time))


###Load unemployment data
unemploy=import("Data/Unemployment Indicator/Unemployment_Data.xlsx")
unemploy2=gather(unemploy%>%select(-`Indicator Code`,-`Indicator Name`),
                 key = "Year", value = "Unemployment_Rate",`1960`:`2019`)

unemploy2=unemploy2%>%filter(is.na(Unemployment_Rate)==0)
unemploy3=unemploy2%>%mutate(ck=1)%>%
  group_by(`Country Name`)%>%
  arrange(desc(Year))%>%
  mutate(Unemployment_Rate_Change=(Unemployment_Rate-lead(Unemployment_Rate))/lead(Unemployment_Rate),
         Year_lag=(as.numeric(Year) + ck))%>%select(-ck)


##Bring unemployment rate into picture
base3=base3%>%mutate(country=tolower(country),country_code=tolower(country_code))
unemploy3=unemploy3%>%ungroup(`Country Name`)%>%
  mutate(`Country Name`=tolower(`Country Name`),
         `Country Code`=tolower(substr(`Country Code`,1,2)),
         Year=as.numeric(as.character(Year)))
base4=inner_join(base3,unemploy3,by=c("Year"="Year","country"="Country Name"))
