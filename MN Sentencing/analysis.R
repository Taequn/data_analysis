library(tidyverse)

csv.1<-read_csv("countycodes.csv")
csv.2<-read_csv("grids.csv")
csv.3<-read_csv("allmn.csv")

head(csv.3, 5)
colnames(csv.3)


# csv.3.updated <- csv.3 %>%
#   mutate(fullname = paste(jlname, jfname))%>%
#   mutate(rateSev = case_when(severity < 6 ~ "nonS",
#                              (severity >= 6 & severity <= 11) ~ "S",
#                              (severity >= 12 & severity <= 20) ~ "SS",
#                              TRUE ~ "Something's wrong"))

csv.3.updated <- csv.3 %>%
  mutate(fullname = paste(jlname, jfname))%>%
  mutate(rateSev = case_when(severity <= 12 ~ "Standard",
                             (severity >= 13 & severity <= 20) ~ "Sexual assault",
                             (severity >= 51 & severity <= 59) ~ "Drugs",
                             TRUE ~ "Something's wrong"))%>%
  mutate(timeDif = case_when(
    confine == 0 ~ (Maxtime-confine)
  ))



  
  
