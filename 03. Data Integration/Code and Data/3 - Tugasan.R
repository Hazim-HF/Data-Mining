library(dplyr)
library(plyr)
library(openxlsx)

load("C:\\Users\\PC 11\\Desktop\\P152419\\DATA MINING\\3 - custdata2.RData")
custdata2i <- custdata2
custdata3i <- read.csv("C:\\Users\\PC 11\\Desktop\\P152419\\DATA MINING\\3 - custdata3.csv")

#Data Pertama
data1 <- merge(custdata2i, custdata3i, by.x = "custid", by.y = "custid")
data1 <- data1 %>% select(custid, state.of.res.x, sex.x, is.employed.x, income.x, marital.stat.x, health.ins.x, 
                          housing.type.x, recent.move.x, num.vehicles.x, age.x, is.employed.fix1.x, 
                          Median.Income.x, gp.x, income.lt.30K.x, age.range.x, Income.x)

newdata1 <- data1 %>% filter(sex.x == "M", income.x > 7000) %>% select(state.of.res.x, custid, marital.stat.x, 
                                                                   health.ins.x, housing.type.x, num.vehicles.x, 
                                                                   sex.x, income.x) 

newdata1 <- newdata1 %>% arrange(income)

 add_data <- data.frame(
  state.of.res.x = c("alabama", "Louisiana", "new York"),
  custid.x = c(567891, 33421, 21134),
  marital.stat.x = c("Married", "Never Married", "bercerai"),
  health.ins.x = c(TRUE, FALSE, TRUE),
  housing.type.x = c("Sewa", "Not Available", "loan"),
  num.vehicles.x = c(2,1,2),
  sex.x = c("M", "Male", "lelaki"),
  income.x = c(99200, "Not Available", 150341)
)

newdata4 <- rbind(newdata1, add_data)

#Data Kedua
custdata3i <- custdata3i %>% select(-X)
data2 <- rbind(custdata2i, custdata3i)
newdata2 <- data2 %>% filter(sex == "M", income > 7000) %>% select(state.of.res, custid, marital.stat, health.ins, 
                                                                 housing.type, num.vehicles, sex, income) 

newdata2 <- newdata2 %>% arrange(income)

add_data <- data.frame(
  state.of.res = c("alabama", "Louisiana", "new York"),
  custid = c(567891, 33421, 21134),
  marital.stat = c("Married", "Never Married", "bercerai"),
  health.ins = c(TRUE, FALSE, TRUE),
  housing.type = c("Sewa", "Not Available", "loan"),
  num.vehicles = c(2,1,2),
  sex = c("M", "Male", "lelaki"),
  income = c(99200, "Not Available", 150341)
)
  
newdata3 <- rbind(newdata2, add_data)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
