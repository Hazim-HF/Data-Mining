#integrasi data

#1. import data ke dalam R

#1.1 Data jenis R
data(iris)
data(precip)
?iris

#drag data format R ke konsol


#1.2 Data jenis .xlsx
#Big Mart Dataset

install.packages('openxlsx')
library(openxlsx)
data.ex <- read.xlsx(xlsxFile = file.choose(), sheet = 1, startRow = 1)

#1.3 Data jenis .csv
data3 <- read.csv(file.choose(),header = T)

#1.4 Data jenis .txt
data4 <- read.table(file.choose(), header = T)

#2 Teknik integrasi Data dari sumber/format berbeza
#2.1 Integrasi data yang berlainan atribut
mydata1 <- read.table(file.choose(), header = T)

mydata2 <- read.csv(file.choose(), header = T)

mydata3 <- cbind(mydata1, mydata2)

mydata3new <- mydata3[, -c(7,8)]

#2.2 Integrasi data dengan nama atribut yang tak konsisten
#drag mydata4 ke konsol

mydata5 <- read.csv(file.choose(), header = T)

mydata6 <- merge(mydata4,mydata5, by.x = "ID", by.y = "IDPerson")

#2.3 Integrasi data dengan saiz tak sama
mydata7 <- mydata5[1:10,]
mydata8 <- merge(mydata4, mydata7, by.x = "ID", by.y = "IDPerson")
#by default:
#Semuda data yang tak sepadan akan dikeluarkan dalam data yang digabungkan

#jika ingin kekalkan data yang tak sepadan dalam data yang digabungkan
#Data yang tak sepadan akan ditakrif sebagai missing value N/A
mydata9 <- merge(mydata4, mydata7, by.x = "ID", by.y = "IDPerson", all = T)

#3. Menamakan semula atribut
library(plyr)
mydata10 <- rename(mydata9, c("ID" = "Nombor ID", "house" = "Number of House"))

#4. Ubah suai data yang tak konsisten

#4.1 Ubah suai secara manual
mydata11 <- edit(mydata10)

#4.2 Ubah suai data tak konsisten dari segi huruf besar & kecil
dataM1 <- read.csv(file.choose(), header = T)

library(dplyr)
#Takrifkan nama atribut yang nak diubah suai

city_name <- function(city){
  city <- tolower(city) #tukarkan semua ayat ke huruf kecil
  city <- trimws(city) #buang semua spacing
  city <- gsub("+", "", city) #ganti dengan hanya 1 spacing
  city <- tools::toTitleCase(city) #tukar format ke Title Case
  
  return(city)
}

dataM1$City <- sapply(dataM1$City, city_name)

#4.3 Ubah suai data yang tak konsisten
#Ejaan singkat & ejaan penuh
dataM2 <- read.csv(file.choose(), header = T)

#Petakan (mapping) data ejaan singkat kepada ejaan penuh
city_mapping <- list("NY"="New York", "CHI" = "Chicago", "LA" = "Los Angeles")

#Bina fungsi untuk ubah suai nama ejaan singkat kepada ejaan penuh
standard_city <- function(city) {
  if(city %in% names(city_mapping)){
    return(city_mapping[[city]])}
      else{
        return(city)
      }
}

dataM2$City <- sapply(dataM1$City, standard_city)

#5. Buang data yang berulang
dataM3 <- read.csv(file.choose(), header = T, sep = ";")

dataM3_NoDup <- dataM3 %>% distinct(id, .keep_all = T)

#6. Eksport data (save data)

getwd() # to find working directory (file path): where the file will be save at

#6.1 save file R
save(dataM3_NoDup, file = "dataM3_NoDup.Rdata")

#6.2 save file csv
write.csv(dataM3_NoDup, file = "dataM3_NoDup.csv")

#6.3 save file txt
write.table(dataM3_NoDup, file = "dataM3_NoDup.txt", sep = "\t")

#7. Kesan duplikasi data dengan ID yang sama
dataM4 <- read.csv(file.choose(), header = T, sep = ";")
duplikasi_ID <- dataM4[duplicated(dataM4$id) | duplicated(dataM4$id, fromLast = T),]



























