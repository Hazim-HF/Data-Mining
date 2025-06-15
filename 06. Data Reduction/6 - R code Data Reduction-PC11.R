# Data reduction
# reduce - buat secara manual & domain knowledge
# 1. Remove attribute
# - kalau lebih kurang, buang je
# 2. Attribute yang tak relevan
# 3. Buang yang tak significant
library(ISLR)
install.packages('ISLR')
data(Hitters)
head(Hitters)
?Hitters
hitters2 <- na.omit(Hitters)
model.f <- lm(Salary~., data=hitters2)
summary(model.f)
dim(hitters2)
attach(hitters2)
hitters3 <- cbind(AtBat, Hits, Walks, CWalks, Division, PutOuts)
head(hitters3)
# PCA (Primary Component Analysis) (Analisis Komponent Utama) - banyak guna dalam image processing
fp_reading = "C:\\Users\\PC 11\\OneDrive - Universiti Kebangsaan Malaysia\\P152419\\DATA MINING\\6 - Data set\\READING120n.csv"
reading <- read.csv(fp_reading)
# remove attribute yang bukan nombor
dat <- reading[, -1]
library(psych)
describe(dat)

zdat <- scale(dat)

# Plot kolerasi dan taburan data
pairs(~., data=zdat)
r <- cor(zdat)
p <- ncol(zdat)
# nilai dan vektor eigen
e <- eigen(r)
ev <- e$values
evr <- e$vectors
# Peratusan varians bagi setiap p/ubah PCA
prop.var <- ev/length(ev)
cumsum(prop.var)
# 0.674 0.819 0.889 0.939 0.978 1.000
# kalau kekalkan 1, dapat menerangkan 67.4% dari data asal
# kekalkan 2, dah menerangkan 81.9% dari data asal
y <- zdat %*% evr
colnames(y) <- c('PCA1', 'PCA2', 'PCA3', 'PCA4', 'PCA5', 'PCA6')
# data yang dikekalkan untuk analisis hanyalah PCA1 & PCA2
data2 <- y[, c(1,2)]

# Analisis faktor ----------------------------------------------------------------------
fp_ft <-"C:\\Users\\PC 11\\OneDrive - Universiti Kebangsaan Malaysia\\P152419\\DATA MINING\\6 - Data set\\food-texture.csv"
foodtexture <- read.csv(fp_ft, row.names="X")
# piawaikan data
z_skor <- scale(foodtexture)
library(corrplot)
install.packages("corrplot")
corrplot(cor(z_skor), order = "hclust")

# scree plot
scree(z_skor)
f.a <- factanal(z_skor, factors = 2, rotation = "varimax")
# boleh kekalkan 0.761 dari data asal
# pemboleh ubah yang dominan dari factor1 = oil, industry, crispy, fracture
# pemboleh ubah yang dominan dari factor2 = crispy, fracture, hardness
f.a_skor <- factanal(z_skor, factors = 2, scores = c("regression"), 
                 rotation = "varimax")
f.a_skor$scores

data <- read.csv(file.choose(), sep = ";")

# Kita berminat dengan hubungan y=expenditure terhadap fitur yang lain, x1 = income, x2 = education_level, x3 = work_experience
# model regresi
# andaian model regresi: y menghampiri bertaburan normal
attach(data)
hist(data$expenditure)
# kita nampak y tak normal, jadi kene transform dulu
y2 <- log(expenditure)
hist(y2)
# haa, baru normal
# suaikan model regresi terhadap data
model_reg <- lm(log(expenditure)~income+education_level+work_experience, data = data)
# ehh, define education level as factor dulu
data$education_level <- as.factor(data$education_level)
# run balik model_reg
# r2 > 0.99, boleh menerangkan lebih 99% daripada variasi data asal
# bila dah ngam, simpan maklumat berkaitan model
# parameter model
coef(model_reg)
# Dalam bentuk persamaan matematik
log(expenditure) = 
# maklumat fitur:
# X1 = Income
hist(income)
muIn <- mean(income)
sdIn <- sd(income)
IncomeR <- range(income)

# X2 = Education level
edu_range <- 1:5

# X3 = work experience
hist(work_experience)
muwe <- mean(work_experience)
sdwe <- sd(work_experience)
rwe <- range(work_experience)

# simulasi fitur
x1 = income
n <- 200

# X1 = Income
income.sim <-rnorm(n, mean = muIn, sd = sdIn)
par(mfrow=c(2,3))
hist(income.sim, main = "data simulation")

# X2 = Education level
education.sim <- sample(1:5, n, replace = T)

# X3 = work experience
work.sim <- rnorm(n, mean = muwe, sd = sdwe)
hist(work.sim, main = 'simulation')

fitur.sim <- data.frame(expenditure, work.sim, income.sim, education_level)
fitur.sim$education.sim <- as.factor(fitur.sim$education.sim)

# data simulasi
data2 <- data.frame(income.sim, education.sim, work.sim, expenditure)

# data simulasi y= expenditure dari model regresi
sim_expenditure <- predict(model_reg, fitur.sim)

kewangand <- read.table(file.choose())
attach(kewangand)
summary(kewangand)
str(kewangand)
kewangand$Bangsa <- as.factor(Bangsa)
str(kewangand)

# strata bangsa di Malaysia
table(Bangsa) / length(Bangsa)

# instead of use data, we use general knowledge
# 60% malay, 30% cina, 10% indian

# Persampelan berstrata
sample_malay <- 3000 * 0.6
sample_chinese <- 3000 * 0.3
sample_indian <- 3000 * 0.1

# asingkan berdasarkan strata
# Melayu
d1 <- subset(kewangand, Bangsa == "Melayu")

# Persampelan semula butstrap
n1 <- sample(nrow(d1), size = sample_malay, replace = F)
sn1 <- d1[n1,]

# Cina
d2 <- subset(kewangand, Bangsa == "Cina")

# Persampelan semula butstrap
n2 <- sample(nrow(d2), size = sample_chinese, replace = F)
sn2 <- d2[n2,]

# India
d3 <- subset(kewangand, Bangsa == "India")

# Persampelan semula butstrap
n3 <- sample(nrow(d3), size = sample_indian, replace = F)
sn3 <- d3[n3,]

newdata <- rbind(sn1, sn2, sn3)



















