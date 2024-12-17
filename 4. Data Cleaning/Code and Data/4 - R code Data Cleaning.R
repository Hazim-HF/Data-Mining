# Pembersihan Data
# 1. Kenalpasti corak data-data lenyap
# mice = Multivariate Imputation via Chained Equations
library(mice)

filepath = "C:\\Users\\PC 11\\Desktop\\P152419\\DATA MINING\\4 - Data set\\MData.csv"
mdata = read.csv(filepath, header = T, sep = ";")
md.pattern(mdata)
summary(mdata)

# 2. Keluarkan cerapan yang mengandungi data lenyap
mdata2 = mdata[complete.cases(mdata),]

# Lihat cerapan yang mempunyai data lenyap
mdata[!complete.cases(mdata),]

# 3. Lengkapkan data lenyap secara manual
mdata$crim
mdata$indus
indus.fix <- edit(mdata$indus)

# 4. Lengkapkan data menggunakan sukatan memusat (centralized value (mean, median, mode))
# pincang = skew
# cerapan = observation
# we can use mean for normal shape data
# if skew, use median
# if non numerical data (categorical data), use mode
attach(mdata)
par(mfrow=c(1,3))
hist(crim) # tak simetri
hist(indus) # tak simetri
hist(medv) # tak simetri

windows(10,10)
hist(rm) # simetri

# Taburan pincang & tak simetri
# Anggar data lenyap berdasarkan ukuran median
median.crim <- median(crim, na.rm = T)
crim.fix <- ifelse(is.na(crim), median.crim, crim)
crim.fix
hist(crim, main = "Bentuk taburan data asal")
hist(crim.fix, main = "Bentuk taburan data dengan anggaran data lenyap")

# Data taburan yang simetri & tak pincang
# anggaran data lenyap berdasarkan ukuran min
mean.rm <- mean(rm, na.rm = T)
rm.fix <- ifelse(is.na(rm), mean.rm, rm)
hist(rm)
hist(rm.fix)

# Latihan
# Anggarkan data lenyap bagi pembolehubah indus & medv
hist(indus)
median.indus <- median(indus, na.rm = T) # na = not available, rm = remove
indus.fix <- ifelse(is.na(indus), median.indus, indus)
hist(indus)
hist(indus.fix)
indus.fix

hist(medv)
medv
median.medv <- median(medv, na.rm = T)
medv.fix <- ifelse(is.na(medv), median.medv, medv)
hist(medv, main = "Data with missing value")
hist(medv.fix, main = "Data with missing value converted to median")
medv.fix

# Complete dataset without any missing value
# Data lengkap tanpa sebarang nilai hilang
mdata3 <- mdata[, -c(1,2,4,7,15)]
mdata.lengkap <- cbind(mdata3, crim.fix, rm.fix, indus.fix, medv.fix)
md.pattern(mdata.lengkap)

par(mfrow=c(1,1))
md.pattern(mdata)

# 5. Fill missing value using K-nearest neighbor
filepath = "C:\\Users\\PC 11\\Desktop\\P152419\\DATA MINING\\4 - Data set\\iris.mis1.csv"
iris.mis1 <- read.csv(filepath, header = T)
head(iris.mis1)
install.packages("multiUS")
library(multiUS)
iris.mis1 <- KNNimp(data=iris.mis1, k=10)

?KNNimp

# 6. Fill missing value using statistical method
# package(mice)
library(mice)
# Data dengan semua pembolehubah nilai berangka
filepathairquality = "C:\\Users\\PC 11\\Desktop\\P152419\\DATA MINING\\4 - Data set\\airquality.txt"
airquality <- read.table(filepathairquality, header = T)
md.pattern(airquality)

impdata <- mice(airquality, m=5, meth = 'pmm') # meth = 'pmm' => method = Predictive Mean Matching
completedata <- complete(impdata)

?mice

# Data dengan semua pembolehubah jenis berbeza
filepathdat2 = "C:\\Users\\PC 11\\Desktop\\P152419\\DATA MINING\\4 - Data set\\dat2.csv"
dat2 <- read.csv(filepathdat2, header = T)
md.pattern(dat2)
str(dat2)
# Transform data to appropriate format
# Gender should be nominal but appear as chr in structure
library(dplyr)

dat <- dat2 %>% 
  mutate(Smoking = as.factor(Smoking)) %>%
  mutate(Education = as.factor(Education)) %>%
  mutate(Gender = as.factor(Gender))

str(dat)

# Impute data
init <- mice(dat, maxit = 0) # maxit = max iteration
meth <- init$method # method
predm <- init$predictorMatrix # prediction model

# setkan kaedah imputasi yang digunakan
# setiap pembolehubah yang mengambil kaedah yang berbeza bergantung kepada jenis data
meth[c("Age")] = "pmm"
meth[c("Cholesterol")] = "pmm"
meth[c("BMI")] = "pmm"
meth[c("Smoking")] = "logreg"
meth[c("Education")] = "polyreg"
meth[c("Gender")] = "logreg"
meth[c("SystolicBP")] = "pmm"

imputeddata <- mice(dat, method = meth, predictorMatrix = predm)
completeddata <- complete(imputeddata)

# Data pencil/outlier
# 1. Univariate method
filepathozone3 <- "C:\\Users\\PC 11\\Desktop\\P152419\\DATA MINING\\4 - Data set\\ozone3.csv"
ozone3 <- read.csv(filepathozone3, header = T)

attach(ozone3)
names(ozone3)
par(mfrow=c(2,5))

boxplot(ozone_reading, main = "Ozone Reading")
boxplot(pressure_height, main = 'Pressure Height')
boxplot(Wind_speed, main = 'Wind Speed')
boxplot(Humidity, main = 'Humidity')

boxplot(Temperature_Sandburg, main = 'Temperature Sandburg')
boxplot(Temperature_ElMonte, main = 'Temperature ElMonte')
boxplot(Inversion_base_height, main = 'Inversion base height')
boxplot(Pressure_gradient, main = 'Pressuregradient')
boxplot(Inversion_temperature, main = 'Inversion temperature')
boxplot(Visibility, main = 'Visibility')

# Detect outlier
outlier_ozone <- boxplot.stats(ozone_reading)$out # out = outlier
out_ind <- which(ozone_reading %in% c(outlier_ozone))

ozone3[c(out_ind), ]

#Latihan
# Detect outliers in "pressure height", "wind speed", & "visibility"
out_ph <- boxplot.stats(pressure_height)$out
out_ph_loc <- which(pressure_height %in% c(out_ph))
ozone3[c(out_ph_loc), ]

out_ws <- boxplot.stats(Wind_speed)$out
out_ws_loc <- which(Wind_speed %in% c(out_ws))
ozone3[c(out_ws_loc), ]

out_v <- boxplot.stats(Visibility)$out
out_v_loc <- which(VisibSSility %in% c(out_v))
ozone3[c(out_v_loc), ]

# Bivariate method

# x ialah berkategori (aras) dan Y ialah selanjar/continuous
boxplot(ozone_reading~Month, main = "Plot Kotak Bacaan Ozone Bulanan")
boxplot(ozone_reading~Day_of_week, main = "Plot Kotak Bacaan Harian")

# Kesan data pencil dari set data
outbiv <- boxplot(ozone_reading~Month)$out
out_d <- which(ozone_reading %in% c(outbiv))
ozone3[c(out_d), ]

outbiv2 <- boxplot(ozone_reading~Day_of_week)$out
out_d2 <- which(ozone_reading %in% c(outbiv2))
ozone3[c(out_d2), ]

# X ialah berangka dan Y ialah berangka
boxplot(ozone_reading~pressure_height, main = "Plot Kotak ozone_reading vs pressure height")

# tidak sesuai untuk menggunakan plot kotak
# maka, kita gunakan plot serakan

plot(ozone_reading~pressure_height, main = "Plot Serakan ozone_reading vs pressure height")

# Use domain knowledge to determine treshold for outlier
# Setkan nilai ambang (treshold) yang bersesuaian

# Contoh nilai ambang bawah
x_min <- 5400
y_min <- 5

abline(v=x_min, col="blue", lty=2)
abline(h=y_min, col="blue", lty=2)

# contoh nilai ambang bawah
x_max <- 5900
y_max <- 35
  
abline(v=x_max, col="red", lty=2)
abline(h=y_max, col="red", lty=2)
outlier_min <- ozone3[pressure_height < x_min & ozone_reading < y_min, ]

outlier_max <- ozone3[pressure_height > x_max & ozone_reading > y_max, ]


# Di is cook's distance, slide 17
# 3. Mutlivaraite approach
# 3.1 Multivariate aproach (supervised learning)
# Y = ozone_reading
# X = lain-lain p/ubah
model.reg <- lm(ozone_reading ~ ., data=ozone3)
str(model.reg)

# Calculate cook's distance
cooksd <- cooks.distance(model.reg)

# Identify outlier
par(mforw=c(1,1))
plot(cooksd, main = 'Data Pencil berdasarkan cooks distance')
# 4 x min jarak cook
min.cook <- 4*mean(cooksd)
abline(h=min.cook, col="red", lty=2)
text(x=1:length(cooksd), y=cooksd, labels = ifelse(cooksd > min.cook, names(cooksd), ""), col = "blue")

# Ekstrak data outlier
outlier.cd <- as.numeric(names(cooksd)[cooksd>min.cook])
ozone3[outlier.cd,]

# Mahalanobi distance
# Calculate distance not on a straight line

# Unsupervised learning
filepathdatamus = "C:\\Users\\PC 11\\Desktop\\P152419\\DATA MINING\\4 - Data set\\dataMus.csv"
datamus <- read.csv(filepathdatamus, header = T)

# Calculate mahalanobis distance
m_dist <- mahalanobis(datamus, colMeans(datamus), cov(datamus))

# Setkan nilai ambang untuk kesan data pencil
# 97.5 persentil untuk taburan khi-kuasa dua

ambang <- qchisq(0.975, df=3)

# Kenal data pencil

outliermd <- which(m_dist>ambang)

library(scatterplot3d)
s3d <- scatterplot3d(datamus)

s3d$points(datamus[outliermd, ], col="red")












































































