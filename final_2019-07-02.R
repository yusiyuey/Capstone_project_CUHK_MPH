library(tidyverse)
library(lubridate)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(ggpubr)

setwd("C:/Users/Faith/OneDrive - The Chinese University of Hong Kong/public health/Rare. Diseases/Dropbox/duplicate/2019-04-21-Yu_MG/")

data.merge.over18.spss <- read_csv("data_merge_SPSS_all_over18_2019-04-21.csv", guess_max = 2000)
data.merge.over18<-read_csv("data.merge.over18_2019-04-23.csv", guess_max = 2000)
#gender
gender <- data.merge.over18.spss$`8`
#gender_re 
#0=male   1=female
typeof(data.merge.over18.spss$`8`)
data.merge.over18.spss$`gender_re` <- factor(data.merge.over18.spss$`8`,levels = c("1", "2"), labels = c("0", "1"))
table(data.merge.over18.spss$`gender_re`) 

#average_age
data.merge.over18 <-
  data.merge.over18 %>% mutate(age = as.numeric(as.Date("2018-05-02") - birth_date_dateformat) /365.25)
mean.tmp <- mean(data.merge.over18$age, na.rm = T)
data.merge.over18$age[is.na(data.merge.over18$age)] <- mean.tmp
average_age <- data.merge.over18$age
#education
#for descriptive statistics 
#Table1
Mgmeeting_educa <- data.merge.over18.spss$`440`
Mgmeeting_educa[Mgmeeting_educa == 2] <- 1
Mgmeeting_educa[Mgmeeting_educa == 3] <- 1
Mgmeeting_educa[Mgmeeting_educa == 6] <- 4
Mgmeeting_educa[Mgmeeting_educa == 5] <- 7
Mgmeeting_educa[Mgmeeting_educa == 8] <- 7
table(Mgmeeting_educa)
Mgmeeting_educa <- factor(Mgmeeting_educa,levels = c("1","4","7"), labels = c("1","2","3"))
table(Mgmeeting_educa)

round(table(Mgmeeting_educa) / length(Mgmeeting_educa)*100 ,2) #propotion
tapply(Mgmeeting_educa, gender, function(x){
  round(table(x) / length(x)*100, 2)
})
table(Mgmeeting_educa,gender)
chisq.test(Mgmeeting_educa, gender)
#education_continuou
#for linear regression analysis
tmp_educa <- data.merge.over18.spss$`440`
tmp_educa[tmp_educa == 2] <- 1
tmp_educa[tmp_educa == 8] <- 7
table(tmp_educa)
tmp_educa <- factor(tmp_educa,levels = c("1","3","4","5","6","7"), labels = c("1","2","4","5","3","6"))
table(tmp_educa)
tmp_educa <- as.numeric(tmp_educa)
table(tmp_educa)
data.merge.over18.spss$`education` <- tmp_educa

#Employment_past_six_months
table(data.merge.over18.spss$`443`)
employment_past6_1 <-  data.merge.over18.spss$`443`
employment_past6_1 <- as.factor(employment_past6_1)
employment_past6_1 <- as.numeric(employment_past6_1)
employment_past6_1[employment_past6_1=="2"] = "1"
table(employment_past6_1)

Employment_past_six_months <- factor(employment_past6_1,levels = c("1","3"), labels = c("0", "1"))
table(Employment_past_six_months)
round(table(Employment_past_six_months)/sum(table(Employment_past_six_months))*100,2)

tapply(Employment_past_six_months, gender, function(x){
  table(x) / sum(table(x))
})
chisq.test(Employment_past_six_months, gender)
#Annual_household_income
iqr.459 <- IQR(data.merge.over18.spss$`459_new`, na.rm = T)
quantile.459 <- quantile(data.merge.over18.spss$`459_new`, na.rm = T)

ul.459 <- quantile.459[4] + 1.5 * iqr.459
check.over.ul.459 <- data.merge.over18.spss$`459_new` > ul.459
pos.true.459 <- which(check.over.ul.459)
check.over.ul.459[pos.true.459] <- NA

Annual_household_income_rm.olt <- data.merge.over18.spss$`459_new`[!check.over.ul.459]
median.tmp <- median(Annual_household_income_rm.olt, na.rm = T)
mean.tmp <- mean(Annual_household_income_rm.olt, na.rm = T)
Annual_household_income_rm.olt[is.na(Annual_household_income_rm.olt)] <- mean.tmp
Annual_household_income <- Annual_household_income_rm.olt
table(Annual_household_income)
quantile(Annual_household_income)

tapply(Annual_household_income, gender, quantile)
wilcox.test(Annual_household_income[gender == "1"], Annual_household_income[gender == "2"])
#average_onset_age
average_onset_age <- data.merge.over18$`onset age`
mean.tmp <- mean(average_onset_age, na.rm = T)
average_onset_age[is.na(average_onset_age)] <- mean.tmp
#dis_diagnosed_years
interval.tmp <-
  as.period(data.merge.over18$`47_new`) - years(data.merge.over18$`onset age`)
interval.tmp <- period_to_seconds(interval.tmp) / 60 / 60 / 24 / 365.25

iqr.interval.tmp<-IQR(interval.tmp, na.rm = T)
quantile.interval.tmp<-quantile(interval.tmp, na.rm = T)
ul.interval.tmp<-quantile.interval.tmp[4]+1.5*iqr.interval.tmp
check.over.ul.interval.tmp<-interval.tmp > ul.interval.tmp
pos.true.interval.tmp<-which(check.over.ul.interval.tmp)
check.over.ul.interval.tmp[pos.true.interval.tmp]<-NA
ll.interval.tmp <- quantile.interval.tmp[2]-1.5*iqr.interval.tmp
check.under.ll.interval.tmp <- interval.tmp < ll.interval.tmp
pos.true.interval.tmp <- which(check.under.ll.interval.tmp)
check.over.ul.interval.tmp[pos.true.interval.tmp] <- NA
dis_diagnosed_years<-interval.tmp[!check.over.ul.interval.tmp]

mean.tmp <- mean(dis_diagnosed_years, na.rm = T)
dis_diagnosed_years[is.na(dis_diagnosed_years)] <- mean.tmp
#disaese-duration
ill.time <-
  as.Date("2018/5/2") - as.Date(data.merge.over18$`46_new`)
ill.time <- as.numeric(ill.time) / 365.25

mean.tmp <- mean(ill.time, na.rm = T)
ill.time[is.na(ill.time)] <- mean.tmp
dis_duration <- ill.time
#hyperlasia
xxzs <- (str_detect(data.merge.over18$`61`, "胸腺增生") & str_detect(data.merge.over18$`63`, "(NA)")) | str_detect(data.merge.over18$`63`, "胸腺增生")
table(xxzs)
xxzs <- as.numeric(xxzs)
table(xxzs)

xxzs[is.na(xxzs)] <- 2
table(xxzs)
data.merge.over18.spss$hyperlasia <- xxzs
#Thymoma
data.merge.over18.spss$Thymoma <- factor(data.merge.over18.spss$`80`,levels = c("1", "2","3"), labels = c("1","0","2"))
data.merge.over18.spss$Thymoma <- as.character(data.merge.over18.spss$Thymoma)
data.merge.over18.spss$Thymoma <- as.numeric(data.merge.over18.spss$Thymoma)
data.merge.over18.spss$Thymoma[data.merge.over18.spss$Thymoma=="2"] = NA
table(data.merge.over18.spss$`Thymoma`)
#thymectomy
data.merge.over18.spss$thymectomy <- factor(data.merge.over18.spss$`64`,levels = c("1", "2"), labels = c("1", "0"))
data.merge.over18.spss$thymectomy <- as.character(data.merge.over18.spss$thymectomy)
data.merge.over18.spss$thymectomy <- as.numeric(data.merge.over18.spss$thymectomy)
table(data.merge.over18.spss$thymectomy)
#ICU_admission
data.merge.over18.spss$`ICU_admission` <- factor(data.merge.over18.spss$`107`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$ICU_admission <- as.character(data.merge.over18.spss$ICU_admission)
data.merge.over18.spss$ICU_admission <- as.numeric(data.merge.over18.spss$ICU_admission)
data.merge.over18.spss$ICU_admission[data.merge.over18.spss$ICU_admission=="2"] = NA
table(data.merge.over18.spss$`ICU_admission`)
#Excerbation
table(data.merge.over18.spss$`342`)
data.merge.over18.spss$`Excerbation` <- factor(data.merge.over18.spss$`342`,levels = c("1", "2","3"), labels = c("1", "0","2"))
table(data.merge.over18.spss$`Excerbation`)
data.merge.over18.spss$Excerbation <- as.character(data.merge.over18.spss$Excerbation)
data.merge.over18.spss$Excerbation <- as.numeric(data.merge.over18.spss$Excerbation)
data.merge.over18.spss$Excerbation[data.merge.over18.spss$Excerbation=="2"] = NA
table(data.merge.over18.spss$`Excerbation`)
Excerbation <- data.merge.over18.spss$Excerbation

round(table(Excerbation) / length(Excerbation)*100 ,2) #propotion
tapply(Excerbation, gender, function(x){
  round(table(x) / length(x)*100, 2)
})
chisq.test(Excerbation,gender)
#Comorbidities
#psoriasis
tmp_prs <- sapply(data.merge.over18.spss$`129`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_prs)
chisq.test(tmp_prs, gender)

data.merge.over18.spss$`psoriasis` <- factor(data.merge.over18.spss$`129`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$psoriasis <- as.character(data.merge.over18.spss$psoriasis)
data.merge.over18.spss$psoriasis <- as.numeric(data.merge.over18.spss$psoriasis)
data.merge.over18.spss$psoriasis[data.merge.over18.spss$psoriasis=="2"] = NA
table(data.merge.over18.spss$`psoriasis`)
#diabetes_type2
tmp_type2 <- sapply(data.merge.over18.spss$`135`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_type2)

chisq.test(tmp_type2, gender)

data.merge.over18.spss$`diabetes_type2` <- factor(data.merge.over18.spss$`135`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$diabetes_type2 <- as.character(data.merge.over18.spss$diabetes_type2)
data.merge.over18.spss$diabetes_type2 <- as.numeric(data.merge.over18.spss$diabetes_type2)
data.merge.over18.spss$diabetes_type2[data.merge.over18.spss$diabetes_type2=="2"] <- NA
table(data.merge.over18.spss$`diabetes_type2`)
#diabetes_type1
#chisq.test(fufa6.xxqc, gender.xxqc)

tmp_diabe <- sapply(data.merge.over18.spss$`124`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_diabe)
chisq.test(tmp_diabe, gender)

table(data.merge.over18.spss$`124`)
data.merge.over18.spss$`diabetes_type1` <- factor(data.merge.over18.spss$`124`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$diabetes_type1 <- as.character(data.merge.over18.spss$diabetes_type1)
data.merge.over18.spss$diabetes_type1 <- as.numeric(data.merge.over18.spss$diabetes_type1)
data.merge.over18.spss$diabetes_type1[data.merge.over18.spss$diabetes_type1=="2"] = NA
table(data.merge.over18.spss$`diabetes_type1`)

tapply(data.merge.over18.spss$`diabetes_type1`, average_age, function(x){
  table(x) / sum(table(x))})

#rheumatoid_arthritis
tmp_rheu <- sapply(data.merge.over18.spss$`125`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_rheu)

chisq.test(tmp_rheu, gender)

table(data.merge.over18.spss$`125`)
data.merge.over18.spss$`rheumatoid_arthritis` <- factor(data.merge.over18.spss$`125`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$rheumatoid_arthritis <- as.character(data.merge.over18.spss$rheumatoid_arthritis)
data.merge.over18.spss$rheumatoid_arthritis <- as.numeric(data.merge.over18.spss$rheumatoid_arthritis)
data.merge.over18.spss$rheumatoid_arthritis[data.merge.over18.spss$rheumatoid_arthritis=="2"] = NA
table(data.merge.over18.spss$`rheumatoid_arthritis`)

#autoimmune_thyroidtable(data.merge.over18.spss$`125`)
tmp_auto_thy <- sapply(data.merge.over18.spss$`126`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_auto_thy)
chisq.test(tmp_auto_thy, gender)


data.merge.over18.spss$`autoimmune_thyroid` <- factor(data.merge.over18.spss$`126`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$autoimmune_thyroid <- as.character(data.merge.over18.spss$autoimmune_thyroid)
data.merge.over18.spss$autoimmune_thyroid <- as.numeric(data.merge.over18.spss$autoimmune_thyroid)
data.merge.over18.spss$autoimmune_thyroid[data.merge.over18.spss$autoimmune_thyroid=="2"] = NA
table(data.merge.over18.spss$`autoimmune_thyroid`)

#lupus
tmp_auto_lu <- sapply(data.merge.over18.spss$`127`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_auto_lu)
chisq.test(tmp_auto_lu, gender)

data.merge.over18.spss$`lupus` <- factor(data.merge.over18.spss$`127`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$lupus <- as.character(data.merge.over18.spss$lupus)
data.merge.over18.spss$lupus <- as.numeric(data.merge.over18.spss$lupus)
data.merge.over18.spss$lupus[data.merge.over18.spss$lupus=="2"] = NA
table(data.merge.over18.spss$`lupus`)

#multiple_sclerosis
tmp_mul <- sapply(data.merge.over18.spss$`128`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_mul)
chisq.test(tmp_mul, gender)

data.merge.over18.spss$`multiple_sclerosis` <- factor(data.merge.over18.spss$`128`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$multiple_sclerosis <- as.character(data.merge.over18.spss$multiple_sclerosis)
data.merge.over18.spss$multiple_sclerosis <- as.numeric(data.merge.over18.spss$multiple_sclerosis)
data.merge.over18.spss$multiple_sclerosis[data.merge.over18.spss$multiple_sclerosis=="2"] = NA
table(data.merge.over18.spss$`multiple_sclerosis`)

#inflammatory_bowel
tmp_infl <- sapply(data.merge.over18.spss$`130`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_infl)
chisq.test(tmp_infl, gender)

data.merge.over18.spss$`inflammatory_bowel` <- factor(data.merge.over18.spss$`130`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$inflammatory_bowel <- as.character(data.merge.over18.spss$inflammatory_bowel)
data.merge.over18.spss$inflammatory_bowel <- as.numeric(data.merge.over18.spss$inflammatory_bowel)
data.merge.over18.spss$inflammatory_bowel[data.merge.over18.spss$inflammatory_bowel=="2"] = NA
table(data.merge.over18.spss$`inflammatory_bowel`)

#B12_deficiency
tmp_defi <- sapply(data.merge.over18.spss$`131`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_defi)
chisq.test(tmp_defi, gender)

data.merge.over18.spss$`B12_deficiency` <- factor(data.merge.over18.spss$`131`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$B12_deficiency <- as.character(data.merge.over18.spss$B12_deficiency)
data.merge.over18.spss$B12_deficiency <- as.numeric(data.merge.over18.spss$B12_deficiency)
data.merge.over18.spss$B12_deficiency[data.merge.over18.spss$B12_deficiency=="2"] = NA
table(data.merge.over18.spss$`B12_deficiency`)

#neuropathy
tmp_neur <- sapply(data.merge.over18.spss$`132`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_neur)
chisq.test(tmp_neur, gender)

data.merge.over18.spss$`neuropathy` <- factor(data.merge.over18.spss$`132`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$neuropathy <- as.character(data.merge.over18.spss$neuropathy)
data.merge.over18.spss$neuropathy <- as.numeric(data.merge.over18.spss$neuropathy)
data.merge.over18.spss$neuropathy[data.merge.over18.spss$neuropathy=="2"] = NA
table(data.merge.over18.spss$`neuropathy`)

#asthma
tmp_asth <- sapply(data.merge.over18.spss$`133`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_asth)
chisq.test(tmp_asth, gender)

data.merge.over18.spss$`asthma` <- factor(data.merge.over18.spss$`133`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$asthma <- as.character(data.merge.over18.spss$asthma)
data.merge.over18.spss$asthma <- as.numeric(data.merge.over18.spss$asthma)
data.merge.over18.spss$asthma[data.merge.over18.spss$asthma=="2"] = NA
table(data.merge.over18.spss$`asthma`)

#COPD
tmp_copd <- sapply(data.merge.over18.spss$`134`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_copd)
chisq.test(tmp_copd, gender)

data.merge.over18.spss$`COPD` <- factor(data.merge.over18.spss$`134`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$COPD <- as.character(data.merge.over18.spss$COPD)
data.merge.over18.spss$COPD <- as.numeric(data.merge.over18.spss$COPD)
data.merge.over18.spss$COPD[data.merge.over18.spss$COPD=="2"] = NA
table(data.merge.over18.spss$`COPD`)

#high_blood_pressure
tmp_highbl <- sapply(data.merge.over18.spss$`136`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_highbl)
chisq.test(tmp_highbl, gender)

data.merge.over18.spss$`high_blood_pressure` <- factor(data.merge.over18.spss$`136`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$high_blood_pressure <- as.character(data.merge.over18.spss$high_blood_pressure)
data.merge.over18.spss$high_blood_pressure <- as.numeric(data.merge.over18.spss$high_blood_pressure)
data.merge.over18.spss$high_blood_pressure[data.merge.over18.spss$high_blood_pressure=="2"] = NA
table(data.merge.over18.spss$`high_blood_pressure`)

#osteoporosis
tmp_ost <- sapply(data.merge.over18.spss$`137`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_ost)
chisq.test(tmp_ost, gender)

data.merge.over18.spss$`osteoporosis` <- factor(data.merge.over18.spss$`137`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$osteoporosis <- as.character(data.merge.over18.spss$osteoporosis)
data.merge.over18.spss$osteoporosis <- as.numeric(data.merge.over18.spss$osteoporosis)
data.merge.over18.spss$osteoporosis[data.merge.over18.spss$osteoporosis=="2"] = NA
table(data.merge.over18.spss$`osteoporosis`)

#high_cholesterol
tmp_chol <- sapply(data.merge.over18.spss$`138`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_chol)
chisq.test(tmp_chol, gender)

data.merge.over18.spss$`high_cholesterol` <- factor(data.merge.over18.spss$`138`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$high_cholesterol <- as.character(data.merge.over18.spss$high_cholesterol)
data.merge.over18.spss$high_cholesterol <- as.numeric(data.merge.over18.spss$high_cholesterol)
data.merge.over18.spss$high_cholesterol[data.merge.over18.spss$high_cholesterol=="2"] = NA
table(data.merge.over18.spss$`high_cholesterol`)

#depression
tmp_depr <- sapply(data.merge.over18.spss$`139`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_depr)
chisq.test(tmp_depr, gender)

data.merge.over18.spss$`depression` <- factor(data.merge.over18.spss$`139`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$depression <- as.character(data.merge.over18.spss$depression)
data.merge.over18.spss$depression <- as.numeric(data.merge.over18.spss$depression)
data.merge.over18.spss$depression[data.merge.over18.spss$depression=="2"] = NA
table(data.merge.over18.spss$`depression`)

#anxiety
table(data.merge.over18.spss$`140`)
tmp_anxi <- sapply(data.merge.over18.spss$`140`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_anxi)
chisq.test(tmp_anxi, gender)

data.merge.over18.spss$`anxiety` <- factor(data.merge.over18.spss$`140`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$anxiety <- as.character(data.merge.over18.spss$anxiety)
data.merge.over18.spss$anxiety <- as.numeric(data.merge.over18.spss$anxiety)
data.merge.over18.spss$anxiety[data.merge.over18.spss$anxiety=="2"] = NA
table(data.merge.over18.spss$`anxiety`)

#cancer
tmp_canc <- sapply(data.merge.over18.spss$`144`, function(x){
  if(is.na(x)){
    return(NA)
  } else if(x == 3){
    return(NA)
  } else {
    return(x)
  }
})
table(tmp_canc)
chisq.test(tmp_canc, gender)

data.merge.over18.spss$`cancer` <- factor(data.merge.over18.spss$`144`,levels = c("1", "2","3"), labels = c("1", "0","2"))
data.merge.over18.spss$cancer <- as.character(data.merge.over18.spss$cancer)
data.merge.over18.spss$cancer <- as.numeric(data.merge.over18.spss$cancer)
data.merge.over18.spss$cancer[data.merge.over18.spss$cancer=="2"] = NA
table(data.merge.over18.spss$`cancer`)
#multiple_comorbidities
df.comorbid <- data.merge.over18.spss %>% select(as.character(c(124:140, 144)))
comorbid <- apply(df.comorbid, 1, function(x){
  sum(x==1)
})
data.merge.over18.spss$multiple_comorbidities <- comorbid
multiple_comorbidities <- data.merge.over18.spss$multiple_comorbidities
table(multiple_comorbidities)
cuttime = c(0,1,2,3,20)
multiple_comorbidities.1 <- cut(multiple_comorbidities, cuttime,right = F, include.lowest = T)
multiple_comorbidities.1 <- factor(multiple_comorbidities.1,
                                   levels = c("[0,1)", "[1,2)", "[2,3)",
                                              "[3,20]"),
                                   labels = c("0","1","2","3"))
#Social_suppot
#tangible_support
tangible.support <- data.merge.over18[,names(data.merge.over18) %in% 
                                        as.character(c(477,480,487,490))]

tangible.support <- tangible.support %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "完全没有", "1")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "偶尔", "2")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "有些时候", "3")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "大部分时候", "4")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "所有时候", "5")}) 

tangible.support <- tangible.support %>% transmute_all(as.numeric)

tangible.support.100 <- apply(tangible.support, 1, function(x){
  (sum(x) - 4) / 16 * 100
})
# affectionate_support
affectionate.support <- data.merge.over18[,names(data.merge.over18) %in% 
                                            as.character(c(478,479,483,484,
                                                           488,491,492,494))]

affectionate.support <- affectionate.support %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "完全没有", "1")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "偶尔", "2")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "有些时候", "3")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "大部分时候", "4")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "所有时候", "5")}) 

affectionate.support <- affectionate.support %>% transmute_all(as.numeric)

affectionate.support.100 <- apply(affectionate.support, 1, function(x){
  (sum(x) - 8) / 32 * 100
})
# positive_social_suuport
positive.social.support <- data.merge.over18[,names(data.merge.over18) %in% 
                                               as.character(c(482,486,489,493))]

positive.social.support <- positive.social.support %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "完全没有", "1")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "偶尔", "2")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "有些时候", "3")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "大部分时候", "4")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "所有时候", "5")}) 

positive.social.support <- positive.social.support %>% transmute_all(as.numeric)

positive.social.support.100 <- apply(positive.social.support, 1, function(x){
  (sum(x) - 4) / 16 * 100
})

# emotional_support
emotional.support <- data.merge.over18[,names(data.merge.over18) %in% 
                                         as.character(c(481,485,495))]

emotional.support <- emotional.support %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "完全没有", "1")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "偶尔", "2")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "有些时候", "3")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "大部分时候", "4")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "所有时候", "5")}) 

emotional.support <- emotional.support %>% transmute_all(as.numeric)

emotional.support.100 <- apply(emotional.support, 1, function(x){
  (sum(x) - 3) / 12 * 100
})
# leisure_activites
tmp_leisure_activites <- data.merge.over18[,names(data.merge.over18) %in% 
                                            as.character(496:502)]

tmp_leisure_activites <- tmp_leisure_activites %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "从不", "1")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "一年几次或更少", "2")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "一月几次", "3")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "一周几次", "4")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "每天", "5")}) 

tmp_leisure_activites <- tmp_leisure_activites %>% transmute_all(as.numeric)

tmp_leisure_activites <- tmp_leisure_activites %>% transmute_all(function(x){x*20})
leisure_activites <- apply(tmp_leisure_activites, 1, mean)
#ADL
data.mgadl <- data.merge.over18[,names(data.merge.over18) %in% as.character(22:29)]

data.mgadl <- data.mgadl %>% 
  transmute_all(.funs = function(x){str_sub(x,1,1)}) %>% 
  transmute_all(function(x){as.numeric(x) - 1})

ADL <- apply(data.mgadl, 1, sum)
#MG-QOL
data.mgqol <- data.merge.over18[,names(data.merge.over18) %in% as.character(30:44)]

data.mgqol <- data.mgqol %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "非常符合", "0")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "完全不符合", "6.66666667")}) %>% 
  transmute_all(.funs = function(x){str_replace_all(x, "有一点符合", "3.33333333")})

data.mgqol <- data.mgqol %>% transmute_all(as.numeric)
mg.qol.sum <- apply(data.mgqol, 1, sum)
# QOL-15 mean and sd
#use Shapiro-Wilk test to test normality
# compare gender differences by using Mann–Whitney U test
mg.qol.sum <- apply(data.mgqol, 1, sum)

mean(mg.qol.sum)
sd(mg.qol.sum)
tapply(mg.qol.sum, gender, function(x){
  mean(x, na.rm = T)
})
tapply(mg.qol.sum, gender, function(x){
  sd(x, na.rm = T)
})

shapiro.test(mg.qol.sum)
wilcox.test(mg.qol.sum[gender=="1"], mg.qol.sum[gender=="2"])
# QoL gender differences plot
df.mgqol <- bind_cols(data.mgqol, gender = factor(gender, levels = c(1, 2), labels = c("male", "female"))) %>%  gather(key = "key", value = "value", 1:15)
df.mgqol <- df.mgqol %>% group_by(key, gender) %>% summarise(Mean = mean(value), sd = sd(value))
df.mgqol$key <- as.numeric(df.mgqol$key)
data.mgqol.pvalue <- apply(data.mgqol, 2, function(x){
  tmp <- wilcox.test(x[gender == "1"], x[gender == "2"])
  return(tmp$p.value)
})
data.mgqol.pvalue.check <- data.mgqol.pvalue < 0.05
data.mgqol.pvalue.sig <- as.numeric(names(data.mgqol.pvalue))[data.mgqol.pvalue.check]
df.mgqol.sig <- df.mgqol %>% filter(key %in% data.mgqol.pvalue.sig)

# histogram
df.mgqol$Items <- factor(df.mgqol$key, 
                         levels = c(31, 32, 34, 36, 38, 39, 41, 42,44,
                                    33, 35, 37, 30, 40, 43),
                         
                         labels = c("2. I have trouble with my eyes","I have trouble eating","5. My MG limits my ability to enjoy hobbies and fun activities", "7. I have to make plans around my MG", "9. I have difficulty speaking", "10. I have lost some personal independence", "12. I have trouble walking", "13. I have trouble getting around public places","15. I have trouble performing my personal grooming needs","4. I have limited my social activity","6. I have trouble meeting the needs of my family","8. I am bothered by limitations in performing my work", "1. I am frustrated by my MG","11. I am depressed about my MG","I feel overwhelmed"))
df.mgqol$group <- factor(rep(c("Subjective well-being", "Personal mobility and MG symptoms", "Personal mobility and MG symptoms", "Mobility in society", "Personal mobility and MG symptoms", "Mobility in society","Personal mobility and MG symptoms","Mobility in society","Personal mobility and MG symptoms","Personal mobility and MG symptoms","Subjective well-being","Personal mobility and MG symptoms","Personal mobility and MG symptoms","Subjective well-being","Personal mobility and MG symptoms"), each = 2))
df.mgqol.sig <- df.mgqol %>% filter(key %in% data.mgqol.pvalue.sig)
df.mgqol.sig.male <- df.mgqol.sig %>% filter(gender == "male")
# df.mgqol$gender_chi <- factor(df.mgqol$gender, levels = c("male", "female"),
# labels = c("男", "女"))
df.mgqol %>% ggplot(aes(x = Items, y = Mean, fill = gender, group = gender)) +
  geom_col(position = "dodge") +
  #geom_line() +
  geom_point(data = df.mgqol.sig.male, aes(x = Items, y = Mean +0.1), shape = 8) +
  facet_grid(group~., switch = "y", scales = "free_y", space = "free_y") +
  coord_flip()+
  xlab("y")+
  ylab("x")+
  scale_fill_discrete(name="Gender", labels = c("Male", "Female"))

#physical_QoL
data.mgqol_physQoL <- data.mgqol
Physical_QoL1 <- data.mgqol_physQoL[,names(data.mgqol_physQoL) %in% as.character(c(31,32,34,36,38,39,41,42,44))]
Physical_QoL <- apply(Physical_QoL1, 1, sum)
Physical_QoL <- Physical_QoL/60*100
mean(Physical_QoL)
sd(Physical_QoL)
tapply(Physical_QoL, gender, mean)


tapply(Physical_QoL, gender, sd)
shapiro.test(Physical_QoL)
wilcox.test(Physical_QoL[gender=="1"], Physical_QoL[gender=="2"])

wilcox.test(mg.qol.sum[gender=="1"], mg.qol.sum[gender=="2"])
#Social_QoL
data.mgqol_sociamobi <- data.mgqol
Social_QoL1 <- data.mgqol_sociamobi[,names(data.mgqol_sociamobi) %in% as.character(c(33,35,37))]
Social_QoL1 <- apply(Social_QoL1, 1, sum)
Social_QoL <- Social_QoL1/20*100
mean(Social_QoL)
sd(Social_QoL)
tapply(Social_QoL, gender, mean)
tapply(Social_QoL, gender, sd)
shapiro.test(Social_QoL)
wilcox.test(Social_QoL[gender=="1"], Social_QoL[gender=="2"])
#Emotional_QoL
data.mgqol_emotional_wellgener <- data.mgqol
Emotional_QoL1 <- data.mgqol_emotional_wellgener[,names(data.mgqol_emotional_wellgener) %in% as.character(c(30,40,43))]
Emotional_QoL1 <- apply(Emotional_QoL1, 1, sum)
Emotional_QoL <- Emotional_QoL1/20*100
mean(Emotional_QoL)
sd(Emotional_QoL)
tapply(Emotional_QoL, gender, mean)
tapply(Emotional_QoL, gender, sd)
shapiro.test(Emotional_QoL)
wilcox.test(Emotional_QoL[gender=="1"], Emotional_QoL[gender=="2"])

#Correlation between ADL and QOL
cor.test(ADL, mg.qol.sum)
cor.test(ADL,Physical_QoL )
cor.test(ADL,Social_QoL )
cor.test(ADL,Emotional_QoL )

#
#pyridostigmine
Thymoma <- data.merge.over18.spss$`Thymoma`

df.tmp.thymoma_mg.qolsum <- bind_cols(data.merge.over18.spss, mg.qol.sum = mg.qol.sum) %>% filter(!is.na(Thymoma) & !is.na(Thymoma))

df.tmp.thymo_physicalqol <- bind_cols(data.merge.over18.spss, Physical_QoL = Physical_QoL) %>% filter(!is.na(Thymoma) )

df.tmp.thymo_socialqol <- bind_cols(data.merge.over18.spss, Social_QoL = Social_QoL) %>% filter(!is.na(Thymoma))

df.tmp.thymo_emotional <- bind_cols(data.merge.over18.spss, Emotional_QoL = Emotional_QoL) %>% filter(!is.na(Thymoma) )

gender <- data.merge.over18.spss$`gender_re`

a <- ggplot(df.tmp.thymoma_mg.qolsum)+
  geom_boxplot(aes(x = factor(Thymoma), y = mg.qol.sum, color = factor(gender))) + 
  geom_smooth(aes(x = Thymoma + 1, y = mg.qol.sum, color = factor(gender)), method = "lm")+
  xlab("osteoporosis")+
  ylab("Overall QoL")+
  scale_color_discrete(name = "pyridostigmine")

b <- ggplot(df.tmp.thymo_physicalqol)+
  geom_boxplot(aes(x = factor(gender), y = Physical_QoL, color = factor(Thymoma))) + 
  geom_smooth(aes(x = gender + 1, y = Physical_QoL, color = factor(Thymoma)), method = "lm")+
  xlab("osteoporosis")+
  ylab("Physical QoL")+
  scale_color_discrete(name = "pyridostigmine")

c <- ggplot(df.tmp.thymo_socialqol)+
  geom_boxplot(aes(x = factor(gender), y = Social_QoL, color = factor(Thymoma))) + 
  geom_smooth(aes(x = gender + 1, y = Social_QoL, color = factor(Thymoma)), method = "lm")+
  xlab("osteoporosis")+
  ylab("Social QoL")+
  scale_color_discrete(name = "pyridostigmine")

d <- ggplot(df.tmp.thymo_emotional)+
  geom_boxplot(aes(x = factor(gender), y = Emotional_QoL, color = factor(Thymoma))) + 
  geom_smooth(aes(x = gender + 1, y = Emotional_QoL, color = factor(Thymoma)), method = "lm")+
  xlab("osteoporosis")+
  ylab("Emotional QoL")+
  scale_color_discrete(name = "pyridostigmine")


library(ggpubr)
ggarrange(a ,b ,c ,d, labels = c("a", "b", "c", "d"), common.legend = T, legend = "right") 
#generate a new table 
df.for.lm.final <- tibble(mg.qol.sum = mg.qol.sum,
                    Physical_QoL = Physical_QoL,
                    Social_QoL = Social_QoL,
                    Emotional_QoL = Emotional_QoL,
                    gender = data.merge.over18.spss$`gender_re`,
                    average_age = average_age,
                    education = data.merge.over18.spss$`education`,
                    Employment_past_six_months = Employment_past_six_months,
                    Annual_household_income = Annual_household_income,
                    average_onset_age = average_onset_age,
                    dis_diagnosed_years = dis_diagnosed_years,
                    dis_duration = dis_duration,
                    hyperlasia = data.merge.over18.spss$hyperlasia,
                    Thymoma = data.merge.over18.spss$Thymoma,
                    thymectomy = data.merge.over18.spss$thymectomy,
                    ICU_admission = data.merge.over18.spss$`ICU_admission`,
                    Excerbation = Excerbation,
                    multiple_comorbidities = multiple_comorbidities,
                    tangible_support = tangible.support.100,
                    affectionate_support = affectionate.support.100,
                    positive_social_support = positive.social.support.100,
                    emotional_support = emotional.support.100,
                    leisure_activites = leisure_activites,
                    ADL = ADL)

write_excel_csv(df.for.lm.final, "df.for.lm.final.csv")

#lm_factor
df.for.lm.final <- df.for.lm.final %>% mutate_at(c("gender",
                                                   "Employment_past_six_months",
                                                   "hyperlasia",
                                                   "Thymoma",
                                                   "thymectomy",
                                                   "ICU_admission",
                                                   "Excerbation"), as_factor)

#lm_model
names(df.for.lm.final)
df.for.lm.final <- na.exclude(df.for.lm.final)
head(df.for.lm.final)
glimpse(df.for.lm.final)
model <- lm(Physical_QoL ~ 
              gender+
              average_age+
              #gender*average_age+
              education+
              Employment_past_six_months+
              Annual_household_income+
              #average_onset_age+
              #dis_diagnosed_years+
              #dis_duration+
              #hyperlasia+
              Thymoma+
                #gender*Thymoma+
              #hyperlasia+
              thymectomy+
              #gender*thymectomy+
              ICU_admission+
              Excerbation+
              multiple_comorbidities+
                gender*multiple_comorbidities+
              tangible_support+
              affectionate_support+
              positive_social_support+
              emotional_support+
              leisure_activites+
                ADL, 
              data = df.for.lm.final)

options(max.print = 10000000)
summary(model)

sink("lm.txt")
print(summary(model))

sink()

library(MASS)
#getOption("na.action")
model.step <- stepAIC(model)
summary(model.step)
model.step$model
