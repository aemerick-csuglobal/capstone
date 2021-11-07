library(readr)
library(dplyr)
library(plyr)
library(readxl)
library(janitor)
library(ROCR)
library(caTools)
library(caret)
library(Amelia)
library(readxl)
library(ggplot2)
library(cvms)
library(shiny)
library(e1071)
library(fastmatch)
library(lubridate)
library(randomForest)
library(tidyverse)
library(sqldf)
library(neuralnet)


#direct <- "C:/Users/emericka/Desktop/Shields"
desktop <-  "C:/Users/emericka/Desktop"

CY19_df <- read_csv("C:/Users/emericka/Desktop/Shields/Combined_df.19.csv")
CY20_df <- read_csv("C:/Users/emericka/Desktop/Shields/Combined_df.20.csv")
CY21_df <- read_csv("C:/Users/emericka/Desktop/Shields/Combined_df.21.csv")


CY19_df <- clean_names(CY19_df)
CY20_df <- clean_names(CY20_df)
CY21_df <- clean_names(CY21_df)

gc()

capstone_df <- capstone_df[!duplicated(capstone_df[,c('pharmacy_name', 'zip_code', 'medname', 'pat_zip_index', 'mrn_index')]),]
#> 11442748-7090720
#[1] 4352028

drop_col <- c(1,2,6:8,18:24,26:27,29,30,32:34,36:37,40:45)

CY19_df <- CY19_df[,-drop_col]
CY20_df <- CY20_df[,-drop_col]
CY21_df <- CY21_df[,-drop_col]

CY19_df <- CY19_df[!CY19_df$dme == 'Yes',]
CY20_df <- CY20_df[!CY20_df$dme == 'Yes',]
CY21_df <- CY21_df[!CY21_df$dme == 'Yes',]

CY19_df <- CY19_df[!CY19_df$otc == 'Y',]
CY20_df <- CY20_df[!CY20_df$otc == 'Y',]
CY21_df <- CY21_df[!CY21_df$otc == 'Y',]

CY19_df$patient_zip_code <- substr(CY19_df$patient_zip_code, 0, 5)
CY20_df$patient_zip_code <- substr(CY20_df$patient_zip_code, 0, 5)
CY21_df$patient_zip_code <- substr(CY21_df$patient_zip_code, 0, 5)

CY19_df$zip_code <- substr(CY19_df$zip_code, 0, 5)
CY20_df$zip_code <- substr(CY20_df$zip_code, 0, 5)
CY21_df$zip_code <- substr(CY21_df$zip_code, 0, 5)

#capstone_df <- capstone_df[!capstone_df$controlled_med == 'N',]

capstone_df <- rbind(CY19_df, CY20_df)
capstone_df <- rbind(capstone_df, CY21_df)

capstone_df <- transform(capstone_df, pat_zip_index=fmatch(patient_zip_code, unique(patient_zip_code)))
capstone_df <- transform(capstone_df, mrn_index=fmatch(mrn, unique(mrn)))
capstone_df <- capstone_df[complete.cases(capstone_df[ ,c(1:12,17:18)]),]
# rows that have missing values: 11740124-11442748
single_inst <-  !capstone_df %in% capstone_df[duplicated(capstone_df[,2:6])]
capstone_df$order_inst <- mdy_hm(capstone_df$order_inst)
capstone_df <- capstone_df[order(capstone_df$order_inst) ,]

final_df <- capstone_df[,-c(16:19)]

controlled_meds <- capstone_df[!capstone_df$controlled_med == 'N',]

final$patient_contact_department
rm(CY19_df, CY20_df, CY21_df, drop_col, C2_20, C2_21, C2_19)

specialty <- capstone_df[!capstone_df$spec_med == 'No',]
colnames(specialty)
specialty <- specialty[,-c(1,6,8:11,13,14,16)]
spec_zip <- specialty[!duplicated(specialty[,c('pharmacy_name', 'zip_code', 'medname', 'pat_zip_index', 'mrn_index')]),]
spec_table <- table(spec_zip$patient_zip_code)

date_vector <- capstone_df$order_inst

first_date <- date_vector[1:(length(date_vector)-1)]
second_date <- date_vector[2:length(date_vector)]
second_gap <- difftime(second_date, first_date, units="mins")
yduplicate_index <- second_gap>90
duplicate_index <- c(T, duplicate_index)
stone_df <- capstone_df[duplicate_index, ]

rm(duplicate_index, second_gap, date_vector, second_date, first_date)



write.csv(spec_table, file.path(desktop, "spec_zip.csv"))
write.csv(specialty, file.path(desktop, "specialty_meds.csv"))



final <- sqldf("select*
      from filtered left join df
      on filtered.pat_zip_index = f2.pat_zip_index",
      row.names=T)

zips <- final[!grepl("mail", final$pharmacy_name, ignore.case=T) ,]
View(table(zips$pharmacy_name))
zips <- final[!grepl("accredo", final$pharmacy_name, ignore.case=T) ,]
zips <- final[!grepl("home delivery", final$pharmacy_name, ignore.case=T) ,]
zips <- zips[!grepl("home delivery", zips$pharmacy_name, ignore.case=T) ,]
zips <- zips[!grepl("mail", zips$pharmacy_name, ignore.case=T) ,]
zips <- zips[!grepl("specialty", zips$pharmacy_name, ignore.case=T) ,]
zips <- zips[!grepl("accredo", zips$pharmacy_name, ignore.case=T) ,]
zips <- zips[!grepl("express scripts", zips$pharmacy_name, ignore.case=T) ,]
zips <- zips[!grepl("SPECIALTY", zips$pharmacy_name, ignore.case=T) ,]
View(table(zips$pharmacy_name))
zips <- zips[!grepl("briova", zips$pharmacy_name, ignore.case=T) ,]
zips <- final[grepl(", CO -", final$pharmacy_name, ignore.case=T) ,]

CO_Zips <- zips[grep("Yes", zips$spec_med), ]
specialty.df <- transform(specialty.df, dept_index=fmatch(patient_contact_department, unique(patient_contact_department)))
specialty.df <- transform(specialty.df, med_index=fmatch(medname, unique(medname)))

test.df <- CO_Zips[,c(4:5,19,20)]


final <- final[final$patient_zip_code >80000 & final$patient_zip_code < 81659, ]
final <- final[final$zip_code >80000 & final$zip_code < 81659, ]
hist(final$patient_zip_code, main = "Histogram of Prescribing Patterns by Zip Code", xlab = "Zip Code")

Norm_funct <- function(x){
  (x-min(x))/(max(x)-min(x))
}

specialty.df <- final[ grep("Yes", final$spec_med), ]
norm.nn.df <- as.data.frame(lapply(specialty.df[,c(4:5,19,20)], Norm_funct))
set.seed(72)
smp_size <- floor(0.6 * nrow(norm.nn.df ))
train.index <- sample(seq_len(nrow(norm.nn.df )), size = smp_size)
training.df <- norm.nn.df [train.index,]
valid.df <- norm.nn.df [-train.index,]
nn <- neuralnet(patient_zip_code~., data = training.df, linear.output = F, hidden = 3)
plot(nn, rep="best")
predict.nn <- compute(nn, valid.df$patient_zip_code)
predicted.class <- apply(predict.nn$net.restult, 1, which.max)-1
confusionMatrix(predicted.class, valid.df$patient_zip_code)
View(predict.nn$net.result)


controlled_meds <- capstone_df[!capstone_df$controlled_med == 'N',]
control_month <- substr(controlled_meds$order_inst, 0,7)
plot_controls <- setNames(data.frame(table(control_month)), c("Date", "Count"))

specialty <- capstone_df[!capstone_df$spec_med == 'No',]

colnames(specialty)
# date_vector <- capstone_df$order_inst
# first_date <- date_vector[1:(length(date_vector)-1)]
# second_date <- date_vector[2:length(date_vector)]
# second_gap <- difftime(second_date, first_date, units="mins")
# yduplicate_index <- second_gap>90
# duplicate_index <- c(T, duplicate_index)
# stone_df <- capstone_df[duplicate_index, ]

#rm(duplicate_index, second_gap, date_vector, second_date, first_date)

C2_meds <- function(x,y){
  a <- x[grep("oxycodone", y, ignore.case = T),]
  b <- x[grep("oxycontin", y, ignore.case = T),]
  c <- x[grep("fentanyl", y, ignore.case = T),]
  d <- x[grep("methadone", y, ignore.case = T),]
  e <- x[grep("codeine", y, ignore.case = T),]
  f <- x[grep("hydrocodone", y, ignore.case = T),]
  g <- x[grep("norco", y, ignore.case = T),]
  h <- x[grep("perocet", y, ignore.case = T),]
  i <- x[grep("morphine", y, ignore.case = T),]
  j <- x[grep("hydromorphone", y, ignore.case = T),]
  k <- x[grep("opium", y, ignore.case = T),]
  l <- x[grep("oxymorphone", y, ignore.case = T),]
  all_c2 <- rbind(a,b)
  all_c2 <- rbind(all_c2,c)
  all_c2 <- rbind(all_c2,d)
  all_c2 <- rbind(all_c2,e)
  all_c2 <- rbind(all_c2,f)
  all_c2 <- rbind(all_c2,g)
  all_c2 <- rbind(all_c2,h)
  all_c2 <- rbind(all_c2,i)
  all_c2 <- rbind(all_c2,j)
  all_c2 <- rbind(all_c2,k)
  all_c2 <- rbind(all_c2,l)
  return(all_c2)
}

desktop <- "C:\\Users\\emeri\\Desktop"

spec.glm.col <- c(2:4,9,11:14)

smp_size <- floor(0.60 * nrow(specialty))
set.seed(72)
train.index <- sample(seq_len(nrow(specialty)), size = smp_size)
train.df <- specialty[train.index,spec.glm.col]
test.df <- specialty[-train.index,spec.glm.col]

spec_glm <- lm(pat_zip_index ~., data = train.df)
summary(spec_glm)

write.csv(capstone_df, file.path(desktop, "filtered.csv"))
write.csv(controlled_meds, file.path(desktop, "all_controlled.csv"))
write.csv(only_c2_meds, file.path(desktop, "only_narcs.csv"))
write.csv(specialty, file.path(desktop, "specialty_meds.csv"))

sapply(specialty, function(x){length(unique(x))})

specialty.glm <- transform(specialty.glm, pharmacy_index=fmatch(pharmacy_name, unique(pharmacy_name)))
specialty.glm <- transform(specialty.glm, department_index=fmatch(department, unique(department)))
specialty.glm <- transform(specialty.glm, medication_index=fmatch(medname, unique(medname)))
specialty.glm <- transform(specialty.glm, medication_index=fmatch(medname, unique(medname)))
specialty.glm <- transform(specialty.glm, pharm_zip_index=fmatch(zip_code, unique(zip_code)))
colSums(is.na(specialty.glm))
pharmacy_name       
CO.pharmacy <- specialty.glm[grep(", CO -", specialty.glm$pharmacy_name) , ]
CO.pharmacy <- CO.pharmacy[,c(8,10:13)]
specialty.col <- c(2,4,8,10:15)


smp_size <- floor(0.60 * nrow(CO.pharmacy))
set.seed(72)
train.index <- sample(seq_len(nrow(CO.pharmacy)), size = smp_size)
train.df <- CO.pharmacy[train.index,]
test.df <- CO.pharmacy[-train.index,]

spec_glm <- lm(pat_zip_index ~., data = train.df)
summary(spec_glm)

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

spec_rf <- randomForest(pat_zip_index~., data=train.df, ntree=500, mtry=4, nodesize=5, importance=T)
varImpPlot(spec_rf, ttype = 1)
rf_pred <- predict(spec_rf, test.df)
confusion_matrix(rf_pred, test.df$pat_zip_index)
