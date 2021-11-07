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

capstone_df <- capstone_df[!duplicated(capstone_df[,c('pharmacy_name', 'zip_code', 'medname', 'pat_zip_index', 'mrn_index')]),]
#> 11442748-7090720
#[1] 4352028

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