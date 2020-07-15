## ----data, echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(COVID19)
library(forecast)
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(stringr)
library(anytime)
# devtools::install_github("mtna/rds-r", build_vignettes = TRUE)
library(rds.r)
library(ggrepel)
library(ggpubr)


gmr <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"


us <- COVID19::covid19(country="US", level=3, gmr=gmr, verbose=FALSE)
tn <- subset(us, administrative_area_level_2=="Tennessee")
knox <- subset(us, administrative_area_level_3=="Knox" & administrative_area_level_2=="Tennessee")
knox$percentconfirmed <- 100*knox$confirmed/knox$population


tn_aggregate <- tn %>% group_by(date) %>% summarise(confirmed = sum(confirmed), population=sum(population))
tn_aggregate$percentconfirmed <- 100*tn_aggregate$confirmed/tn_aggregate$population

tn_diff <- data.frame(date=tn_aggregate$date[-1], daily_confirmed=diff(tn_aggregate$confirmed), daily_percent_confirmed=diff(tn_aggregate$percentconfirmed))


us_aggregate <- us %>% group_by(date) %>% summarise(confirmed = sum(confirmed), population=sum(population))
us_aggregate$percentconfirmed <- 100*us_aggregate$confirmed/us_aggregate$population

us_diff <- data.frame(date=us_aggregate$date[-1], daily_confirmed=diff(us_aggregate$confirmed), daily_percent_confirmed=diff(tn_aggregate$percentconfirmed))

knox_diff <- data.frame(
  date=knox$date[-1],
  daily_confirmed=diff(knox$confirmed),
  daily_tested=diff(knox$tests),
  daily_recovered=diff(knox$recovered),
  daily_workplace=diff(knox$workplaces_percent_change_from_baseline),
  daily_retail_recreation=diff(knox$retail_and_recreation_percent_change_from_baseline),
  daily_residential=diff(knox$residential_percent_change_from_baseline),
  daily_percent_confirmed = diff(knox$percentconfirmed)
)

#
#
# knox_diff_last_three_weeks <- tail(knox_diff,21)
# knox_diff_last_three_weeks$daily_retail_recreation_2wk_lag <- head(tail(knox_diff$daily_retail_recreation, 21+14),21)



temp = tempfile(fileext = ".xlsx")
dataURL <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-County-New.XLSX"
download.file(dataURL, destfile=temp, mode='wb')

daily <- readxl::read_xlsx(temp, sheet =1, col_types=c("date", "text", rep("numeric",18)))

daily_knox <- subset(daily, COUNTY=="Knox")

daily_focal <- subset(daily, COUNTY %in% c("Knox", "Anderson"))

knox_pop <- max(knox$population)



## ----plotsA, echo=FALSE, message=FALSE, warning=FALSE-------------------------


knox_new <- ggplot(daily_knox[!is.na(daily_knox$NEW_CASES),], aes(x=DATE, y=NEW_CASES)) + geom_smooth() + geom_point() + ylab("Number of new cases in Knox each day") + xlab("Date") + ylim(0,NA)
print(knox_new)

knox_active <- ggplot(daily_knox[!is.na(daily_knox$TOTAL_ACTIVE),], aes(x=DATE, y=TOTAL_ACTIVE)) + geom_smooth() + geom_point() + ylab("Number of active cases in Knox each day") + xlab("Date") + ylim(0,NA)
print(knox_active)




## ----greenzone, echo=FALSE, message=FALSE, warning=FALSE, eval=FALSE----------
## # Knox County has published <a href="https://covid.knoxcountytn.gov/case-count.html#covid_data">its guidelines</a>. For it to be green for the number of new cases, it requires "No three-day shifts of 1.5 standard deviations above a rolling mean (based on data from the previous 14 days)." It's not quite clear to me what this means (the average of the three days can't exceed this or **all** or **any** of the three days can't exceed it?);  I'm taking the interpretation that if the three day mean is above this value, there's a problem. They update their stop lights weekly; this looks at each day to see if the average of that day and the two previous days exceeds what seems to be their threshold. Remember that this is just me playing with the data -- they're the experts to decide if cases are growing enough to be worrisome.
## 
## 
## GetZone <- function(cases) {
##   if(length(cases[!is.na(cases)])<17) {
##     return("black")
##   }
##   old.cases <- head(tail(cases,17),14)
##   new.cases <- tail(cases,3)
##   max.allowed.green <- mean(old.cases, na.rm=TRUE)+1.5*sd(old.cases, na.rm=TRUE)
##   max.allowed.yellow <- mean(old.cases, na.rm=TRUE)+3*sd(old.cases, na.rm=TRUE)
##   result <- "red"
##   if(mean(new.cases, na.rm=TRUE)<max.allowed.yellow) {
##     result <- "yellow"
##   }
##   if(mean(new.cases, na.rm=TRUE)<max.allowed.green) {
##     result <- "green"
##   }
##   return(result)
## }
## 
## daily3_knox <- data.frame(date=daily_knox$DATE[-(1:2)], avg=NA, col="darkgray", stringsAsFactors = FALSE)
## for(i in sequence(nrow(daily3_knox))) {
##   daily3_knox$avg[i] <- mean(daily_knox$NEW_CASES[i:(i+2)])
##   daily3_knox$col[i] <- GetZone(head(daily_knox$NEW_CASES,i+2))
## }
## 
## daily3_knox <- daily3_knox[!is.na(daily3_knox$avg),]
## daily3_knox$col <- gsub("yellow", "yellow2", daily3_knox$col)
## daily3_knox$col <- gsub("black", "darkgray", daily3_knox$col)
## 
## knox_new3 <- ggplot(daily3_knox, aes(x=date, y=avg)) + geom_smooth() + geom_point(shape=21, colour="black", fill=daily3_knox$col) + ylab("Three day average of new cases") + xlab("Date") + ylim(0,NA)
## print(knox_new3)
## 


## ----plotsB, echo=FALSE, message=FALSE, warning=FALSE-------------------------


knox_testing <- ggplot(daily_knox[!is.na(daily_knox$NEW_TESTS),], aes(x=DATE, y=NEW_TESTS)) + geom_smooth() + geom_point() + ylab("Number of new tests in Knox each day") + xlab("Date") + ylim(0,NA)
knox_testing <- knox_testing + geom_hline(yintercept=(knox_pop*45/1000)/30, col="black") + geom_hline(yintercept=(knox_pop*30/1000)/30, col="black")
print(knox_testing)




## ----plotsB2, echo=FALSE, message=FALSE, warning=FALSE------------------------

# daily_knox$NEW_PROPORTION_CONFIRMED <- 100*daily_knox$NEW_CONFIRMED/daily_knox$NEW_TESTS
#
# knox_proportion_pos <- ggplot(daily_knox[!is.na(daily_knox$NEW_PROPORTION_CONFIRMED),], aes(x=DATE, y=NEW_PROPORTION_CONFIRMED)) + geom_smooth() + geom_point() + ylab("Percentage of positive tests in Knox each day") + xlab("Date") + ylim(0,NA) + geom_hline(yintercept=10, col="black")
# print(knox_proportion_pos)


daily_focal$NEW_PROPORTION_CONFIRMED <- 100*daily_focal$NEW_CONFIRMED/daily_focal$NEW_TESTS

focal_proportion_pos <- ggplot(daily_focal[!is.na(daily_focal$NEW_PROPORTION_CONFIRMED),], aes(x=DATE, y=NEW_PROPORTION_CONFIRMED, group=COUNTY)) + geom_smooth(aes(colour=COUNTY)) + ylab("Percentage of positive tests in focal counties each day") + xlab("Date") + ylim(0,NA) + geom_hline(yintercept=10, col="black") + scale_colour_viridis_d(end=0.8)
print(focal_proportion_pos)

# par(mfcol=c(1,2))
# plot(knox$date, knox$confirmed, type="l")
# confirmed.ts <- ts(data=knox$confirmed, start=knox$date[1], end=knox$date[length(knox$date)])
# confirmed_plot <- confirmed.ts %>%
#   auto.arima() %>%
#   forecast(h=20) %>%
#   autoplot()
# print(confirmed_plot)



## ----age, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE---------------
# covidServer <- get.rds("https://knxhx.richdataservices.com/rds")
# catalog <- getCatalog(covidServer, "kcdh")
# products <- getDataProducts(catalog)
# dataProduct <- getDataProduct(catalog, "us_tn_kchd_age")
# { sink("/dev/null"); ages <- rds.select(dataProduct, autoPage =  TRUE)@records; sink(); }
# ages$Age <- as.character(ages$age_group)
# ages$age_group <- as.numeric(as.character(ages$age_group))
# ages$Age[(ages$age_group%%10==1)] <- paste(ages$age_group[(ages$age_group%%10==1)],"-",ages$age_group[(ages$age_group%%10==1)]+9, sep="")
# ages$Age[ages$age_group==0] <- "0-10"
# ages$Age[ages$age_group==99] <- "100+"
# ages$pct_confirmed <- as.numeric(as.character(ages$pct_confirmed))
#
# label_indices <- which(ages$date_stamp == min(ages$date_stamp) | ages$date_stamp == max(ages$date_stamp))
# labels <- rep("", nrow(ages))
# labels[label_indices] <- ages$Age[label_indices]
# ages$Label <- labels
#
# ageplot <- ggplot(ages, aes(x=date_stamp, y=pct_confirmed, group=Age)) + geom_line(aes(colour=Age)) + ylab("Cumulative percent of cases in each age group") + xlab("Date") + scale_colour_viridis_d(end=0.8) + geom_label_repel(aes(label = Label),na.rm = TRUE) + guides(colour = "none")
# print(ageplot)


## ----age2, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE--------------

temp = tempfile(fileext = ".xlsx")
dataURL <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-County-Age-Group.XLSX"
download.file(dataURL, destfile=temp, mode='wb')

age_county <- readxl::read_xlsx(temp, sheet =1)
age_county <- subset(age_county, AGE_GROUP != "Pending")

age_county$AGE_GROUP_SIMPLE <- age_county$AGE_GROUP
age_county$AGE_GROUP_SIMPLE <- gsub("71-80 years", "71+ years", age_county$AGE_GROUP_SIMPLE)
age_county$AGE_GROUP_SIMPLE <- gsub("81\\+ years", "71+ years", age_county$AGE_GROUP_SIMPLE)
age_county %<>% group_by(COUNTY, AGE_GROUP_SIMPLE, DATE) %>% mutate(SUM_CASE_COUNT = sum(CASE_COUNT))
age_county <- subset(age_county, AGE_GROUP != "81+ years") #get rid of pruned
age_county <- data.frame(DATE=age_county$DATE, COUNTY=age_county$COUNTY, AGE_GROUP = age_county$AGE_GROUP_SIMPLE, CASE_COUNT=age_county$SUM_CASE_COUNT)

age_county %<>% group_by(COUNTY, AGE_GROUP) %>% mutate(Difference=CASE_COUNT - lag(CASE_COUNT))
age_county %<>% group_by(COUNTY, DATE) %>% mutate(PercentDaily=100*Difference/sum(Difference), PercentCumulative=100*CASE_COUNT/sum(CASE_COUNT))

ageplot_knox_daily <- ggplot(subset(age_county, COUNTY=="Knox"), aes(x=DATE, y=PercentDaily, group=AGE_GROUP)) + geom_smooth(aes(colour=AGE_GROUP), se=FALSE) + ylab("Daily percentage of cases by age group in Knox County (smoothed)") + xlab("Date") + scale_colour_brewer(type="qual", palette="Dark2")
ageplot_knox_cumulative <- ggplot(subset(age_county, COUNTY=="Knox"), aes(x=DATE, y=PercentCumulative, group=AGE_GROUP)) + geom_line(aes(colour=AGE_GROUP)) + ylab("Cumulative percentage of cases by age group in Knox County") + xlab("Date") + scale_colour_brewer(type="qual", palette="Dark2")
#ageplot_knox_both <- ggarrange(ageplot_knox_daily, #ageplot_knox_cumulative, labels=c("Daily", "Cumulative"), ncol=2, nrow=1)
print(ageplot_knox_daily)
print(ageplot_knox_cumulative)



## ----utactive, echo=FALSE, message=FALSE, warning=FALSE-----------------------
webfiles <- list.files(path="/Users/bomeara/Dropbox/UTKCovid", pattern="*html", full.names =TRUE)
utk.cases <- data.frame()
for(i in seq_along(webfiles)) {
  raw <- paste0(readLines(webfiles[i]),collapse=" ")
  raw <- gsub('\\x3c', '', raw, fixed=TRUE)
  raw <- gsub('\\x3d', '', raw, fixed=TRUE)
  raw <- gsub('\\x3e', '', raw, fixed=TRUE)
  raw <- gsub('/span/tdtd style\"width: \\d+\\.*\\d*%;\"span style\"font-size: 18px;\"', '', raw, fixed=FALSE)
  raw <- gsub('/span/tdtd style\"width: \\d+\\.*\\d*%; text-align: left;\"span style\"font-size: 18px;\"', '', raw, fixed=FALSE)
  students <- as.numeric(gsub("Students", "", stringr::str_extract(raw, "Students\\d+")))
    faculty <- as.numeric(gsub("Faculty", "", stringr::str_extract(raw, "Faculty\\d+")))
    staff <- as.numeric(gsub("Staff", "", stringr::str_extract(raw, "Staff\\d+")))
  actual_time <- anytime::anytime(stringr::str_extract(webfiles[i], "\\d+_\\d+_\\d+_\\d+_\\d+_\\d+"))
  result <- data.frame(date=rep(actual_time, 3), count=c(students, faculty, staff), group=c("students", "faculty", "staff"))
  if(i==1) {
    utk.cases <- result
  } else {
    utk.cases <- rbind(utk.cases, result)
  }
}
utk.cases$group <- as.factor(utk.cases$group)
utk_plot <- ggplot(utk.cases, aes(x=date, y=count, group=group)) + geom_line(aes(colour=group)) + ylab("Number of active cases at UTK") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
print(utk_plot)


## ----plotsC, echo=FALSE, message=FALSE, warning=FALSE-------------------------

knox_activity <- reshape2::melt(knox[,c("date", colnames(knox)[grepl("baseline", colnames(knox))])],id.var='date')
knox_activity <- knox_activity[!grepl("transit", knox_activity$variable),]
knox_activity$variable <- gsub("_percent_change_from_baseline", "", knox_activity$variable )
knox_activity <- knox_activity[!is.na(knox_activity$value),]
g <- ggplot(knox_activity, aes(x=date, y=value, col=variable)) + geom_smooth() + ylab("Activity in Knox County over time as percentage of baseline activity\nData from Google")
g <- g + geom_hline(yintercept=0, col="black")
print(g)

# p <- ggplot(knox_diff, aes(x=date, y=daily_confirmed)) + geom_smooth(span=14/nrow(knox_diff)) + geom_point()
# print(p)



## ----hospitalcapacitydata, echo=FALSE, message=FALSE, warning=FALSE-----------

counties_in_region <- c("Knox", "Anderson", "Roane", "Scott", "Blount", "Claiborne", "Jefferson", "Campbell", "Sevier", "Loudon", "Hamblen", "Cocke", "Monroe", "McMinn")
daily_in_region <- subset(daily, COUNTY %in% counties_in_region)

covidServer <- get.rds("https://knxhx.richdataservices.com/rds")
catalog <- getCatalog(covidServer, "kchd")
products <- getDataProducts(catalog)
dataProduct <- getDataProduct(catalog, "us_tn_kchd_capacity")
{ sink("/dev/null"); hospital_resources <- rds.select(dataProduct, autoPage =  TRUE)@records; sink(); }
hospital_resources$label = NA
hospital_resources$label[hospital_resources$resource_type==0] <- "All beds"
hospital_resources$label[hospital_resources$resource_type==1] <- "ICU beds"
hospital_resources$label[hospital_resources$resource_type==2] <- "Ventilators"
hospital_resources$cnt_available <- as.numeric(as.character(hospital_resources$cnt_available))
hospital_resources$cnt_capacity <- as.numeric(as.character(hospital_resources$cnt_capacity))
hospital_resources$pct_used <- as.numeric(as.character(hospital_resources$pct_used))
hospital_resources$pct_available <- as.numeric(as.character(hospital_resources$pct_available))


# hospitalfiles <- list.files(path="/Users/bomeara/Dropbox/KnoxCovid", pattern="*bed*", full.names =TRUE)
# capacity.df <- data.frame()
# previoushospitaldata <- data.frame()
# current.capacity.df <- data.frame()
# for (i in seq_along(hospitalfiles)) {
#   actual_time <- anytime::anytime(stringr::str_extract(hospitalfiles[i], "\\d+_\\d+_\\d+_\\d+_\\d+_\\d+"))
#   hospitaldata <- read.csv(hospitalfiles[i], stringsAsFactors=FALSE)
#   hospitaldata$Current.Utilization <- as.numeric(gsub('%', '', hospitaldata$Current.Utilization))
#   hospitaldata$Resource <- hospitaldata$East.Region.Hospitals
#
#   if(i==1) {
#     previoushospitaldata <- hospitaldata
#     hospitaldata$Date <- actual_time
#     capacity.df <- hospitaldata
#   } else {
#     if(all(dim(hospitaldata)==dim(previoushospitaldata))) {
#       if(any(hospitaldata!=previoushospitaldata)) {
#         previoushospitaldata <- hospitaldata
#         hospitaldata$Date <- actual_time
#         capacity.df <- rbind(capacity.df, hospitaldata)
#       }
#     }
#   }
#   current.capacity.df <- hospitaldata
#   rownames(current.capacity.df) <- current.capacity.df$Resource
# }


## ----hospitalcapacityplot, echo=FALSE, message=FALSE, warning=FALSE-----------
try(hosp_plot <- ggplot(hospital_resources, aes(x=date_stamp, y=pct_used, group=label)) + geom_line(aes(colour=label)) + ylab("Percent Utilization in East Tennessee Region") + xlab("Date") + ylim(0,100) + scale_colour_viridis_d(end=0.8))
try(print(hosp_plot))



## ----plotsD, echo=FALSE, message=FALSE, warning=FALSE-------------------------


knox_new_hospitalization <- ggplot(daily_knox[!is.na(daily_knox$NEW_HOSPITALIZED),], aes(x=DATE, y=NEW_HOSPITALIZED)) + geom_smooth() + geom_point() + ylab("Number of new covid hospitalizations in Knox each day") + xlab("Date") + ylim(0,NA)
print(knox_new_hospitalization)

tn_daily_aggregate <- daily %>% group_by(DATE) %>% summarise(new_hosp = sum(NEW_HOSPITALIZED))


tn_new_hospitalization <- ggplot(tn_daily_aggregate[!is.na(tn_daily_aggregate$new_hosp),], aes(x=DATE, y=new_hosp)) + geom_smooth() + geom_point() + ylab("Number of new covid hospitalizations in TN each day") + xlab("Date") + ylim(0,NA)
print(tn_new_hospitalization)

# all_confirmed <- data.frame(date=c(us_aggregate$date, tn_aggregate$date, knox$date), percentconfirmed=c(us_aggregate$percentconfirmed, tn_aggregate$percentconfirmed, knox$percentconfirmed), region=c(rep("US", nrow(us_aggregate)),rep("TN", nrow(tn_aggregate)), rep("Knox", nrow(knox))))
# con <- ggplot(all_confirmed, aes(x=date, y=percentconfirmed, color=region)) + geom_smooth() + geom_point() + ylab("Percent of population with confirmed tests")
# print(con)
#
# three_weeks_ago <- tail(sort(unique(all_confirmed$date)),21)[1]
# all_confirmed_3 <- all_confirmed[all_confirmed$date>=three_weeks_ago,]
#
# con3 <- ggplot(all_confirmed_3, aes(x=date, y=percentconfirmed, color=region)) + geom_smooth() + geom_point() + ylab("Percent of population with confirmed tests")
# print(con3)
#
# diff_confirmed <-  data.frame(date=c(us_diff$date, tn_diff$date, knox_diff$date), daily_percent_confirmed=c(us_diff$daily_percent_confirmed, tn_diff$daily_percent_confirmed, knox_diff$daily_percent_confirmed), region=c(rep("US", nrow(us_diff)),rep("TN", nrow(tn_diff)), rep("Knox", nrow(knox_diff))))
# diffplot <- ggplot(diff_confirmed, aes(x=date, y=daily_percent_confirmed, color=region)) + geom_smooth(span=14/nrow(knox_diff)) + geom_point() + ylab("Percent of population new confirmed tests daily")
# print(diffplot)
#
#
# diff_confirmed_3 <- diff_confirmed[diff_confirmed$date>=three_weeks_ago,]
# diffplot3 <- ggplot(diff_confirmed_3, aes(x=date, y=daily_percent_confirmed, color=region)) + geom_smooth() + geom_point() + ylab("Percent of population new confirmed tests daily")
# print(diffplot3)


