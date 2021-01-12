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
library(lubridate)
library(tidyquant)
library(binom)
library(scales)
library(jsonlite)

options(timeout=600) # let things download for at least ten minutes
options(download.file.method = "libcurl")

#gmr <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"


counties_in_hospital_region <- c("Knox", "Anderson", "Roane", "Scott", "Blount", "Claiborne", "Jefferson", "Campbell", "Sevier", "Loudon", "Hamblen", "Cocke", "Monroe", "McMinn")

us <- COVID19::covid19(country="US", level=3, verbose=FALSE)
tn <- subset(us, administrative_area_level_2=="Tennessee")
knox <- subset(us, administrative_area_level_3=="Knox" & administrative_area_level_2=="Tennessee")
oakridge <- subset(us, administrative_area_level_3 %in% c("Roane", "Anderson") & administrative_area_level_2=="Tennessee")
region <-  subset(us, administrative_area_level_3 %in% counties_in_hospital_region & administrative_area_level_2=="Tennessee")
knox$percentconfirmed <- 100*knox$confirmed/knox$population

utk_testing_zukowski <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRRoX-icFl5a6T5OVpSllMJ3QGVplgdDEKPHtuvEjcDKNEvw5X6dcgGYSMGmynFcdxUwH2u4kZjBTiT/pub?gid=1083850404&single=true&output=csv")
utk_testing_zukowski$Date <- lubridate::mdy(paste(utk_testing_zukowski$Date, 2020, sep=", "))

utk_reported_zukowski <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRRoX-icFl5a6T5OVpSllMJ3QGVplgdDEKPHtuvEjcDKNEvw5X6dcgGYSMGmynFcdxUwH2u4kZjBTiT/pub?gid=1053792052&single=true&output=csv")

tn_aggregate <- tn %>% group_by(date) %>% summarise(confirmed = sum(confirmed), population=sum(population))
tn_aggregate$percentconfirmed <- 100*tn_aggregate$confirmed/tn_aggregate$population

tn_diff <- data.frame(date=tn_aggregate$date[-1], daily_confirmed=diff(tn_aggregate$confirmed), daily_percent_confirmed=diff(tn_aggregate$percentconfirmed))


us_aggregate <- us %>% group_by(date) %>% summarise(confirmed = sum(confirmed), population=sum(population))
us_aggregate$percentconfirmed <- 100*us_aggregate$confirmed/us_aggregate$population

oakridge_aggregate <- oakridge %>% group_by(date) %>% summarise(confirmed = sum(confirmed), population=sum(population))
oakridge_aggregate$percentconfirmed <- 100*oakridge_aggregate$confirmed/oakridge_aggregate$population

region_aggregate <- region %>% group_by(date) %>% summarise(confirmed = sum(confirmed), population=sum(population))
region_aggregate$percentconfirmed <- 100*region_aggregate$confirmed/region_aggregate$population

#us_diff <- data.frame(date=us_aggregate$date[-1], daily_confirmed=diff(us_aggregate$confirmed), daily_percent_confirmed=diff(tn_aggregate$percentconfirmed))



knox_diff <- data.frame(
  date=knox$date[-1],
  daily_confirmed=diff(knox$confirmed),
  daily_tested=diff(knox$tests),
  daily_recovered=diff(knox$recovered),
#  daily_workplace=diff(knox$workplaces_percent_change_from_baseline),
  #daily_retail_recreation=diff(knox$retail_and_recreation_percent_change_from_baseline),
#  daily_residential=diff(knox$residential_percent_change_from_baseline),
  daily_percent_confirmed = diff(knox$percentconfirmed)
)


knox_pop <- max(knox$population)
oakridge_pop <- max(oakridge_aggregate$population)
region_pop <- max(region_aggregate$population)

#
#
# knox_diff_last_three_weeks <- tail(knox_diff,21)
# knox_diff_last_three_weeks$daily_retail_recreation_2wk_lag <- head(tail(knox_diff$daily_retail_recreation, 21+14),21)



temp = tempfile(fileext = ".xlsx")
dataURL <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-County-New.XLSX"
download.file(dataURL, destfile=temp, mode='wb')

daily <- readxl::read_xlsx(temp, sheet =1, col_types=c("date", "text", rep("numeric",23)))

daily_knox <- subset(daily, COUNTY=="Knox") %>% select(-"COUNTY")
daily_knox$Region <- "Knox County"
daily_knox$Population <- knox_pop

daily_oakridge <- subset(daily, COUNTY %in% c("Roane", "Anderson")) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)
daily_oakridge$Region <- "Anderson + Roane"
daily_oakridge$Population <- oakridge_pop
#daily_oakridge$New_cases_per_100k <- 100000*(daily_oakridge$NEW_CASES/daily_oakridge$Population)



daily_region<- subset(daily, COUNTY %in% counties_in_hospital_region) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)

daily_region$Region <- "East TN"
daily_region$Population <- region_pop

daily_utk_testing_zukowski <- data.frame(DATE=utk_testing_zukowski$Date, TOTAL_CASES=utk_testing_zukowski$TESTS_POS_TOTAL, NEW_CASES=utk_testing_zukowski$TESTS_POS_NEW, TOTAL_CONFIRMED=utk_testing_zukowski$TESTS_POS_TOTAL, NEW_CONFIRMED=utk_testing_zukowski$TESTS_POS_NEW, POS_TESTS=utk_testing_zukowski$TESTS_POS_TOTAL, NEW_POS_TESTS=utk_testing_zukowski$TESTS_POS_NEW, NEG_TESTS=utk_testing_zukowski$TESTS_NEG_TOTAL, NEW_NEG_TESTS=utk_testing_zukowski$TESTS_NEG_NEW, TOTAL_TESTS=utk_testing_zukowski$TESTS_TOTAL, NEW_TESTS=utk_testing_zukowski$TESTS_NEW, Population=30000, Region="UTK Student Testing")

daily_utk_reported_zukowski <- data.frame(DATE=lubridate::ymd(utk_reported_zukowski$DATE), TOTAL_CASES=utk_reported_zukowski$CASES_TOTAL, NEW_CASES=utk_reported_zukowski$CASES_NEW, TOTAL_CONFIRMED=utk_reported_zukowski$CASES_TOTAL, NEW_CONFIRMED=utk_reported_zukowski$CASES_NEW,  TOTAL_ACTIVE=utk_reported_zukowski$CASES_ACTIVE, Population=30000, Region="UTK Reported")

#daily_focal <- dplyr::bind_rows(daily_knox, daily_oakridge, daily_region, daily_utk_testing_zukowski, daily_utk_reported_zukowski)

daily_focal <- dplyr::bind_rows(daily_knox, daily_oakridge, daily_utk_testing_zukowski, daily_utk_reported_zukowski)

daily_focal$Tests_per_100k <- 100000*(daily_focal$NEW_TESTS/daily_focal$Population)
daily_focal$New_cases_per_100k <- 100000*(daily_focal$NEW_CASES/daily_focal$Population)
daily_focal$Active_cases_per_100k <- 100000*(daily_focal$TOTAL_ACTIVE/daily_focal$Population)



temp = tempfile(fileext = ".xlsx")
dataURL <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-County-Cases-5-18-Years.XLSX"
download.file(dataURL, destfile=temp, mode='wb')

schoolkids <- readxl::read_xlsx(temp, sheet =1, col_types=c("date", "text", rep("numeric",2)))

schoolkids_region<- subset(schoolkids, COUNTY %in% counties_in_hospital_region) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)

schoolkids_oakridge<- subset(schoolkids, COUNTY %in% c("Anderson", "Roane")) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)

schoolkids_knox<- subset(schoolkids, COUNTY %in% c("Knox")) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)

schoolkids_region$Region <- "East TN"
schoolkids_knox$Region <- "Knox County"
schoolkids_oakridge$Region <- "Anderson + Roane"
schoolkids_daily <- rbind(schoolkids_knox, schoolkids_oakridge, schoolkids_region)





## ----plotsA, echo=FALSE, message=FALSE, warning=FALSE-------------------------

daily_focal_no_ut <- daily_focal[!grepl("UTK", daily_focal$Region), ]
local_new <- ggplot(daily_focal_no_ut[!is.na(daily_focal_no_ut$NEW_CASES),], aes(x=DATE, y=NEW_CASES, group=Region)) + geom_ma(aes(colour=Region, linetype="a"), n=7) + guides(linetype = FALSE)  + ylab("Number of new cases in area each day") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
print(local_new)

#local_active <- ggplot(daily_focal[!is.na(daily_focal$TOTAL_ACTIVE),], aes(x=DATE, y=TOTAL_ACTIVE, group=Region)) +  geom_smooth(aes(colour=Region), se=FALSE) + geom_point(aes(colour=Region), size=0.5) + ylab("Number of active cases in area each day") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
#local_active <- ggplot(daily_focal[!is.na(daily_focal$TOTAL_ACTIVE),], aes(x=DATE, y=TOTAL_ACTIVE, group=Region)) +  geom_line(aes(colour=Region)) + geom_point(aes(colour=Region), size=0.5) + ylab("Number of active cases in area each day") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
#print(local_active)




## ----plotsA2, echo=FALSE, message=FALSE, warning=FALSE, eval=FALSE------------
## 
## 
## #local_new_100k <- ggplot(daily_focal[!is.na(daily_focal$New_cases_per_100k),], aes(x=DATE, y=New_cases_per_100k, group=Region)) + geom_smooth(aes(colour=Region), se=FALSE) + geom_point(aes(colour=Region), size=0.5)  + ylab("Number of new cases in area each day per 100,000 people") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
## local_new_100k <- ggplot(daily_focal_no_ut[!is.na(daily_focal_no_ut$New_cases_per_100k),], aes(x=DATE, y=New_cases_per_100k, group=Region)) + geom_ma(aes(colour=Region, linetype="a"), n=7) + guides(linetype = FALSE)  + ylab("Number of new cases in area each day per 100,000 people") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
## print(local_new_100k)
## 
## #local_active_100k <- ggplot(daily_focal[!is.na(daily_focal$Active_cases_per_100k),], aes(x=DATE, y=Active_cases_per_100k, group=Region)) +  geom_smooth(aes(colour=Region), se=FALSE) + geom_point(aes(colour=Region), size=0.5) + ylab("Number of active cases in area each day per 100,000 residents") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
## #local_active_100k <- ggplot(daily_focal[!is.na(daily_focal$Active_cases_per_100k),], aes(x=DATE, y=Active_cases_per_100k, group=Region)) +  geom_line(aes(colour=Region)) + geom_point(aes(colour=Region), size=0.5) + ylab("Number of active cases in area each day per 100,000 residents") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
## #print(local_active_100k)
## 
## 


## ----newcasesharvard, echo=FALSE, message=FALSE, warning=FALSE, eval=TRUE-----
#daily_focal_away_from_zero <- subset(daily_focal_no_ut, New_cases_per_100k>0.5) #to keep the plot from blowing up towards zero
local_new_100k_harvard <- ggplot(daily_focal_no_ut[!is.na(daily_focal_no_ut$New_cases_per_100k),], aes(x=DATE, y=New_cases_per_100k, group=Region)) + 
geom_rect(mapping=aes(xmin=min(daily_focal_no_ut$DATE), xmax=max(daily_focal_no_ut$DATE), ymin=0, ymax=1), fill="darkolivegreen1") +
  geom_rect(mapping=aes(xmin=min(daily_focal_no_ut$DATE), xmax=max(daily_focal_no_ut$DATE), ymin=1, ymax=10), fill="khaki1") +
  geom_rect(mapping=aes(xmin=min(daily_focal_no_ut$DATE), xmax=max(daily_focal_no_ut$DATE), ymin=10, ymax=25), fill="tan1") +
  geom_rect(mapping=aes(xmin=min(daily_focal_no_ut$DATE), xmax=max(daily_focal_no_ut$DATE), ymin=25, ymax=35), fill="indianred1") + 
geom_ma(aes(colour=Region, linetype="a"), n=7) + guides(linetype = FALSE) + ylab("Number of new cases in area each day per 100,000 people") + xlab("Date") + ylim(0,35) + scale_colour_viridis_d(end=0.8) 
# daily_focal_away_from_zero <- daily_focal_no_ut
# local_new_100k_log1p <- ggplot(daily_focal_away_from_zero[!is.na(daily_focal_away_from_zero$New_cases_per_100k),], aes(x=DATE, y=New_cases_per_100k, group=Region)) + 
# geom_rect(mapping=aes(xmin=min(daily_focal_away_from_zero$DATE), xmax=max(daily_focal_away_from_zero$DATE), ymin=0, ymax=1), fill="darkolivegreen1") +
#   geom_rect(mapping=aes(xmin=min(daily_focal_away_from_zero$DATE), xmax=max(daily_focal_away_from_zero$DATE), ymin=1, ymax=10), fill="khaki1") +
#   geom_rect(mapping=aes(xmin=min(daily_focal_away_from_zero$DATE), xmax=max(daily_focal_away_from_zero$DATE), ymin=10, ymax=25), fill="tan1") +
#   geom_rect(mapping=aes(xmin=min(daily_focal_away_from_zero$DATE), xmax=max(daily_focal_away_from_zero$DATE), ymin=25, ymax=40), fill="indianred1") + 
# geom_ma(aes(colour=Region, linetype="a"), n=7) + guides(linetype = FALSE) + ylab("Number of new cases in area each day per 100,000 people") + xlab("Date") + ylim(0,40) + scale_colour_viridis_d(end=0.8) 
#+ scale_y_continuous(trans = "log1p", breaks = c(1, 10, 25))
print(local_new_100k_harvard)


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


## ----plotsB, echo=FALSE, message=FALSE, warning=FALSE, eval=FALSE-------------
## 
## # A question is whether testing is adequate. The White House has said about 30 tests per 1000 people per month is adequate (this is also what <a href="https://projects.propublica.org/reopening-america/#notes">ProPublica</a> uses); others have argued that around 45 tests per 1000 people per month is better, though with variation depending on infection rate (see <a href="https://www.statnews.com/2020/04/27/coronavirus-many-states-short-of-testing-levels-needed-for-safe-reopening/">here</a>). These are the black lines on the plots below (calculated as tests per day, given Knox County's population of `r knox_pop`, and assuming 30 days per month).
## 
## 
## knox_testing <- ggplot(daily_knox[!is.na(daily_knox$NEW_TESTS),], aes(x=DATE, y=NEW_TESTS)) + geom_smooth() + geom_point() + ylab("Number of new tests in Knox each day") + xlab("Date") + ylim(0,NA)
## knox_testing <- knox_testing + geom_hline(yintercept=(knox_pop*45/1000)/30, col="black") + geom_hline(yintercept=(knox_pop*30/1000)/30, col="black")
## print(knox_testing)
## 
## 


## ----plotsB2, echo=FALSE, message=FALSE, warning=FALSE------------------------

proportional_testing <- ggplot(daily_focal_no_ut[!is.na(daily_focal_no_ut$Tests_per_100k),], aes(x=DATE, y=Tests_per_100k, group=Region)) + geom_ma(aes(colour=Region, linetype="a"), n=7) + guides(linetype = FALSE) + ylab("Number of new tests per 100,000 people each day") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
print(proportional_testing)




## ----plotsB3, echo=FALSE, message=FALSE, warning=FALSE------------------------

# daily_knox$NEW_PROPORTION_CONFIRMED <- 100*daily_knox$NEW_CONFIRMED/daily_knox$NEW_TESTS
#
# knox_proportion_pos <- ggplot(daily_knox[!is.na(daily_knox$NEW_PROPORTION_CONFIRMED),], aes(x=DATE, y=NEW_PROPORTION_CONFIRMED)) + geom_smooth() + geom_point() + ylab("Percentage of positive tests in Knox each day") + xlab("Date") + ylim(0,NA) + geom_hline(yintercept=10, col="black")
# print(knox_proportion_pos)


daily_focal_no_ut$NEW_PROPORTION_CONFIRMED <- 100*daily_focal_no_ut$NEW_CONFIRMED/daily_focal_no_ut$NEW_TESTS

focal_proportion_pos <- ggplot(daily_focal_no_ut[!is.na(daily_focal_no_ut$NEW_PROPORTION_CONFIRMED),], aes(x=DATE, y=NEW_PROPORTION_CONFIRMED, group=Region)) + geom_ma(aes(colour=Region, linetype="a"), n=7) + guides(linetype = FALSE) + ylab("Percentage of positive tests in focal areas each day (7 day avg)") + xlab("Date") + geom_hline(yintercept=5, col="black") + scale_colour_viridis_d(end=0.8)
print(focal_proportion_pos)

#focal_proportion_pos_log1p <- ggplot(daily_focal[!is.na(daily_focal$NEW_PROPORTION_CONFIRMED),], aes(x=DATE, y=NEW_PROPORTION_CONFIRMED, group=Region)) + geom_ma(aes(colour=Region, linetype="a"), n=7) + guides(linetype = FALSE) + ylab("Percentage of positive tests in focal areas each day") + xlab("Date") + geom_hline(yintercept=5, col="black") + scale_colour_viridis_d(end=0.8) + scale_y_continuous(trans = "log1p", breaks = c(1, 5, 10, 25, 50, 100))
#print(focal_proportion_pos_log1p)

# par(mfcol=c(1,2))
# plot(knox$date, knox$confirmed, type="l")
# confirmed.ts <- ts(data=knox$confirmed, start=knox$date[1], end=knox$date[length(knox$date)])
# confirmed_plot <- confirmed.ts %>%
#   auto.arima() %>%
#   forecast(h=20) %>%
#   autoplot()
# print(confirmed_plot)



## ----oakridgesevendays, echo=FALSE, message=FALSE, warning=FALSE--------------
daily_oakridge2 <- subset(daily_focal, Region=="Anderson + Roane")
oakridge_seven <- data.frame(DATE=daily_oakridge2$DATE, new_confirmed=zoo::rollsum(daily_oakridge2$NEW_CONFIRMED, k=7, align="right", fill=NA), new_tests=zoo::rollsum(daily_oakridge2$NEW_TESTS, k=7, align="right", fill=NA), New_cases_per_100k=zoo::rollmean(daily_oakridge2$New_cases_per_100k, k=7, align="right", fill=NA))
oakridge_seven$positivity = 100*oakridge_seven$new_confirmed/oakridge_seven$new_tests

plot_oakridge_seven <- ggplot(oakridge_seven[!is.na(oakridge_seven$positivity),], aes(x=DATE, y=positivity)) + geom_rect(mapping=aes(xmin=as.POSIXct("2020/07/29"), xmax=as.POSIXct(Sys.Date()+7), ymin=0, ymax=5), fill="pale green") + geom_rect(mapping=aes(xmin=as.POSIXct("2020/07/29"), xmax=as.POSIXct(Sys.Date()+7), ymin=5, ymax=0.5+max(oakridge_seven$positivity,na.rm=TRUE)), fill="indianred1") + geom_line() + geom_smooth(se=FALSE) + ylab("Percentage of positive tests over seven days ending with date") + xlab("Date") + ylim(0,NA) + geom_hline(yintercept=5, col="black", lty="dotted")

print(plot_oakridge_seven)


## ----harvardstandards, echo=FALSE, message=FALSE, warning=FALSE, fig.height=13----
maxval <- max(30, max(oakridge_seven$New_cases_per_100k, na.rm=TRUE))
harvard_oakridge <- ggplot(oakridge_seven[!is.na(oakridge_seven$New_cases_per_100k),], aes(x=DATE, y=New_cases_per_100k)) + ylab("Average new cases per day per 100K population") + xlab("Date") + ylim(0,NA) +
  geom_rect(mapping=aes(xmin=min(oakridge_seven$DATE), xmax=max(oakridge_seven$DATE), ymin=0, ymax=1), fill="darkolivegreen1") +
  geom_rect(mapping=aes(xmin=min(oakridge_seven$DATE), xmax=max(oakridge_seven$DATE), ymin=1, ymax=10), fill="khaki1") +
  geom_rect(mapping=aes(xmin=min(oakridge_seven$DATE), xmax=max(oakridge_seven$DATE), ymin=10, ymax=25), fill="tan1") +
  geom_rect(mapping=aes(xmin=min(oakridge_seven$DATE), xmax=max(oakridge_seven$DATE), ymin=25, ymax=maxval), fill="indianred1") +
 annotate("text", x = min(oakridge_seven$DATE), y=c(0.5, 5.5, 17.5, mean(c(25, maxval))), label = c("", "First reopening priority: PreK-5 and special ed preK-8\nSecond reopening priority: 6-8 and special ed 9-12\nThird reopening priority: 9-12 on hybrid schedule", "First reopening priority: PreK-5 and special ed preK-8\nSecond reopening priority: 6-8 and special ed 9-12\nOnline only: 9-12", "All learning remote for everyone"), hjust=0) +
 geom_line()
print(harvard_oakridge)



## ----studentinfections, echo=FALSE, message=FALSE, warning=FALSE--------------
try(student_covid_total <- ggplot(schoolkids_daily, aes(x=DATE, y=CASE_COUNT, group=Region)) + geom_line(aes(colour=Region)) +  ylab("Total number of students who have tested positive") + xlab("Date") + scale_colour_viridis_d(end=0.8))
try(print(student_covid_total))

try(student_covid_daily <- ggplot(schoolkids_daily, aes(x=DATE, y=NEW_CASES, group=Region)) +  geom_ma(aes(colour=Region, linetype="a")) +  guides(linetype = FALSE) + ylab("Number of students with new positive covid results daily, 7 day avg") + xlab("Date") + scale_colour_viridis_d(end=0.8))
try(print(student_covid_daily))


## ----andersonstandards, echo=FALSE, message=FALSE, warning=FALSE, include=FALSE, eval=FALSE----
## #<a href="https://www.acs.ac/">Anderson County</a> had a school plan based on the percentage of the community actively infected with covid. Their schools open later than Oak Ridge schools, but it might be a useful indicator for community thinking. Note that Anderson's metric is based on the *number* of people with active covid (those with positive tests who are not yet recovered or deceased) and so it may be sensitive to the number of tests in a way that the positivity rate is not; in the areas between phases they use other information to make a determination, such as number of absences of students and teachers.
## 
## daily_focal$Active_cases_percent <- 100*daily_focal$Active_cases_per_100k/100000
## daily_focal<-daily_focal[!is.na(daily_focal$Active_cases_percent),]
## local_active_percent <- ggplot(daily_focal[!is.na(daily_focal$Active_cases_percent),], aes(x=DATE, y=Active_cases_percent, group=Region)) +
## geom_rect(mapping=aes(xmin=min(daily_focal$DATE), xmax=max(daily_focal$DATE), ymin=-0.2, ymax=0), color="light blue", fill=NA) +
## geom_rect(mapping=aes(xmin=min(daily_focal$DATE), xmax=max(daily_focal$DATE), ymin=0, ymax=.1), color="pale green", fill=NA) +
## geom_rect(mapping=aes(xmin=min(daily_focal$DATE), xmax=max(daily_focal$DATE), ymin=.1, ymax=.4), color="olivedrab3", fill=NA) +
## geom_rect(mapping=aes(xmin=min(daily_focal$DATE), xmax=max(daily_focal$DATE), ymin=.4, ymax=.5), color="olivedrab1", fill=NA) +
## geom_rect(mapping=aes(xmin=min(daily_focal$DATE), xmax=max(daily_focal$DATE), ymin=.5, ymax=.7), color="yellow", fill=NA) +
## geom_rect(mapping=aes(xmin=min(daily_focal$DATE), xmax=max(daily_focal$DATE), ymin=.7, ymax=.8), color="orange", fill=NA) +
## geom_rect(mapping=aes(xmin=min(daily_focal$DATE), xmax=max(daily_focal$DATE), ymin=.8, ymax=1), color="red", fill=NA) +
## annotate("text", x = min(daily_focal$DATE), y=c(-0.1, 0.25, 0.6, 0.9), label = c("Phase 0, all normal", "Phase 1, schools open and virtual option", "Phase 2, blended learning plan", "Phase 3, all schools closed, all students virtual"), hjust=0) +
##  geom_line(aes(colour=Region)) + ylab("Percent of people with active covid infections") + xlab("Date") + ylim(-0.2,1) + scale_colour_viridis_d(end=0.9, option="A")
## print(local_active_percent)


## ----hospitalcapacitydata, echo=FALSE, message=FALSE, warning=FALSE-----------

hospital_knox_files <- list.files(path="/Users/bomeara/Dropbox/KnoxCovid", pattern="*covid_bed_capacity.csv", full.names=TRUE)
hospital_knox <- data.frame()
for (i in seq_along(hospital_knox_files)) {
  local_beds <- NA
  try(local_beds <- read.csv(hospital_knox_files[i]), silent=TRUE)
  if(!is.na(local_beds)) {
	local_beds$East.Region.Hospitals <- gsub('All Hospital Beds*', 'All Hospital Beds *', gsub('All Hospital Beds *', 'All Hospital Beds', local_beds$East.Region.Hospitals, fixed=TRUE), fixed=TRUE)
	local_beds$Total.Capacity <- as.numeric(gsub(",",'', local_beds$Total.Capacity))
	local_beds$Current.Census <- as.numeric(gsub(",",'', local_beds$Current.Census))
	local_beds$Current.Utilization <- as.numeric(gsub('%','', local_beds$Current.Utilization))
	local_beds$Available.Capacity <- as.numeric(gsub('%','', local_beds$Available.Capacity))
	local_beds$Date <- anytime::anytime(stringr::str_extract(hospital_knox_files[i], "\\d+_\\d+_\\d+_\\d+_\\d+_\\d+"))
	if (i==1) {
		hospital_knox <- local_beds
	} else {
		hospital_knox <- rbind(hospital_knox, local_beds)
	}
  }
}
hospital_knox <- subset(hospital_knox, East.Region.Hospitals != "Adult Floor Beds/Non-ICU")
hospital_knox <- hospital_knox[which(nchar(hospital_knox$East.Region.Hospitals)>0),]
hospital_knox$Current.Utilization[which(hospital_knox$Current.Utilization>100)] <- hospital_knox$Current.Utilization[which(hospital_knox$Current.Utilization>100)]/100 #to fix two days of data where Knox County was multiplying these by 100, getting 7935% utilization

# covidServer <- get.rds("https://knxhx.richdataservices.com/rds")
# catalog <- getCatalog(covidServer, "kchd")
# products <- getDataProducts(catalog)
# dataProduct <- getDataProduct(catalog, "us_tn_kchd_capacity")
# { sink("/dev/null"); hospital_resources <- rds.select(dataProduct, autoPage =  TRUE)@records; sink(); }
# hospital_resources$label = NA
# hospital_resources$label[hospital_resources$resource_type==0] <- "All beds"
# hospital_resources$label[hospital_resources$resource_type==1] <- "ICU beds"
# hospital_resources$label[hospital_resources$resource_type==2] <- "Ventilators"
# hospital_resources$cnt_available <- as.numeric(as.character(hospital_resources$cnt_available))
# hospital_resources$cnt_capacity <- as.numeric(as.character(hospital_resources$cnt_capacity))
# hospital_resources$pct_used <- as.numeric(as.character(hospital_resources$pct_used))
# hospital_resources$pct_available <- as.numeric(as.character(hospital_resources$pct_available))

last_hospital_update <- hospital_knox$Date[1]
resources <- unique(hospital_knox$East.Region.Hospitals)
for (resource_index in sequence(length(resources))) {
  hospital_resources_subset <- subset(hospital_knox, East.Region.Hospitals==resources[resource_index])
  cnt_used_previous = hospital_resources_subset$Current.Census[1]
  for(row_index in sequence(nrow(hospital_resources_subset))) {
	  if(row_index>1) {
		if(hospital_resources_subset$Current.Census[row_index] != cnt_used_previous) {
		cnt_used_previous <- hospital_resources_subset$Current.Census[row_index]
		last_hospital_update <- max(last_hospital_update, hospital_resources_subset$Date[row_index])
		}
	  }
  }
}


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
try(hosp_plot <- ggplot(hospital_knox, aes(x=Date, y=Current.Utilization, group=East.Region.Hospitals)) + geom_line(aes(colour=East.Region.Hospitals)) +  ylab("Percent Utilization in East Tennessee Region") + xlab("Date") + ylim(0,100) + scale_colour_viridis_d(end=0.8) + geom_hline(yintercept=100, col="red"))
try(print(hosp_plot))



## ----plotsD, echo=FALSE, message=FALSE, warning=FALSE-------------------------


new_hospitalization <- ggplot(daily_focal[!is.na(daily_focal$NEW_HOSPITALIZED),], aes(x=DATE, y=NEW_HOSPITALIZED, group=Region)) + geom_ma(aes(colour=Region, linetype="a"), n=7) + guides(linetype = FALSE) + ylab("Number of new covid hospitalizations each day (7 day avg)") + xlab("Date") + ylim(0,NA)  + scale_colour_viridis_d(end=0.8) + geom_vline(xintercept=as.POSIXct(last_hospital_update), col="black", linetype="dotted") 
print(new_hospitalization)

# tn_daily_aggregate <- daily %>% group_by(DATE) %>% summarise(new_hosp = sum(NEW_HOSPITALIZED))
#
#
# tn_new_hospitalization <- ggplot(tn_daily_aggregate[!is.na(tn_daily_aggregate$new_hosp),], aes(x=DATE, y=new_hosp)) + geom_smooth() + geom_point() + ylab("Number of new covid hospitalizations in TN each day") + xlab("Date") + ylim(0,NA)
# print(tn_new_hospitalization)

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



## ----hospitalcapacityplotvsCA, echo=FALSE, message=FALSE, warning=FALSE-------
hospital_knox_ICU <- subset(hospital_knox, East.Region.Hospitals=="ICU Beds")
try(hosp_plot_CA <- ggplot(hospital_knox_ICU, aes(x=Date, y=Current.Utilization)) + geom_line() +  ylab("Percent of ICU beds filled") + xlab("Date") + scale_colour_viridis_d(end=0.8) + geom_hline(yintercept=85, col="dodgerblue") + geom_hline(yintercept=100, col="red"))
try(print(hosp_plot_CA))



## ----hhshospitalization, echo=FALSE, message=FALSE, warning=FALSE-------------
hhs_sources <- jsonlite::fromJSON("https://healthdata.gov/data.json?page=0")
capacity_by_facility_number <- grep("COVID-19 Reported Patient Impact and Hospital Capacity by Facility Data Dictionary", hhs_sources[[6]][[5]])
capacity_by_facility_url <- hhs_sources[[6]][[6]][capacity_by_facility_number][[1]]$downloadURL
temp = tempfile(fileext = ".csv")

	utils::download.file(capacity_by_facility_url, temp, method="libcurl")
hhs_capacity <- read.csv(file=temp)
hhs_capacity_tn <- subset(hhs_capacity, state=="TN")
# for(i in sequence(nrow(hhs_capacity_tn))) {
# 	for (j in sequence(ncol(hhs_capacity_tn))) {
# 		if(!is.na(hhs_capacity_tn[i,j])) {
# 			if(hhs_capacity_tn[i,j]==-999999) {
# 				hhs_capacity_tn[i,j] <- NA
# 			}
# 		}
# 	}
# }

# field info from https://healthdata.gov/covid-19-reported-patient-impact-and-hospital-capacity-facility-data-dictionary

# fields: 
# collection_week - This date indicates the start of the period of reporting (the starting Friday).
# state - [FAQ - 1. d)] The two digit state/territory code for the hospital.
# hospital_name - [FAQ - 1. a)] The name of the facility reporting.
# city - The city of the facility reporting.
# zip - The 5-digit zip code of the facility reporting.

# all_adult_hospital_inpatient_bed_occupied_7_day_avg - [FAQ - 4. b)] Average of total number of staffed inpatient adult beds that are occupied reported during the 7-day period.
# total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg - [FAQ - 9. a)] Average number of patients currently hospitalized in an adult inpatient bed who have laboratory-confirmed or suspected COVID19, including those in observation beds reported during the 7-day period.
# all_adult_hospital_inpatient_beds_7_day_avg - [FAQ - 3. b)] Average of total number of staffed inpatient adult beds in the hospital including all overflow and active surge/expansion beds used for inpatients (including all designated ICU beds) reported during the 7-day period.
# 
# total_staffed_adult_icu_beds_7_day_avg - [FAQ - 5. b)] Average of total number of staffed adult ICU beds reported in the 7-day period.
# staffed_adult_icu_bed_occupancy_7_day_avg - [FAQ - 6. b)] Average of total number of staffed inpatient adult ICU beds that are occupied reported in the 7-day period.
# staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg - [FAQ - 12. a)] Average number of patients currently hospitalized in a designated adult ICU bed who have suspected or laboratory-confirmed COVID-19 reported in the 7-day period.

# note that -999999 is apparently used for missing data

hhs_capacity_tn$percentage_adult_hospital_inpatient_bed_occupied_covid_confirmed_or_suspected_7_day_avg_of_all_occupied <- 100 * hhs_capacity_tn$total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg / hhs_capacity_tn$all_adult_hospital_inpatient_bed_occupied_7_day_avg

hhs_capacity_tn$percentage_adult_hospital_inpatient_bed_occupied_of_all_inpatient_beds <- 100 * hhs_capacity_tn$all_adult_hospital_inpatient_bed_occupied_7_day_avg / hhs_capacity_tn$all_adult_hospital_inpatient_beds_7_day_avg

hhs_capacity_tn$percentage_adult_hospital_inpatient_bed_unoccupied_of_all_inpatient_beds <- 100 - hhs_capacity_tn$percentage_adult_hospital_inpatient_bed_occupied_of_all_inpatient_beds

hhs_capacity_tn$number_unoccupied_adult_hospital_inpatient_beds <- hhs_capacity_tn$all_adult_hospital_inpatient_beds_7_day_avg - hhs_capacity_tn$all_adult_hospital_inpatient_bed_occupied_7_day_avg



hhs_capacity_tn$percentage_adult_hospital_ICU_bed_occupied_covid_confirmed_or_suspected_7_day_avg_of_all_ICU_occupied <- 100 * hhs_capacity_tn$staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg / hhs_capacity_tn$staffed_adult_icu_bed_occupancy_7_day_avg

hhs_capacity_tn$percentage_adult_hospital_inpatient_ICU_bed_occupied_of_all_inpatient_ICU_beds <- 100 * hhs_capacity_tn$staffed_adult_icu_bed_occupancy_7_day_avg / hhs_capacity_tn$total_staffed_adult_icu_beds_7_day_avg

hhs_capacity_tn$percentage_adult_hospital_inpatient_ICU_bed_unoccupied_of_all_inpatient_ICU_beds <- 100 - hhs_capacity_tn$percentage_adult_hospital_inpatient_ICU_bed_occupied_of_all_inpatient_ICU_beds

hhs_capacity_tn$number_unoccupied_adult_hospital_ICU_beds <- hhs_capacity_tn$total_staffed_adult_icu_beds_7_day_avg - hhs_capacity_tn$staffed_adult_icu_bed_occupancy_7_day_avg




focal_cities <- toupper(c("Oak Ridge", "Knoxville", "Lenoir City", "Maryville", "Sweetwater", "Harriman", "Powell", "Jefferson City", "Athens", "Morristown", "Sevierville", "Tazewell", "La Follette", "Jellico", "Sneedville", "Oneida"))
hhs_capacity_tn_focal <- hhs_capacity_tn[hhs_capacity_tn$city%in%focal_cities,]

hhs_capacity_tn_focal <- subset(hhs_capacity_tn_focal, 
	!is.na(all_adult_hospital_inpatient_bed_occupied_7_day_avg) & 
	!is.na(total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg) & 
	!is.na(all_adult_hospital_inpatient_beds_7_day_avg) &
	!is.na(total_staffed_adult_icu_beds_7_day_avg) &
	!is.na(staffed_adult_icu_bed_occupancy_7_day_avg) &
	!is.na(staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg) & 
	!is.na(percentage_adult_hospital_inpatient_bed_occupied_covid_confirmed_or_suspected_7_day_avg_of_all_occupied) &
	!is.na(percentage_adult_hospital_inpatient_bed_occupied_of_all_inpatient_beds) &
	!is.na(percentage_adult_hospital_ICU_bed_occupied_covid_confirmed_or_suspected_7_day_avg_of_all_ICU_occupied) &
	!is.na(percentage_adult_hospital_inpatient_ICU_bed_occupied_of_all_inpatient_ICU_beds)
)


hhs_capacity_tn_focal <- subset(hhs_capacity_tn_focal, 
	(all_adult_hospital_inpatient_bed_occupied_7_day_avg >= 0) & 
	(total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg >= 0) & 
	(all_adult_hospital_inpatient_beds_7_day_avg >= 0) &
	(total_staffed_adult_icu_beds_7_day_avg >= 0) &
	(staffed_adult_icu_bed_occupancy_7_day_avg >= 0) &
	(staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg >= 0)
)

#hhs_capacity_tn_focal <- subset(hhs_capacity_tn_focal, all_adult_hospital_inpatient_beds_7_day_avg>=100)

few_update_hospitals <- names(which(table(hhs_capacity_tn_focal$hospital_name)<=2))

if(length(few_update_hospitals)>0) {
	hhs_capacity_tn_focal <- subset(hhs_capacity_tn_focal, !(hospital_name %in% few_update_hospitals)) #deleting the tiny hospitals that don't update
}

hhs_capacity_tn_focal$hospital_name <- gsub("TENNOVA HEALTHCARE", "TENNOVA", hhs_capacity_tn_focal$hospital_name )
hhs_capacity_tn_focal$hospital_name <- stringr::str_to_title(hhs_capacity_tn_focal$hospital_name)
hhs_capacity_tn_focal$hospital_name[grepl("University Of Tn", hhs_capacity_tn_focal$hospital_name, ignore.case=TRUE)] <- "University of TN Medical Center"




hhs_capacity_tn_focal$DATE <- as.Date(hhs_capacity_tn_focal$collection_week)

hhs_capacity_tn_focal_cities <- hhs_capacity_tn_focal

hhs_capacity_tn_focal_cities <- hhs_capacity_tn_focal %>% group_by(city, DATE) %>% summarize_at(vars(number_unoccupied_adult_hospital_inpatient_beds, number_unoccupied_adult_hospital_ICU_beds), list(sum = ~sum(., na.rm=TRUE)))

combinations <- expand.grid(DATE = unique(hhs_capacity_tn_focal_cities$DATE), city = unique(hhs_capacity_tn_focal_cities$city))

hhs_capacity_tn_focal_cities <- full_join(hhs_capacity_tn_focal_cities, combinations, by = c("DATE" = "DATE", "city" = "city")) %>% mutate(number_unoccupied_adult_hospital_inpatient_beds_sum = ifelse(is.na(number_unoccupied_adult_hospital_inpatient_beds_sum), 0, number_unoccupied_adult_hospital_inpatient_beds_sum)) %>% mutate(number_unoccupied_adult_hospital_ICU_beds_sum = ifelse(is.na(number_unoccupied_adult_hospital_ICU_beds_sum), 0, number_unoccupied_adult_hospital_ICU_beds_sum))

hhs_capacity_tn_focal_cities$city <- stringr::str_to_title(hhs_capacity_tn_focal_cities$city)

hhs_capacity_tn_focal_latest <- subset(hhs_capacity_tn_focal, DATE==max(DATE))
hhs_capacity_tn_focal_latest <- hhs_capacity_tn_focal_latest[order(hhs_capacity_tn_focal_latest$all_adult_hospital_inpatient_beds_7_day_avg, decreasing=TRUE),]
hhs_capacity_tn_focal_latest_pretty <- hhs_capacity_tn_focal_latest[,c(
	"hospital_name", 
	"city", 
	"all_adult_hospital_inpatient_beds_7_day_avg", 
	"number_unoccupied_adult_hospital_inpatient_beds", 
	"percentage_adult_hospital_inpatient_bed_unoccupied_of_all_inpatient_beds", 
	"total_staffed_adult_icu_beds_7_day_avg", 
	"number_unoccupied_adult_hospital_ICU_beds", 
	"percentage_adult_hospital_inpatient_ICU_bed_unoccupied_of_all_inpatient_ICU_beds"
)]
colnames(hhs_capacity_tn_focal_latest_pretty) <- c(
	"Hospital", 
	"City", 
	"Adult beds total", 
	"Adult beds number avail", 
	"Adult beds % avail", 
	"Adult ICU total", 
	"Adult ICU number avail", 
	"Adult ICU % avail"
)

for (i in 3:ncol(hhs_capacity_tn_focal_latest_pretty)) {
	hhs_capacity_tn_focal_latest_pretty[,i]<- round(hhs_capacity_tn_focal_latest_pretty[,i])
}

hhs_capacity_tn_focal_latest_pretty$City <- stringr::str_to_title(hhs_capacity_tn_focal_latest_pretty$City)
rownames(hhs_capacity_tn_focal_latest_pretty) <- NULL


## ----hhstable, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE----------
knitr::kable(hhs_capacity_tn_focal_latest_pretty)


## ----hhsfirstplot, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE------
hhs_plot2b <- ggplot(hhs_capacity_tn_focal_cities, aes(x=DATE, y=number_unoccupied_adult_hospital_inpatient_beds_sum, group=city, fill=city)) + geom_area() + ylab("Average total number of unoccupied adult inpatient beds\n(Higher is better)") + xlab("Start of collection week") + theme(legend.position="bottom") + theme(legend.text=element_text(size=8))
print(hhs_plot2b)

hhs_plot4b <- ggplot(hhs_capacity_tn_focal_cities, aes(x=DATE, y=number_unoccupied_adult_hospital_ICU_beds_sum, group=city, fill=city)) + geom_area() + ylab("Average total number of unoccupied adult ICU beds\n(Higher is better)") + xlab("Start of collection week") + theme(legend.position="bottom") + theme(legend.text=element_text(size=8))
print(hhs_plot4b)


## ----hhspart2, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE, fig.height=7----

hhs_plot1 <- ggplot(hhs_capacity_tn_focal, aes(x=DATE, y=percentage_adult_hospital_inpatient_bed_occupied_of_all_inpatient_beds, group=hospital_name)) + geom_line() + ylab("Percentage of all adult inpatient beds occupied") + xlab("Start of collection week") + facet_wrap(facets=vars(hospital_name), ncol=2) + theme(plot.title = element_text(size=2)) + ylim(0,100)
print(hhs_plot1)



hhs_plot3 <- ggplot(hhs_capacity_tn_focal, aes(x=DATE, y=percentage_adult_hospital_inpatient_ICU_bed_occupied_of_all_inpatient_ICU_beds, group=hospital_name)) + geom_line() + ylab("Percentage of all adult ICU beds occupied") + xlab("Start of collection week") + facet_wrap(facets=vars(hospital_name), ncol=2)  + ylim(0,100)
print(hhs_plot3)





## ----hhspart3, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE, fig.height=7----

hhs_plot2 <- ggplot(hhs_capacity_tn_focal, aes(x=DATE, y=percentage_adult_hospital_inpatient_bed_occupied_covid_confirmed_or_suspected_7_day_avg_of_all_occupied, group=hospital_name)) + geom_line() + ylab("Percentage of adult inpatient beds with confirmed and suspected covid") + xlab("Start of collection week") + facet_wrap(facets=vars(hospital_name), ncol=2)  + ylim(0,100)
print(hhs_plot2)


hhs_plot4 <- ggplot(hhs_capacity_tn_focal, aes(x=DATE, y=percentage_adult_hospital_inpatient_ICU_bed_occupied_of_all_inpatient_ICU_beds, group=hospital_name)) + geom_line() + ylab("Percentage of adult ICU beds with confirmed and suspected covid") + xlab("Start of collection week") + facet_wrap(facets=vars(hospital_name), ncol=2)  + ylim(0,100)
print(hhs_plot4)


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

ageplot_knox_daily <- ggplot(subset(age_county, COUNTY=="Knox"), aes(x=DATE, y=PercentDaily, group=AGE_GROUP)) + geom_ma(aes(colour=AGE_GROUP, linetype="a"), n=7) + guides(linetype = FALSE) + ylab("Daily percentage of cases by age group in Knox County (7 day avg)") + xlab("Date") + scale_colour_brewer(type="qual", palette="Dark2")
ageplot_knox_cumulative <- ggplot(subset(age_county, COUNTY=="Knox"), aes(x=DATE, y=PercentCumulative, group=AGE_GROUP)) + geom_line(aes(colour=AGE_GROUP)) + ylab("Cumulative percentage of cases by age group in Knox County") + xlab("Date") + scale_colour_brewer(type="qual", palette="Dark2")
#ageplot_knox_both <- ggarrange(ageplot_knox_daily, #ageplot_knox_cumulative, labels=c("Daily", "Cumulative"), ncol=2, nrow=1)
print(ageplot_knox_daily)
print(ageplot_knox_cumulative)



## ----utactive, echo=FALSE, message=FALSE, warning=FALSE, eval=FALSE-----------
## # cached downloads of https://veoci.com/veoci/p/form/4jmds5x4jj4j#tab=entryForm
## webfiles <- list.files(path="/Users/bomeara/Dropbox/UTKCovid", pattern="*html", full.names =TRUE)
## utk.cases <- data.frame()
## for(i in seq_along(webfiles)) {
##   raw <- paste0(readLines(webfiles[i]),collapse=" ")
##   raw <- gsub('\\x3c', '', raw, fixed=TRUE)
##   raw <- gsub('\\x3d', '', raw, fixed=TRUE)
##   raw <- gsub('\\x3e', '', raw, fixed=TRUE)
##   raw <- gsub('/span/tdtd style\"width: \\d+\\.*\\d*%;\"span style\"font-size: 18px;\"', '', raw, fixed=FALSE)
##   raw <- gsub('/span/tdtd style\"width: \\d+\\.*\\d*%; text-align: left;\"span style\"font-size: 18px;\"', '', raw, fixed=FALSE)
##   students <- as.numeric(gsub("Students", "", stringr::str_extract(raw, "Students\\d+")))
##     faculty <- as.numeric(gsub("Faculty", "", stringr::str_extract(raw, "Faculty\\d+")))
##     staff <- as.numeric(gsub("Staff", "", stringr::str_extract(raw, "Staff\\d+")))
##   actual_time <- anytime::anytime(stringr::str_extract(webfiles[i], "\\d+_\\d+_\\d+_\\d+_\\d+_\\d+"))
##   result <- data.frame(date=rep(actual_time, 3), count=c(students, faculty, staff), group=c("students", "faculty", "staff"))
##   if(i==1) {
##     utk.cases <- result
##   } else {
##     utk.cases <- rbind(utk.cases, result)
##   }
## }
## utk.cases$group <- as.factor(utk.cases$group)
## utk_plot <- ggplot(utk.cases[!is.na(utk.cases$count),], aes(x=date, y=count, group=group)) + geom_line(aes(colour=group)) + ylab("Number of active cases at UTK") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
## print(utk_plot)


## ----salivadata, echo=FALSE, message=FALSE, warning=FALSE---------------------
saliva_data <- read.csv(file="7 LIVE_saliva_test_data_Page 1_Table.csv", stringsAsFactors=FALSE)
saliva_data$Active_cases_per_100k = 100000*saliva_data$Positive.diagnostic.tests./saliva_data$Samples
saliva_data$Active_cases_per_30k = 30000*saliva_data$Positive.diagnostic.tests./saliva_data$Samples
saliva_data$New_cases_per_100k = saliva_data$Active_cases_per_100k / 14
#saliva_data$DATE <- as.Date(saliva_data$Week,"%m/%d/%y")
saliva_data$DATE <- as.Date(saliva_data$Week,"%b %d, %y")
#saliva_data$DATE <- as.Date(saliva_data$Week,"%d-%m-%y")

saliva_data$New_cases_per_100k_lower <- 100000*binom::binom.confint(saliva_data$Positive.diagnostic.tests., saliva_data$Samples, method="exact")$lower/14
saliva_data$New_cases_per_100k_upper<- 100000*binom::binom.confint(saliva_data$Positive.diagnostic.tests., saliva_data$Samples, method="exact")$upper/14



## ----plotstestingut, echo=FALSE, message=FALSE, warning=FALSE-----------------

daily_utk <- subset(daily_focal, Region=="UTK Student Testing")
daily_utk$DATE <- as.Date(daily_utk$DATE)
utk_official_testing <- read.csv(file="8 LIVE_SHC_test_data_Page 1_Table.csv", stringsAsFactors=FALSE)
utk_official_testing$DATE <- as.Date(utk_official_testing$Week,"%b %d, %y")
utk_official_testing$NEW_TESTS <- utk_official_testing$Total/7
utk_official_testing$DailyNegative <- utk_official_testing$Negative.tests/7
utk_official_testing$NEW_CONFIRMED <- utk_official_testing$Positive.tests/7
real_rows <- nrow(utk_official_testing)
for (i in 1:6) {
	next_day <- utk_official_testing[1:real_rows,]
	next_day$DATE <- next_day$DATE+i
	utk_official_testing <- rbind(utk_official_testing, next_day)
}

daily_utk$Data_source <- "UTK Zukowski release"
utk_official_testing$Data_source <- "UTK weekly release"
daily_utk <- plyr::rbind.fill(daily_utk, utk_official_testing)
daily_utk <- daily_utk[order(daily_utk$DATE), ]


ut_testing_plot <- ggplot(daily_utk[!is.na(daily_utk$NEW_TESTS),], aes(x=DATE, y=NEW_TESTS, group=Data_source)) + geom_line(aes(colour=Data_source)) + guides(linetype = FALSE) + ylab("Actual number of new tests in student health center each day") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8) + geom_vline(xintercept=as.Date("2020-09-15"), color="black", lty="dotted") + scale_x_date(limits=as.Date(range(c(daily_utk$DATE[!is.na(daily_utk$TOTAL_TESTS)], as.POSIXct(Sys.Date()))))) 
print(ut_testing_plot)



## ----plotsutkproportion, echo=FALSE, message=FALSE, warning=FALSE-------------

daily_utk$NEW_PROPORTION_CONFIRMED <- 100*daily_utk$NEW_CONFIRMED/daily_utk$NEW_TESTS


#focal_proportion_pos_utk <- ggplot(daily_utk[!is.na(daily_utk$NEW_PROPORTION_CONFIRMED),], aes(x=DATE, y=NEW_PROPORTION_CONFIRMED, group=Region)) + geom_ma(aes(colour=Region, linetype="a"), n=7) + guides(linetype = FALSE) + ylab("Percentage of positive tests in student center (7 day avg)") + xlab("Date") + geom_hline(yintercept=5, col="black") + scale_colour_viridis_d(end=0.8) + ylim(0,NA) + scale_x_date(limits=as.Date(range(c(daily_utk$DATE[!is.na(daily_utk$TOTAL_TESTS)], as.POSIXct(Sys.Date())))))
focal_proportion_pos_utk <- ggplot(daily_utk[!is.na(daily_utk$NEW_PROPORTION_CONFIRMED),], aes(x=DATE, y=NEW_PROPORTION_CONFIRMED, group=Region)) + geom_line(aes(colour=Data_source)) + guides(linetype = FALSE) + ylab("Percentage of positive tests in student center") + xlab("Date") + geom_hline(yintercept=5, col="black") + scale_colour_viridis_d(end=0.8) + ylim(0,NA) + scale_x_date(limits=as.Date(range(c(daily_utk$DATE[!is.na(daily_utk$TOTAL_TESTS)], as.POSIXct(Sys.Date())))))
print(focal_proportion_pos_utk)


## ----plotsaliva100k, echo=FALSE, message=FALSE, warning=FALSE-----------------
saliva_plot <- ggplot(saliva_data, aes(x=DATE, y=New_cases_per_100k)) + 
geom_rect(mapping=aes(xmin=min(DATE), xmax=Sys.Date(), ymin=0, ymax=1), fill="darkolivegreen1") +
  geom_rect(mapping=aes(xmin=min(DATE), xmax=Sys.Date(), ymin=1, ymax=10), fill="khaki1") +
  geom_rect(mapping=aes(xmin=min(DATE), xmax=Sys.Date(), ymin=10, ymax=25), fill="tan1") +
  geom_rect(mapping=aes(xmin=min(DATE), xmax=Sys.Date(), ymin=25, ymax=max(New_cases_per_100k_upper)), fill="indianred1") + 
geom_point() + ylab("Est. daily new cases 100,000 people based on UTK saliva samples") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8) + geom_errorbar(aes(ymin=New_cases_per_100k_lower, ymax=New_cases_per_100k_upper), width=0.1) 
print(saliva_plot)


## ----plotutactiveproportion, echo=FALSE, message=FALSE, warning=FALSE---------
utk_active_proportion_df <- rbind(
	data.frame(DATE=as.Date(utk_reported_zukowski$DATE), Active_percentage=100*utk_reported_zukowski$CASES_ACTIVE/30000, Assumption="Every active case knows and reports"),
	data.frame(DATE=as.Date(saliva_data$DATE), Active_percentage=100*saliva_data$Positive.diagnostic.tests. / saliva_data$Samples, Assumption="Student saliva testing is representative")
)
utk_active_proportion_plot <- ggplot(utk_active_proportion_df, aes(x=DATE, y=Active_percentage, group=Assumption)) + geom_line(aes(colour=Assumption)) + scale_colour_viridis_d(end=0.8) 
print(utk_active_proportion_plot)


saliva_active <- round(saliva_data$Active_cases_per_30k[nrow(saliva_data)])
reported_active <- round(utk_reported_zukowski$CASES_ACTIVE[utk_reported_zukowski$DATE %in% as.character(max(saliva_data$DATE))])
reported_isolation_total <- round(utk_reported_zukowski$SELF_ISOLATED_TOTAL[utk_reported_zukowski$DATE %in% as.character(max(saliva_data$DATE))])

max_spreader_proportion <- (saliva_active-reported_active)/30000
min_spreader_proportion <- (max(0,saliva_active-reported_isolation_total))/30000
group_sizes <- seq(from=1, to=100, by=1)
avoidance_df <- rbind(
	data.frame(Group_size = group_sizes, Percentage_infected_groups=100*(1-dbinom(0, size=group_sizes, prob=min_spreader_proportion)), Assumption="Everyone self-isolating has covid"),
	data.frame(Group_size = group_sizes, Percentage_infected_groups=100*(1-dbinom(0, size=group_sizes, prob=max_spreader_proportion)), Assumption="Minimal self isolation of infected individuals")
)


## ----plotencounterprob, echo=FALSE, message=FALSE, warning=FALSE--------------

utk_active_spreader_plot <- ggplot(avoidance_df, aes(x=Group_size, y=Percentage_infected_groups, group=Assumption)) + geom_line(aes(colour=Assumption)) + scale_colour_viridis_d(end=0.8)
print(utk_active_spreader_plot)



