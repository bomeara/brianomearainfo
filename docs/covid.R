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
library(gganimate)
library(ggrepel)
library(ggpubr)
options(timeout=600) # let things download for at least ten minutes
options(download.file.method = "libcurl")


#gmr <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"


counties_in_hospital_region <- c("Knox", "Anderson", "Roane", "Scott", "Blount", "Claiborne", "Jefferson", "Campbell", "Sevier", "Loudon", "Hamblen", "Cocke", "Monroe", "McMinn")

us <- COVID19::covid19(country="US", level=3, verbose=FALSE)
tn <- subset(us, administrative_area_level_2=="Tennessee")
knox <- subset(us, administrative_area_level_3=="Knox" & administrative_area_level_2=="Tennessee")
oakridge <- subset(us, administrative_area_level_3 %in% c("Anderson") & administrative_area_level_2=="Tennessee")
region <-  subset(us, administrative_area_level_3 %in% counties_in_hospital_region & administrative_area_level_2=="Tennessee")
knox$percentconfirmed <- 100*knox$confirmed/knox$population



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

daily <- readxl::read_xlsx(temp, sheet =1, col_types=c("date", "text", rep("numeric",21)))

daily_knox <- subset(daily, COUNTY=="Knox") %>% select(-"COUNTY")
daily_knox$Region <- "Knox County"
daily_knox$Population <- knox_pop
daily_knox$New_cases_per_100k_per_week <- 100000*zoo::rollsum(daily_knox$NEW_CASES, k=7, align="right", fill=NA)/knox_pop
daily_knox$PositivityRate_per_week <- zoo::rollsum(daily_knox$NEW_POS_TESTS, k=7, align="right", fill=NA) / zoo::rollsum(daily_knox$NEW_TESTS, k=7, align="right", fill=NA)



daily_oakridge <- subset(daily, COUNTY %in% c("Anderson")) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)
daily_oakridge$Region <- "Anderson"
daily_oakridge$Population <- oakridge_pop
daily_oakridge$New_cases_per_100k_per_week <- 100000*zoo::rollsum(daily_oakridge$NEW_CASES, k=7, align="right", fill=NA)/oakridge_pop
daily_oakridge$PositivityRate_per_week <- zoo::rollsum(daily_oakridge$NEW_POS_TESTS, k=7, align="right", fill=NA) / zoo::rollsum(daily_oakridge$NEW_TESTS, k=7, align="right", fill=NA)

#daily_oakridge$New_cases_per_100k <- 100000*(daily_oakridge$NEW_CASES/daily_oakridge$Population)



daily_region<- subset(daily, COUNTY %in% counties_in_hospital_region) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)

daily_region$Region <- "East TN"
daily_region$Population <- region_pop
daily_region$New_cases_per_100k_per_week <- 100000*zoo::rollsum(daily_region$NEW_CASES, k=7, align="right", fill=NA)/region_pop
daily_region$PositivityRate_per_week <- zoo::rollsum(daily_region$NEW_POS_TESTS, k=7, align="right", fill=NA) / zoo::rollsum(daily_region$NEW_TESTS, k=7, align="right", fill=NA)


daily_focal <- dplyr::bind_rows(daily_knox, daily_oakridge, daily_region)
daily_focal$Tests_per_100k <- 100000*(daily_focal$NEW_TESTS/daily_focal$Population)
daily_focal$New_cases_per_100k <- 100000*(daily_focal$NEW_CASES/daily_focal$Population)
daily_focal$Active_cases_per_100k <- 100000*(daily_focal$TOTAL_ACTIVE/daily_focal$Population)
daily_focal$PositivityPercentage_per_week <- 100*daily_focal$PositivityRate_per_week



temp = tempfile(fileext = ".xlsx")
dataURL <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-County-Cases-5-18-Years.XLSX"
download.file(dataURL, destfile=temp, mode='wb')

schoolkids <- readxl::read_xlsx(temp, sheet =1, col_types=c("date", "text", rep("numeric",2)))

schoolkids_region<- subset(schoolkids, COUNTY %in% counties_in_hospital_region) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)

schoolkids_oakridge<- subset(schoolkids, COUNTY %in% c("Anderson")) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)

schoolkids_knox<- subset(schoolkids, COUNTY %in% c("Knox")) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)

schoolkids_region$Region <- "East TN"
schoolkids_knox$Region <- "Knox County"
schoolkids_oakridge$Region <- "Anderson"
schoolkids_daily <- rbind(schoolkids_knox, schoolkids_oakridge, schoolkids_region)



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


oakridge_school_files <- list.files(path="/Users/bomeara/Dropbox/OakRidgeCovid", pattern="*oak_ridge_schools.csv", full.names=TRUE)
schools_oakridge <- data.frame()
for (i in seq_along(oakridge_school_files)) {
  school_oakridge_info <- NULL
  try(school_oakridge_info <- read.csv(oakridge_school_files[i]), silent=TRUE)
  if(!is.null(school_oakridge_info)) {
	school_oakridge_info[is.na(school_oakridge_info)] <- 0
	colnames(school_oakridge_info)[9] <- "student.population"
	local_info <- data.frame(Date=rep(anytime::anytime(stringr::str_extract(oakridge_school_files[i], "\\d+_\\d+_\\d+_\\d+_\\d+_\\d+")), nrow(school_oakridge_info)), School=school_oakridge_info$School, PercentPositiveStudentsYearToDate=100*school_oakridge_info$YTD.Student.Cases/school_oakridge_info$student.population, PercentActiveCovidStudents=100*school_oakridge_info$Current.Student.Cases/school_oakridge_info$student.population)
	if (i==1) {
		schools_oakridge <- local_info
	} else {
		schools_oakridge <- rbind(schools_oakridge, local_info)
	}
  }
}
schools_oakridge <- schools_oakridge[is.finite(schools_oakridge$PercentPositiveStudentsYearToDate),]


hhs_sources <- jsonlite::fromJSON("https://healthdata.gov/data.json?page=0")
capacity_by_facility_number <- grep("COVID-19 Reported Patient Impact and Hospital Capacity by Facility", hhs_sources$dataset$title)


capacity_by_facility_url <- hhs_sources$dataset$distribution[capacity_by_facility_number][[1]]$downloadURL[1] #often a week behind though
temp = tempfile(fileext = ".csv")
#


# scraped_url <- readLines("https://healthdata.gov/dataset/covid-19-reported-patient-impact-and-hospital-capacity-facility")
# scraped_url <- scraped_url[grepl("https://healthdata.gov/sites/default/files/reported_hospital_capacity_admissions_facility_level_weekly_average_timeseries", scraped_url)]
# capacity_by_facility_url <- str_extract(scraped_url, "https.*csv")


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

# data from US census

population_total <- 6829174

population_female <- population_total*0.512
population_male <- population_total*(1-0.512)

population_white <- population_total*0.784
population_black_africanamerican <- population_total*0.171
population_asian <- population_total*0.02
population_americanindian_alaskannative <- population_total*0.005
population_nativehawaiian_other_pacificislander <- population_total*0.001
population_twoormoreraces <- population_total*0.02

population_hispanic <- population_total*0.057
population_not_hispanic <- population_total*(1-0.057)


population_age_under_5 <- population_total*0.06
population_age_under_18 <- population_total*0.221
population_age_65_and_over <- population_total*0.167



temp = tempfile(fileext = ".xlsx")
dataURL <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/COVID_VACCINE_DEMOGRAPHICS.XLSX"
download.file(dataURL, destfile=temp, mode='wb')
demographics_vaccine <- readxl::read_xlsx(temp, sheet =1)
demographics_vaccine$RECIPIENT_COUNT[is.na(demographics_vaccine$RECIPIENT_COUNT)] <- demographics_vaccine$VACCINE_COUNT[is.na(demographics_vaccine$RECIPIENT_COUNT)]
demographics_vaccine$RECIP_FULLY_VACC[is.na(demographics_vaccine$RECIP_FULLY_VACC)] <- 0





race_vaccine <- subset(demographics_vaccine, CATEGORY=="RACE")
race_vaccine$Race <- stringr::str_to_title(race_vaccine$CAT_DETAIL)
race_vaccine$PercentFullyVaccinated <- NA
race_vaccine$PercentFullyVaccinated <- as.numeric(race_vaccine$PercentFullyVaccinated)

race_vaccine[which(race_vaccine$Race=="White"),]$PercentFullyVaccinated <- 100*race_vaccine[which(race_vaccine$Race=="White"),]$RECIP_FULLY_VACC/population_white
race_vaccine[which(race_vaccine$Race=="Black Or African American"),]$PercentFullyVaccinated <- 100*race_vaccine[which(race_vaccine$Race=="Black Or African American"),]$RECIP_FULLY_VACC/population_black_africanamerican
race_vaccine[which(race_vaccine$Race=="Asian"),]$PercentFullyVaccinated <- 100*race_vaccine[which(race_vaccine$Race=="Asian"),]$RECIP_FULLY_VACC/population_asian

race_vaccine <- race_vaccine %>% group_by(Race) %>% mutate(PreviousPercentFullyVaccinated=dplyr::lag(PercentFullyVaccinated, n=1, default=0))
race_vaccine$IncreasePercentFullyVaccinated <- race_vaccine$PercentFullyVaccinated - race_vaccine$PreviousPercentFullyVaccinated
race_vaccine$IncreasePercentFullyVaccinated[race_vaccine$IncreasePercentFullyVaccinated<0] <- 0 # for cases with sudden drops, probably due to issues in the original data

race_vaccine <- race_vaccine %>% group_by(Race) %>% mutate(IncreasePercentFullyVaccinated7Day=zoo::rollmean(IncreasePercentFullyVaccinated, k=7, align="right", fill=0))

race_vaccine <- subset(race_vaccine, !is.na(PercentFullyVaccinated))

ethnicity_vaccine <- subset(demographics_vaccine, CATEGORY=="ETHN")
ethnicity_vaccine$Ethnicity <- stringr::str_to_title(ethnicity_vaccine$CAT_DETAIL)

ethnicity_vaccine$PercentFullyVaccinated <- NA
ethnicity_vaccine$PercentFullyVaccinated <- as.numeric(ethnicity_vaccine$PercentFullyVaccinated)

ethnicity_vaccine[which(ethnicity_vaccine$Ethnicity=="Hispanic Or Latino"),]$PercentFullyVaccinated <- 100*ethnicity_vaccine[which(ethnicity_vaccine$Ethnicity=="Hispanic Or Latino"),]$RECIP_FULLY_VACC/population_hispanic
ethnicity_vaccine[which(ethnicity_vaccine$Ethnicity=="Not Hispanic Or Latino"),]$PercentFullyVaccinated <- 100*ethnicity_vaccine[which(ethnicity_vaccine$Ethnicity=="Not Hispanic Or Latino"),]$RECIP_FULLY_VACC/population_not_hispanic


ethnicity_vaccine <- ethnicity_vaccine %>% group_by(Ethnicity) %>% mutate(PreviousPercentFullyVaccinated=dplyr::lag(PercentFullyVaccinated, n=1, default=0))
ethnicity_vaccine$IncreasePercentFullyVaccinated <- ethnicity_vaccine$PercentFullyVaccinated - ethnicity_vaccine$PreviousPercentFullyVaccinated
ethnicity_vaccine$IncreasePercentFullyVaccinated[ethnicity_vaccine$IncreasePercentFullyVaccinated<0] <- 0 # for cases with sudden drops, probably due to issues in the original data

ethnicity_vaccine <- ethnicity_vaccine %>% group_by(Ethnicity) %>% mutate(IncreasePercentFullyVaccinated7Day=zoo::rollmean(IncreasePercentFullyVaccinated, k=7, align="right", fill=0))

ethnicity_vaccine <- subset(ethnicity_vaccine, !is.na(PercentFullyVaccinated))


sex_vaccine <- subset(demographics_vaccine, CATEGORY=="SEX")
sex_vaccine$Sex <- stringr::str_to_title(sex_vaccine$CAT_DETAIL)
sex_vaccine$Sex <- gsub("F", "Female", sex_vaccine$Sex)
sex_vaccine$Sex <- gsub("M", "Male", sex_vaccine$Sex)
sex_vaccine$Sex <- gsub("U", "Unknown", sex_vaccine$Sex)
sex_vaccine$Sex <- gsub("O", "Other", sex_vaccine$Sex)
sex_vaccine$PercentFullyVaccinated <- NA
sex_vaccine$PercentFullyVaccinated <- as.numeric(sex_vaccine$PercentFullyVaccinated)
sex_vaccine[which(sex_vaccine$Sex=="Female"),]$PercentFullyVaccinated <- 100*sex_vaccine[which(sex_vaccine$Sex=="Female"),]$RECIP_FULLY_VACC/population_female
sex_vaccine[which(sex_vaccine$Sex=="Male"),]$PercentFullyVaccinated <- 100*sex_vaccine[which(sex_vaccine$Sex=="Male"),]$RECIP_FULLY_VACC/population_male



sex_vaccine <- sex_vaccine %>% group_by(Sex) %>% mutate(PreviousPercentFullyVaccinated=dplyr::lag(PercentFullyVaccinated, n=1, default=0))
sex_vaccine$IncreasePercentFullyVaccinated <- sex_vaccine$PercentFullyVaccinated - sex_vaccine$PreviousPercentFullyVaccinated
sex_vaccine$IncreasePercentFullyVaccinated[sex_vaccine$IncreasePercentFullyVaccinated<0] <- 0 # for cases with sudden drops, probably due to issues in the original data

sex_vaccine <- sex_vaccine %>% group_by(Sex) %>% mutate(IncreasePercentFullyVaccinated7Day=zoo::rollmean(IncreasePercentFullyVaccinated, k=7, align="right", fill=0))


sex_vaccine <- subset(sex_vaccine, !is.na(PercentFullyVaccinated))







temp = tempfile(fileext = ".xlsx")
dataURL <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-RaceEthSex.XLSX"
download.file(dataURL, destfile=temp, mode='wb')
demographics <- readxl::read_xlsx(temp, sheet =1)
demographics$percent_of_demographic_with_cases <- 0
demographics$percent_of_demographic_dead <- 0
demographics$Date <- demographics$DATE
demographics <- subset(demographics, as.character(demographics$DATE)!="2020-07-28") #there was an error that day for Native Hawaiian or Other Pacific Islander: it went from 2 to 15 recorded deaths. Remove date from all data just to be safe.



race <- subset(demographics, CATEGORY=="RACE")
race <- subset(race, CAT_DETAIL != "Pending")


race[which(race$CAT_DETAIL=="White"),]$percent_of_demographic_with_cases <- 100*race[which(race$CAT_DETAIL=="White"),]$CAT_TOTALCASES/population_white
race[which(race$CAT_DETAIL=="White"),]$percent_of_demographic_dead <- 100*race[which(race$CAT_DETAIL=="White"),]$CAT_TOTALDEATHS/population_white
race[which(race$CAT_DETAIL=="Black or African American"),]$percent_of_demographic_with_cases <- 100*race[which(race$CAT_DETAIL=="Black or African American"),]$CAT_TOTALCASES/population_black_africanamerican
race[which(race$CAT_DETAIL=="Black or African American"),]$percent_of_demographic_dead <- 100*race[which(race$CAT_DETAIL=="Black or African American"),]$CAT_TOTALDEATHS/population_black_africanamerican
race[which(race$CAT_DETAIL=="Asian"),]$percent_of_demographic_with_cases <- 100*race[which(race$CAT_DETAIL=="Asian"),]$CAT_TOTALCASES/population_asian
race[which(race$CAT_DETAIL=="Asian"),]$percent_of_demographic_dead <- 100*race[which(race$CAT_DETAIL=="Asian"),]$CAT_TOTALDEATHS/population_asian
race[which(race$CAT_DETAIL=="American Indian or Alaska Native"),]$percent_of_demographic_with_cases <- 100*race[which(race$CAT_DETAIL=="American Indian or Alaska Native"),]$CAT_TOTALCASES/population_americanindian_alaskannative
race[which(race$CAT_DETAIL=="American Indian or Alaska Native"),]$percent_of_demographic_dead <- 100*race[which(race$CAT_DETAIL=="American Indian or Alaska Native"),]$CAT_TOTALDEATHS/population_americanindian_alaskannative
race[which(race$CAT_DETAIL=="Native Hawaiian or Other Pacific Islander"),]$percent_of_demographic_with_cases <- 100*race[which(race$CAT_DETAIL=="Native Hawaiian or Other Pacific Islander"),]$CAT_TOTALCASES/population_nativehawaiian_other_pacificislander
race[which(race$CAT_DETAIL=="Native Hawaiian or Other Pacific Islander"),]$percent_of_demographic_dead <- 100*race[which(race$CAT_DETAIL=="Native Hawaiian or Other Pacific Islander"),]$CAT_TOTALDEATHS/population_nativehawaiian_other_pacificislander
race[which(race$CAT_DETAIL=="Other/Multiracial"),]$percent_of_demographic_with_cases <- 100*race[which(race$CAT_DETAIL=="Other/Multiracial"),]$CAT_TOTALCASES/population_twoormoreraces
race[which(race$CAT_DETAIL=="Other/Multiracial"),]$percent_of_demographic_dead <- 100*race[which(race$CAT_DETAIL=="Other/Multiracial"),]$CAT_TOTALDEATHS/population_twoormoreraces
race <- subset(race, CAT_DETAIL!="Other/Multiracial") # see note above
race <- subset(race, CAT_DETAIL!="Other/ Multiracial") # see note above

race$Race <- race$CAT_DETAIL


ethnicity <- subset(demographics, CATEGORY=="ETHNICITY")
ethnicity <- subset(ethnicity, CAT_DETAIL != "Pending")
ethnicity$Ethnicity <- ethnicity$CAT_DETAIL


ethnicity[which(ethnicity$CAT_DETAIL=="Hispanic"),]$percent_of_demographic_with_cases <- 100*ethnicity[which(ethnicity$CAT_DETAIL=="Hispanic"),]$CAT_TOTALCASES/population_hispanic
ethnicity[which(ethnicity$CAT_DETAIL=="Hispanic"),]$percent_of_demographic_dead <- 100*ethnicity[which(ethnicity$CAT_DETAIL=="Hispanic"),]$CAT_TOTALDEATHS/population_hispanic


ethnicity[which(ethnicity$CAT_DETAIL=="Not Hispanic or Latino"),]$percent_of_demographic_with_cases <- 100*ethnicity[which(ethnicity$CAT_DETAIL=="Not Hispanic or Latino"),]$CAT_TOTALCASES/population_not_hispanic
ethnicity[which(ethnicity$CAT_DETAIL=="Not Hispanic or Latino"),]$percent_of_demographic_dead <- 100*ethnicity[which(ethnicity$CAT_DETAIL=="Not Hispanic or Latino"),]$CAT_TOTALDEATHS/population_not_hispanic



sex <- subset(demographics, CATEGORY=="SEX")
sex <- subset(sex, CAT_DETAIL != "Pending")


sex[which(sex$CAT_DETAIL=="Female"),]$percent_of_demographic_with_cases <- 100*sex[which(sex$CAT_DETAIL=="Female"),]$CAT_TOTALCASES/population_female
sex[which(sex$CAT_DETAIL=="Female"),]$percent_of_demographic_dead <- 100*sex[which(sex$CAT_DETAIL=="Female"),]$CAT_TOTALDEATHS/population_female

sex[which(sex$CAT_DETAIL=="Male"),]$percent_of_demographic_with_cases <- 100*sex[which(sex$CAT_DETAIL=="Male"),]$CAT_TOTALCASES/population_male
sex[which(sex$CAT_DETAIL=="Male"),]$percent_of_demographic_dead <- 100*sex[which(sex$CAT_DETAIL=="Male"),]$CAT_TOTALDEATHS/population_male
sex$Sex <- sex$CAT_DETAIL


# temp = tempfile(fileext = ".xlsx")
# dataURL <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-County-Age-Group.XLSX"
# download.file(dataURL, destfile=temp, mode='wb')

# age_county <- readxl::read_xlsx(temp, sheet =1)
# age_county <- subset(age_county, AGE_GROUP != "Pending")
# age_county$DATE <- age_county$date

# age_county$AGE_GROUP_SIMPLE <- age_county$AGE_GROUP
# age_county$AGE_GROUP_SIMPLE <- gsub("71-80 years", "71+ years", age_county$AGE_GROUP_SIMPLE)
# age_county$AGE_GROUP_SIMPLE <- gsub("81\\+ years", "71+ years", age_county$AGE_GROUP_SIMPLE)
# age_county %<>% group_by(COUNTY, AGE_GROUP_SIMPLE, DATE) %>% mutate(SUM_CASE_COUNT = sum(CASE_COUNT))
# age_county <- subset(age_county, AGE_GROUP != "81+ years") #get rid of pruned
# age_county <- data.frame(DATE=age_county$DATE, COUNTY=age_county$COUNTY, AGE_GROUP = age_county$AGE_GROUP_SIMPLE, CASE_COUNT=age_county$SUM_CASE_COUNT)

# age_county %<>% group_by(COUNTY, AGE_GROUP) %>% mutate(Difference=CASE_COUNT - lag(CASE_COUNT))
# age_county %<>% group_by(COUNTY, DATE) %>% mutate(PercentDaily=100*Difference/sum(Difference), PercentCumulative=100*CASE_COUNT/sum(CASE_COUNT))

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

saliva_data <- read.csv(file="7 LIVE_saliva_test_data_Page 1_Table.csv", stringsAsFactors=FALSE)
saliva_data2 <- read.csv(file="LIVE spring 2021 saliva table_Page 1_Table.csv", stringsAsFactors=FALSE)
saliva_data2 <- subset(saliva_data2, Category=="Residents")
saliva_data2$Locations <- "Residents"
saliva_data2$Week <- saliva_data2$Week.ending
saliva_data2$Positive.diagnostic.tests. <- saliva_data2$Positive.diagnostic.tests
saliva_data2$Positive.pools <- saliva_data2$Number.of.positive.pools

saliva_data2$Participation.rate <- as.numeric(gsub('%', '', saliva_data2$Participation.rate))/100
saliva_data <- plyr::rbind.fill(saliva_data, saliva_data2)

saliva_data$Active_cases_per_100k = 100000*saliva_data$Positive.diagnostic.tests./saliva_data$Samples
saliva_data$Active_cases_per_30k = 30000*saliva_data$Positive.diagnostic.tests./saliva_data$Samples
saliva_data$New_cases_per_100k = saliva_data$Active_cases_per_100k / 14
saliva_data$DATE <- as.Date(saliva_data$Week,"%b %d, %Y")

saliva_data$New_cases_per_100k_lower <- 100000*binom::binom.confint(saliva_data$Positive.diagnostic.tests., saliva_data$Samples, method="exact")$lower/14
saliva_data$New_cases_per_100k_upper<- 100000*binom::binom.confint(saliva_data$Positive.diagnostic.tests., saliva_data$Samples, method="exact")$upper/14


daily_utk <- subset(daily_focal, Region=="UTK Student Testing")
daily_utk$DATE <- as.Date(daily_utk$DATE)
utk_official_testing <- read.csv(file="8 LIVE_SHC_test_data_Page 1_Table.csv", stringsAsFactors=FALSE)
utk_official_testing$DATE <- as.Date(utk_official_testing$Week,"%b %d, %Y")
utk_official_testing$NEW_TESTS <- utk_official_testing$Total/7
utk_official_testing$DailyNegative <- utk_official_testing$Negative.tests/7
utk_official_testing$NEW_CONFIRMED <- utk_official_testing$Positive.tests/7
real_rows <- nrow(utk_official_testing)
for (i in 1:6) {
	next_day <- utk_official_testing[1:real_rows,]
	next_day$DATE <- next_day$DATE+i
	utk_official_testing <- rbind(utk_official_testing, next_day)
}

utk_official_testing$Data_source <- "UTK weekly release"
daily_utk <- utk_official_testing
daily_utk <- daily_utk[order(daily_utk$DATE), ]

daily_utk$NEW_PROPORTION_CONFIRMED <- 100*daily_utk$NEW_CONFIRMED/daily_utk$NEW_TESTS

daily_focal_no_ut <- daily_focal[!grepl("UTK", daily_focal$Region), ]
daily_focal_no_ut_cleaned <- daily_focal_no_ut

utk_active_cases_reported <- read.csv("1 active cases_Page 1_Line chart.csv")
utk_active_cases_reported$DATE <- as.Date(utk_active_cases_reported[,1], "%B %d, %Y")
utk_active_cases_reported$ProportionFacultyStaff <- utk_active_cases_reported$Employees/6600 # Using size of pool from Chancellor update of Aug 19, 2021
utk_active_cases_reported$ProportionStudents <- utk_active_cases_reported$Students/30000 # Using size of pool from Chancellor update of Aug 19, 2021
utk_active_cases_reported$StudentProportionOverEmployeeProportion <- utk_active_cases_reported$ProportionStudents/utk_active_cases_reported$ProportionFacultyStaff


utk_isolations_reported_raw <- read.csv("3 active self_isolations_group_Page 1_Bar chart.csv")
utk_isolations_reported_raw$DATE <- as.Date(utk_isolations_reported_raw[,1], "%B %d, %Y")
utk_isolations_reported <- rbind(
	data.frame(DATE=utk_isolations_reported_raw$DATE, Percentage=100*utk_isolations_reported_raw$Employees/6600, Population="Employees"),
	data.frame(DATE=utk_isolations_reported_raw$DATE, Percentage=100*utk_isolations_reported_raw$Students..residential./8000, Population="Students (residential)"),
	data.frame(DATE=utk_isolations_reported_raw$DATE, Percentage=100*utk_isolations_reported_raw$Students..non.residential./22000, Population="Students (nonresidential)")
)



## ----summarytabletn, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE----
# I'm doing this poorly using hard coding, but I don't know of a better way
racesum <- data.frame(Category="Race", Group=c("American Indian or Alaska Native", "Asian", "Black or African American", "Native Hawaiian or Other Pacific Islander", "White"), Positive_Covid_Test=NA, Covid_Death=NA, At_Least_One_Vaccination=NA, Fully_Vaccinated=NA)
race_recent <- subset(race, Date==max(race$Date))
for (i in seq_along(racesum$Group)) {
	racesum$Positive_Covid_Test[i] <- race_recent$percent_of_demographic_with_cases[which(race_recent$CAT_DETAIL==racesum$Group[i])]
	racesum$Covid_Death[i] <- race_recent$percent_of_demographic_dead[which(race_recent$CAT_DETAIL==racesum$Group[i])]

}
race_vaccine_recent <- subset(race_vaccine, DATE==max(race_vaccine$DATE))

racesum[which(racesum$Group=="Asian"),]$At_Least_One_Vaccination <- 100*race_vaccine_recent[which(race_vaccine_recent$Race=="Asian"),]$RECIPIENT_COUNT / population_asian

racesum[which(racesum$Group=="Asian"),]$Fully_Vaccinated <- 100*race_vaccine_recent[which(race_vaccine_recent$Race=="Asian"),]$RECIP_FULLY_VACC / population_asian


racesum[which(racesum$Group=="Black or African American"),]$At_Least_One_Vaccination <- 100*race_vaccine_recent[which(race_vaccine_recent$Race=="Black Or African American"),]$RECIPIENT_COUNT / population_black_africanamerican

racesum[which(racesum$Group=="Black or African American"),]$Fully_Vaccinated <- 100*race_vaccine_recent[which(race_vaccine_recent$Race=="Black Or African American"),]$RECIP_FULLY_VACC / population_black_africanamerican


racesum[which(racesum$Group=="White"),]$At_Least_One_Vaccination <- 100*race_vaccine_recent[which(race_vaccine_recent$Race=="White"),]$RECIPIENT_COUNT / population_white

racesum[which(racesum$Group=="White"),]$Fully_Vaccinated <- 100*race_vaccine_recent[which(race_vaccine_recent$Race=="White"),]$RECIP_FULLY_VACC / population_white

sumtab <- racesum


ethnicitysum <- data.frame(Category="Ethnicity", Group=c("Hispanic", "Not Hispanic or Latino"), Positive_Covid_Test=NA, Covid_Death=NA, At_Least_One_Vaccination=NA, Fully_Vaccinated=NA)
ethnicity_recent <- subset(ethnicity, Date==max(ethnicity$Date))
for (i in seq_along(ethnicitysum$Group)) {
	ethnicitysum$Positive_Covid_Test[i] <- ethnicity_recent$percent_of_demographic_with_cases[which(ethnicity_recent$CAT_DETAIL==ethnicitysum$Group[i])]
	ethnicitysum$Covid_Death[i] <- ethnicity_recent$percent_of_demographic_dead[which(ethnicity_recent$CAT_DETAIL==ethnicitysum$Group[i])]

}
ethnicity_vaccine_recent <- subset(ethnicity_vaccine, DATE==max(ethnicity_vaccine$DATE))

ethnicitysum[which(ethnicitysum$Group=="Hispanic"),]$At_Least_One_Vaccination <- 100*ethnicity_vaccine_recent[which(ethnicity_vaccine_recent$Ethnicity=="Hispanic Or Latino"),]$RECIPIENT_COUNT / population_hispanic

ethnicitysum[which(ethnicitysum$Group=="Hispanic"),]$Fully_Vaccinated <- 100*ethnicity_vaccine_recent[which(ethnicity_vaccine_recent$Ethnicity=="Hispanic Or Latino"),]$RECIP_FULLY_VACC / population_hispanic


ethnicitysum[which(ethnicitysum$Group=="Not Hispanic or Latino"),]$At_Least_One_Vaccination <- 100*ethnicity_vaccine_recent[which(ethnicity_vaccine_recent$Ethnicity=="Not Hispanic Or Latino"),]$RECIPIENT_COUNT / population_not_hispanic

ethnicitysum[which(ethnicitysum$Group=="Not Hispanic or Latino"),]$Fully_Vaccinated <- 100*ethnicity_vaccine_recent[which(ethnicity_vaccine_recent$Ethnicity=="Not Hispanic Or Latino"),]$RECIP_FULLY_VACC / population_not_hispanic

sumtab <- rbind(sumtab, ethnicitysum)

sexsum <- data.frame(Category="Sex", Group=c("Female", "Male"), Positive_Covid_Test=NA, Covid_Death=NA, At_Least_One_Vaccination=NA, Fully_Vaccinated=NA)
sex_recent <- subset(sex, Date==max(sex$Date))
for (i in seq_along(sexsum$Group)) {
	sexsum$Positive_Covid_Test[i] <- sex_recent$percent_of_demographic_with_cases[which(sex_recent$CAT_DETAIL==sexsum$Group[i])]
	sexsum$Covid_Death[i] <- sex_recent$percent_of_demographic_dead[which(sex_recent$CAT_DETAIL==sexsum$Group[i])]

}
sex_vaccine_recent <- subset(sex_vaccine, DATE==max(sex_vaccine$DATE))

sexsum[which(sexsum$Group=="Female"),]$At_Least_One_Vaccination <- 100*sex_vaccine_recent[which(sex_vaccine_recent$Sex=="Female"),]$RECIPIENT_COUNT / population_female

sexsum[which(sexsum$Group=="Female"),]$Fully_Vaccinated <- 100*sex_vaccine_recent[which(sex_vaccine_recent$Sex=="Female"),]$RECIP_FULLY_VACC / population_female

sexsum[which(sexsum$Group=="Male"),]$At_Least_One_Vaccination <- 100*sex_vaccine_recent[which(sex_vaccine_recent$Sex=="Male"),]$RECIPIENT_COUNT / population_male

sexsum[which(sexsum$Group=="Male"),]$Fully_Vaccinated <- 100*sex_vaccine_recent[which(sex_vaccine_recent$Sex=="Male"),]$RECIP_FULLY_VACC / population_male

sumtab <- rbind(sumtab, sexsum)

sumtab[,c(3,5,6)] <- round(sumtab[,c(3,5,6)], 1)
sumtab[,4] <- round(sumtab[,4], 2)

for(i in seq(from=3, to=6, by=1)) {
  sumtab[,i] <- paste0(sumtab[,i], "%")
}
colnames(sumtab) <-gsub("_", " ",colnames(sumtab) )
for (i in sequence(nrow(sumtab))) {
  for (j in sequence(ncol(sumtab))) {
    if(sumtab[i,j]=="NA%") {
      sumtab[i,j]<- ""
    }
  }
}
sumtab[which(sumtab=="NA%")] <- " "

knitr::kable(sumtab)



## ----dejavuhospitalization, echo=FALSE, message=FALSE, warning=FALSE----------
daily_focal_by_year <- daily_focal[!is.na(daily_focal$NEW_HOSPITALIZED),]
daily_focal_by_year$Year <- format(daily_focal_by_year$DATE, format="%Y")
daily_focal_by_year$MONTH_DAY_IFFY_YEAR <- as.Date(as.POSIXct(paste0("2021-",format(daily_focal_by_year$DATE, format="%m-%d"), "%Y-%m-%d"))) #the year isn't right for some, but this helps plotting
daily_focal_by_year <- subset(daily_focal_by_year, Region=="East TN")
try(daily_focal_by_year_plot <- ggplot(daily_focal_by_year, aes(x=MONTH_DAY_IFFY_YEAR, y=NEW_HOSPITALIZED, group=Year)) + theme_classic() + geom_ma(aes(colour=Year, linetype="a")) +  guides(linetype = FALSE) + ylab("Number of people hospitalized per day") + xlab("Date") + ggtitle("Hospitalizations in different years in East TN") + scale_x_date(date_labels = "%b", breaks = "2 months", limits=c(as.Date("2021-01-01", format="%Y-%m-%d"), as.Date("2021-12-31", format="%Y-%m-%d"))) + scale_color_manual(values=c("darkgray",'red')) + theme(legend.position='top', legend.justification='left',legend.direction='horizontal'))
try(print(daily_focal_by_year_plot))


## ----dejavudeath, echo=FALSE, message=FALSE, warning=FALSE--------------------
try(daily_death_by_year_plot <- ggplot(daily_focal_by_year, aes(x=MONTH_DAY_IFFY_YEAR, y=NEW_DEATHS, group=Year)) + theme_classic() + geom_ma(aes(colour=Year, linetype="a")) +  guides(linetype = FALSE) + ylab("Deaths per day") + xlab("Date") + ggtitle("Deaths in different years in East TN") + scale_x_date(date_labels = "%b", breaks = "2 months", limits=c(as.Date("2021-01-01", format="%Y-%m-%d"), as.Date("2021-12-31", format="%Y-%m-%d"))) + scale_color_manual(values=c("darkgray",'red')) + theme(legend.position='top', legend.justification='left',legend.direction='horizontal')) 
try(print(daily_death_by_year_plot))


## ----dejavucases, echo=FALSE, message=FALSE, warning=FALSE--------------------
try(daily_case_by_year_plot <- ggplot(daily_focal_by_year, aes(x=MONTH_DAY_IFFY_YEAR, y=NEW_CASES, group=Year)) + theme_classic() + geom_ma(aes(colour=Year, linetype="a")) +  guides(linetype = FALSE) + ylab("New cases per day") + xlab("Date") + ggtitle("Cases in different years in East TN") + scale_x_date(date_labels = "%b", breaks = "2 months", limits=c(as.Date("2021-01-01", format="%Y-%m-%d"), as.Date("2021-12-31", format="%Y-%m-%d"))) + scale_color_manual(values=c("darkgray",'red')) + theme(legend.position='top', legend.justification='left',legend.direction='horizontal'))
try(print(daily_case_by_year_plot))


## ----dejavuschoolkids, echo=FALSE, message=FALSE, warning=FALSE---------------
schoolkids_daily_by_year <- schoolkids_daily
schoolkids_daily_by_year$Year <- format(schoolkids_daily_by_year$DATE, format="%Y")
schoolkids_daily_by_year$MONTH_DAY_IFFY_YEAR <- as.Date(as.POSIXct(paste0("2021-",format(schoolkids_daily_by_year$DATE, format="%m-%d"), "%Y-%m-%d"))) #the year isn't right for some, but this helps plotting
schoolkids_daily_by_year <- subset(schoolkids_daily_by_year, Region=="East TN")
try(student_covid_daily_by_year_plot <- ggplot(schoolkids_daily_by_year, aes(x=MONTH_DAY_IFFY_YEAR, y=NEW_CASES, group=Year)) + theme_classic() + geom_ma(aes(colour=Year, linetype="a")) +  guides(linetype = FALSE) + ylab("New cases for age 5-18 students, 7 day avg") + xlab("Date") + ggtitle("New Age 5-18 student cases in East Tennessee by year") + scale_x_date(date_labels = "%b", breaks = "2 months", limits=c(as.Date("2021-01-01", format="%Y-%m-%d"), as.Date("2021-12-31", format="%Y-%m-%d"))) + scale_color_manual(values=c("darkgray",'red')) + theme(legend.position='top', legend.justification='left',legend.direction='horizontal'))
try(print(student_covid_daily_by_year_plot))


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------

nonzero_schools <- unique(subset(schools_oakridge, PercentPositiveStudentsYearToDate>0)$School)
nonzero_schools <- nonzero_schools[!grepl("Totals for",nonzero_schools)]
schools_oakridge_nonzero <- schools_oakridge[which(schools_oakridge$School %in% nonzero_schools),]
data_ends <- subset(schools_oakridge_nonzero, Date==max(schools_oakridge$Date))
try(oakridge_schools_ytd <- ggplot(schools_oakridge_nonzero, aes(x=Date, y=PercentPositiveStudentsYearToDate, group=School)) + geom_line(aes(colour=School)) + theme_classic() + ylab("Percentage of students testing positive for covid\n(since start of 2021-2 school year)") + xlab("Date") + ggtitle("Student covid infections in Oak Ridge, TN") + scale_color_discrete(guide = FALSE) + scale_y_continuous(sec.axis = sec_axis(~ ., labels=data_ends$School, breaks = data_ends$PercentPositiveStudentsYearToDate)))
print(oakridge_schools_ytd)


## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
data_ends <- subset(schools_oakridge_nonzero, Date==max(schools_oakridge$Date))
try(oakridge_schools_active <- ggplot(schools_oakridge_nonzero, aes(x=Date, y=PercentActiveCovidStudents, group=School)) +
geom_rect(mapping=aes(xmin=as.POSIXct("2021-08-08"), xmax=as.POSIXct("2021-08-19"), ymin=2, ymax=5), fill="lightgray") + geom_line(aes(colour=School)) + theme_classic() + ylab("Percentage of students with active covid infections") + xlab("Date") + ggtitle("Student active covid infections in Oak Ridge, TN") + scale_color_discrete(guide = FALSE) + scale_y_continuous(sec.axis = sec_axis(~ ., labels=data_ends$School, breaks = data_ends$PercentActiveCovidStudents)))
print(oakridge_schools_active)


## ----vaccination1, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE------






plot_race_covid_vaccination_full <- ggplot(race_vaccine, aes(x=DATE, y=PercentFullyVaccinated, group=Race)) + geom_line(aes(colour=Race))+ xlab("Date") + ylim(0,100) + theme_classic() + ylab("Percentage of people fully vaccinated") 
print(plot_race_covid_vaccination_full)

plot_ethnicity_covid_vaccination_full <- ggplot(ethnicity_vaccine, aes(x=DATE, y=PercentFullyVaccinated, group=Ethnicity)) + geom_line(aes(colour=Ethnicity))+ xlab("Date") + theme_classic() + ylim(0,100) + ylab("Percentage of people fully vaccinated") 
print(plot_ethnicity_covid_vaccination_full)

plot_sex_covid_vaccination_full <- ggplot(sex_vaccine, aes(x=DATE, y=PercentFullyVaccinated, group=Sex)) + geom_line(aes(colour=Sex))+ xlab("Date") + theme_classic() + ylim(0,100) + ylab("Percentage of people fully vaccinated") 
print(plot_sex_covid_vaccination_full)

#vax_plot <- ggarrange(plot_race_covid_vaccination_full, plot_ethnicity_covid_vaccination_full, plot_sex_covid_vaccination_full, labels=c("", "", ""), ncol=3, nrow=1)
#print(vax_plot)


## ----vaccination_increase, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE----

plot_race_covid_vaccination_delta_full <- ggplot(subset(race_vaccine, !is.na(race_vaccine$IncreasePercentFullyVaccinated7Day)), aes(x=DATE, y=IncreasePercentFullyVaccinated7Day, group=Race)) + geom_line(aes(colour=Race))+ theme_classic() + xlab("Date") + ylab("Daily % of people becoming fully vaccinated per day \n(seven day mean)") 
print(plot_race_covid_vaccination_delta_full)

plot_ethnicity_covid_vaccination_delta_full <- ggplot(subset(ethnicity_vaccine, !is.na(ethnicity_vaccine$IncreasePercentFullyVaccinated7Day)), aes(x=DATE, y=IncreasePercentFullyVaccinated7Day, group=Ethnicity)) + geom_line(aes(colour=Ethnicity))+ theme_classic() + xlab("Date") + ylab("Daily % of people becoming fully vaccinated per day \n(seven day mean)") 
print(plot_ethnicity_covid_vaccination_delta_full)

plot_sex_covid_vaccination_delta_full <- ggplot(subset(sex_vaccine, !is.na(sex_vaccine$IncreasePercentFullyVaccinated7Day)), aes(x=DATE, y=IncreasePercentFullyVaccinated7Day, group=Sex)) + geom_line(aes(colour=Sex))+ theme_classic() + xlab("Date") + ylab("Daily % of people becoming fully vaccinated per day \n(seven day mean)") 
print(plot_sex_covid_vaccination_delta_full)

#vax_speed_plot <- ggarrange(plot_race_covid_vaccination_delta_full, plot_ethnicity_covid_vaccination_delta_full, plot_sex_covid_vaccination_delta_full, labels=c("", "", ""), ncol=3, nrow=1)
#print(vax_speed_plot)


## ----plots_new_cdc, echo=FALSE, message=FALSE, warning=FALSE------------------

#local_new <- ggplot(daily_focal_no_ut_cleaned, aes(x=DATE, y=New_cases_per_100k_per_week, group=Region, colour=Region)) + 
#annotate(geom="rect", xmin=min(daily_focal_no_ut_cleaned$DATE), xmax=max(daily_focal_no_ut_cleaned$DATE), ymin=0, ymax=max(daily_focal_no_ut_cleaned$New_cases_per_100k_per_week), alpha=.8, fill="lightblue1") +
#annotate(geom="rect", xmin=min(daily_focal_no_ut_cleaned$DATE), xmax=max(daily_focal_no_ut_cleaned$DATE), ymin=9.5, ymax=max(daily_focal_no_ut_cleaned$New_cases_per_100k_per_week), alpha=.8, fill="khaki1") +
#annotate(geom="rect", xmin=min(daily_focal_no_ut_cleaned$DATE), xmax=max(daily_focal_no_ut_cleaned$DATE), ymin=49.5, ymax=max(daily_focal_no_ut_cleaned$New_cases_per_100k_per_week), alpha=.8, fill="tan1") +
#annotate(geom="rect", xmin=min(daily_focal_no_ut_cleaned$DATE), xmax=max(daily_focal_no_ut_cleaned$DATE), ymin=99.5, ymax=max(daily_focal_no_ut_cleaned$New_cases_per_100k_per_week), alpha=.8, fill="indianred1") +
#theme_classic() + geom_line()  + ylab("Number of new cases in area each week per 100,000 people") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.3) 

local_new <- ggplot(subset(daily_focal_no_ut_cleaned, DATE>="2021-06-01"), aes(x=DATE, y=New_cases_per_100k_per_week, group=Region, colour=Region)) + 
theme_classic() + geom_line()  + ylab("Number of new cases in area each week per 100,000 people") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8) 
print(local_new)

#local_active <- ggplot(daily_focal[!is.na(daily_focal$TOTAL_ACTIVE),], aes(x=DATE, y=TOTAL_ACTIVE, group=Region)) +  geom_smooth(aes(colour=Region), se=FALSE) + geom_point(aes(colour=Region), size=0.5) + ylab("Number of active cases in area each day") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
#local_active <- ggplot(daily_focal[!is.na(daily_focal$TOTAL_ACTIVE),], aes(x=DATE, y=TOTAL_ACTIVE, group=Region)) +  geom_line(aes(colour=Region)) + geom_point(aes(colour=Region), size=0.5) + ylab("Number of active cases in area each day") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
#print(local_active)


local_positivity <- ggplot(subset(daily_focal_no_ut_cleaned, DATE>="2021-06-01"), aes(x=DATE, y=PositivityPercentage_per_week, group=Region, colour=Region)) + 
#annotate(geom="rect", xmin=min(daily_focal_no_ut_cleaned$DATE), xmax=max(daily_focal_no_ut_cleaned$DATE), ymin=0, ymax=max(daily_focal_no_ut_cleaned$PositivityPercentage_per_week), alpha=.8, fill="lightblue1") +
#annotate(geom="rect", xmin=min(daily_focal_no_ut_cleaned$DATE), xmax=max(daily_focal_no_ut_cleaned$DATE), ymin=5, ymax=max(daily_focal_no_ut_cleaned$PositivityPercentage_per_week), alpha=.8, fill="khaki1") +
#annotate(geom="rect", xmin=min(daily_focal_no_ut_cleaned$DATE), xmax=max(daily_focal_no_ut_cleaned$DATE), ymin=7.95, ymax=max(daily_focal_no_ut_cleaned$PositivityPercentage_per_week), alpha=.8, fill="tan1") +
#annotate(geom="rect", xmin=min(daily_focal_no_ut_cleaned$DATE), xmax=max(daily_focal_no_ut_cleaned$DATE), ymin=9.95, ymax=max(daily_focal_no_ut_cleaned$PositivityPercentage_per_week), alpha=.8, fill="indianred1") +
theme_classic() + geom_line()  + ylab("Percentage of positive tests per week") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8) 
print(local_positivity)

#cases_plot <- ggarrange(local_new, local_positivity, labels=c("Cases", "Positivity"), ncol=2, nrow=1)
#print(cases_plot)

#local_active <- ggplot(daily_focal[!is.na(daily_focal$TOTAL_ACTIVE),], aes(x=DATE, y=TOTAL_ACTIVE, group=Region)) +  geom_smooth(aes(colour=Region), se=FALSE) + geom_point(aes(colour=Region), size=0.5) + ylab("Number of active cases in area each day") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
#local_active <- ggplot(daily_focal[!is.na(daily_focal$TOTAL_ACTIVE),], aes(x=DATE, y=TOTAL_ACTIVE, group=Region)) +  geom_line(aes(colour=Region)) + geom_point(aes(colour=Region), size=0.5) + ylab("Number of active cases in area each day") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
#print(local_active)




## ----studentinfections, echo=FALSE, message=FALSE, warning=FALSE--------------
try(student_covid_daily <- ggplot(subset(schoolkids_daily, DATE>="2021-06-01"), aes(x=DATE, y=NEW_CASES, group=Region)) + theme_classic() + geom_ma(aes(colour=Region, linetype="a")) +  guides(linetype = FALSE) + ylab("Number of students with new positive covid results daily, 7 day avg") + xlab("Date") + scale_colour_viridis_d(end=0.8))
try(print(student_covid_daily))


## ----hospitalcapacitydata, echo=FALSE, message=FALSE, warning=FALSE-----------


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
#try(hosp_plot <- ggplot(subset(hospital_knox, Date>="2021-06-01"), aes(x=Date, y=Current.Utilization, group=East.Region.Hospitals)) + geom_line(aes(colour=East.Region.Hospitals)) +  ylab("Percent Utilization in East Tennessee Region") + xlab("Date") + ylim(0,100) + theme_classic() + scale_colour_viridis_d(end=0.8) + geom_hline(yintercept=100, col="red"))
try(hosp_plot <- ggplot(hospital_knox, aes(x=Date, y=Current.Utilization, group=East.Region.Hospitals)) + geom_line(aes(colour=East.Region.Hospitals)) +  ylab("Percent Utilization in East Tennessee Region") + xlab("Date") + ylim(0,100) + theme_classic() + scale_colour_viridis_d(end=0.8) + geom_hline(yintercept=100, col="red"))
try(print(hosp_plot))



## ----plotsD, echo=FALSE, message=FALSE, warning=FALSE-------------------------


new_hospitalization <- ggplot(subset(daily_focal[!is.na(daily_focal$NEW_HOSPITALIZED),], DATE>="2021-06-01"), aes(x=DATE, y=NEW_HOSPITALIZED, group=Region)) + geom_ma(aes(colour=Region, linetype="a"), n=7) + guides(linetype = FALSE) + ylab("Number of new covid hospitalizations each day (7 day avg)") + xlab("Date") + ylim(0,NA)  + scale_colour_viridis_d(end=0.8) + theme_classic() + geom_vline(xintercept=as.POSIXct(last_hospital_update), col="black", linetype="dotted") 

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



## ----utk2021, echo=FALSE, message=FALSE, warning=FALSE------------------------
utk_active_cases_pivoted <- data.frame(DATE=rep(utk_active_cases_reported$DATE,2), PercentageActive=100*c(utk_active_cases_reported$ProportionFacultyStaff, utk_active_cases_reported$ProportionStudents), Population=c(rep("Employees", nrow(utk_active_cases_reported)),rep("Students", nrow(utk_active_cases_reported) )))

utk_active_cases_pivoted_plot <- ggplot(subset(utk_active_cases_pivoted, DATE>="2021-08-01"), aes(x=DATE, y=PercentageActive, group=Population, colour=Population)) +  geom_line() + ylab("Percent with active covid infections reported to UTK") + xlab("Date") + ylim(0,NA)  + scale_colour_viridis_d(end=0.8) + theme_classic() 

print(utk_active_cases_pivoted_plot)


## ----utk2021isolations, echo=FALSE, message=FALSE, warning=FALSE--------------


utk_isolations_plot <- ggplot(subset(utk_isolations_reported, DATE>="2021-08-01"), aes(x=DATE, y=Percentage, group=Population, colour=Population)) +  geom_line() + ylab("Percent reporting self-isolation to UTK\n(quarantine or isolation)") + xlab("Date") + ylim(0,NA)  + scale_colour_viridis_d(end=0.8) + theme_classic() 

print(utk_isolations_plot)


## ---- fig.alt = "Chancellor Plowman with two students in Chancellor's office", out.width="400px", echo=FALSE, message=FALSE, warning=FALSE----
knitr::include_graphics("Aug20_2021.png")


## ---- fig.alt = "Dean Smith, Chancellor Plowman, and various masked students", out.width="400px", echo=FALSE, message=FALSE, warning=FALSE----
knitr::include_graphics("Aug17_2021b.png")


## ---- fig.alt = "Freshmen in College of Nursing welcome", out.width="400px", echo=FALSE, message=FALSE, warning=FALSE----
knitr::include_graphics("Aug17_2021c.png")


## ---- fig.alt = "Students at Torchnight ceremony in Neyland stadium", out.width="400px", echo=FALSE, message=FALSE, warning=FALSE----
knitr::include_graphics("Aug17_2021a.png")


## ---- fig.alt = "UT Nursing Training", out.width="400px", echo=FALSE, message=FALSE, warning=FALSE----
knitr::include_graphics("Aug16_2021.png")


## ---- fig.alt = "UT Silent Disco", out.width="400px", echo=FALSE, message=FALSE, warning=FALSE----
knitr::include_graphics("Aug14_2021.png")


## ---- fig.alt = "UT Band at Neyland Stadium (I'm not sure if this space is indoors or outdoors)", out.width="400px", echo=FALSE, message=FALSE, warning=FALSE----
knitr::include_graphics("Aug13_2021.png")


## ---- fig.alt = "UT Success Academy group photos", out.width="400px", echo=FALSE, message=FALSE, warning=FALSE----
knitr::include_graphics("Aug12_2021.png")


## ---- fig.alt = "UT Student Life welcome", out.width="400px", echo=FALSE, message=FALSE, warning=FALSE----
knitr::include_graphics("Aug12_2021b.png")


## ---- fig.alt = "Guidance for masks", out.width="400px", echo=FALSE, message=FALSE, warning=FALSE----
knitr::include_graphics("Aug9_2021.png")


## ---- fig.alt = "Chancellor Plowman with staff member Sheila Burchfield-Bishop", out.width="400px", echo=FALSE, message=FALSE, warning=FALSE----
knitr::include_graphics("Aug9_2021b.png")


## ----utactive, echo=FALSE, message=FALSE, warning=FALSE, eval=FALSE-----------
## # cached downloads of https://veoci.com/veoci/p/form/4jmds5x4jj4j#tab=entryForm
## 
## utk_plot <- ggplot(utk.cases[!is.na(utk.cases$count),], aes(x=date, y=count, group=group)) + geom_line(aes(colour=group)) + ylab("Number of active cases at UTK") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8)
## print(utk_plot)


## ----plotsaliva100k, echo=FALSE, message=FALSE, warning=FALSE-----------------
saliva_plot <- ggplot(saliva_data, aes(x=DATE, y=New_cases_per_100k)) + 
#geom_rect(mapping=aes(xmin=min(DATE), xmax=Sys.Date(), ymin=0, ymax=1), fill="darkolivegreen1") +
#  geom_rect(mapping=aes(xmin=min(DATE), xmax=Sys.Date(), ymin=1, ymax=10), fill="khaki1") +
#  geom_rect(mapping=aes(xmin=min(DATE), xmax=Sys.Date(), ymin=10, ymax=25), fill="tan1") +
#  geom_rect(mapping=aes(xmin=min(DATE), xmax=Sys.Date(), ymin=25, ymax=max(New_cases_per_100k_upper)), fill="indianred1") + 
geom_point() + ylab("Est. daily new cases 100,000 people based on UTK saliva samples") + xlab("Date") + ylim(0,NA) + scale_colour_viridis_d(end=0.8) + geom_errorbar(aes(ymin=New_cases_per_100k_lower, ymax=New_cases_per_100k_upper), width=0.1) + ggtitle("Saliva results 2020-2021 school year")
print(saliva_plot)


## ----plotsalivacompliance, echo=FALSE, message=FALSE, warning=FALSE-----------
saliva_data$Participation.percentage <- 100*saliva_data$Participation.rate
saliva_compliance <- ggplot(saliva_data, aes(x=DATE, y=Participation.percentage)) + 
geom_line() + ylab("Percentage of resident students participating in mandatory teseting") + xlab("Date") + ylim(0,100) + ggtitle("Saliva compliance 2020-2021 school year")
print(saliva_compliance)

