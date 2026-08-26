#installing and loading required packages and datasets

install.packages("medicaldata")
install.packages("remotes")
install.packages("here")
install.packages("readr")
remotes::install_github("higgi13425/medicaldata")
library(medicaldata)
library(readr)
library(gtsummary)
data(package = "medicaldata")

cytomeg <- medicaldata::cytomegalovirus



###"READ IN A DATASET AND SAVE A FILE USING THE HERE PACKAGE AND REFER TO###
###THE FILE PATHS AT LEAST TWICE"

#saving data to computer with here::
getwd()
setwd("data")

if (!dir.exists(here::here("data", "project", "data.csv"))) {
	dir.create(here::here("data", "project", "data.csv"))
}



write_rds(cytomeg, here::here("data", "clean", "cytomegalovirus.rds"))

#here::here again?
cyto <- read_csv(here::here("data", "raw", "cytomegalovirus.rds"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = cytomeg_cols)

###"WRITE AND USE A FUNCTION WITH THE DATA"###
#creating a function to turn variables into character variables
cytomeg$prior.radiation <- as.character(cytomeg$prior.radiation)

characterize <- function(data, variable){
	char <- as.character(data[[variable]])
	return(char)
}


class(cytomeg$prior.transplant)
characterize(cytomeg, "prior.transplant")


cytomeg$diagnosis.type <- as.character(cytomeg$diagnosis.type)
cytomeg$cmv<-as.character(cytomeg$cmv)


###CREATE A GTSUMMARY TABLE WITH DESCRIPTIVE STATISTICS"###

#sex data dictionary: 1 is male, 0 is female
#diagnosis: 1 is myeloid, 0 is lymphoid

tbl_summary(
	cytomeg,
	by = sex,
	include = c(
		sex, diagnosis.type, prior.radiation, prior.chemo, cmv, time.to.cmv
	),
	label = list(
		diagnosis.type ~ "Type of Diagnosis (0 = lymphoid, 1 = myeloid)",
		prior.radiation ~ "Prior radiation (0 = no, 1 = yes)",
		time.to.cmv ~ "Time to CMV reactivation (in months)",
		cmv ~ "Cytomegalovirus reactivation posttransplant (0 = no, 1 = yes)",
		prior.chemo ~ "Number of prior chemotherapy regimens"
	),   missing_text = "Missing"
) |>
	# change the test used to compare sex_cat groups
	add_p(test = list(
		all_continuous() ~ "t.test",
		all_categorical() ~ "chisq.test"
	)) |>
	# add a total column with the number of observations
	add_overall(col_label = "**Total** N = {N}") |>
	bold_labels()
#this doesn't work well because it doesn't really portray the diagnosis data since it's dichotomous :/



###"CREATE A FIGURE"###


hist(cytomeg$time.to.cmv)
