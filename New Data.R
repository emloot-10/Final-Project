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
#getwd()
#setwd("data")

#if (!dir.exists(here::here("data", "project", "data.csv"))) {
#	dir.create(here::here("data", "project", "data.csv"))
#}



#write_rds(cytomeg, here::here("data", "clean", "cytomegalovirus.rds"))

#here::here again?
#cyto <- read_csv(here::here("data", "raw", "cytomegalovirus.rds"),
#								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
#								 skip = 1, col_names = cytomeg_cols)




###CREATE A GTSUMMARY TABLE WITH DESCRIPTIVE STATISTICS"###

#sex data dictionary: 1 is male, 0 is female
#diagnosis: 1 is myeloid, 0 is lymphoid
#first: need to make the variables of interest character vars not numeric
cytomeg$diagnosis.type <- as.character(cytomeg$diagnosis.type)
cytomeg$cmv<-as.character(cytomeg$cmv)
cytomeg$prior.transplant <- as.character(cytomeg$prior.transplant)
cytomeg$sex <- as.character(cytomeg$sex)
cytomeg$prior.radiation <- as.character(cytomeg$prior.radiation)



tbl_summary(
	cytomeg,
	by = sex,
	include = c(
		sex, diagnosis.type, prior.radiation, prior.chemo, prior.transplant, cmv, time.to.cmv
	),
	label = list(
		diagnosis.type ~ "Type of Diagnosis (0 = lymphoid, 1 = myeloid)",
		prior.radiation ~ "Prior radiation (0 = no, 1 = yes)",
		time.to.cmv ~ "Time to CMV reactivation (in months)",
		cmv ~ "Cytomegalovirus reactivation posttransplant (0 = no, 1 = yes)",
		prior.chemo ~ "Number of prior chemotherapy regimens",
		prior.transplant ~ "Prior transplant (0 = no, 1 = yes)"
	),   missing_text = "Missing"
) |>
	add_p(test = list(
		all_continuous() ~ "t.test",
		all_categorical() ~ "chisq.test"
	)) |>
	add_overall(col_label = "**Total** N = {N}") |>
	bold_labels()




###"CREATE A FIGURE"###

hist(cytomeg$time.to.cmv, main = "Time to CMV Reactivation", xlab = "Months",
		 col = "navy")


###"WRITE AND USE A FUNCTION WITH THE DATA"###
#function to turn 0's into no and 1's to yes for certain variables

y_n <- function(data){
	ifelse(data == 0, "no",
				 ifelse(data == 1, "yes", NA))
}

y_n(cytomeg$diagnosis.type)
