#installing and loading required packages and datasets

install.packages("medicaldata")
remotes::install_github("higgi13425/medicaldata")
library(medicaldata)
data(package = "medicaldata")

cytomeg <- medicaldata::cytomegalovirus

#saving data to computer with here::
if (!dir.exists(here::here("data", "clean"))) {
	dir.create(here::here("data", "clean"))
}

write_rds(cytomeg, here::here("data", "clean", "cytomegalovirus.rds"))

#here::here again?
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols)


#gtsummary tables
library(gtsummary)

#sex data dictionary: 1 is male, 0 is female
#diagnosis: 1 is myeloid, 0 is lymphoid

tbl_summary(
	cytomeg,
	by = sex,
	include = c(
		sex, diagnosis.type, prior.radiation, time.to.cmv
	),
	label = list(
		diagnosis.type ~ "Type of Diagnosis",
		prior.radiation ~ "Prior radiation",
		time.to.cmv ~ "Time to CMV reactivation (in months)"
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

