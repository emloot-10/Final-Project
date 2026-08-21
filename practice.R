install.packages("devtools")
devtools::install_github("benyamindsmith/fightr")

library (readr)
library(fightr)
library(gtsummary)

# Loads from local cache, fetching updates automatically if needed
athletes <- get_ufc_data("ufc_athletes")
fights   <- get_ufc_data("ufc_fights")

# Force a manual refresh of all datasets
update_all_ufc_data()

#making df with only active fighters
active_athletes <- subset(athletes, status == "Active")


#create the data/clean folder
	if(!dir.exists(here::here("data", "clean"))) {
		dir.create(here::here("data", "clean"))
	}


write_rds(active_athletes, here::here("data", "clean", "acive_athletes.rds"))

#need library(gtsummary) before this
wintbl<-tbl_summary(
	active_athletes,
	by = weight_class,
	include = c(
		wins, losses, ko_tko_percent, average_fight_time, takedowns_attempted
	), missing_text = "Missing") %>%
	add_p (test = list(
		all_continuous() ~ "t.test",
		all_categorical() ~ "chisq.test"
	)) %>% add_overall(col_label = "**Total** N = {N}" ) %>%
	bold_labels()

