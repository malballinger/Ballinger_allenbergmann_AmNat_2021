# Rule
# target : prerequisite1 prerequisite2
#	(tab)recipe

README.md : README.Rmd
	R -e "library(rmarkdown); render('README.Rmd')"

.PHONY:
exploratory : \
				exploratory/WeeklyPhenotypes.md\
				exploratory/NachmanLab_transects.md

submission/Ballinger_et_al_2021_AmNat.pdf submission/Ballinger_et_al_2021_AmNat.docx: submission/Ballinger_et_al_2021_AmNat.Rmd
	R -e 'library(rmarkdown); render("submission/Ballinger_et_al_2021_AmNat.Rmd", output_format="all")'

submission/manuscript_working.pdf submission/manuscript_working.docx: submission/manuscript_working.Rmd\
			data/raw/weekly_metadata_RAW_2021-02-11.csv\
	R -e 'library(rmarkdown); render("submission/manuscript_working.Rmd", output_format="all")'