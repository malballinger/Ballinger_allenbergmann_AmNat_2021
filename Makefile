# Rule
# target : prerequisite1 prerequisite2
#	(tab)recipe

README.md : README.Rmd
	R -e "library(rmarkdown); render('README.Rmd')"

.PHONY:
exploratory : \
				exploratory/NachmanLab_transects.md\
				exploratory/2021-02-08_RidgelinePlots.md\
				exploratory/WeeklyPhenotypes.md\

figures/Nachman_transects.pdf: code/plot_NachmanTransects.R\
				data/raw/NachmanLab_MusTransects_Metadata_2021-02-15.csv
			./code/plot_NachmanTransects.R

figures/generation_phenotypes.pdf: code/plot_generation_phenotypes.R\
				data/processed/AllBZNYmice_wild&colony.csv
			./code/plot_generation_phenotypes.R

figures/weekly_phenotypes.pdf: code/plot_weekly_phenotypes.R\
				data/raw/weekly_metadata_RAW_2021-02-11.csv
			./code/plot_weekly_phenotypes.R

figures/RXNs.pdf: code/plot_RXNs.R\
				data/raw/post_dissection_metadata_RAW_2021-02-18.csv
			./code/plot_RXNs.R

figures/VertNet_metadata.pdf: code/plot_VertNet_metadata.R\
				data/processed/VertNet_Mus_processed.csv
			./code/plot_VertNet_metadata.R

submission/figure_1.tiff : figures/Nachman_transects.tiff
			convert -compress lzw $< $@

submission/figure_2.tiff : figures/generation_phenotypes.tiff
			convert -compress lzw $< $@

submission/figure_3.tiff : figures/weekly_phenotypes.tiff
			convert -compress lzw $< $@

submission/Ballinger_et_al_2021_AmNat.pdf submission/Ballinger_et_al_2021_AmNat.docx: submission/Ballinger_et_al_2021_AmNat.Rmd\
				data/raw/NachmanLab_MusTransects_Metadata_2021-02-15.csv\
				data/processed/AllBZNYmice_wild&colony.csv\
				data/raw/weekly_metadata_RAW_2021-02-11.csv\
				data/raw/post_dissection_metadata_RAW_2021-02-18.csv\
				data/processed/VertNet_Mus_processed.csv\
				figures/Nachman_transects.pdf\
				figures/generation_phenotypes.pdf\
				figures/weekly_phenotypes.pdf\
				figures/RXNs.pdf\
				figures/VertNet_metadata.pdf
	R -e 'library(rmarkdown); render("submission/Ballinger_et_al_2021_AmNat.Rmd", output_format="all")'

submission/manuscript_working.pdf submission/manuscript_working.docx: submission/manuscript_working.Rmd\
			data/raw/weekly_metadata_RAW_2021-02-11.csv
	R -e 'library(rmarkdown); render("submission/manuscript_working.Rmd", output_format="all")'