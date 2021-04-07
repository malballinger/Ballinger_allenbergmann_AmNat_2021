# Rule
# target : prerequisite1 prerequisite2
#	(tab)recipe

README.md : README.Rmd
	R -e "library(rmarkdown); render('README.Rmd')"

data/processed/VertNetMetadata_Mus_2021-03-18.csv: code/clean_VertNetMetadata.R\
				data/raw/VertNet_Mus_specimen_20201013.tsv\
				data/processed/VertNet_Mus_processed.xlsx
			./code/clean_VertNetMetadata.R

data/processed/EnvAdapProj_Nachman_Arctos_transects_2021_03_15.csv: code/clean_NachmanTransects.R\
				data/raw/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv
			./code/clean_NachmanTransects.R

data/processed/GenerationColonyData.csv: code/clean_Generations.R\
				data/raw/colony_metadata_RAW.xlsx
			./code/clean_Generations.R

data/processed/WeeklyPhenotypeData.csv: code/clean_WeeklyPhenotypes.R\
				data/raw/weekly_metadata_RAW_2021-02-11.csv
			./code/clean_WeeklyPhenotypes.R

data/processed/PostDissectionMetaData.csv: code/clean_RXNs.R\
				data/raw/post_dissection_metadata_RAW_2021-02-18.csv
			./code/clean_RXNs.R

results/tables/VertNetMetadata_Mus.csv: code/clean_VertNetMetadata.R\
				data/raw/VertNet_Mus_specimen_20201013.tsv\
				data/processed/VertNet_Mus_processed.xlsx
			./code/clean_VertNetMetadata.R

results/tables/EnvAdapProj_Nachman_Arctos.csv: code/clean_NachmanTransects.R\
				data/raw/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv
			./code/clean_NachmanTransects.R

results/tables/GenerationColonyData.csv: code/clean_Generations.R\
				data/raw/colony_metadata_RAW.xlsx
			./code/clean_Generations.R

results/tables/WeeklyPhenotypeData.csv: code/clean_WeeklyPhenotypes.R\
				data/raw/weekly_metadata_RAW_2021-02-11.csv
			./code/clean_WeeklyPhenotypes.R

results/tables/PostDissectionMetaData.csv: code/clean_RXNs.R\
				data/raw/post_dissection_metadata_RAW_2021-02-18.csv
			./code/clean_RXNs.R

results/figures/VertNet_Arctos.pdf: code/plot_ArctosVertNet.R\
				data/processed/VertNetMetadata_Mus_2021-03-18.csv\
				data/processed/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv
			./code/plot_ArctosVertNet.R

results/figures/Generations_colony.pdf: code/plot_Generations.R\
				data/processed/GenerationColonyData.csv
			./code/plot_Generations.R

results/figures/Weekly_BW.pdf: code/plot_WeeklyPhenotypes.R\
				data/processed/WeeklyPhenotypeData.csv
			./code/plot_WeeklyPhenotypes.R

results/figures/Weekly_Tails.pdf: code/plot_WeeklyPhenotypes.R\
				data/processed/WeeklyPhenotypeData.csv
			./code/plot_WeeklyPhenotypes.R

results/figures/RXNs_BW.pdf: code/plot_RXNs.R\
				data/raw/post_dissection_metadata_RAW_2021-02-18.csv
			./code/plot_RXNs.R

results/figures/RXNs_Extremities.pdf: code/plot_RXNs.R\
				data/raw/post_dissection_metadata_RAW_2021-02-18.csv
			./code/plot_RXNs.R

results/figures/VertNet_metadata.pdf: code/plot_VertNet_metadata.R\
				data/processed/VertNetMetadata_Mus_2021-03-18.csv
			./code/plot_VertNet_metadata.R

results/figures/GenerationsModel.pdf: code/model_Generations.R\
				data/processed/GenerationColonyData.csv
			./code/model_Generations.R

results/figures/RXNsModel.pdf: code/model_RXNs.R\
				data/processed/PostDissectionMetaData.csv
			./code/model_RXNs.R

submission/figure_1.tiff : figures/VertNet_Arctos.tiff
			convert -compress lzw $< $@

submission/figure_2.tiff : figures/generation_phenotypes.tiff
			convert -compress lzw $< $@

submission/figure_3.tiff : figures/weekly_phenotypes.tiff
			convert -compress lzw $< $@

submission/Ballinger_et_al_2021_AmNat.pdf submission/Ballinger_et_al_2021_AmNat.docx: submission/Ballinger_et_al_2021_AmNat.Rmd\
				data/processed/VertNetMetadata_Mus_2021-03-18.csv\
				data/processed/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv\
				data/processed/GenerationColonyData.csv\
				data/processed/WeeklyPhenotypeData.csv\
				data/processed/PostDissectionMetaData.csv\
				results/figures/VertNet_Arctos.pdf\
				results/figures/Generations_colony.pdf\
				results/figures/Weekly_BW.pdf\
				results/figures/Weekly_Tails.pdf\
				results/figures/RXNs_BW.pdf\
				results/figures/RXNs_Extremities.pdf\
				results/figures/VertNet_metadata.pdf\
				results/figures/GenerationsModel.pdf\
				results/figures/RXNsModel.pdf\
				submission/AmNat.csl\
				submission/references.bib
	R -e 'library(rmarkdown); render("submission/Ballinger_et_al_2021_AmNat.Rmd", output_format="all")'
