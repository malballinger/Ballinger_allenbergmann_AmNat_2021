# Rule
# target : prerequisite1 prerequisite2
#	(tab)recipe

README.md : README.Rmd
	R -e "Sys.setenv(RSTUDIO_PANDOC='/Applications/RStudio.app/Contents/MacOS/pandoc'); library(rmarkdown); render('README.Rmd')"

data/processed/VertNetMetadata_Mus_2021-03-18.csv: code/clean_VertNetMetadata.R\
				data/raw/VertNet_Mus_specimen_20201013.tsv\
				data/processed/VertNet_Mus_processed.xlsx
			./code/clean_VertNetMetadata.R

data/processed/VertNetMetadata_Mus_2021_03-18_mean_temp.csv: code/basic_raster_extraction.R\
				data/processed/VertNetMetadata_Mus_2021-03-18.csv
			./code/basic_raster_extraction.R

data/processed/GenerationColonyData_N0-N4.csv: code/clean_Generations_N0-N4.R\
				data/raw/colony_metadata_RAW_v2.xlsx
			./code/clean_Generations_N0-N4.R

data/processed/N2N3_h2_data.csv: code/clean_N2N3Heritability.R\
				data/raw/N2vsN3_h2.xlsx
			./code/clean_N2N3Heritability.R

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

results/tables/GenerationColonyData_N0-N4.csv: code/clean_Generations_N0-N4.R\
				data/raw/colony_metadata_RAW_v2.xlsx
			./code/clean_Generations_N0-N4.R

results/tables/N2N3_h2_data.csv: code/clean_N2N3Heritability.R\
				data/raw/N2vsN3_h2.xlsx
			./code/clean_N2N3Heritability.R

results/tables/WeeklyPhenotypeData.csv: code/clean_WeeklyPhenotypes.R\
				data/raw/weekly_metadata_RAW_2021-02-11.csv
			./code/clean_WeeklyPhenotypes.R

results/tables/PostDissectionMetaData.csv: code/clean_RXNs.R\
				data/raw/post_dissection_metadata_RAW_2021-02-18.csv
			./code/clean_RXNs.R

results/figures/VertNet_relative.pdf: code/plot_VertNetMetadata_relative.R\
				data/processed/VertNetMetadata_Mus_2021-03-18.csv
			./code/plot_VertNetMetadata_relative.R

results/figures/VertNet_meantemp.pdf: code/plot_VertNetMetadata_mean_temp.R\
				data/processed/VertNetMetadata_Mus_2021-03-18_mean_temp.csv
			./code/plot_VertNetMetadata_mean_temp.R

results/figures/Generations_relative_N0-N4.pdf: code/plot_Generations_relative_N0-N4.R\
				data/processed/GenerationColonyData_N0-N4.csv
			./code/plot_Generations_relative_N0-N4.R

results/figures/N2N3_h2.pdf: code/plot_N2N3Heritability.R\
				data/processed/N2N3_h2_data.csv
			./code/plot_N2N3Heritability.R

results/figures/FigS3_h2.pdf: code/plot_N2N3Heritability.R\
				data/processed/N2N3_h2_data.csv
			./code/plot_N2N3Heritability.R

results/figures/Weekly_RXN_BW.pdf: code/plot_Weekly_RXN_BW.R\
				data/processed/WeeklyPhenotypeData.csv\
				data/processed/PostDissectionMetaData.csv
			./code/plot_Weekly_RXN_BW.R

results/figures/Weekly_Tails.pdf: code/plot_WeeklyPhenotypes.R\
				data/processed/WeeklyPhenotypeData.csv
			./code/plot_WeeklyPhenotypes.R

results/figures/RXNs_Extremities_relative.pdf: code/plot_RXNs_relative.R\
				data/processed/PostDissectionMetaData.csv
			./code/plot_RXNs_relative.R

results/figures/RXNs_BMI.pdf: code/plot_RXNs.R\
				data/processed/PostDissectionMetaData.csv
			./code/plot_RXNs.R

submission/figure_1.tiff : results/figures/VertNet_relative.tiff
			convert -compress lzw $< $@

submission/figure_2.tiff : results/figures/N2N3_h2.tiff
			convert -compress lzw $< $@

submission/figure_3.tiff : results/figures/Weekly_RXN_BW.tiff
			convert -compress lzw $< $@

submission/figure_4.tiff : results/figures/Weekly_Tails.tiff
			convert -compress lzw $< $@

submission/figure_5.tiff : results/figures/RXNs_Extremities_relative.tiff
			convert -compress lzw $< $@

submission/Ballinger_et_al_2021_AmNat.pdf submission/Ballinger_et_al_2021_AmNat.docx: submission/Ballinger_et_al_2021_AmNat.Rmd\
				data/processed/VertNetMetadata_Mus_2021-03-18.csv\
				data/processed/GenerationColonyData_N0-N4.csv\
				data/processed/N2N3_h2_data.csv\
				data/processed/WeeklyPhenotypeData.csv\
				data/processed/PostDissectionMetaData.csv\
				results/figures/VertNet_relative.pdf\
				results/figures/N2N3_h2.pdf\
				results/figures/Weekly_RXN_BW.pdf\
				results/figures/Weekly_Tails.pdf\
				results/figures/RXNs_Extremities_relative.pdf\
				results/figures/VertNet_meantemp.pdf\
				results/figures/Generations_relative_N0-N4.pdf\
				results/figures/FigS3_h2.pdf\
				results/figures/RXNs_BMI.pdf\
				submission/AmNat.csl\
				submission/references.bib
	R -e "Sys.setenv(RSTUDIO_PANDOC='/Applications/RStudio.app/Contents/MacOS/pandoc'); library(rmarkdown); render('submission/Ballinger_et_al_2021_AmNat.Rmd', output_format='all')"
