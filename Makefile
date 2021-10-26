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

results/tables/EnvAdapProj_Nachman_Arctos.csv: code/clean_NachmanTransects.R\
				data/raw/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv
			./code/clean_NachmanTransects.R

results/tables/GenerationColonyData.csv: code/clean_Generations.R\
				data/raw/colony_metadata_RAW.xlsx
			./code/clean_Generations.R

results/tables/N2N3_h2_data.csv: code/clean_N2N3Heritability.R\
				data/raw/N2vsN3_h2.xlsx
			./code/clean_N2N3Heritability.R

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

results/figures/VertNet_relative.pdf: code/plot_VertNetMetadata_relative.R\
				data/processed/VertNetMetadata_Mus_2021-03-18.csv
			./code/plot_VertNetMetadata_relative.R

results/figures/Generations_relative.pdf: code/plot_Generations_relative.R\
				data/processed/GenerationColonyData.csv
			./code/plot_Generations_relative.R

results/figures/N2N3_h2.pdf: code/plot_N2N3Heritability.R\
				data/processed/N2N3_h2_data.csv
			./code/plot_N2N3Heritability.R

results/figures/Weekly_BW.pdf: code/plot_WeeklyPhenotypes.R\
				data/processed/WeeklyPhenotypeData.csv
			./code/plot_WeeklyPhenotypes.R

results/figures/Weekly_Tails.pdf: code/plot_WeeklyPhenotypes.R\
				data/processed/WeeklyPhenotypeData.csv
			./code/plot_WeeklyPhenotypes.R

results/figures/RXNs_BW.pdf: code/plot_RXNs.R\
				data/processed/PostDissectionMetaData.csv
			./code/plot_RXNs.R

results/figures/RXNs_Extremities_relative.pdf: code/plot_RXNs_relative.R\
				data/processed/PostDissectionMetaData.csv
			./code/plot_RXNs_relative.R

results/figures/RXNs_BMI.pdf: code/plot_RXNs.R\
				data/processed/PostDissectionMetaData.csv
			./code/plot_RXNs.R

results/figures/VertNet_Male_Adult.pdf: code/plot_ArctosVertNet.R\
				data/processed/VertNetMetadata_Mus_2021-03-18.csv\
				data/processed/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv
			./code/plot_ArctosVertNet.R

results/figures/Arctos.pdf: code/plot_ArctosVertNet.R\
				data/processed/VertNetMetadata_Mus_2021-03-18.csv\
				data/processed/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv
			./code/plot_ArctosVertNet.R

results/figures/GenerationsModel_relative.pdf: code/model_Generations_relative.R\
				data/processed/GenerationColonyData.csv
			./code/model_Generations_relative.R

results/figures/RXNsModel_relative.pdf: code/model_RXNs_relative.R\
				data/processed/PostDissectionMetaData.csv
			./code/model_RXNs_relative.R

results/figures/RXNs_BMI_Model.pdf: code/model_RXNs.R\
				data/processed/PostDissectionMetaData.csv
			./code/model_RXNs.R

submission/figure_1.tiff : results/figures/VertNet_relative.tiff
			convert -compress lzw $< $@

submission/figure_2.tiff : results/figures/Generations_relative.tiff
			convert -compress lzw $< $@

submission/figure_3.tiff : results/figures/Weekly_BW.tiff
			convert -compress lzw $< $@

submission/figure_4.tiff : results/figures/Weekly_Tails.tiff
			convert -compress lzw $< $@

submission/figure_5.tiff : results/figures/RXNs_BW.tiff
			convert -compress lzw $< $@

submission/figure_6.tiff : results/figures/RXNs_Extremities_relative.tiff
			convert -compress lzw $< $@

submission/Ballinger_et_al_2021_AmNat.pdf submission/Ballinger_et_al_2021_AmNat.docx: submission/Ballinger_et_al_2021_AmNat.Rmd\
				data/processed/VertNetMetadata_Mus_2021-03-18.csv\
				data/processed/GenerationColonyData.csv\
				data/processed/N2N3_h2_data.csv\
				data/processed/WeeklyPhenotypeData.csv\
				data/processed/PostDissectionMetaData.csv\
				results/figures/VertNet_relative.pdf\
				results/figures/Generations_relative.pdf\
				results/figures/N2N3_h2.pdf\
				results/figures/Weekly_BW.pdf\
				results/figures/Weekly_Tails.pdf\
				results/figures/RXNs_BW.pdf\
				results/figures/RXNs_Extremities_relative.pdf\
				results/figures/GenerationsModel_relative.pdf\
				results/figures/RXNsModel_relative.pdf\
				results/figures/RXNs_BMI.pdf\
				submission/AmNat.csl\
				submission/references.bib
	R -e 'library(rmarkdown); render("submission/Ballinger_et_al_2021_AmNat.Rmd", output_format="all")'

submission/supplementary_material.pdf submission/supplementary_material.docx: submission/supplementary_material.Rmd\
				data/processed/VertNetMetadata_Mus_2021-03-18.csv\
				data/processed/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv\
				data/processed/GenerationColonyData.csv\
				data/processed/WeeklyPhenotypeData.csv\
				data/processed/PostDissectionMetaData.csv\
				results/figures/VertNet_Male_Adult.pdf\
				results/figures/GenerationsModel.pdf\
				results/figures/RXNsModel.pdf\
				results/figures/RXNs_BMI.pdf\
				results/figures/RXNs_BMI_Model.pdf
	R -e 'library(rmarkdown); render("submission/supplementary_material.Rmd", output_format="all")'