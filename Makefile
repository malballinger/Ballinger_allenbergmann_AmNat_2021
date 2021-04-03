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

results/tables/VertNetMetadata_Mus.csv: code/clean_VertNetMetadata.R\
				data/raw/VertNet_Mus_specimen_20201013.tsv\
				data/processed/VertNet_Mus_processed.xlsx
			./code/clean_VertNetMetadata.R

results/figures/VertNet_metadata.pdf: code/plot_VertNet_metadata.R\
				data/processed/VertNetMetadata_Mus_2021-03-18.csv
			./code/plot_VertNet_metadata.R

results/figures/Nachman_transects.pdf: code/plot_NachmanTransects.R\
				data/processed/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv
			./code/plot_NachmanTransects.R

results/figures/VertNet_Arctos.pdf: code/plot_ArctosVertNet.R\
				data/processed/VertNetMetadata_Mus_2021-03-18.csv\
				data/processed/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv
			./code/plot_ArctosVertNet.R

results/figures/generation_phenotypes.pdf: code/plot_generation_phenotypes.R\
				data/processed/AllBZNYmice_wild&colony.csv
			./code/plot_generation_phenotypes.R

results/figures/weekly_phenotypes.pdf: code/plot_weekly_phenotypes.R\
				data/raw/weekly_metadata_RAW_2021-02-11.csv
			./code/plot_weekly_phenotypes.R

results/figures/RXNs.pdf: code/plot_RXNs.R\
				data/raw/post_dissection_metadata_RAW_2021-02-18.csv
			./code/plot_RXNs.R

results/figures/VertNet_metadata.pdf: code/plot_VertNet_metadata.R\
				data/processed/VertNet_Mus_processed.csv
			./code/plot_VertNet_metadata.R

submission/figure_1.tiff : figures/VertNet_Arctos.tiff
			convert -compress lzw $< $@

submission/figure_2.tiff : figures/generation_phenotypes.tiff
			convert -compress lzw $< $@

submission/figure_3.tiff : figures/weekly_phenotypes.tiff
			convert -compress lzw $< $@

submission/Ballinger_et_al_2021_AmNat.pdf submission/Ballinger_et_al_2021_AmNat.docx: submission/Ballinger_et_al_2021_AmNat.Rmd\
				data/processed/VertNetMetadata_Mus_2021-03-18.csv\
				data/raw/EnvAdapProj_Nachman_Arctos_transects_2021-03-15.csv\
				data/processed/AllBZNYmice_wild&colony.csv\
				data/raw/weekly_metadata_RAW_2021-02-11.csv\
				data/raw/post_dissection_metadata_RAW_2021-02-18.csv\
				results/figures/VertNet_Arctos.pdf\
				results/figures/generation_phenotypes.pdf\
				results/figures/weekly_phenotypes.pdf\
				results/figures/RXNs.pdf\
				submission/AmNat.csl\
				submission/references.bib
	R -e 'library(rmarkdown); render("submission/Ballinger_et_al_2021_AmNat.Rmd", output_format="all")'

submission/manuscript_working.pdf submission/manuscript_working.docx: submission/manuscript_working.Rmd\
			data/raw/weekly_metadata_RAW_2021-02-11.csv
	R -e 'library(rmarkdown); render("submission/manuscript_working.Rmd", output_format="all")'