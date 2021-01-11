

VertNet_Mus_specimen_20201013 is all Mus specimens downloaded from VertNet. Query search: vntype:specimen genus:Mus. Query records: 62139.
	'forever deleted' the following entries:
		a. deleted fossil and occurence 'basis of record' (only kept Preserved Specimens)
		b. deleted the following continents: ND, Africa, Asia, Zoo
		c. deleted all island groups
		d. deleted any entries that had no data
		e. deleted all entries without latitude/longitude data
		f. deleted all entries that were 'founders of colonies', as these animals were apart of a lab colony
		g. deleted all entries described as 'road kill'
		h. deleted all entries that had multiple entries listed for one
		i. deleted all entries associated with Panama, as they were part of the 'Panama Hantavirus Project'



data %>%
	rename(column1=renamedcolumn1)