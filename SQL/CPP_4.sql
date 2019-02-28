SELECT
EVENT_ID,
POT_ID,
FREQUENCY,
CARAPACE_LENGTH_MM,

IIf(IsNull(DATA_AWL_SHELLFISH_PWS_SPOT_SURVEY.WEIGHT_GRAMS), 
			Exp(-7.004908)*[DATA_AWL_SHELLFISH_PWS_SPOT_SURVEY.CARAPACE_LENGTH_MM]^2.863889,
			DATA_AWL_SHELLFISH_PWS_SPOT_SURVEY.WEIGHT_GRAMS )
			* DATA_AWL_SHELLFISH_PWS_SPOT_SURVEY.FREQUENCY
			/1000
			AS KG, 
FK_SEX_CODE			
			
			
FROM DATA_AWL_SHELLFISH_PWS_SPOT_SURVEY

WHERE ((DATA_AWL_SHELLFISH_PWS_SPOT_SURVEY.FK_SPECIES_CODE)="965")



