SELECT 
DATA_AWL_SHELLFISH_PWS_SPOT_SURVEY.EVENT_ID,
DATA_AWL_SHELLFISH_PWS_SPOT_SURVEY.POT_ID,
Count(DATA_AWL_SHELLFISH_PWS_SPOT_SURVEY.CARAPACE_LENGTH_MM) AS CountOfCARAPACE_LENGTH_MM

FROM DATA_AWL_SHELLFISH_PWS_SPOT_SURVEY

WHERE (((DATA_AWL_SHELLFISH_PWS_SPOT_SURVEY.FK_SPECIES_CODE)="965"))

GROUP BY DATA_AWL_SHELLFISH_PWS_SPOT_SURVEY.EVENT_ID, DATA_AWL_SHELLFISH_PWS_SPOT_SURVEY.POT_ID;
