SELECT
DATA_CATCHCOMP_PWS_SPOTS.EVENT_ID,
DATA_CATCHCOMP_PWS_SPOTS.POT_ID,
Sum(DATA_CATCHCOMP_PWS_SPOTS.COUNT) AS all_Cnt,
Sum(SAMPLE_WEIGHT_KG ) AS all_Kg

FROM DATA_CATCHCOMP_PWS_SPOTS

WHERE (((DATA_CATCHCOMP_PWS_SPOTS.FK_SPECIES_CODE)="965")
		 AND DATA_CATCHCOMP_PWS_SPOTS.CONDITION_CODE = "01")

GROUP BY 
	DATA_CATCHCOMP_PWS_SPOTS.EVENT_ID,
	DATA_CATCHCOMP_PWS_SPOTS.POT_ID;
