SELECT 
DATA_POT_PERFORMANCE_PWS_SPOTS.Year,
DATA_POT_PERFORMANCE_PWS_SPOTS.EVENT_ID,
DATA_POT_PERFORMANCE_PWS_SPOTS.SITE_ID,
DATA_POT_PERFORMANCE_PWS_SPOTS.STATION,
DATA_POT_PERFORMANCE_PWS_SPOTS.POT_ID,
DATA_POT_PERFORMANCE_PWS_SPOTS.SAMPLE_POT

FROM DATA_POT_PERFORMANCE_PWS_SPOTS
WHERE (((DATA_POT_PERFORMANCE_PWS_SPOTS.SAMPLE_POT)="sample"));