SELECT 
CPP_3.YEAR,
CPP_3.EVENT_ID,
CPP_3.SITE_ID,
CPP_3.STATION,
CPP_3.POT_ID,
CPP_3.SAMPLE_POT,
CPP_3.all_Cnt as all_Cnt_cc,
CPP_3.all_Kg as all_Kg_cc,

NZ(	[CPP_3.all_Cnt] * [CPP_5.propLgCnt_awl],
	0) AS lrg_Cnt,

Round(NZ(
	[CPP_3.all_Kg] * [CPP_5.propLgWt_awl],
	0),3) AS lrg_Kg

INTO CPP 

FROM (CPP_3
	LEFT JOIN
		CPP_5 ON (CPP_3.POT_ID = CPP_5.POT_ID)
		 AND (CPP_3.EVENT_ID = CPP_5.EVENT_ID))
;