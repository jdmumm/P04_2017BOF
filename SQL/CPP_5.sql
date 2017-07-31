SELECT DISTINCT

EVENT_ID,
POT_ID, 

DSum("[FREQUENCY]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND CARAPACE_LENGTH_MM >= 32")
	/
	DSum("[FREQUENCY]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND CARAPACE_LENGTH_MM >= -1")
	AS propLgCnt_awl,
	
DSum("[KG]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND CARAPACE_LENGTH_MM >= 32")
	/
	DSum("[KG]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND CARAPACE_LENGTH_MM >= -1")
	AS propLgWt_awl	

FROM 
CPP_4