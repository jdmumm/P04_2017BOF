SELECT DISTINCT

EVENT_ID,
POT_ID, 
  
  NZ(
  NZ(DSum("[FREQUENCY]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND CARAPACE_LENGTH_MM >= 32"),0)
	/
	NZ(DSum("[FREQUENCY]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND CARAPACE_LENGTH_MM >= -1"),0),0)
	AS propLgCnt_awl,
	
  NZ(
  NZ(DSum("[KG]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND CARAPACE_LENGTH_MM >= 32"),0)
	/
	NZ(DSum("[KG]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND CARAPACE_LENGTH_MM >= -1"),0),0)
	AS propLgWt_awl	

FROM 
CPP_4