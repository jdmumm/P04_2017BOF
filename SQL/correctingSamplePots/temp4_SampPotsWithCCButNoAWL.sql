UPDATE
temp2_aggCC
RIGHT JOIN (temp1_PPsamp 
LEFT JOIN temp3_aggAWL ON (temp1_PPsamp.POT_ID = temp3_aggAWL.POT_ID) AND (temp1_PPsamp.EVENT_ID = temp3_aggAWL.EVENT_ID)) 
ON (temp2_aggCC.POT_ID = temp1_PPsamp.POT_ID) AND (temp2_aggCC.EVENT_ID = temp1_PPsamp.EVENT_ID) 
SET temp1_PPsamp.SAMPLE_POT = "prevSample"
WHERE (((temp1_PPsamp.SAMPLE_POT)="sample") AND ((temp2_aggCC.SumOfCOUNT) Is Not Null) AND ((temp3_aggAWL.CountOfCARAPACE_LENGTH_MM) Is Null));
