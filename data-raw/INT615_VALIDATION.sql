/*
INT615 - Care Home Insight
Validation Dataset
Version 1.0

Created by Adnan Shroufi
Created on 09-11-2021

AMENDMENTS:
	--DATE------:--NAME-------------:--DETAILS-------------------------------------------------------------------------------------
    12-11-2021  :   Steven Buckley  :   Reviewed code base, adding some commentary
                                    :   
                                    
    


DESCRIPTION:
    NHS Prescription data does not identify whether or not the patient is a care home resident.
    It is proposed to infer this based on whether or not the patient's addess information aligns with a care home property.
    The care home properties are to be identified based on data stored in AddressBasePlus (maintained by Ordnance Survey).
    
    Following the full matching process, some checks may be required to confirm accuracy of matching
    This dataset will show each patient address identified as a care home along with:
        +   what matching stage produced the match
        +   the details of any AddressBase property matched to


DEPENDENCIES:
        INT615_RESULTS              :   Final output dataset at individual prescription item level
                                    :   Each prescription item will be supplement with any care home flag information
        INT615_PRESC_ADDRESS_BASE   :   unique patient address level dataset, including summary of token information
        INT615_MATCH                
        

OUTPUTS:
        INT615_RESULTS              :   Final output dataset at individual prescription item level
                                    :   Each prescription item will be supplement with any care home flag information
        INT615_PRESC_ADDRESS_BASE   :   unique patient address level dataset, including summary of token information
        INT615_MATCH                :   Address level match results for each unique patient address
        
*/

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------

drop table INT615_RESULTS_VALIDATION;
create table INT615_RESULTS_VALIDATION compress for query high as

with
-----SECTION START: Match result summary--------------------------------------------------------------------------------------------------------------
--summarise the results of the matching outcomes
--identify the patient address record, any UPRN details, the match type and the form count
match_summary as
(
select      ADDRESS_RECORD_ID,
            UPRN,
            MATCH_TYPE,
            count(distinct(PF_ID)) as FORM_COUNT
from        INT615_RESULTS
where       1=1
    and     CH_FLAG = 1
group by    ADDRESS_RECORD_ID,
            UPRN,
            MATCH_TYPE
)
--select * from match_summary;
-----SECTION END: Match result summary----------------------------------------------------------------------------------------------------------------


-----OUTPUT-------------------------------------------------------------------------------------------------------------------------------------------
select      ms.ADDRESS_RECORD_ID, 
            pab.PAT_ADDRESS,
            pab.PAT_POSTCODE,
            ms.UPRN,
            m.AB_ADDRESS,
            ms.MATCH_TYPE,
            ms.FORM_COUNT,
            pab.MAX_MONTHLY_PATIENTS,
            pab.PATIENT_COUNT
from        match_summary   ms
left join   INT615_PRESC_ADDRESS_BASE   pab on  ms.ADDRESS_RECORD_ID    =   pab.ADDRESS_RECORD_ID
left join   INT615_MATCH                m   on  ms.ADDRESS_RECORD_ID    =   m.ADDRESS_RECORD_ID
                                            and m.CAREHOME_FLAG = 1 --only pull back the AddressBase information for matches to care homes
;

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------