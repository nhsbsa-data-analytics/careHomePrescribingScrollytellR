/*
INT615 - Care Home Insight
Additional Matching and Results
Version 1.0
Created by Adnan Shroufi
Created on 09-11-2021
AMENDMENTS:
	--DATE------:--NAME-------------:--DETAILS-------------------------------------------------------------------------------------
    12-11-2021  :   Steven Buckley  :   Reviewed code base, adding some commentary
                                    :   Added additional limitations to the "patient_count_match" section
                                    :       only include properties that share a postcode with a care home location in AddressBase
                                    :       T.B.C (currently not applied) limit to properties with 5+ patients aged 65+ in x months
    26-11-2021  :   Steven Buckley  :   Updated code to reflect amended to address matching where CQC data was incorporated
                                    :   As CQC data is included in the string matching this can be removed from this script
                                        
DESCRIPTION:
    NHS Prescription data does not identify whether or not the patient is a care home resident.
    It is proposed to infer this based on whether or not the patient's addess information aligns with a care home property.
    The care home properties are to be identified based on data stored in AddressBasePlus (maintained by Ordnance Survey).
    
    Following address matching some address will be able to be mapped to care home locations.
    However, some care home addresses may have been missed, although they could be identified using additional checks:
        +   Keyword matching can identify addresses containing words that would strongly suggest a care home property
        +   Patient count matching can identify addresses supplied by multiple distinct elderly patients in a single month, suggesting a care home
    
    The results of each matching stage can be fed back to identify a single outcome for each address supplied by a patient
    
    These results can be linked back to the precsription item level data to flag, for each record, if the patient is inferred to be a care home patient or not
    
DEPENDENCIES:
        INT615_MATCH                :   Address level match results for each unique patient address
        INT615_PRESC_ADDRESS_BASE   :   unique patient address level dataset, including summary of token information
        INT615_PRESC_BASE           :   prescription form level dataset including cleansed address information
        
        
OUTPUTS:
        INT615_RESULTS  :   Final output dataset at individual prescription item level
                        :   Each prescription item will be supplement with any care home flag information
        
*/

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
-- NOTE: TAKES ~10 MINS


drop table INT615_RESULTS;
create table INT615_RESULTS compress for query high as

with

-----SECTION START: Identify care home address matches------------------------------------------------------------------------------------------------
--following the address matching processes identify which patient addresses can be matched to a care home property
--exclude any addresses containing anything that may strongly suggest the property is not a care home for the elderly
address_match as
(
select  /*+ materialize */
            ADDRESS_RECORD_ID,
            CAREHOME_FLAG,
            NURSING_HOME_FLAG,
            RESIDENTIAL_HOME_FLAG,
            ADDRESS_ID  as CH_ADDRESS_ID,
            MATCH_TYPE
from        INT615_MATCH
where       1=1
    and     CAREHOME_FLAG = 1
    --the exclusion list can be less restrictive at this point as the address matching has suggested that this is a care home
    and     regexp_instr(PAT_ADDRESS, 'CHILDREN|MOBILE|ABOVE|CARAVAN|RESORT|HOLIDAY|NO FIXED ABODE') = 0
)
--select * from address_match;
-----SECTION END: Identify care home address matches--------------------------------------------------------------------------------------------------

,

-----SECTION START: Address keyword match-------------------------------------------------------------------------------------------------------------
--for any patient addresses that did not match based on address matching to AddressBase
--identify if the patient address contains words that strongly suggest a care home property
--exclude any addresses containing anything that may strongly suggest the property is not a care home for the elderly
keyword_match as
(
select  /*+ materialize */
            ADDRESS_RECORD_ID,
            1           as CAREHOME_FLAG,
            null        as NURSING_HOME_FLAG,
            null        as RESIDENTIAL_HOME_FLAG,
            null        as CH_ADDRESS_ID,
            'KEY_WORD'  as MATCH_TYPE
from        INT615_MATCH
where       1=1
    and     CAREHOME_FLAG = 0
    and     regexp_instr(PAT_ADDRESS, 'NURSING HOME|NURSING-HOME|RESIDENTIAL HOME|RESIDENTIAL-HOME|REST HOME|REST-HOME|CARE HOME|CARE-HOME') != 0
    --the exclusion list needs to be more restrictive at this point as there is no other support that this is a care home
    and     regexp_instr(PAT_ADDRESS, 'CHILDREN|MOBILE|ABOVE|CARAVAN|RESORT|CONVENT|MONASTERY|HOLIDAY|MARINA|RECOVERY|HOSPITAL|NO FIXED ABODE') = 0
)
--select * from keyword_match;
-----SECTION END: Address keyword match---------------------------------------------------------------------------------------------------------------

,

-----SECTION START: Patient count match-------------------------------------------------------------------------------------------------------------
--for any patient addresses that did not match based on address matching to AddressBase
--identify if the address was used by 5 or more patients aged 65 plus in a single month
--exclude any addresses containing words that strongly suggest a care home property (picked up in previous sub-query)
--exclude any addresses containing anything that may strongly suggest the property is not a care home for the elderly
patient_count_match as
(
select  /*+ materialize */
            m.ADDRESS_RECORD_ID,
            1           as CAREHOME_FLAG,
            null        as NURSING_HOME_FLAG,
            null        as RESIDENTIAL_HOME_FLAG,
            null        as CH_ADDRESS_ID,
            'PATIENT_COUNT'  as MATCH_TYPE
from        INT615_MATCH                m
left join   INT615_PRESC_ADDRESS_BASE   pab     on  m.ADDRESS_RECORD_ID =   pab.ADDRESS_RECORD_ID
where       1=1
    and     m.CAREHOME_FLAG = 0
    and     pab.MAX_MONTHLY_PATIENTS >= 5
    --and     pab.MONTHS_5PLUS_PATIENTS >= 3  : to be confirmed
    and     m.PAT_POSTCODE in (select AB_POSTCODE from INT615_AB_PLUS_BASE)
    and     regexp_instr(m.PAT_ADDRESS, 'NURSING HOME|NURSING-HOME|RESIDENTIAL HOME|RESIDENTIAL-HOME|REST HOME|REST-HOME|CARE HOME|CARE-HOME') = 0
    --the exclusion list needs to be more restrictive at this point as there is no other support that this is a care home
    and     regexp_instr(m.PAT_ADDRESS, 'CHILDREN|MOBILE|ABOVE|CARAVAN|RESORT|CONVENT|MONASTERY|HOLIDAY|MARINA|RECOVERY|HOSPITAL|NO FIXED ABODE') = 0
)
--select * from patient_count_match;
-----SECTION END: Address Keyword Match---------------------------------------------------------------------------------------------------------------

,

-----SECTION START: Link results back to prescription forms-------------------------------------------------------------------------------------------
--link all the matching results back to the prescription form level table
--for each form this will identify if a care home match was possible and the details of any match
--use COALESCE to pick the first applicable match (although each ADDRESS_RECORD_ID will only appear one of the match result sets)
form_match_results as
(
select      pb.PF_ID,
            pb.ADDRESS_RECORD_ID,
            coalesce(am.CAREHOME_FLAG, km.CAREHOME_FLAG, pcm.CAREHOME_FLAG, 0)                      as CH_FLAG,
            coalesce(am.NURSING_HOME_FLAG, km.NURSING_HOME_FLAG, pcm.NURSING_HOME_FLAG)             as NURSING_HOME_FLAG,
            coalesce(am.RESIDENTIAL_HOME_FLAG, km.RESIDENTIAL_HOME_FLAG, pcm.RESIDENTIAL_HOME_FLAG) as RESIDENTIAL_HOME_FLAG,
            coalesce(am.CH_ADDRESS_ID, km.CH_ADDRESS_ID, pcm.CH_ADDRESS_ID)                         as CH_ADDRESS_ID,
            coalesce(am.MATCH_TYPE, km.MATCH_TYPE, pcm.MATCH_TYPE, 'NO_MATCH')                      as MATCH_TYPE
from        INT615_PRESC_BASE   pb
left join   address_match       am  on  pb.ADDRESS_RECORD_ID    =   am.ADDRESS_RECORD_ID
left join   keyword_match       km  on  pb.ADDRESS_RECORD_ID    =   km.ADDRESS_RECORD_ID
left join   patient_count_match pcm on  pb.ADDRESS_RECORD_ID    =   pcm.ADDRESS_RECORD_ID
)
--select * from form_match_results;
-----SECTION END: Link results back to prescription forms---------------------------------------------------------------------------------------------

-----OUTPUT-------------------------------------------------------------------------------------------------------------------------------------------
select      fact.YEAR_MONTH,
            fact.EPS_PART_DATE,
            fact.EPM_ID,
            fact.PF_ID,
            fact.PRESC_TYPE_PRNT,
            fact.PRESC_ID_PRNT,
            fact.ITEM_COUNT,
            fact.ITEM_PAY_DR_NIC,
            fact.ITEM_CALC_PAY_QTY,
            fact.PDS_GENDER,
            fact.CALC_AGE,
            fact.NHS_NO,
            -- Take PATIENT_ADDR_POSTCODE first and then the derived postcode
            -- Some postcodes will still however remain as null 
            replace(coalesce(fact.PATIENT_ADDR_POSTCODE, ppr.POSTCODE), ' ','') as PCD_NO_SPACES,
            fact.PATIENT_ADDR_POSTCODE,
            fact.CALC_PREC_DRUG_RECORD_ID,
            fact.EPS_FLAG,
            nvl(fmr.CH_FLAG,0) as CH_FLAG,
            fmr.NURSING_HOME_FLAG,
            fmr.RESIDENTIAL_HOME_FLAG,
            fmr.CH_ADDRESS_ID,
            fmr.ADDRESS_RECORD_ID,
            nvl(fmr.MATCH_TYPE, 'NO_MATCH') as MATCH_TYPE
from        SB_AML.PX_FORM_ITEM_ELEM_COMB_FACT  fact  
left join   form_match_results                  fmr     on  fact.PF_ID      = fmr.PF_ID
left join   DALL_REF.INT615_PAPER_PFID_ADDRESS_20_21 ppr on fact.YEAR_MONTH = ppr.YEAR_MONTH
                                                            and fact.PF_ID  = ppr.PF_ID
where       1=1
    and     fact.YEAR_MONTH in (202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103)
    and     fact.PATIENT_IDENTIFIED = 'Y'
	and     fact.CALC_AGE >= 65
    and     fact.PAY_DA_END   = 'N' -- excludes disallowed items
	and     fact.PAY_ND_END   = 'N' -- excludes not dispensed items
	and     fact.PAY_RB_END   = 'N' -- excludes referred back items
	and     fact.CD_REQ       = 'N' -- excludes controlled drug requisitions 
	and     fact.OOHC_IND     = 0   -- excludes out of hours dispensing
	and     fact.PRIVATE_IND  = 0   -- excludes private dispensers
	and     fact.IGNORE_FLAG  = 'N' -- excludes LDP dummy forms
;

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------