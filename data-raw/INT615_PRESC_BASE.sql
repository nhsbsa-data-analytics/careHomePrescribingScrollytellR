/*
INT615 - Care Home Insight
Process Prescription Data
Version 1.0

Created by Adnan Shroufi
Created on 09-11-2021

AMENDMENTS:
	--DATE------:--NAME-------------:--DETAILS-------------------------------------------------------------------------------------
    11-11-2021  :   Steven Buckley  :   Reviewed code base, adding some commentary
                                    :   Combined seperate PDS stages into single sub-query
                                    :   Combined seperate ETP stages into single sub-query
                                    :   Removed age filter from ETP SCD section as this excluded a small number of patients with updated data in the fact table
                                    :   Added an additional step to produce an address level dataset of unique patient addresses
    


DESCRIPTION:
    NHS Prescription data does not identify whether or not the patient is a care home resident.
    It is proposed to infer this based on whether or not the patient's addess information aligns with a care home property.
    The care home properties are to be identified based on data stored in AddressBasePlus (maintained by Ordnance Survey).
    
    Prior to matching data to AddressBasePlus the patient information needs to be identified from NHS Prescription data.
    For performance, prescription records will initially be limited to only include records where:
        +   the patient is identified as being aged 65+
        +   the patient's captured address includes a postcode which aligns with a care home property
    
    Once address information has been identified the address will be split into individual tokens so that each part of the address can be matched
    Tokenising the address will removed issues where the same address has been captured but with words missing or in different orders


DEPENDENCIES:
        SB_AML.PX_FORM_ITEM_ELEM_COMB_FACT          :   prescription item level data for NHS prescriptions
        DALL_REF.INT615_PAPER_PFID_ADDRESS_20_21    :   dataset linking patient information to paper prescription records
                                                    :   patient information is based on the closest aligned data for the patients from PDS & ETP
        SCD2.SCD2_ETP_DY_PAYLOAD_MSG_DATA           :   copy of raw ETP prescription data, including patient level information
        
        INT615_AB_PLUS_BASE                     	:   Subset of AddressBasePlus data limited to addresses at same postcode as identifed care homes
        

OUTPUTS:
        INT615_PRESC_BASE           :   prescription form level dataset including cleansed address information
        INT615_PRESC_TOKENS         :   tokenised records for each unique patient address
        INT615_PRESC_ADDRESS_BASE   :   unique patient address level dataset, including summary of tokenn information

*/

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
-- PROCESS PRESCRIPTION DATA
-- Extract the form level information from NHS Prescription data
-- Limit records to patients aged 65+
-- Cleanse the address information to remove any non-alphanumeric characters

-- NOTE: TAKES ~40 MINS

drop table INT615_PRESC_BASE;
create table INT615_PRESC_BASE compress for query high as 

with

-----SECTION START: Paper Prescribing Dataset---------------------------------------------------------------------------------------------------------
--pull the address information for all paper prescribing relating to patients aged 65+
pds_fact as (
select  /*+ materialize */
            pt.YEAR_MONTH,
            pt.PF_ID,
            pt.NHS_NO,
            pt.ADDRESS,
            pt.POSTCODE
from        DALL_REF.INT615_PAPER_PFID_ADDRESS_20_21    pt
inner join  SB_AML.PX_FORM_ITEM_ELEM_COMB_FACT          fact    on pt.pf_id  =  fact.pf_id
where       1=1
    --limit the paper address record data to 2020/21
    and     pt.YEAR_MONTH in (202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103)
    --limit the main fact data to paper prescribing for 2020/21 and identified patients aged 65+
    and     fact.YEAR_MONTH in (202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103)
    and     fact.PATIENT_IDENTIFIED = 'Y'
	and     fact.EPS_FLAG     = 'N'
    and     fact.CALC_AGE     >= 65
    --standard exclusions for fact table
    and     fact.PAY_DA_END   = 'N' -- excludes disallowed items
	and     fact.PAY_ND_END   = 'N' -- excludes not dispensed items
	and     fact.PAY_RB_END   = 'N' -- excludes referred back items
	and     fact.CD_REQ       = 'N' -- excludes controlled drug requisitions 
	and     fact.OOHC_IND     = 0   -- excludes out of hours dispensing
	and     fact.PRIVATE_IND  = 0   -- excludes private dispensers
	and     fact.IGNORE_FLAG  = 'N' -- excludes LDP dummy forms
)
--select * from pds_fact
-----SECTION END: Paper Prescribing Dataset-----------------------------------------------------------------------------------------------------------

,

-----SECTION START: ETP Prescribing Dataset-----------------------------------------------------------------------------------------------------------
--pull the address information for all ETP prescribing relating to patients aged 65+
etp_fact as (
select  /*+ materialize */
            fact.YEAR_MONTH,
            fact.PF_ID,
            fact.NHS_NO,
            upper(etp.PAT_ADDRESS_LINE1||' '||etp.PAT_ADDRESS_LINE2||' '||etp.PAT_ADDRESS_LINE3||' '||etp.PAT_ADDRESS_LINE4)   as  ADDRESS,
            etp.PAT_ADDRESS_POSTCODE    as POSTCODE
from        SCD2.SCD2_ETP_DY_PAYLOAD_MSG_DATA@dwcpb     etp
inner join  SB_AML.PX_FORM_ITEM_ELEM_COMB_FACT          fact    on etp.EPM_ID       =   fact.EPM_ID
                                                                and etp.PART_DATE   =   fact.EPS_PART_DATE
where       1=1
    --limit the etp record data to 2020/21
    and     substr(etp.part_date, 1, 6) in (202003, 202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103, 202104)
    --limit the main fact data to paper prescribing for 2020/21 and identified patients aged 65+
    and     fact.YEAR_MONTH in (202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103)
    and     fact.PATIENT_IDENTIFIED = 'Y'
	and     fact.EPS_FLAG     = 'Y'
    and     fact.CALC_AGE     >= 65
    --standard exclusions for fact table
    and     fact.PAY_DA_END   = 'N' -- excludes disallowed items
	and     fact.PAY_ND_END   = 'N' -- excludes not dispensed items
	and     fact.PAY_RB_END   = 'N' -- excludes referred back items
	and     fact.CD_REQ       = 'N' -- excludes controlled drug requisitions 
	and     fact.OOHC_IND     = 0   -- excludes out of hours dispensing
	and     fact.PRIVATE_IND  = 0   -- excludes private dispensers
	and     fact.IGNORE_FLAG  = 'N' -- excludes LDP dummy forms
)
--select * from etp_fact
-----SECTION END: ETP Prescribing Dataset-------------------------------------------------------------------------------------------------------------

,

-----SECTION START: Combine Paper and ETP datasets----------------------------------------------------------------------------------------------------
--combine the Paper and ETP data, and format the address and postcode strings
pds_etp as
(
select  /*+ materialize */
            YEAR_MONTH,
            PF_ID,
            NHS_NO,
            trim(replace(regexp_replace(replace(regexp_replace(regexp_replace(regexp_replace(ADDRESS,
                                                                                            '[,.();:#'']', ' '),    --replace special characters with a single space
                                                                                            '(\d)(\D)', '\1 \2'),   --add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
                                                                                            '(\D)(\d)', '\1 \2'),   --add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
                                                                                            '&', ' and '),          --replace the ampersand character with the string "and"
                                                                                            '( ){2,}', ' '),        --replace and multiple spaces with a single space
                                                                                            ' - ','-')              --remove any spaces around a hyphen
                                                                                            ) as PAT_ADDRESS,
            regexp_replace(upper(POSTCODE),'[^A-Z0-9]','')  as PAT_POSTCODE
from        (            
            select  distinct
                        YEAR_MONTH,
                        PF_ID,
                        NHS_NO,
                        ADDRESS,
                        POSTCODE
            from        ETP_FACT
            
            union all
            
            select  distinct
                        YEAR_MONTH,
                        PF_ID,
                        NHS_NO,
                        ADDRESS,
                        POSTCODE
            from        PDS_FACT
            )
)
--select * from pds_etp
-----SECTION END: Combine Paper and ETP datasets------------------------------------------------------------------------------------------------------


-----OUTPUT-------------------------------------------------------------------------------------------------------------------------------------------
--produce the output dataset including a unique id for each distinct combination of address and postcode
select      YEAR_MONTH,
            NHS_NO,
            PF_ID,
            PAT_POSTCODE,
            PAT_ADDRESS,
            dense_rank() over(order by PAT_ADDRESS, PAT_POSTCODE)  as  ADDRESS_RECORD_ID
from        pds_etp
;

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
-- TOKENISE PRESCRIPTION ADDRESS DATA
-- Split the address information into individual records for each individual word in the address
-- Limit records to only those with a postcode that aligns to a care home as these are the only addresses that are applicable for string matching

-- NOTE: TAKES ~9 MIN

drop table INT615_PRESC_TOKENS;
create table INT615_PRESC_TOKENS compress for query high as 

with

-----SECTION START: Define care home postcodes--------------------------------------------------------------------------------------------------------
--to improve performance, as only care homes are wanting to be matched, identify which postcodes align with care home properties
ab_postcodes as (
select  /*+ materialize */
        distinct
        AB_POSTCODE
from    INT615_AB_PLUS_BASE
)
--select * from ab_postcodes;
-----SECTION END: Define care home postcodes----------------------------------------------------------------------------------------------------------

,

-----SECTION START: Identify unique addresses---------------------------------------------------------------------------------------------------------
--from the prescription form level data identify all unique combinations of postcode and address
distinct_records as (
select  /*+ materialize */
        distinct
            ADDRESS_RECORD_ID,
            PAT_POSTCODE,
            PAT_ADDRESS
from        INT615_PRESC_BASE
where       1=1
    and     PAT_ADDRESS is not null
    and     PAT_POSTCODE in (select AB_POSTCODE from ab_postcodes)
)
--select * from distinct_records;
-----SECTION END: Identify unique addresses-----------------------------------------------------------------------------------------------------------

,

-----SECTION START: Tokenise addresses into individual records per word-------------------------------------------------------------------------------
pat_tokens as (
select  /*+ materialize */
            dr.ADDRESS_RECORD_ID,
            dr.PAT_POSTCODE,
            trim(regexp_substr(dr.PAT_ADDRESS,'[^ ]+', 1, lines.column_value))  as  PAT_ADDRESS
from        distinct_records  dr,
            table(cast(multiset(select level from DUAL connect by instr(dr.PAT_ADDRESS, ' ', 1, LEVEL - 1) > 0)  as  sys.odciNumberList))  lines
order by    dr.ADDRESS_RECORD_ID
)
--select * from pat_tokens;
-----SECTION END: Tokenise addresses into individual records per word---------------------------------------------------------------------------------


-----OUTPUT-------------------------------------------------------------------------------------------------------------------------------------------
--not all words are of equal value when matching addresses
--for example, on most residential streets the main difference will be the house number
--therefore any numeric values need to be highlighted so that they can be weighted in the scoring process
select      pt.*,
            case when regexp_like(pt.PAT_ADDRESS, '[0-9]') then 1 else 0 end as  INT_FLAG
from        pat_tokens  pt
where       1=1
;

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
-- FORMATTED PRESCRIBING ADDRESS RECORDS
-- To support matching and scoring it may be useful to have a prescribing address level dataset
-- This will include supplementing the address records with basic information about the number of tokens (both character and numeric)

-- NOTE: TAKES ~9 MINS


drop table INT615_PRESC_ADDRESS_BASE;
create table INT615_PRESC_ADDRESS_BASE compress for query high as 

with
-----SECTION START: Identify unique addresses---------------------------------------------------------------------------------------------------------
--from the prescription form level data identify all unique combinations of postcode and address
distinct_records as (
select  /*+ materialize */
            ADDRESS_RECORD_ID,
            PAT_POSTCODE,
            PAT_ADDRESS,
            count(distinct(NHS_NO)) as PATIENT_COUNT
from        INT615_PRESC_BASE
where       1=1
    and     PAT_ADDRESS is not null
group by    ADDRESS_RECORD_ID,
            PAT_POSTCODE,
            PAT_ADDRESS
)
--select * from distinct_records;
-----SECTION END: Identify unique addresses-----------------------------------------------------------------------------------------------------------

,

-----SECTION START: Identify unique addresses---------------------------------------------------------------------------------------------------------
--from the prescription form level data identify all unique combinations of postcode and address
max_monthly_patients as (
select  /*+ materialize */
            ADDRESS_RECORD_ID,
            count(distinct(case when PATIENT_COUNT >= 5 then YEAR_MONTH else null end)) as MONTHS_5PLUS_PATIENTS,
            max(PATIENT_COUNT)  as MAX_MONTHLY_PATIENTS
from        (
            select      YEAR_MONTH,
                        ADDRESS_RECORD_ID,
                        count(distinct(NHS_NO)) as PATIENT_COUNT
            from        INT615_PRESC_BASE
            where       1=1
                and     PAT_ADDRESS is not null
            group by    YEAR_MONTH,
                        ADDRESS_RECORD_ID
            )
group by    ADDRESS_RECORD_ID
)
--select * from distinct_records;
-----SECTION END: Identify unique addresses-----------------------------------------------------------------------------------------------------------

,

-----SECTION START: Count number of tokens per address------------------------------------------------------------------------------------------------
--for each unique address count the number of individual tokens
pat_count as (
select  /*+ materialize */
            ADDRESS_RECORD_ID,
            sum(case when INT_FLAG = 1 then 1 else 0 end)  as  PAT_INT_COUNT,
            sum(case when INT_FLAG = 1 then 0 else 1 end)  as  PAT_CHAR_COUNT,
            sum(1) as PAT_TOTAL
from        INT615_PRESC_TOKENS
where       1=1
group by    ADDRESS_RECORD_ID
)
--select * from pat_count;
-----SECTION END: Count number of tokens per address--------------------------------------------------------------------------------------------------


-----OUTPUT-------------------------------------------------------------------------------------------------------------------------------------------
select      dr.*,
            mmp.MAX_MONTHLY_PATIENTS,
            mmp.MONTHS_5PLUS_PATIENTS,
            pc.PAT_INT_COUNT,
            pc.PAT_CHAR_COUNT,
            pc.PAT_TOTAL
from        distinct_records        dr
left join   max_monthly_patients    mmp on  dr.ADDRESS_RECORD_ID    =   mmp.ADDRESS_RECORD_ID
left join   pat_count               pc  on  dr.ADDRESS_RECORD_ID    =   pc.ADDRESS_RECORD_ID
;

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------