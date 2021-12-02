/*
INT615 - Care Home Insight
Care Home Address Data Preparation
Version 1.0
Created by Steven Buckley
Created on 26-11-2021

AMENDMENTS:
	--DATE------:--NAME-------------:--DETAILS-------------------------------------------------------------------------------------
    26-11-2021  :   Steven Buckley  :   Initial script creation
    

DESCRIPTION:
    NHS Prescription data does not identify whether or not the patient is a care home resident.
    It is proposed to infer this based on whether or not the patient's addess information aligns with a care home property.
    
    The are multiple potential sources of address classification but we will focus on two:
        AddressBasePlus
        Care Quality Commission (CQC)
    
    The AddressBasePlus includes classification for care homes (via data sharing with CQC).
    
    
    The CQC data can be limited to care homes that were active during 2020/21.
    As the CQC data includes UPRN information for some records this may be able to be used to update AddressBase where these UPRNs are not classified as care homes by AddressBase
    Some UPRNs and Addresses in the CQC data can be associated with multiple care home locations, where this occurs the "max" of the classifications can be used to identify type
    
        
DEPENDENCIES:
        DALL_REF.INT615_CQC_CAREHOMES   :   CQC data extracted via the API showing care home location information
        DAL_REF.ADDRESSBASE_PLUS_FLAT   :   AddressBasePlus data at a UPRN level with multiple single line address combinations for each UPRN
                                        :   Each record is a distinct UPRN (for an extract date) with a distinct classification
                                        :   EXTRACT_DATE is used to identify the period the data aligns with
OUTPUTS:
        INT615_AB_PLUS_CQC_BASE_TMP     :   temporary address level dataset including addresses from both AddressBasePlus and CQC
                                        :   limited to addresses at postcodes where a care home can be identified
        INT615_AB_PLUS_CQC_TOKENS       :   tokenised records for each address record of interest
        INT615_AB_PLUS_CQC_BASE         :   final subset of carehome address data limited to addresses at same postcode as identifed care homes
        
*/

------------------------------------------------------------------------------------------------------------------------------------------------------
--IDENTIFY POTENTIAL CARE HOME LOCATION RECORDS-------------------------------------------------------------------------------------------------------

drop table INT615_AB_PLUS_CQC_BASE_TMP;
create table INT615_AB_PLUS_CQC_BASE_TMP compress for query high as
with
-----SECTION START: CQC UPRN CLASSIFICATION: ACTIVE LOCATIONS (2020/21)-------------------------------------------------------------------------------
--for any UPRNs that are linked to care home locations that were active in 2020/21 take the max of the classifications
--if there is only one location this will reflect that locations classification
--if there are multiple active locations this will flag as Y if any of them had a Y
active_cqc_uprn_classification as
(
select      UPRN,
            max(NURSINGHOME)        as NURSING_HOME_FLAG,
            max(RESIDENTIALHOME)    as RESIDENTIAL_HOME_FLAG
from        DALL_REF.INT615_CQC_CAREHOMES
where       1=1
    and     CAREHOME = 'Y'
    and     to_date((case when REGISTRATIONDATE='NULL' then null else REGISTRATIONDATE end) ,'YYYY-MM-DD') <= to_date(20210331,'YYYYMMDD')
    and     (   DEREGISTRATIONDATE = 'NULL'
            or  to_date((case when DEREGISTRATIONDATE='NULL' then null else DEREGISTRATIONDATE end),'YYYY-MM-DD') >= to_date(20200401,'YYYYMMDD')
            )
group by    UPRN
)
--select * from active_cqc_uprn_classification;
-----SECTION END: CQC UPRN CLASSIFICATION: ACTIVE LOCATIONS (2020/21)---------------------------------------------------------------------------------

,

-----SECTION START: CREATE SINGLE RECORD PER ADDRESS FOR CQC CARE HOME DATA (ACTIVE LOCATIONS 2020/21)------------------------------------------------
--  within the CQC care home data there are some instances when the same address links to multiple care homes, which may occur when:
--       multiple care homes operate out of the same location
--       some care homes have been deregistered and new care homes registered
--the main aim of address matching to CQC data is to see if the location is a Nursing or Residential home so we can identify this for each address
clean_cqc_data as
(
--prior to working with the CQC data perform basic formatting to combine and clean the address data
select      ID,
            trim(replace(regexp_replace(replace(regexp_replace(regexp_replace(regexp_replace(regexp_replace(cqc.CH_ADDRESS, '[,.();:#*]', ' '), --replace special characters with a single space
                                                                                                            '['']',''),                         --remove hyphen but don't include space 
                                                                                                            '(\d)(\D)', '\1 \2'),               --add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
                                                                                                            '(\D)(\d)', '\1 \2'),               --add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
                                                                                                            '&', ' and '),                      --replace the ampersand character with the string "and"
                                                                                                            '( ){2,}', ' '),                    --replace and multiple spaces with a single space
                                                                                                            ' - ','-')                          --remove any spaces around a hyphen
                                                                                                            ) as CH_ADDRESS,
            CH_POSTCODE_RAW,
            CH_POSTCODE,
            NURSINGHOME     as NURSING_HOME_FLAG,
            RESIDENTIALHOME as RESIDENTIAL_HOME_FLAG
from        (
            select      ID,
                        upper(NAME||', '||POSTALADDRESSLINE1||', '||POSTALADDRESSTOWNCITY)  as CH_ADDRESS,
                        POSTALCODE                                                          as CH_POSTCODE_RAW,
                        upper(replace(POSTALCODE, ' ', ''))                                 as CH_POSTCODE,
                        NURSINGHOME,
                        RESIDENTIALHOME
            from        DALL_REF.INT615_CQC_CAREHOMES
            where       1=1
                and     CAREHOME = 'Y'
                and     to_date((case when REGISTRATIONDATE='NULL' then null else REGISTRATIONDATE end) ,'YYYY-MM-DD') <= to_date(20210331,'YYYYMMDD')
                and     (   DEREGISTRATIONDATE = 'NULL'
                        or  to_date((case when DEREGISTRATIONDATE='NULL' then null else DEREGISTRATIONDATE end),'YYYY-MM-DD') >= to_date(20200401,'YYYYMMDD')
                        )
            )   cqc
)
--select * from clean_cqc_data;
,

active_cqc_address_classification as 
(
--for any combinations of Postcode/Address that are linked to care home locations that were active in 2020/21 take the max of the classifications
--if there is only one location this will reflect that locations classification
--if there are multiple active locations this will flag as Y if any of them had a Y
select      'CQC-'||rownum              as CQC_ADDRESS_ID,
            CH_ADDRESS,
            CH_POSTCODE_RAW,
            CH_POSTCODE,
            NURSING_HOME_FLAG,
            RESIDENTIAL_HOME_FLAG
from        (
            select      CH_ADDRESS,
                        CH_POSTCODE_RAW,
                        CH_POSTCODE,
                        max(NURSING_HOME_FLAG)      as NURSING_HOME_FLAG,
                        max(RESIDENTIAL_HOME_FLAG)  as RESIDENTIAL_HOME_FLAG
            from        clean_cqc_data
            group by    CH_ADDRESS,
                        CH_POSTCODE_RAW,
                        CH_POSTCODE
            )
)
--select * from active_cqc_address_classification;
-----SECTION END: CREATE SINGLE RECORD PER ADDRESS FOR CQC CARE HOME DATA (ACTIVE LOCATIONS 2020/21)--------------------------------------------------

,

-----SECTION START: SUPPLEMENT ADDRESSBASEPLUS WITH CQC LOCATION TYPE FLAGS---------------------------------------------------------------------------
--CQC includes identifiers for the type of care home (NURSING_HOME_FLAG & RESIDENTIAL_HOME_FLAG)
--these can be appended based on linking via the UPRN across both datasets
--      linking should only be performed where the AddressBasePlus classification already identifies a care home
--      analysis of records from CQC with UPRNs that match to non-carehome properties in AddressBase suggests these should be excluded
--
-- limit the AddressBasePlus data to the extract from March 2021
ab_supplement as
(
select      ab.*,
            acuc.NURSING_HOME_FLAG,
            acuc.RESIDENTIAL_HOME_FLAG
from        DALL_REF.ADDRESSBASE_PLUS_FLAT  ab
left join   active_cqc_uprn_classification  acuc    on to_char(ab.UPRN) = to_char(acuc.UPRN)
where       1=1
    and     ab.EXTRACT_DATE = '2021-03-15'
)
--select * from ab_supplement;
-----SECTION END: SUPPLEMENT ADDRESSBASEPLUS WITH CQC LOCATION TYPE FLAGS-----------------------------------------------------------------------------

,

-----SECTION START: STACK SINGLE LINE ADDRESS INTO SEPERATE RECORDS-----------------------------------------------------------------------------------
--where UPRNs in AddressBasePlus have multiple potential addresses, split these into seperate records
--also include any addresses from CQC data
--
--identify a unique record for each distinct postcode and address combination
--  13,704 distinct POSTCODE/ADDRESS combinations have 2 or more records (e.g. POSTCODE = 'W87AW' and ADDRESS = 'LAURIE HOUSE 16 AIRLIE GARDENS LONDON')
--  priority should be given to ...
stack_address as
(
select      ADDRESS_ID,
            CAREHOME_FLAG,
            POSTCODE,
            ADDRESS,
            NURSING_HOME_FLAG,
            RESIDENTIAL_HOME_FLAG,
        --apply address prioritisation
            rank() over (partition by POSTCODE, ADDRESS order by CAREHOME_FLAG desc, ADDRESS_ID) as ADDRESS_RANK
from        (
            select  to_char(CQC_ADDRESS_ID) as ADDRESS_ID, 1 as CAREHOME_FLAG, CH_POSTCODE as POSTCODE, CH_ADDRESS as ADDRESS, NURSING_HOME_FLAG, RESIDENTIAL_HOME_FLAG from active_cqc_address_classification
            union all
            select  to_char(UPRN) as ADDRESS_ID, CAREHOME_FLAG, AB_POSTCODE, GEO_SINGLE_LINE_ADDRESS, NURSING_HOME_FLAG, RESIDENTIAL_HOME_FLAG from ab_supplement where GEO_SINGLE_LINE_ADDRESS is not null
            union all
            select  to_char(UPRN) as ADDRESS_ID, CAREHOME_FLAG, AB_POSTCODE, DPA_SINGLE_LINE_ADDRESS, NURSING_HOME_FLAG, RESIDENTIAL_HOME_FLAG from ab_supplement where DPA_SINGLE_LINE_ADDRESS is not null
            union all
            select  to_char(UPRN) as ADDRESS_ID, CAREHOME_FLAG, AB_POSTCODE, CORE_SINGLE_LINE_ADDRESS, NURSING_HOME_FLAG, RESIDENTIAL_HOME_FLAG from ab_supplement where CORE_SINGLE_LINE_ADDRESS is not null
            )
group by    ADDRESS_ID,
            CAREHOME_FLAG,
            POSTCODE,
            ADDRESS,
            NURSING_HOME_FLAG,
            RESIDENTIAL_HOME_FLAG
)
--select * from stack_address;
-----SECTION END: STACK SINGLE LINE ADDRESS INTO SEPERATE RECORDS-------------------------------------------------------------------------------------



-----OUTPUT-------------------------------------------------------------------------------------------------------------------------------------------
--select distinct records for combinations of UPRN and postcode/address
--flag which addreses are carehomes
--where a UPRN has multiple entries create a sub_id
select      ADDRESS_ID,
            CAREHOME_FLAG,
            POSTCODE,
            ADDRESS,
            NURSING_HOME_FLAG,
            RESIDENTIAL_HOME_FLAG,
        --create unique ID as some ADDRESS_ID may have more than one POSTCODE/ADDRESS combination
            rownum as ADDRESS_LINE_ID
from        stack_address
where       1=1
    and     ADDRESS_RANK = 1
    --limit to records at postcodes aligned to carehomes
    and     POSTCODE in (select POSTCODE from stack_address where CAREHOME_FLAG = 1)
;

--IDENTIFY POTENTIAL CARE HOME LOCATION RECORDS-------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------
--TOKENISE ADDRESS DATA-------------------------------------------------------------------------------------------------------------------------------
-- Using the distinct address information split the address into distinct tokens that can be used for string matching

drop table INT615_AB_PLUS_CQC_TOKENS;
create table INT615_AB_PLUS_CQC_TOKENS compress for query high as

with
-----SECTION START: Split address into individual tokens----------------------------------------------------------------------------------------------
--split the address into individual records for each individual string in an address
-- e.g. "42 ALBANY LANE BALDALL COMMON COVENTRY" becomes six distinct records 42 | ALBANY | LANE | BALDALL | COMMON | COVENTRY
plus_tokens as (
select  /*+ materialize */
            ADDRESS_ID,
            ADDRESS_LINE_ID,
            POSTCODE,
            trim(regexp_substr(ADDRESS,'[^ ]+', 1, lines.column_value))  as  ADDRESS_TKN
from        INT615_AB_PLUS_CQC_BASE_TMP,
            table(cast(multiset(select level from DUAL connect by instr(ADDRESS, ' ', 1, level - 1) > 0)  as  sys.odciNumberList))  lines
)
--select * from plus_tokens;
-----SECTION END: Split address into individual tokens------------------------------------------------------------------------------------------------


-----OUTPUT-------------------------------------------------------------------------------------------------------------------------------------------
--not all words are of equal value when matching addresses
--for example, on most residential streets the main difference will be the house number
--therefore any numeric values need to be highlighted so that they can be weighted in the scoring process
select      pt.*,
            case when regexp_like(pt.ADDRESS_TKN, '[0-9]') then 1 else 0 end as  INT_FLAG
from        plus_tokens  pt
where       1=1
;

--TOKENISE ADDRESS DATA-------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------------------------------------
--FORMATTED ADDRESS RECORDS---------------------------------------------------------------------------------------------------------------------------
-- To support scoring it may be useful to supplement the address records with basic information about the number of tokens (both character and numeric)

-- NOTE: TAKES ~1 MINS
drop table INT615_AB_PLUS_CQC_BASE;
create table INT615_AB_PLUS_CQC_BASE compress for query high as 

with
-----SECTION START: Count number of tokens per address------------------------------------------------------------------------------------------------
--for each unique address count the number of individual tokens
plus_count as
(
select      ADDRESS_LINE_ID,
            sum(case when regexp_like(ADDRESS_TKN, '[0-9]') then 1 else 0 end)  as  INT_COUNT,
            sum(case when regexp_like(ADDRESS_TKN, '[0-9]') then 0 else 1 end)  as  CHAR_COUNT,
            sum(1) as TOTAL_COUNT
from        INT615_AB_PLUS_CQC_TOKENS
where       1=1
group by    ADDRESS_LINE_ID
)
--select * from plus_count;
-----SECTION END: Count number of tokens per address--------------------------------------------------------------------------------------------------

-----OUTPUT-------------------------------------------------------------------------------------------------------------------------------------------
select      ab.*,
            pc.INT_COUNT,
            pc.CHAR_COUNT,
            pc.TOTAL_COUNT
from        INT615_AB_PLUS_CQC_BASE_TMP     ab
inner join  plus_count                      pc  on  ab.ADDRESS_LINE_ID  =   pc.ADDRESS_LINE_ID
;

--FORMATTED ADDRESS RECORDS---------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------