/*
INT615 - Care Home Insight
Process AddresBase Data
Version 1.0

Created by Adnan Shroufi
Created on 09-11-2021

AMENDMENTS:
	--DATE------:--NAME-------------:--DETAILS-------------------------------------------------------------------------------------
    11-11-2021  :   Steven Buckley  :   Reviewed code base, adding some commentary
                                    :   Removed the postcode data from the initial address string to prevent this being added and removed
                                    :   Applied the token counts to an address summary output rather than repeating them throughout the token table                                    
    


DESCRIPTION:
    NHS Prescription data does not identify whether or not the patient is a care home resident.
    It is proposed to infer this based on whether or not the patient's addess information aligns with a care home property.
    The care home properties are to be identified based on data stored in AddressBasePlus (maintained by Ordnance Survey).
    
    Prior to matching data to patient address information the AddressBasePlus information needs to be cleansed.
    For performance, prescription records will initially be limited to only include records where:
        +   the property has been identifed as a care home within AddressBase
    
    As part of the cleansing, "single line addresses" will be calculated based on the address components.
    These "single line addresses" will be formatted to remove non-alphanumeric characters and to allow consistent comparisons between data sets
    
    Once address information has been identified the address will be split into individual tokens so that each part of the address can be matched
    Tokenising the address will removed issues where the same address has been captured but with words missing or in different orders


DEPENDENCIES:
        DALL_REF.ADDRESSBASE_PLUS   :   extract of AddressBasePlus data, including individual address components as seperate fields
        

OUTPUTS:
        INT615_AB_PLUS_BASE_TMP     :   prescription form level dataset including cleansed address information
        INT615_AB_PLUS_TOKENS		:	tokenised records for each AddressBasePlus record of interest
        INT615_AB_PLUS_BASE         :   Subset of AddressBasePlus data limited to addresses at same postcode as identifed care homes
        

*/

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
-- PROCESS ADDRESS BASE PLUS DATA
-- The AddressBasePlus data set contains address information but split across multiple fields
-- AddressBase do have some suggestions as to how this can be combined into a single line address, although can potentially produce multiple addresses
-- Additionally, it may be possible to create a third version of the address which is a "best fit" combination of the options
-- This script will perform data cleansing operations to find potential addresses from AddressBasePlus

-- For this initiative we are focused on matching against care homes and therefore for performance the initial AddressBase data will be limited to 
-- records for addresses with a postcode that aligns to a care home property (all properties on a care home street)

-- Additionally, as the same address can map to more than one UPRN, only a single UPRN will be retained for each address (priority to care homes)

-- NOTE: TAKES ~10 MINS


drop table INT615_AB_PLUS_BASE_TMP;
create table INT615_AB_PLUS_BASE_TMP compress for query high as

with
-----SECTION START: Define care home postcodes--------------------------------------------------------------------------------------------------------
--to improve performance, as only care homes are wanting to be matched, identify which postcodes align with care home properties
ab_postcodes as (
select  /*+ materialize */
        distinct
            replace(POSTCODE_LOCATOR, ' ', '')  as  AB_POSTCODE
from        DALL_REF.ADDRESSBASE_PLUS
where       1=1
    and     CLASS = 'RI01'              --limit to records classified as a care home in AddressBase
    and     RELEASE_DATE = '15-MAR-21'  --limit to specific extract from AddressBase
)
--select * from ab_postcodes;
-----SECTION END: Define care home postcodes----------------------------------------------------------------------------------------------------------

,

-----SECTION START: Create single line addresses for AddressBasePlus records--------------------------------------------------------------------------
--AddressBasePlus stores individual address components (house name/number, street ...) as seperate fields
--these fields need to be concatenated into single address strings
--however, there are options for how these fields could be concatated with no defined "correct" method
--two options are used here with the outputs of both being combined in subsequent sections of this script
--NB: can we define what DPA and GEO mean and how we settled on these combinations

--NB: what published code???
-- NOTE: SOME FIELD NAME DIFFERENT TO PUBLISHED SQL CODE TO GENERATE SLA
-- DEPENDENT_LOCALITY == DEP_LOCALITY
-- DEPENDENT_THOROUGHFARE == DEP_THOROUGHFARE
-- DOUBLE_DEPENDENT_LOCALITY == DOU_DEP_LOCALITY

sla as (
select  /*+ materialize */
            UPRN,
            CLASS,
            ADDRESSBASE_POSTAL,
            trim(replace(POSTCODE_LOCATOR, ' ', ''))  as  AB_POSTCODE,
            POSTCODE,
            -- DPA SLA
            (
                case when DEPARTMENT_NAME           is not null then DEPARTMENT_NAME|| ', '             else '' end
             || case when RM_ORGANISATION_NAME      is not null then RM_ORGANISATION_NAME || ', '       else '' end
             || case when SUB_BUILDING_NAME         is not null then SUB_BUILDING_NAME || ', '          else '' end
             || case when BUILDING_NAME             is not null then BUILDING_NAME || ', '              else '' end
             || case when BUILDING_NUMBER           is not null then BUILDING_NUMBER || ' '             else '' end
             || case when PO_BOX_NUMBER             is not null then 'PO BOX ' || PO_BOX_NUMBER || ', ' else '' end
             || case when DEP_THOROUGHFARE          is not null then DEP_THOROUGHFARE || ', '           else '' end
             || case when THOROUGHFARE              is not null then THOROUGHFARE || ', '               else '' end
             || case when DOU_DEP_LOCALITY          is not null then DOU_DEP_LOCALITY || ', '           else '' end
             || case when DEP_LOCALITY              is not null then DEP_LOCALITY || ', '               else '' end
             || case when POST_TOWN                 is not null then POST_TOWN || ', '                  else '' end
            ) as DPA_SINGLE_LINE_ADDRESS,
            -- GEO_SLA
            (
                case when LA_ORGANISATION    is not null                                                         then LA_ORGANISATION || ', '          else '' end
                -- sao
             || case when SAO_TEXT           is not null                                                         then SAO_TEXT        || ', '          else '' end
                -- no sao start suffix
             || case when SAO_START_NUMBER   is not null and SAO_START_SUFFIX is null and SAO_END_NUMBER is null then to_char(SAO_START_NUMBER) || ', '
                     when SAO_START_NUMBER   is null                                                             then ''                               else TO_CHAR(SAO_START_NUMBER) ||'' end
                -- no sao end number
             || case when SAO_START_SUFFIX   is not null and SAO_END_NUMBER is null                              then SAO_START_SUFFIX || ', '
                     when SAO_START_SUFFIX   is not null and SAO_END_NUMBER is not null                          then SAO_START_SUFFIX                 else '' end
                -- sao start number & sao end number -> add '-' between them 
             || case when SAO_END_SUFFIX     is not null and SAO_END_NUMBER is not null                          then '-'
                     when SAO_START_NUMBER   is not null and SAO_END_NUMBER is not null                          then '-'                              else '' end
                -- sao end number and sao end suffix
             || case when SAO_END_NUMBER     is not null and SAO_END_SUFFIX is null                              then to_char(SAO_END_NUMBER) || ', '
                     when SAO_END_NUMBER     is null                                                             then ''                               else TO_CHAR(SAO_END_NUMBER) end
                -- sao end suffix
             || case when SAO_END_SUFFIX     is not null                                                         then SAO_END_SUFFIX || ', '           else '' end
                -- pao
             || case when PAO_TEXT           is not null                                                         then PAO_TEXT || ', '                 else '' end
                -- no pao start suffix
             || case when PAO_START_NUMBER   is not null and PAO_START_SUFFIX is null and PAO_END_NUMBER is null then to_char(PAO_START_NUMBER) || ' '
                     when PAO_START_NUMBER   is null                                                             then ''                               else TO_CHAR(PAO_START_NUMBER) || '' end
                -- no pao end number
             || case when PAO_START_SUFFIX   is not null and PAO_END_NUMBER is null                              then PAO_START_SUFFIX || ', '
                     when PAO_START_SUFFIX   is not null and PAO_END_NUMBER is not null                          then PAO_START_SUFFIX                 else '' end
                -- pao start number & pao end number -> add '-' between them 
             || case when PAO_END_SUFFIX     is not null and PAO_END_NUMBER is not null                          then '-'
                     when PAO_START_NUMBER   is not null and PAO_END_NUMBER is not null                          then '-'                              else '' end
                -- pao end number and pao end suffix
             || case when PAO_END_NUMBER     is not null and PAO_END_SUFFIX is null                              then to_char(PAO_END_NUMBER) || ', '
                     when PAO_END_NUMBER     is null                                                             then ''                               else TO_CHAR(PAO_END_NUMBER) end
                -- pao end suffix
             || case when PAO_END_SUFFIX     is not null                                                         then PAO_END_SUFFIX || ', '           else '' end
                -- street information
             || case when STREET_DESCRIPTION is not null                                                         then STREET_DESCRIPTION || ', '       else '' end
                -- localilty
             || case when LOCALITY           is not null                                                         then LOCALITY || ', '                 else '' end
                -- town
             || case when TOWN_NAME          is not null                                                         then TOWN_NAME || ', '                else '' end
            ) as GEO_SINGLE_LINE_ADDRESS 
from        DALL_REF.ADDRESSBASE_PLUS
where       1=1
    --limit to the most appropriate AddressBasePlus extract
    and     RELEASE_DATE = '15-MAR-21'
    --limit to only include records 
    and     trim(replace(POSTCODE_LOCATOR, ' ', '')) in (select AB_POSTCODE from ab_postcodes)
    --exclude any records with classifications that identify what? NB
    and     CLASS not like 'L%'
    and     CLASS not like 'Z%'
    --limit to English addresses only
    and     COUNTRY = 'E'
)
--select * from sla;
-----SECTION END: Create single line addresses for AddressBasePlus records----------------------------------------------------------------------------

,

-----SECTION START: Format single line addresses from AddressBasePlus---------------------------------------------------------------------------------
--the "single line addresses" created will potential include unwanted characters (e.g. special characters, additional spaces...)
--these will need to be removed prior to any address string tokenisation
sla_edit as (
select  /*+ materialize */
        distinct
            UPRN,
            CLASS,
            case when CLASS = 'RI01' then 1 else 0 end as CAREHOME_FLAG,
            ADDRESSBASE_POSTAL,
            AB_POSTCODE,
            trim(replace(regexp_replace(replace(regexp_replace(regexp_replace(regexp_replace(GEO_SINGLE_LINE_ADDRESS,
                                                                                            '[,.();:#'']', ' '),                --replace special characters with a single space
                                                                                            '(\d)(\D)', '\1 \2'),               --add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
                                                                                            '(\D)(\d)', '\1 \2'),               --add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
                                                                                            '&', ' AND '),                      --replace the ampersand character with the string "and"
                                                                                            '( ){2,}', ' '),                    --replace any multiple spaces with a single space
                                                                                            ' - ','-')                          --remove any spaces around a hyphen
                                                                                            ) as GEO_SINGLE_LINE_ADDRESS,
            trim(replace(regexp_replace(replace(regexp_replace(regexp_replace(regexp_replace(DPA_SINGLE_LINE_ADDRESS,
                                                                                            '[,.();:#'']', ' '),                --replace special characters with a single space
                                                                                            '(\d)(\D)', '\1 \2'),               --add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
                                                                                            '(\D)(\d)', '\1 \2'),               --add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
                                                                                            '&', ' AND '),                      --replace the ampersand character with the string "and"
                                                                                            '( ){2,}', ' '),                    --replace any multiple spaces with a single space
                                                                                            ' - ','-')                          --remove any spaces around a hyphen
                                                                                            ) as DPA_SINGLE_LINE_ADDRESS
from        sla
)
--select * from sla_edit;
-----SECTION END: Format single line addresses from AddressBasePlus-----------------------------------------------------------------------------------

,

-----SECTION START: Tokenise the DPA single line address strings--------------------------------------------------------------------------------------
--for the DPA_SINGLE_LINE_ADDRESS tokenise the data to produce a single record for each string component of the address
--also create a field to accomodate the combination of GEO and DPA tokens into a single "master" address
dpa_tokens as (
select  /*+ materialize */
            UPRN,
            DPA_SINGLE_LINE_ADDRESS,
            DPA_ADDRESS_TOKEN,
            DPA_ROW_NUM,
            DPA_ADDRESS_TOKEN || '*' || rank() over (partition by UPRN, DPA_ADDRESS_TOKEN order by DPA_ROW_NUM) || '*' || UPRN as DPA_TOKEN_COUNT
from        (
            select  /*+ materialize */
                        UPRN,
                        DPA_SINGLE_LINE_ADDRESS,
                        trim(regexp_substr(DPA_SINGLE_LINE_ADDRESS,'[^ ]+', 1, lines.column_value))  as  dpa_address_token,
                        row_number() over (partition by UPRN order by lines.column_value)            as  dpa_row_num
            from        sla_edit,
                        table(cast(multiset(select level from DUAL connect by instr(DPA_SINGLE_LINE_ADDRESS, ' ', 1, level - 1) > 0)  as  sys.odciNumberList))  lines
            )
)
--select * from dpa_tokens;
-----SECTION END: Tokenise the DPA single line address strings----------------------------------------------------------------------------------------

,  
    
-----SECTION START: Tokenise the GEO single line address strings--------------------------------------------------------------------------------------
--for the GEO_SINGLE_LINE_ADDRESS tokenise the data to produce a single record for each string component of the address
--also create a field to accomodate the combination of GEO and DPA tokens into a single "master" address
geo_tokens as (
select  /*+ materialize */
            UPRN,
            GEO_SINGLE_LINE_ADDRESS,
            GEO_ADDRESS_TOKEN,
            GEO_ROW_NUM,
            GEO_ADDRESS_TOKEN || '*' || rank() over (partition by UPRN, GEO_ADDRESS_TOKEN order by GEO_ROW_NUM) || '*' || UPRN as GEO_TOKEN_COUNT
from        (
            select  /*+ materialize */
                        uprn,
                        geo_single_line_address,
                        trim(regexp_substr(GEO_SINGLE_LINE_ADDRESS, '[^ ]+', 1, lines.column_value))    as  GEO_ADDRESS_TOKEN,
                        row_number() over (partition by UPRN order by lines.column_value)               as  GEO_ROW_NUM
            from        sla_edit,
                        table(cast(multiset(select level from DUAL connect by instr(GEO_SINGLE_LINE_ADDRESS, ' ', 1, level - 1) > 0)  as  sys.odciNumberList))  lines
            )
)
--select * from geo_tokens;
-----SECTION END: Tokenise the GEO single line address strings----------------------------------------------------------------------------------------

,

-----SECTION START: Combine DPA and GEO addresses into single "combined" address----------------------------------------------------------------------
--using the tokens from both DPA and GEO addresses combine these into a single address
--where tokens are the same they will sit in the same place in the address but unqiue address tokens will then slip into the relevant place
--for example:
--  DPA: 42 ALBANY LANE BALDALL COMMON COVENTRY
--  GEO: THE WHITE HOUSE 42 ALBANY LANE BALDALL COMMON SOLIHULL
--  COMBIND: THE WHITE HOUSE 42 ALBANY LANE BALDALL COMMON COVENTRY SOLIHULL


--align the two tokenised datasets based on common tokens, but including outer joins to ensure all tokens are included
--DPA takes priority over GEO
dpa_geo as (
select  /*+ materialize */
            coalesce(dt.UPRN, gt.UPRN)  as  uprn,
            dt.DPA_ROW_NUM,
            gt.GEO_ROW_NUM,
            coalesce(dt.DPA_ADDRESS_TOKEN, gt.GEO_ADDRESS_TOKEN) as LEAD_TOKEN,
            coalesce(dt.DPA_ROW_NUM, lead(dt.DPA_ROW_NUM ignore nulls) over (partition by coalesce(dt.UPRN, gt.UPRN) order by gt.GEO_ROW_NUM)) as LEAD_ROW
from        dpa_tokens  dt
full join   geo_tokens  gt  on gt.GEO_TOKEN_COUNT = dt.DPA_TOKEN_COUNT
)
--select * from dpa_geo order by uprn, dpa_row_num, geo_row_num;
,
--combine the records for each UPRN into a single record, concatenating the tokens
core as (
select  /*+ materialize */
            UPRN,
            listagg(LEAD_TOKEN, ' ') within group (order by LEAD_ROW, GEO_ROW_NUM) as CORE_SINGLE_LINE_ADDRESS
from        dpa_geo
group by    UPRN
)
--select * from core;
-----SECTION END: Combine DPA and GEO addresses into single "combined" address------------------------------------------------------------------------

,

-----SECTION START: Append created single line address to existing DPA and GEO single line addresses--------------------------------------------------
--join the newly created single line address (based on combination of DPA and GEO) to the existing created single line addresses
core_edit as (
select  /*+ materialize */
            se.UPRN,
            se.CLASS,
            se.CAREHOME_FLAG,
            se.ADDRESSBASE_POSTAL,
            se.AB_POSTCODE,
            se.GEO_SINGLE_LINE_ADDRESS,
            se.DPA_SINGLE_LINE_ADDRESS,
            core.CORE_SINGLE_LINE_ADDRESS
from        sla_edit    se
left join               core    on core.UPRN  =  se.UPRN
)
--select * from core_edit order by uprn;
-----SECTION END: Append created single line address to existing DPA and GEO single line addresses----------------------------------------------------

,

-----SECTION START: Identify unique UPRNs for Postcode and Address------------------------------------------------------------------------------------
--in a small number of instances the same postcode and address may map to multiple UPRNs
--this will make it difficult to determine the correct UPRN to use for these addresses
--rather than lose all potential matches at least one should be retained


--create seperate records for each single line address string (where not null)
--some UPRNs will have multiple address strings and some addresses will have multiple UPRNs (at this point)
core_stack as (
select UPRN, CLASS, CAREHOME_FLAG, AB_POSTCODE, GEO_SINGLE_LINE_ADDRESS as AB_ADDRESS from core_edit where GEO_SINGLE_LINE_ADDRESS is not null
union all
select UPRN, CLASS, CAREHOME_FLAG, AB_POSTCODE, DPA_SINGLE_LINE_ADDRESS as AB_ADDRESS from core_edit where DPA_SINGLE_LINE_ADDRESS is not null
union all
select UPRN, CLASS, CAREHOME_FLAG, AB_POSTCODE, CORE_SINGLE_LINE_ADDRESS as AB_ADDRESS from core_edit where CORE_SINGLE_LINE_ADDRESS is not null
)
--select * from core_stack;
,
--only retain a single UPRN per distinct postcode and address combination
--give care home UPRNs priority over non carehome when both share the exact postcode and address
uprn_distinct as (
select      *
from        (
            select      cs.*,
                        dense_rank() over (partition by AB_POSTCODE, AB_ADDRESS order by CAREHOME_FLAG desc, UPRN) as  UPRN_RANK
            from        core_stack  cs
            )
where       1=1
    and     UPRN_RANK = 1
)
--select * from uprn_distinct order by geo_uprn_rank desc;
-----SECTION END: Identify unique UPRNs for Postcode and Address--------------------------------------------------------------------------------------


-----OUTPUT-------------------------------------------------------------------------------------------------------------------------------------------
--select distinct records for combinations of UPRN and postcode/address
--flag which addreses are carehomes
--where a UPRN has multiple entries create a sub_id
select  distinct
            UPRN,
            CLASS,
            CAREHOME_FLAG,
            AB_POSTCODE,
            AB_ADDRESS,
            dense_rank() over (order by UPRN, AB_ADDRESS)  as UPRN_ID
from        UPRN_DISTINCT
order by    UPRN_ID
;

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------


--NB: it may make more sense to include the string counts at the single line address level rather than the tokenised data

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
-- TOKENISE ADDRESSBASE DATA
-- Using the distinct address information pulled from AddressBasePlus
-- split the address into distinct tokens that can be used for string matching

-- NOTE: TAKES ~1 MINS

drop table INT615_AB_PLUS_TOKENS;
create table INT615_AB_PLUS_TOKENS compress for query high as

with
-----SECTION START: Split address into individual tokens----------------------------------------------------------------------------------------------
--split the address into individual records for each individual string in an address
-- e.g. "42 ALBANY LANE BALDALL COMMON COVENTRY" becomes six distinct records 42 | ALBANY | LANE | BALDALL | COMMON | COVENTRY
plus_tokens as (
select  /*+ materialize */
            UPRN,
            UPRN_ID,
            AB_POSTCODE,
            trim(regexp_substr(AB_ADDRESS,'[^ ]+', 1, lines.column_value))  as  AB_ADDRESS
from        INT615_AB_PLUS_BASE_TMP,
            table(cast(multiset(select level from DUAL connect by instr(AB_ADDRESS, ' ', 1, level - 1) > 0)  as  sys.odciNumberList))  lines
order by    UPRN_ID
)
--select * from plus_tokens;
-----SECTION END: Split address into individual tokens------------------------------------------------------------------------------------------------


-----OUTPUT-------------------------------------------------------------------------------------------------------------------------------------------
--not all words are of equal value when matching addresses
--for example, on most residential streets the main difference will be the house number
--therefore any numeric values need to be highlighted so that they can be weighted in the scoring process
select      pt.*,
            case when regexp_like(pt.AB_ADDRESS, '[0-9]') then 1 else 0 end as  INT_FLAG
from        plus_tokens  pt
where       1=1
;

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
-- FORMATTED ADDRESSBASE RECORDS
-- To support scoring it may be useful to supplement the address records with basic information about the number of tokens (both character and numeric)

-- NOTE: TAKES ~1 MINS
drop table INT615_AB_PLUS_BASE;
create table INT615_AB_PLUS_BASE compress for query high as 

with
-----SECTION START: Count number of tokens per address------------------------------------------------------------------------------------------------
--for each unique address count the number of individual tokens
plus_count as
(
select      UPRN_ID,
            sum(case when regexp_like(AB_ADDRESS, '[0-9]') then 1 else 0 end)  as  AB_INT_COUNT,
            sum(case when regexp_like(AB_ADDRESS, '[0-9]') then 0 else 1 end)  as  AB_CHAR_COUNT,
            sum(1) as AB_TOTAL
from        INT615_AB_PLUS_TOKENS
where       1=1
group by    UPRN_ID
)
--select * from plus_count;
-----SECTION END: Count number of tokens per address--------------------------------------------------------------------------------------------------

-----OUTPUT-------------------------------------------------------------------------------------------------------------------------------------------
select      ab.*,
            pc.AB_INT_COUNT,
            pc.AB_CHAR_COUNT,
            pc.AB_TOTAL
from        INT615_AB_PLUS_BASE_TMP     ab
inner join  plus_count                  pc  on  ab.UPRN_ID  =   pc.UPRN_ID
;

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------