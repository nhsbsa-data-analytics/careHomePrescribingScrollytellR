/*
Process AddresBase Data
Version 1.0

Created by Steven Buckley
Created on 26-11-2021

AMENDMENTS:
	--DATE------:--NAME-------------:--DETAILS-------------------------------------------------------------------------------------
    26-11-2021  :   Steven Buckley  :   Initial script creation
    


DESCRIPTION:
    AddressBasePlus data includes multiple fields representing the components of the location address information.
    To make this data more manageable it is beneficial to perform some cleansing activies to 
        concatenate the component fields into "single line addresses"
        perform basic string cleansing to remove special characters etc.
    
    The are different methods to concatenate the address components so three methods will be used to create three possible single line addresses:
        GEO_SINGLE_LINE_ADDRESS
        DPA_SINGLE_LINE_ADDRESS
        CORE_SINGLE_LINE_ADDRESS


DEPENDENCIES:
        DALL_REF.ADDRESSBASE_PLUS   :   extract of AddressBasePlus data, including individual address components as seperate fields
        

OUTPUTS:
        ADDRESSBASE_PLUS_FLAT   :   AddressBasePlus data at a UPRN level with multiple single line address combinations for each UPRN
                                :   Each record is a distinct UPRN (for an extract date) with a distinct classification
                                :   EXTRACT_DATE is used to identify the period the data aligns with
        

*/
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------

--DECLARATIONS----------------------------------------------------------------------------------------------------------------------------------------
    define var_RELEASE_DATE = '23-APR-20';      --used to limit to a specific release extract
    define var_EXTRACT_DATE = '2021-04-23';     --used as the defined EXTRACT_DATE in the output
    
    
--TABLE CREATION--------------------------------------------------------------------------------------------------------------------------------------

drop table ADDRESSBASE_PLUS_FLAT;
create table ADDRESSBASE_PLUS_FLAT compress for query high as

with

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
    and     RELEASE_DATE = '&var_RELEASE_DATE'
    --limit to only include records 
--    and     trim(replace(POSTCODE_LOCATOR, ' ', '')) in (select AB_POSTCODE from ab_postcodes)
    --exclude any records with classifications that identify what? NB
    and     CLASS not like 'L%'
    and     CLASS not like 'Z%'
    --limit to English addresses only
    and     COUNTRY = 'E'
)
select * from sla;
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



-----OUTPUT-------------------------------------------------------------------------------------------------------------------------------------------
--join the newly created single line address (based on combination of DPA and GEO) to the existing created single line addresses
select      '&var_EXTRACT_DATE' as EXTRACT_DATE,
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
;
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------