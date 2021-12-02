/*
INT615 - Care Home Insight
Address Matching (AddressBasePlus to NHS Prescriptions)
Version 1.0
Created by Adnan Shroufi
Created on 09-11-2021
AMENDMENTS:
	--DATE------:--NAME-------------:--DETAILS-------------------------------------------------------------------------------------
    11-11-2021  :   Steven Buckley  :   Reviewed code base, adding some commentary
                                    :   Removed "results_base" section as this is replaced by INT615_PRESC_ADDRESS_BASE
                                    :   Moved the JW scoring to the "cross_join_diff" section to prevent it being required as a limiting clause
                                    :   Replaced "agg_one" and "agg_two" to include new tie-break for top scoring rank
    26-11-2021  :   Steven Buckley  :   Amended script to run from different source tables that combine AddressBasePlus and CQC datasets
    
DESCRIPTION:
    NHS Prescription data does not identify whether or not the patient is a care home resident.
    It is proposed to infer this based on whether or not the patient's addess information aligns with a care home property.
    The care home properties are to be identified based on data stored in AddressBasePlus (maintained by Ordnance Survey).
    
    Using cleansed datasets for both patient address and AddressBasePlus address information, the address information can be matched across datasets.
    
    An initial "exact match" will identify any instances where the addresses are in the exact same format in each dataset
    
    To accomodate "close" matches, where words may be missing from an address or in a different order, additional steps are required:
        +   using a "tokenised" version of the addresses, each individual word can be compared (where postcodes match)
        +   where the words are not exactly the same, string matching scores can be calculated to determine the potential quality of the match
        +   the individual scores for each word can be combined to find the best scoring match    
        
DEPENDENCIES:
        INT615_PRESC_ADDRESS_BASE   :   unique patient address level dataset, including summary of token information
        INT615_PRESC_TOKENS         :   tokenised records for each unique patient address
        INT615_AB_PLUS_CQC_BASE     :   final subset of carehome address data limited to addresses at same postcode as identifed care homes
		INT615_AB_PLUS_CQC_TOKENS   :   tokenised records for each address record of interest
        
OUTPUTS:
        INT615_MATCH            :   Address level match results for each unique patient address
        
*/

------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------

-- NOTE: TAKES ~10 MINS


drop table INT615_MATCH;
create table INT615_MATCH compress for query high as

with

------------------------------------------------------------------------------------------------------------------------------------------------------
-- PART 1.1: EXACT MATCHES ---------------------------------------------------------------------------------------------------------------------------

-----SECTION START: Exact Matching--------------------------------------------------------------------------------------------------------------------
-- the initial matching simply identifies any records where the address (inc postcode) is in the exact same format in both datasets
exact_matches as (
select  /*+ materialize */
        -- AddressBase info
            ab.ADDRESS_ID,
            ab.ADDRESS_LINE_ID,
            ab.CAREHOME_FLAG,
            ab.NURSING_HOME_FLAG,
            ab.RESIDENTIAL_HOME_FLAG,
            ab.POSTCODE,
            ab.ADDRESS,
        -- Prescribing info
            pab.ADDRESS_RECORD_ID,
        -- Match results
            (pab.PAT_INT_COUNT * 4) + (pab.PAT_CHAR_COUNT)  as  JW_SCORE,
            (pab.PAT_INT_COUNT * 4) + (pab.PAT_CHAR_COUNT)  as  TOTAL_SCORE,
            1                                               as  MATCH_SCORE,
            'EXACT'                                         as  MATCH_TYPE
from        INT615_PRESC_ADDRESS_BASE   pab
inner join  INT615_AB_PLUS_CQC_BASE     ab      on  pab.PAT_ADDRESS     =   ab.ADDRESS
                                                and pab.PAT_POSTCODE    =   ab.POSTCODE
where       1=1
    --limit the patient address to only those that are align to care home postcode and have been tokenised
    and     pab.PAT_TOTAL is not null   
)
--select * from exact_matches;
-----SECTION END: Exact Matching----------------------------------------------------------------------------------------------------------------------

,

------------------------------------------------------------------------------------------------------------------------------------------------------
-- PART 1.2: JW MATCHES ------------------------------------------------------------------------------------------------------------------------------

-----SECTION START: Limit patient address records for token matching----------------------------------------------------------------------------------
--limit the patient address tokens to only include any patient addresses that have not been exactly matched
pat_tokens as (
select  /*+ materialize */
            ADDRESS_RECORD_ID,
            PAT_POSTCODE,
            PAT_ADDRESS,
            INT_FLAG
from        INT615_PRESC_TOKENS
where       1=1
    --limit to only addresses that have not been "exactly" matched
    and     ADDRESS_RECORD_ID not in (select ADDRESS_RECORD_ID from EXACT_MATCHES)
)                             
--select * from pat_tokens;
-----SECTION END: Limit patient address records for token matching------------------------------------------------------------------------------------

,

-----SECTION START: Limit AddressBase records for token matching--------------------------------------------------------------------------------------
--limit the AddressBase tokens to only include any addresses that match postcodes for patients to be match
plus_tokens as (
select  /*+ materialize */
            ADDRESS_ID,
            ADDRESS_LINE_ID,
            POSTCODE,
            ADDRESS_TKN,
            INT_FLAG
from        INT615_AB_PLUS_CQC_TOKENS
where       1=1
    --limit to records with postcodes that appear in the patient address dataset
    and     POSTCODE in (select distinct PAT_POSTCODE from PAT_TOKENS)
)
--select * from plus_tokens;                                 
-----SECTION END: Limit AddressBase records for token matching----------------------------------------------------------------------------------------

,

-----SECTION START: Identify matching tokens between datasets-----------------------------------------------------------------------------------------
--identify where tokens match between AddressBase and Patient address records
--the postcode, address token and string type (integer/number) must match
cross_join_exact as (
select  /*+ materialize */
            ab.ADDRESS_ID,
            ab.ADDRESS_LINE_ID,
            pat.ADDRESS_RECORD_ID,
            pat.PAT_POSTCODE,
            ab.ADDRESS_TKN,
            pat.PAT_ADDRESS,
            pat.INT_FLAG,
            case when pat.INT_FLAG = 1 then 4 else 1 end    as  JW    
from        pat_tokens      pat
inner join  plus_tokens     ab      on  pat.PAT_POSTCODE    =   ab.POSTCODE
                                    and pat.PAT_ADDRESS     =   ab.ADDRESS_TKN
                                    and pat.INT_FLAG        =   ab.INT_FLAG
)                                        
--select * from cross_join_exact;
-----SECTION END: Identify matching tokens between datasets-------------------------------------------------------------------------------------------

,

-----SECTION START: Identify non-matching tokens between datasets-------------------------------------------------------------------------------------
--identify where tokens do not match between AddressBase and Patient address records
--the postcode and string type (integer/number) must match but the individual may not match
--these non-matching strings will be scored using jaro winkler similarity scoring to identify how accuracte the match is
cross_join_diff as (
select  /*+ materialize */
            ab.ADDRESS_ID,
            ab.ADDRESS_LINE_ID,
            pat.ADDRESS_RECORD_ID,
            pat.PAT_POSTCODE,
            ab.ADDRESS_TKN,
            pat.PAT_ADDRESS,
            pat.INT_FLAG,
            utl_match.jaro_winkler(pat.PAT_ADDRESS, ab.ADDRESS_TKN)  as  JW
from        pat_tokens      pat
inner join  plus_tokens     ab      on  pat.PAT_POSTCODE     =   ab.POSTCODE
                                    and pat.PAT_ADDRESS     !=   ab.ADDRESS_TKN
                                    and pat.INT_FLAG         =   ab.INT_FLAG    
where       1=1
    --only look at non-numeric tokens (numeric tokens need to be an exact match or no match)
    and     pat.INT_FLAG = 0
    and     ab.INT_FLAG = 0
    --to limit matches that are very unlikely to score well
    --limit results to where there is some commoninality between datasets
    and     (   substr(ab.ADDRESS_TKN, 1, 1) = substr(pat.PAT_ADDRESS, 1, 1) --initial letter matches across tokens
            or  substr(ab.ADDRESS_TKN, 2, 1) = substr(pat.PAT_ADDRESS, 2, 1) --second letter matches across tokens
            or  substr(ab.ADDRESS_TKN, length(ab.ADDRESS_TKN), 1)  =  substr(pat.PAT_ADDRESS, length(pat.PAT_ADDRESS), 1) --last letter matches across tokens
            or  instr(ab.ADDRESS_TKN, pat.PAT_ADDRESS) > 1   --the Patient token is a substring of the AddressBase token
            or  instr(pat.PAT_ADDRESS, ab.ADDRESS_TKN) > 1   --the AddressBase token is a substring of the Patient token
            )
)                             
--select * from cross_join_diff;
-----SECTION END: Identify non-matching tokens between datasets---------------------------------------------------------------------------------------

,

-----SECTION START: Identify best scoring match for each token---------------------------------------------------------------------------------------------------
--combine the exact and diff matches from the token cross joins
--for each patient address token find the best score from any token within each UPRN_ID
jw_union as 
(
select      ADDRESS_ID,
            ADDRESS_LINE_ID,
            ADDRESS_RECORD_ID,
            PAT_ADDRESS,
            max(JW)     as MAX_JW
from        (
            --combine the exact and diff matches from the token cross joins
            select      *
            from        cross_join_diff 
            where       1=1
                --only retain matches where the similarity score is 0.8 or above
                and     JW >= 0.8
            union all
            select * from cross_join_exact
            )
group by    ADDRESS_ID,
            ADDRESS_LINE_ID,
            ADDRESS_RECORD_ID,
            PAT_ADDRESS
)
--select * from jw_union;                                   
-----SECTION END: Combine and score token matches-----------------------------------------------------------------------------------------------------

,

-----SECTION START: Calculate overall score for each Patient and AddressBase address comparison-------------------------------------------------------
-- for each Patient to AddressBase match, identify the overall match score
-- the overall score is based on a combination of the individual best match scores for each patient address token
-- for each Patient Address, rank the scores to identify which is the highest scoring match
match_score as (
select  /*+ materialize */
            ADDRESS_ID,
            ADDRESS_LINE_ID,
            ADDRESS_RECORD_ID,    
            sum(MAX_JW)                                                             as  JW_SCORE,
            rank() over (partition by ADDRESS_RECORD_ID order by sum(MAX_JW) desc)  as  SCORE_RANK
from        jw_union
group by    ADDRESS_ID,
            ADDRESS_LINE_ID,
            ADDRESS_RECORD_ID
)
--select * from match_score;
-----SECTION END: Calculate overall score for each Patient and AddressBase address comparison---------------------------------------------------------

,

-----SECTION START: Tie breaker for matched "best" scores---------------------------------------------------------------------------------------------
-- it is possible that a single address could have the same matching score for more than one UPRN
-- if the score is the same it is not possible to determine which is the best match
-- rather than simply saying no match is possible, we can prioritise one of the top "best matches"
-- to err on the side of caution if any of the "best matches" are a non-care home property use this so we don not incorrectly assign someone to a care home
-- if all the "best matches" are care home properties then simply take the first of these as we are concerned which care home the assign to at this moment in time
match_score_tb as (
select  /*+ materialize */
            ms.*,
            rank() over (partition by ms.ADDRESS_RECORD_ID order by ab.CAREHOME_FLAG asc, ms.ADDRESS_LINE_ID asc) as MATCH_RANK
from        match_score                 ms
left join   INT615_AB_PLUS_CQC_BASE     ab  on  ms.ADDRESS_LINE_ID =   ab.ADDRESS_LINE_ID
where       1=1
    and     ms.SCORE_RANK = 1
)
--select * from match_score_tb;
-----SECTION END: Tie breaker for matched "best" scores-----------------------------------------------------------------------------------------------

,

-----SECTION START: Score match results---------------------------------------------------------------------------------------------------------------
--produce summary of all Patient addresses with a match to an AddressBase property
--including details of the best match
jw_matches as
(
select  /*+ materialize */
        -- AddressBase info
            ms.ADDRESS_ID,
            ms.ADDRESS_LINE_ID,
            ab.CAREHOME_FLAG,
            ab.NURSING_HOME_FLAG,
            ab.RESIDENTIAL_HOME_FLAG,
            ab.POSTCODE,
            ab.ADDRESS,
        -- Prescribing info
            ms.ADDRESS_RECORD_ID,
        -- Match results
            ms.JW_SCORE  as  JW_SCORE,
            (pab.PAT_INT_COUNT * 4) + (pab.PAT_CHAR_COUNT)                  as  TOTAL_SCORE,
            ms.JW_SCORE / ((pab.PAT_INT_COUNT * 4) + (pab.PAT_CHAR_COUNT))  as  MATCH_SCORE,
            'JW'                                                            as  MATCH_TYPE
from        match_score_tb              ms
inner join  INT615_AB_PLUS_CQC_BASE     ab      on  ms.ADDRESS_ID       =   ab.ADDRESS_ID
                                                and ms.ADDRESS_LINE_ID  =   ab.ADDRESS_LINE_ID
inner join  INT615_PRESC_ADDRESS_BASE   pab on  ms.ADDRESS_RECORD_ID    =   pab.ADDRESS_RECORD_ID
where       1=1
    and     ms.SCORE_RANK = 1
    and     ms.MATCH_RANK = 1
)
--select * from jw_matches;
-----SECTION END: Score match results-----------------------------------------------------------------------------------------------------------------

,

-----SECTION START: No match results------------------------------------------------------------------------------------------------------------------
--produce summary of all Patient addresses with a match to an AddressBase property
--including details of the best match
no_matches as
(
select  /*+ materialize */
        -- AddressBase info
            null    as ADDRESS_ID,
            null    as ADDRESS_LINE_ID,
            0       as CAREHOME_FLAG,
            null    as NURSING_HOME_FLAG,
            null    as RESIDENTIAL_HOME_FLAG,
            null    as POSTCODE,
            null    as ADDRESS,
        -- Prescribing info
            ADDRESS_RECORD_ID,
        -- Match results
            0       as JW_SCORE,
            0       as TOTAL_SCORE,
            0       as MATCH_SCORE,
            'NONE'  as MATCH_TYPE
from        INT615_PRESC_ADDRESS_BASE
where       1=1
    and     ADDRESS_RECORD_ID not in (select ADDRESS_RECORD_ID from exact_matches)
    and     ADDRESS_RECORD_ID not in (select ADDRESS_RECORD_ID from jw_matches)
    
)
--select * from no_matches;
-----SECTION END: No match results--------------------------------------------------------------------------------------------------------------------


-----OUTPUT-------------------------------------------------------------------------------------------------------------------------------------------
--combine the exact, score and no match results
select      pab.*,
            au.ADDRESS_ID,
            au.ADDRESS_LINE_ID,
            au.CAREHOME_FLAG,
            au.NURSING_HOME_FLAG,
            au.RESIDENTIAL_HOME_FLAG,
            au.POSTCODE,
            au.ADDRESS,
            au.JW_SCORE,
            au.TOTAL_SCORE,
            au.MATCH_SCORE,
            au.MATCH_TYPE
from        INT615_PRESC_ADDRESS_BASE   pab
inner join  (
            select * from no_matches
                union all
            select * from jw_matches
                union all
            select * from exact_matches
            )                           au  on pab.ADDRESS_RECORD_ID  =  au.ADDRESS_RECORD_ID
;
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------