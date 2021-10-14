--------------------------------------------------------------------------------
-- PART 1: AB_CORE BASE TABLE --------------------------------------------------


--drop table int593_ab_core_base;
create table int593_ab_core_base compress for query high as

with

-- GET POSTCODES WITHIN WHICH A CAREHOME IS PRESENT

ab_postcodes as (
select
    /*+ materialize */
    distinct
    trim(replace(abc.postcode, ' ', ''))  as  ab_postcode
from
    ab_core_slim  abc
inner join
    ab_plus_total  abp
    on abp.uprn  =  abc.uprn
where
    abp.class = 'RI01'
)
--select count(*) from ab_postcodes;

-- FORMAT ADDRESS
select
    abc.uprn,
    abc.parent_uprn,
    trim(replace(regexp_replace(replace(regexp_replace(regexp_replace(regexp_replace(regexp_replace(abc.single_line_address, '[,][^,]+$'),              --split postcode (!!!)
                                                                                                                    '[,.();:#'']', ' '),                --replace special characters with a single space
                                                                                                                    '(\d)(\D)', '\1 \2'),               --add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
                                                                                                                    '(\D)(\d)', '\1 \2'),               --add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
                                                                                                                    '&', ' and '),                      --replace the ampersand character with the string "and"
                                                                                                                    '( ){2,}', ' '),                    --replace and multiple spaces with a single space
                                                                                                                    ' - ','-')                          --remove any spaces around a hyphen
                                                                                                                    ) as ab_address,
    regexp_replace(upper(abc.postcode),'[^A-Z0-9]','')  as ab_postcode,
    abp.class,
    abpc.class_desc,
    case when abp.class = 'RI01' then 1 else 0 end as carehome_flag,
    abp.latitude,
    abp.longitude
from
    ab_core_slim  abc
inner join
    ab_plus_total  abp
    on abp.uprn  =  abc.uprn
inner join
    ab_plus_class  abpc
    on abpc.code_concat  =  abp.class    
where
    trim(replace(abc.postcode, ' ', '')) in (select ab_postcode from ab_postcodes);


--------------------------------------------------------------------------------
-- PART 2: PRESC BASE TABLE ----------------------------------------------------
-- NOTE: DONE IN 2 STEPS DUE TO DWCP LINKS ... ---------------------------------


--drop table int593_presc_base;
create table int593_presc_base compress for query high as 

with

ab_postcodes as (
select
    /*+ materialize */
    distinct
    ab_postcode
from
    int593_ab_core_base
)
--select * from ab_postcodes;

select
    fact.year_month,
    fact.eps_part_date  as  part_date,
    fact.epm_id,
    fact.nhs_no,
    fact.calc_age,
    fact.pf_id
from
    AML.PX_FORM_ITEM_ELEM_COMB_FACT@dwcpb  fact  
inner join
    ab_postcodes  abp
    on abp.ab_postcode  =  upper(trim(replace(fact.patient_addr_postcode, ' ', ''))) 
where
    1=1
    and fact.year_month in (202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103)
    and fact.patient_identified = 'Y'
	and fact.pay_da_end   = 'N' -- excludes disallowed items
	and fact.pay_nd_end   = 'N' -- excludes not dispensed items
	and fact.pay_rb_end   = 'N' -- excludes referred back items
	and fact.cd_req       = 'N' -- excludes controlled drug requisitions 
	and fact.oohc_ind     = 0   -- excludes out of hours dispensing
	and fact.private_ind  = 0   -- excludes private dispensers
	and fact.ignore_flag  = 'N' -- excludes LDP dummy forms
    and fact.eps_flag = 'Y'
    and fact.calc_age >= 65;
    
    
--------------------------------------------------------------------------------
-- PART 3: PROCESSING OF PRESC_BASE ADDRESSES (DUE TO DWCP SPEED ...) ----------


--drop table int593_presc_address;
create table int593_presc_address compress for query high as 

with

-- POSTCODES WHERE THERE IS A CAREHOME

ab_postcodes as (
select
    /*+ materialize */
    distinct
    ab_postcode
from
    int593_ab_core_base
)
--select * from ab_postcodes;
,
-- BUFFER AROUND YEAR_MONTH AND AGE, 'VERIFIED' WHEN JOINING TO FACT-TABLE LATER ON

epm_base as (
select
    /*+ materialize */
    epmd.epm_id,
    epmd.part_date,
    trim(upper(epmd.pat_address_line1 ||', '|| epmd.pat_address_line2 ||', '|| epmd.pat_address_line3 ||', '|| epmd.pat_address_line4)) as etp_address,
    upper(replace(pat_address_postcode, ' ', '')) as etp_postcode
from
    SCD2.SCD2_ETP_DY_PAYLOAD_MSG_DATA@dwcpb  epmd
inner join
    ab_postcodes  abp
    on abp.ab_postcode  =  upper(trim(replace(epmd.pat_address_postcode, ' ', '')))
where
    1=1
    and substr(epmd.part_date, 1, 6) in (202002, 202003, 202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103, 202104)
    and trunc(months_between(to_date(epmd.part_date, 'YYYYMMDD'), epmd.patient_dob)/12) >= 64
)
--select * from epm_base;

-- MODIFY ADDRESS INFO
select
    /*+ materialize */
    pb.year_month,
    pb.pf_id,
    pb.epm_id,
    pb.part_date,
    pb.nhs_no,
    -- ETP ADDRESS
    trim(replace(regexp_replace(replace(regexp_replace(regexp_replace(regexp_replace(eb.etp_address, '[,.();:#'']', ' '),               --replace special characters with a single space
                                                                                                    '(\d)(\D)', '\1 \2'),               --add a space between any digit followed by a non-digit (e.g. 1A becomes 1 A)
                                                                                                    '(\D)(\d)', '\1 \2'),               --add a space between any non-digit followed by a digit (e.g. A1 becomes A 1)
                                                                                                    '&', ' and '),                      --replace the ampersand character with the string "and"
                                                                                                    '( ){2,}', ' '),                    --replace and multiple spaces with a single space
                                                                                                    ' - ','-')                          --remove any spaces around a hyphen
                                                                                                    ) as etp_address,
    -- OTHER FIELDS
    regexp_replace(eb.etp_postcode, '[^A-Z0-9]', '')  as etp_postcode,
    pb.calc_age,
    dense_rank() over (order by etp_address, etp_postcode)  address_record_id
from
    int593_presc_base  pb
inner join
    epm_base  eb
    on eb.epm_id  = pb.epm_id
    and eb.part_date  =  pb.part_date;


--------------------------------------------------------------------------------
-- PART 4: TOKENISE AB_DATA ----------------------------------------------------


--drop table int593_ab_core_tokens;
create table int593_ab_core_tokens compress for query high as

with

core_tokens as (
select
    /*+ materialize */
    uprn,
    ab_postcode,
    trim(regexp_substr(ab_address,'[^ ]+', 1, lines.column_value))  as  ab_address,
    carehome_flag
from
    int593_ab_core_base,
    table(cast(multiset
    (select level from dual connect by instr(ab_address, ' ', 1, LEVEL - 1) > 0)  as  sys.odciNumberList))  lines
    order by uprn
)
--select * from core_tokens;
,

-- CALCULATE THE NUMBER OF NUMERICAL AND NON-NUMERICAL TOKNENS PER UPRN APPEARING WITHIN AB_CORE (AFTER EARLIER CLASS FILTER)

core_count as (
select
    /*+ materialize */
    sum(case when regexp_like(ct.ab_address, '[0-9]') then 1 else 0 end)  as  ab_int_count,
    sum(case when regexp_like(ct.ab_address, '[0-9]') then 0 else 1 end)  as  ab_char_count,
    ct.uprn
from
    core_tokens  ct
where
    ct.ab_address is not null
group by
    ct.uprn
)

-- FINAL DATA
-- ALL DATA:        2M ROWS
-- CAREHOME FLAG: 185K ROWS

select
    ct.*,
    cc.ab_int_count,
    cc.ab_char_count,
    cc.ab_int_count + cc.ab_char_count  as  ab_total,
    case when regexp_like(ct.ab_address, '[0-9]') then 1 else 0 end as  int_flag
from
    core_tokens  ct
inner join
    core_count  cc
    on cc.uprn  =  ct.uprn
where
    1=1
    and ct.ab_address is not null;


--------------------------------------------------------------------------------
-- PART 5: TOKENISE ETP_DATA ---------------------------------------------------


--drop table int593_etp_tokens;
create table int593_etp_tokens compress for query high as

with

distinct_records as (
select
    /*+ materialize */
    distinct
    address_record_id,
    etp_postcode,
    etp_address
from
    int593_presc_address
)
--select * from reg_sub;
,

-- TOKENISE

etp_tokens as (
select
    /*+ materialize */
    dr.address_record_id,
    dr.etp_postcode,
    trim(regexp_substr(dr.etp_address,'[^ ]+', 1, lines.column_value))  as  etp_address
from
    distinct_records  dr,
    table(cast(multiset
    (select level from dual connect by instr(dr.etp_address, ' ', 1, LEVEL - 1) > 0)  as  sys.odciNumberList))  lines
    order by dr.address_record_id
)
--select * from etp_tokens;
,

-- CALCULATE THE NUMBER OF NUMERICAL AND NON-NUMERICAL TOKNENS PER UPRN APPEARING WITHIN AB_CORE (AFTER EARLIER CLASS FILTER)

etp_count as (
select
    /*+ materialize */
    sum(case when regexp_like(et.etp_address, '[0-9]') then 1 else 0 end)  as  etp_int_count,
    sum(case when regexp_like(et.etp_address, '[0-9]') then 0 else 1 end)  as  etp_char_count,
    et.address_record_id
from
    etp_tokens  et
where
    et.etp_address is not null
group by
    et.address_record_id
)

-- FINAL DATA
-- ALL DATA:        2M ROWS
-- CAREHOME FLAG: 185K ROWS

select
    et.*,
    ec.etp_int_count,
    ec.etp_char_count,
    ec.etp_int_count + ec.etp_char_count  as  etp_total,
    case when regexp_like(et.etp_address, '[0-9]') then 1 else 0 end as  int_flag
from
    etp_tokens  et
inner join
    etp_count  ec
    on ec.address_record_id  =  et.address_record_id
where
    1=1
    and et.etp_address is not null;


--------------------------------------------------------------------------------
-- PART 6: RESULTS BASE TABLE --------------------------------------------------


--drop table int593_results_base;
create table int593_results_base compress for query high as

select
    pa.address_record_id,
    pa.etp_postcode,
    trim(pa.etp_address)            as  etp_address,
    tk.etp_int_count,
    tk.etp_char_count,
    tk.etp_total,
    median(pa.calc_age)             as  age_median,
    count(distinct pa.pf_id)        as  total_forms,
    count(distinct(pa.nhs_no))      as  total_pat
from
    int593_presc_address  pa
left join
    (select distinct address_record_id, etp_int_count, etp_char_count, etp_total from int593_etp_tokens)  tk
    on tk.address_record_id  =  pa.address_record_id
group by
    pa.address_record_id,
    pa.etp_address,
    pa.etp_postcode,
    tk.etp_int_count,
    tk.etp_char_count,
    tk.etp_total;


--------------------------------------------------------------------------------
-- PART 7: ACTUAL ADDRESS MATCHING ---------------------------------------------


--drop table int593_results_total;
create table int593_results_total compress for query high as

-- PART 7.1: EXACT MATCHES -----------------------------------------------------
-- TIME: ~3MINUTES
-- NOTE: MATCHING TIME IS QUICKER THAN BASE TABLE GENERATION TIME
-- NOTE: ADDRESS MATCHING PROCESSING TIME NOW NOT AN ISSUE

with

exact_matches as (
select
    -- AB INFO --------------------
    abc.uprn,
    abc.parent_uprn,
    abc.class,
    abc.class_desc,
    abc.carehome_flag,
    abc.ab_address,
    -- ETP INFO -------------------
    rb.address_record_id,
    -- MATCH INFO -----------------
    (rb.etp_int_count * 4) + (rb.etp_char_count)  as  jw_score,
    (rb.etp_int_count * 4) + (rb.etp_char_count)  as  total_score,
    1                                             as  match_score,
    'EXACT'                                       as  match_type
from
    int593_results_base  rb
inner join
    int593_ab_core_base  abc
    --ON ABC.AB_ADDRESS  =  RB.ETP_ADDRESS
    on trim(replace(abc.ab_address, ' - ', '-'))  =  trim(replace(rb.etp_address, ' - ', '-'))
)
--select count(*) from exact_matches;
,

--------------------------------------------------------------------------------
-- PART 7.2: JW MATCHES --------------------------------------------------------


etp_tokens as (
select
    /*+ materialize */
    distinct
    address_record_id,
    etp_postcode,
    etp_address,
    int_flag,
    etp_int_count,
    etp_char_count,
    etp_total
from
    int593_etp_tokens
where
    address_record_id not in (select address_record_id from exact_matches)
--and address_record_id <= 1000
)
--select count(*) from etp_tokens;                                  
--select count(*) from int593_etp_tokens;                           
--select count(distinct address_record_id) from etp_tokens;         
--select count(distinct address_record_id) from int593_etp_tokens;       
,

core_tokens as (
select
    /*+ materialize */
    uprn,
    ab_postcode,
    ab_address,
    int_flag
from
    int593_ab_core_tokens
where
    ab_postcode in (select distinct etp_postcode from etp_tokens)
)
--select count(*) from int593_ab_core_tokens;                         
--select count(*) from core_tokens;                                  
,

cross_join_exact as (
select  /*+ materialize */      
    ab.uprn,
    pat.address_record_id,
    pat.etp_postcode,
    ab.ab_address,
    pat.etp_address,
    pat.int_flag,
    case when pat.int_flag = 1 then 4 else 1 end    as  jw    
from
    etp_tokens  pat
inner join
    core_tokens  ab      
    on  pat.etp_postcode    =   ab.ab_postcode
    and pat.etp_address     =   ab.ab_address
    and pat.int_flag        =   ab.int_flag
)
--select count(*) from cross_join_exact;                                         
--select * from cross_join_exact;
,

cross_join_diff as (
select 
    /*+ materialize */      
    ab.uprn,
    pat.address_record_id,
    pat.etp_postcode,
    ab.ab_address,
    pat.etp_address,
    pat.int_flag
    
from
    etp_tokens  pat
inner join
    core_tokens  ab      
    on  pat.etp_postcode    =   ab.ab_postcode
    and pat.etp_address     !=   ab.ab_address
    and pat.int_flag        =   ab.int_flag    
where
    1=1
    and pat.int_flag = 0
    and ab.int_flag = 0
    and     (   substr(ab.ab_address, 1, 1) = substr(pat.etp_address, 1, 1)
            or  substr(ab.ab_address, 2, 1) = substr(pat.etp_address, 2, 1)
            or  substr(ab.ab_address, length(ab.ab_address), 1)  =  substr(pat.etp_address, length(pat.etp_address), 1)
            or  instr(ab.ab_address, pat.etp_address) > 1
            or  instr(pat.etp_address, ab.ab_address) > 1
            )
)
--select count(*) from cross_join_diff;                              
--select * from cross_join_diff;
,

jw_union as (
select
    /*+ materialize */
    uprn,
    address_record_id,
    etp_postcode,
    ab_address,
    etp_address,
    int_flag,
    utl_match.jaro_winkler(etp_address, ab_address)  as  jw
from
    cross_join_diff  cje
where
    utl_match.jaro_winkler(etp_address, ab_address) >= 0.8
    
union all
select * from cross_join_exact
)
--select * from jw_union;
--select count(*) from jw_union;                                       
,

-- NOTE: NHS_TOKENS WITHIN GROUP_BY, CORE_TOKEN NOT WITHIN GROUP_BY
-- THIS CALCULATES THE MAXIMUM AB-TOKEN SCORE, AGAINST EACH PAT-TOKEN, FOR EACH UPRN
-- NOTE: DON'T NEED POSTCODE AS BOTH UPRN AND RECORD_ID SHARE POSTCODE ALREADY

agg_one as (
select
    /*+ materialize */
    uprn,
    address_record_id,
    etp_address,
    max(jw)  as  max_val
from
    jw_union
group by
    uprn,
    address_record_id,
    etp_address
)
--select count(*) from agg_one;
--select * from agg_one;
,

-- FOR EACH UPRN, THIS SUMS UP ALL OF THE ABOVE MAXIMUM VALUES
-- THIS MEANS THAT EACH UPRN HAS A SUMMED TOTAL FOR EACH PAT-ADDRESS THAT IT SHARES A POSTCODE WITH
-- NOTE: REMOVE TOKENS (IE 'ETP_ADDRESS' AS THESE ARE WHAT ARE BEING SUMMED)

agg_two as (
select
    /*+ materialize */
    ao.uprn,
    ao.address_record_id,
    sum(ao.max_val)                                                                               as  jw_score,
    ((et.etp_int_count * 4) + et.etp_char_count)                                                  as  total_score,
    round(sum(ao.max_val) / ((et.etp_int_count * 4) + et.etp_char_count), 4)                      as  match_score,
    rank() over (partition by ao.address_record_id order by sum(ao.max_val) desc)                 as  score_rank,
    row_number() over (partition by ao.address_record_id, sum(ao.max_val) order by ao.uprn desc)  as  desc_top_rank,
    row_number() over (partition by ao.address_record_id, sum(ao.max_val) order by ao.uprn asc)   as  asc_top_rank
from
    agg_one  ao
inner join
    (select distinct address_record_id, etp_postcode, etp_char_count, etp_int_count, etp_total from etp_tokens)  et
    on et.address_record_id  =  ao.address_record_id
group by
    ao.uprn,
    ao.address_record_id,
    ((et.etp_int_count * 4) + et.etp_char_count)
)
--select * from agg_two;
--select count(*) from agg_two;
,

-- SCORE_RANK. DESC_TOP_RANK AND ASC_TOP_RANK ENABLE TOP_RANKING MATCH TO BE FILTERED

jw_matches as (
select
    /*+ materialize */
    -- AB_CORE INFO ---------------
    agt.uprn,
    abc.parent_uprn,
    abc.class,
    abc.class_desc,
    abc.carehome_flag,
    abc.ab_address,
    -- ETP INFO -------------------
    agt.address_record_id,
    -- SCORE INFO -----------------
    agt.jw_score,
    agt.total_score,
    agt.match_score,
    'JW'  as  match_type
from
    agg_two  agt
inner join
    int593_ab_core_base  abc
    on abc.uprn  = agt.uprn
where
    1=1
    and score_rank = 1
    and desc_top_rank = 1
    and asc_top_rank = 1
)
--select count(*) from jw_matches;
--select * from jw_matches;
,

no_matches as (
select
    /*+ materialize */
    -- AB_CORE INFO ---------------
    null  as  uprn,
    null  as  parent_uprn,
    null  as  class,
    null  as  class_desc,
    0  as  carehome_flag,
    null  as  ab_address,
    -- ETP INFO -------------------
    rb.address_record_id,
    -- SCORE INFO -----------------
    0  as  jw_score,
    0  as  total_score,
    0  as  match_score,
    'NONE'  as  match_type
from
    int593_results_base  rb
where
    1=1
    and address_record_id not in (select address_record_id from jw_matches)
    and address_record_id not in (select address_record_id from exact_matches)
)
--select count(*) from exact_matches;               -- 47,202
--select count(*) from jw_matches;                  --392,451
--select count(*) from no_matches;                  -- 39,172

select
    rb.*,
    au.uprn,
    au.parent_uprn,
    au.class,
    au.class_desc,
    au.carehome_flag,
    au.ab_address,
    au.jw_score,
    au.total_score,
    au.match_score,
    au.match_type
from
    int593_results_base  rb
inner join
        (
        select * from no_matches
        union all
        select * from jw_matches
        union all
        select * from exact_matches
        )  au
        on au.address_record_id  =  rb.address_record_id;


--select count(*) from int593_results_total where match_type = 'EXACT';
--select count(*) from int593_results_total where match_type = 'JW';
--select count(*) from int593_results_total where match_type = 'NONE';
--select count(*) from int593_results_total where carehome_flag = 1;
--select count(*) from int593_results_total where carehome_flag = 0;
--select sum(total_forms) from int593_results_total where carehome_flag = 1;
--select sum(total_forms) from int593_results_total where carehome_flag = 0;
--------------------------------------------------------------------------------
-- PART 8: PAT_LEVEL RESULTS FOR DISTINCT PAT_COUNT ----------------------------


--drop table int615_etp_pfid;
create table int615_etp_pfid compress for query high as

with

address_match as (
select
    pa.address_record_id,
    pa.year_month,
    pa.pf_id,
    rt.uprn,
    match_type
from
    int593_presc_address  pa
inner join
    int593_results_total  rt
    on rt.address_record_id  =  pa.address_record_id
where
    carehome_flag = 1
)
--select * from address_match;
,

address_pf as (
select
    distinct
    year_month,
    pf_id,
    uprn,
    1  as  ch_flag,
    match_type
from
    address_match
)
--select * from address_pf;
,

pat_match as (
select
    pa.year_month,
    pa.etp_address,
    count(distinct pa.nhs_no)  as  pat_count
from
    int593_results_total  rt
inner join
    int593_presc_address  pa
    on pa.address_record_id  = rt.address_record_id
where
    1=1
    AND rt.address_record_id not in (select address_record_id from address_match)
group by
    pa.year_month,
    pa.etp_address
having
    count(distinct pa.nhs_no) >= 5
)
--select count(*) from pat_match;
,

pat_pf as (
select
    distinct
    pa.year_month,
    pa.pf_id,
    null  as  uprn,
    1  as  ch_flag,
    'PATIENT_COUNT'  as  match_type
from
    int593_presc_address  pa
inner join
    pat_match  pm
    on pm.year_month  =  pa.year_month
    and pm.etp_address  =  pa.etp_address
)
--select * from pat_pf;
--select count(*) from pat_pf;
,

word_pf as (
select
    distinct
    year_month,
    pf_id,
    null  as  uprn,
    1     as  ch_flag,
    'KEY_WORD'  as  match_type
from
    int593_presc_address
where
    1=1
    and pf_id not in (select pf_id from address_pf)
    and pf_id not in (select pf_id from pat_pf)
    and regexp_instr(etp_address, 'NURSING|RESIDENTIAL HOME|RESPITE|ELDERLY|CONVALESCENT|REST HOME|CARE HOME') != 0
    and regexp_instr(etp_address, 'CHILD|MOBILE|RESIDENTIAL SITE|RESIDENTIAL UNIT|RESIDENTIAL PARK|ABOVE|HOSPITAL') = 0
)

select * from address_pf
union all
select * from pat_pf
union all
select * from word_pf;


--select match_type, sum(ch_flag) from int615_etp_pfid group by match_type;
--select count(*) from int593_presc_base where pf_id in (select pf_id from int615_etp_pfid);
--select year_month, sum(ch_flag) from int615_etp_pfid group by year_month;
--select count(distinct uprn) from int615_etp_pfid;
--select year_month, count(distinct uprn) from int615_etp_pfid group by year_month;


--------------------------------------------------------------------------------
-- PART NINE: JOIN CH_FLAF PF_IDS TO BASE-FACT TABLE ---------------------------


create table int615_base_table compress for query high as

with

ft as (
select
    year_month,
    pf_id,
    item_count,
    item_pay_dr_nic,
    item_calc_pay_qty,
    pds_gender,
    calc_age,
    nhs_no,
    patient_addr_postcode,
    calc_prec_drug_record_id,
    eps_flag
from
    AML.PX_FORM_ITEM_ELEM_COMB_FACT@dwcpb  fact  
where
    1=1
    and fact.year_month in (202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103)
    and fact.patient_identified = 'Y'
	and fact.pay_da_end   = 'N' -- excludes disallowed items
	and fact.pay_nd_end   = 'N' -- excludes not dispensed items
	and fact.pay_rb_end   = 'N' -- excludes referred back items
	and fact.cd_req       = 'N' -- excludes controlled drug requisitions 
	and fact.oohc_ind     = 0   -- excludes out of hours dispensing
	and fact.private_ind  = 0   -- excludes private dispensers
	and fact.ignore_flag  = 'N' -- excludes LDP dummy forms
    and fact.eps_flag = 'Y'
    and fact.calc_age >= 65
)

select
    ft.*,
    pf.uprn,
    nvl(pf.ch_flag, 0)  as  ch_flag,
    pf.match_type
from
    ft
left join
    int615_etp_pfid  pf
    on pf.year_month  =  ft.year_month
    and pf.pf_id      =  ft.pf_id;


--grant select on int615_base_table to dall_ref;
--select count(*) from int615_base_table where ch_flag = 1;
--select * from int615_base_table where rownum <= 100000;
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------