
DROP TABLE INT615_PCD_REF;
CREATE TABLE INT615_PCD_REF AS

with

pcd as
(
select  /*+ materialize */
            opdd.POSTCODE,
        --GEOGRAPHY MAPPINGS
            opdd.LSOA_CODE          as PCD_LSOA_CODE,
            ogm1.PARENT_ONS_CODE    as PCD_WARD_CODE,
            ogm1.PARENT_NAME        as PCD_WARD_NAME,
            ogm2.PARENT_ONS_CODE    as PCD_CCG_CODE,
            ogm2.PARENT_ALT_CODE    as PCD_CCG_ID,
            ogm2.PARENT_NAME        as PCD_CCG_NAME,
            ogm3.PARENT_ONS_CODE    as PCD_LAD_CODE, 
            ogm3.PARENT_NAME        as PCD_LAD_NAME,
            ogm4.PARENT_ONS_CODE    as PCD_REGION_CODE, 
            ogm4.PARENT_NAME        as PCD_REGION_NAME,
            ogmstp.PARENT_ONS_CODE  as PCD_STP_CODE,
            ogmstp.PARENT_NAME      as PCD_STP_NAME,
        --INDICES OF MULTIPLE DEPRIVATION
            iomd.INDEX_OF_MULT_DEPRIV_SCORE,
            iomd.INDEX_OF_MULT_DEPRIV_RANK,
            iomd.INDEX_OF_MULT_DEPRIV_DECILE,
            iomd.INCOME_SCORE,
            iomd.INCOME_RANK,
            iomd.INCOME_DECILE,
            iomd.HEALTH_DEPRIVATION_SCORE,
            iomd.HEALTH_DEPRIVATION_RANK,
            iomd.HEALTH_DEPRIVATION_DECILE,
            iomd.INCOME_DEPRIV_CHILDREN_SCORE,
            iomd.INCOME_DEPRIV_CHILDREN_RANK,
            iomd.INCOME_DEPRIV_CHILDREN_DECILE,
            iomd.INCOME_DEPRIV_ELDERLY_SCORE,
            iomd.INCOME_DEPRIV_ELDERLY_RANK,
            iomd.INCOME_DEPRIV_ELDERLY_DECILE,
        --CENSUS OUTPUT AREAS
            pl.CENSUS_OUTPUT_AREA,
            pl.CENSUS_OUTPUT_AREA_CODE             as SUBGROUP_CODE,
            pl.CENSUS_OUTPUT_AREA_SUBGROUP         as SUBGROUP_NAME,
            substr(pl.CENSUS_OUTPUT_AREA_CODE,1,2) as GROUP_CODE,
            pl.CENSUS_OUTPUT_AREA_GROUP            as GROUP_NAME,
            substr(pl.CENSUS_OUTPUT_AREA_CODE,1,1) as SUPERGROUP_CODE,
            pl.CENSUS_OUTPUT_AREA_SUPERGROUP       as SUPERGROUP_NAME        

from        (
            --find the latest postcode data for each postcode
            select
                        regexp_replace(upper(POSTCODE),'[^A-Z0-9]','') as POSTCODE,
                        CENSUS_LOWER as LSOA_CODE,                        
                        YEAR_MONTH,
                        rank() over (partition by POSTCODE order by YEAR_MONTH desc) as rnk

            from        DIM.ONS_POSTCODE_DATA_DIM@dwcpb
            )                                                   opdd
inner join      STBUC.SCD2_INDEX_OF_MULTIPLE_DEPRIVATIONS@dwcpb iomd    on  opdd.LSOA_CODE          =   iomd.LSOA_CODE
left join       DALL_REF.POSTCODE_LOOKUP                        pl      on  opdd.POSTCODE           =   pl.PCD_NO_SPACES
inner join      DALL_REF.ONS_GEOGRAPHY_MAPPING                  ogm1    on  opdd.LSOA_CODE          =   ogm1.CHILD_ONS_CODE
                                                                        and ogm1.RELATIONSHIP       =   'LSOA_WARD2020'
inner join      DALL_REF.ONS_GEOGRAPHY_MAPPING                  ogm2    on  opdd.LSOA_CODE          =   ogm2.CHILD_ONS_CODE
                                                                        and ogm2.RELATIONSHIP       =   'LSOA_CCG2021'
inner join      DALL_REF.ONS_GEOGRAPHY_MAPPING                  ogmstp  on  opdd.LSOA_CODE          =   ogmstp.CHILD_ONS_CODE
                                                                        and ogmstp.RELATIONSHIP     =   'LSOA_STP2021'                                                                        
inner join      DALL_REF.ONS_GEOGRAPHY_MAPPING                  ogm3    on  ogm1.PARENT_ONS_CODE    =   ogm3.CHILD_ONS_CODE
                                                                        and ogm3.RELATIONSHIP       =   'WARD2020_LAD2020'
inner join      DALL_REF.ONS_GEOGRAPHY_MAPPING                  ogm4    on  ogm3.PARENT_ONS_CODE    =   ogm4.CHILD_ONS_CODE
                                                                        and ogm4.RELATIONSHIP       =   'LAD2020_REG2020'

where       1=1
    and     opdd.rnk = 1
)

select * from pcd;

-----SECTION END: POSTCODE CLASSIFICATION-------------------------------------------------------------------------------------------------------------