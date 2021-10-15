SELECT
    uprn,
    class,
    addressbase_postal,
    -- Delivery Point Address
    department_name,
    rm_organisation_name,
    sub_building_name,
    building_name,
    building_number,
    po_box_number,
    dependent_thoroughfare,
    thoroughfare,
    double_dependent_locality,
    dependent_locality,
    post_town,
    postcode,
    (
        CASE WHEN department_name           IS NOT NULL THEN department_name|| ', '             ELSE '' END
     || CASE WHEN rm_organisation_name      IS NOT NULL THEN rm_organisation_name || ', '       ELSE '' END
     || CASE WHEN sub_building_name         IS NOT NULL THEN sub_building_name || ', '          ELSE '' END
     || CASE WHEN building_name             IS NOT NULL THEN building_name || ', '              ELSE '' END
     || CASE WHEN building_number           IS NOT NULL THEN building_number || ' '             ELSE '' END
     || CASE WHEN po_box_number             IS NOT NULL THEN 'PO BOX ' || po_box_number || ', ' ELSE '' END
     || CASE WHEN dependent_thoroughfare    IS NOT NULL THEN dependent_thoroughfare || ', '     ELSE '' END
     || CASE WHEN thoroughfare              IS NOT NULL THEN thoroughfare || ', '               ELSE '' END
     || CASE WHEN double_dependent_locality IS NOT NULL THEN double_dependent_locality || ', '  ELSE '' END
     || CASE WHEN dependent_locality        IS NOT NULL THEN dependent_locality || ', '         ELSE '' END
     || CASE WHEN post_town                 IS NOT NULL THEN post_town || ', '                  ELSE '' END
     || postcode
    ) AS dpa_single_line_address,
    -- Geographic Address
    la_organisation,
    -- Secondary Addressable Information (sao)
    sao_text, 
    sao_start_number, 
    sao_start_suffix, 
    sao_end_number, 
    sao_end_suffix,
    -- Primary Addressable Information (pao)
    pao_text, 
    pao_start_number, 
    pao_start_suffix, 
    pao_end_number, 
    pao_end_suffix,
    street_description,
    locality,
    town_name,
    postcode_locator,
    (
        CASE WHEN la_organisation    IS NOT NULL                                                         THEN la_organisation || ', '          ELSE '' END
        -- sao
     || CASE WHEN sao_text           IS NOT NULL                                                         THEN sao_text        || ', '          ELSE '' END
        -- no sao start suffix
     || CASE WHEN sao_start_number   IS NOT NULL AND sao_start_suffix IS NULL AND sao_end_number IS NULL THEN TO_CHAR(sao_start_number) || ', '
             WHEN sao_start_number   IS NULL                                                             THEN ''                               ELSE TO_CHAR(sao_start_number) ||'' END
        -- no sao end number
     || CASE WHEN sao_start_suffix   IS NOT NULL AND sao_end_number IS NULL                              THEN sao_start_suffix || ', '
             WHEN sao_start_suffix   IS NOT NULL AND sao_end_number IS NOT NULL                          THEN sao_start_suffix                 ELSE '' END
        -- sao start number & sao end number -> add '-' between them 
     || CASE WHEN sao_end_suffix     IS NOT NULL AND sao_end_number IS NOT NULL                          THEN '-'
             WHEN sao_start_number   IS NOT NULL AND sao_end_number IS NOT NULL                          THEN '-'                              ELSE '' END
        -- sao end number and sao end suffix
     || CASE WHEN sao_end_number     IS NOT NULL AND sao_end_suffix IS NULL                              THEN TO_CHAR(sao_end_number) || ', '
             WHEN sao_end_number     IS NULL                                                             THEN ''                               ELSE TO_CHAR(sao_end_number) END
        -- sao end suffix
     || CASE WHEN sao_end_suffix     IS NOT NULL                                                         THEN sao_end_suffix || ', '           ELSE '' END
        -- pao
     || CASE WHEN pao_text           IS NOT NULL                                                         THEN pao_text || ', '                 ELSE '' END
        -- no pao start suffix
     || CASE WHEN pao_start_number   IS NOT NULL AND pao_start_suffix IS NULL AND pao_end_number IS NULL THEN TO_CHAR(pao_start_number) || ' '
             WHEN pao_start_number   IS NULL                                                             THEN ''                               ELSE TO_CHAR(pao_start_number) || '' END
        -- no pao end number
     || CASE WHEN pao_start_suffix   IS NOT NULL AND pao_end_number IS NULL                              THEN pao_start_suffix || ', ' 
             WHEN pao_start_suffix   IS NOT NULL AND pao_end_number IS NOT NULL                          THEN pao_start_suffix                 ELSE '' END
        -- pao start number & pao end number -> add '-' between them 
     || CASE WHEN pao_end_suffix     IS NOT NULL AND pao_end_number IS NOT NULL                          THEN '-'
             WHEN pao_start_number   IS NOT NULL AND pao_end_number IS NOT NULL                          THEN '-'                              ELSE '' END
        -- pao end number and pao end suffix
     || CASE WHEN pao_end_number     IS NOT NULL AND pao_end_suffix IS NULL                              THEN TO_CHAR(pao_end_number) || ', '
             WHEN pao_end_number     IS NULL                                                             THEN ''                               ELSE TO_CHAR(pao_end_number) END
        -- pao end suffix
     || CASE WHEN pao_end_suffix     IS NOT NULL                                                         THEN pao_end_suffix || ', '           ELSE '' END
        -- street information
     || CASE WHEN street_description IS NOT NULL                                                         THEN street_description || ', '       ELSE '' END
        -- localilty
     || CASE WHEN locality           IS NOT NULL                                                         THEN locality || ', '                 ELSE '' END
        -- town
     || CASE WHEN town_name          IS NOT NULL                                                         THEN town_name || ', '                ELSE '' END
        -- postcode
     || CASE WHEN postcode_locator   IS NOT NULL                                                         THEN postcode_locator                 ELSE '' END
    ) AS geo_single_line_address 
    
FROM
    adnsh.ab_plus_total