SELECT b.stay_id, a.subject_id, a.hadm_id, a.icd_code, b.los, b.intime, b.outtime 
FROM mimiciv_hosp.diagnoses_icd a
INNER JOIN mimiciv_icu.icustays b ON a.hadm_id = b.hadm_id
WHERE 
    a.icd_code IN ('1620', '1622', '1623', '1624', '1625', '1628', '1629', 
                   'C33', 'C3400', 'C3401', 'C3402', 'C3410', 'C3411', 'C3412', 
                   'C342', 'C3430', 'C3431', 'C3432', 'C3480', 'C3481', 'C3482', 
                   'C3490', 'C3491', 'C3492');