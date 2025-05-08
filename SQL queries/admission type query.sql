SELECT ad.subject_id, ad.hadm_id, icu.stay_id, ad.admission_type
FROM mimiciv_hosp.admissions ad
INNER JOIN mimiciv_icu.icustays icu ON ad.hadm_id = icu.hadm_id;

SELECT DISTINCT admission_type, COUNT(*) as count
FROM mimiciv_hosp.admissions
GROUP BY admission_type
ORDER BY count DESC;