SELECT pa.subject_id, a.hadm_id, icu.stay_id, pa.anchor_age + (EXTRACT(YEAR FROM a.admittime) - pa.anchor_year) AS age, pa.gender, a.race, a.hospital_expire_flag, a.insurance
FROM mimiciv_hosp.patients pa
JOIN mimiciv_hosp.admissions a ON pa.subject_id = a.subject_id
INNER JOIN mimiciv_icu.icustays icu ON a.hadm_id = icu.hadm_id;

