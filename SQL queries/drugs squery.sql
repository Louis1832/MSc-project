WITH drug_records AS (
SELECT pr.subject_id, pr.hadm_id, icu.stay_id, pr.drug, pr.starttime, pr.stoptime, icu.intime,
ROW_NUMBER() OVER (PARTITION BY pr.hadm_id, pr.drug ORDER BY pr.starttime DESC) AS rn
FROM mimiciv_hosp.prescriptions pr
INNER JOIN mimiciv_icu.icustays icu ON pr.hadm_id = icu.hadm_id
WHERE LOWER(pr.drug) LIKE '%calcium gluconate%' OR LOWER(pr.drug) LIKE '%ondansetron%'
AND pr.starttime < icu.intime AND pr.stoptime > icu.intime)

SELECT *
FROM drug_records
WHERE rn = 1;