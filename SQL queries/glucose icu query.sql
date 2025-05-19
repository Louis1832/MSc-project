WITH glucose_records AS (
SELECT ce.subject_id, ce.charttime, ce.hadm_id, icu.stay_id, ce.valuenum AS glucose, icu.intime, icu.outtime, icu.los,
ROW_NUMBER() OVER (PARTITION BY ce.hadm_id, icu.stay_id ORDER BY ce.charttime DESC) AS rn
FROM mimiciv_icu.chartevents ce
JOIN mimiciv_icu.d_items di ON ce.itemid = di.itemid
INNER JOIN mimiciv_icu.icustays icu ON ce.hadm_id = icu.hadm_id
WHERE LOWER(di.label) LIKE '%glucose%'
  AND ce.charttime < icu.intime - INTERVAL '1 hour'
  AND ce.valuenum IS NOT NULL)
  
SELECT *
FROM glucose_records
WHERE rn = 1
ORDER BY subject_id DESC;