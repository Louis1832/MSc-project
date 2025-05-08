WITH gluc_records AS (
SELECT le.subject_id, le.hadm_id, a.stay_id, le.valuenum AS glucose, a.intime, a.outtime, a.los,
ROW_NUMBER() OVER (PARTITION BY le.hadm_id ORDER BY le.charttime DESC) AS rn
FROM mimiciv_hosp.labevents le
JOIN mimiciv_hosp.d_labitems di ON le.itemid = di.itemid
INNER JOIN mimiciv_icu.icustays a ON le.hadm_id = a.hadm_id
WHERE LOWER(di.label) LIKE '%glucose%'
  AND le.charttime < a.intime
  AND le.valuenum IS NOT NULL)
  
SELECT *
FROM gluc_records
WHERE rn = 1;