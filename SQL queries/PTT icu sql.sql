WITH PTT_records AS (
SELECT ce.subject_id, ce.charttime, ce.hadm_id, icu.stay_id, ce.valuenum AS PTT, icu.intime, icu.outtime, icu.los,
ROW_NUMBER() OVER (PARTITION BY ce.hadm_id ORDER BY ce.charttime DESC) AS rn
FROM mimiciv_icu.chartevents ce
JOIN mimiciv_icu.d_items di ON ce.itemid = di.itemid
INNER JOIN mimiciv_icu.icustays icu ON ce.hadm_id = icu.hadm_id
WHERE LOWER(di.label) LIKE '%ptt%'
  AND ce.charttime < icu.intime
  AND ce.valuenum IS NOT NULL)
  
SELECT *
FROM PTT_records
WHERE rn = 1
ORDER BY subject_id DESC;