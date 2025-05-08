WITH pulseox_records AS (
SELECT le.subject_id, le.hadm_id, a.stay_id, le.valuenum AS pulseox, a.intime, a.outtime, a.los,
ROW_NUMBER() OVER (PARTITION BY le.hadm_id ORDER BY le.charttime DESC) AS rn
FROM mimiciv_hosp.labevents le
JOIN mimiciv_hosp.d_labitems di ON le.itemid = di.itemid
INNER JOIN mimiciv_icu.icustays a ON le.hadm_id = a.hadm_id
WHERE di.label LIKE '%O2 saturation pulseoxymetry%'
  AND le.charttime < a.intime
  AND le.valuenum IS NOT NULL)
  
SELECT *
FROM pulseox_records
WHERE rn = 1;

SELECT 
    ce.subject_id,
    ce.stay_id,
    ce.charttime,
    ce.valuenum AS spo2_percent
FROM mimiciv_icu.chartevents ce
JOIN mimiciv_icu.d_items di ON ce.itemid = di.itemid
WHERE di.label = 'O2 saturation pulseoxymetry'
  AND ce.valuenum IS NOT NULL
  AND ce.valuenum BETWEEN 0 AND 100 -- filter out likely erroneous values
ORDER BY ce.subject_id, ce.charttime;
