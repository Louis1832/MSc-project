SELECT DISTINCT (b.stay_id), a.subject_id, a.hadm_id, b.stay_id, a.icd_code, b.los, d.drug 
FROM mimiciv_hosp.diagnoses_icd a
INNER JOIN mimiciv_hosp.prescriptions d ON a.hadm_id = d.hadm_id
INNER JOIN mimiciv_icu.icustays b ON a.hadm_id = b.hadm_id
WHERE 
    a.icd_code IN ('1620', '1622', '1623', '1624', '1625', '1628', '1629', 
                   'C33', 'C3400', 'C3401', 'C3402', 'C3410', 'C3411', 'C3412', 
                   'C342', 'C3430', 'C3431', 'C3432', 'C3480', 'C3481', 'C3482', 
                   'C3490', 'C3491', 'C3492')
    AND (
       d.drug ILIKE '%nivolumab%' 
       OR d.drug ILIKE '%pembrolizumab%' 
       OR d.drug ILIKE '%cemiplimab%'  -- PD-1 inhibitors
       OR d.drug ILIKE '%atezolizumab%' 
       OR d.drug ILIKE '%durvalumab%' 
       OR d.drug ILIKE '%avelumab%'  -- PD-L1 inhibitors
       OR d.drug ILIKE '%ipilimumab%' 
       OR d.drug ILIKE '%tremelimumab%'  -- CTLA-4 inhibitors    
       OR d.drug ILIKE '%bevacizumab%'  -- Anti-VEGF
       OR d.drug ILIKE '%aldesleukin%' 
       OR d.drug ILIKE '%tilsotolimod%'  -- Interleukins
       OR d.drug ILIKE '%interferon%'  -- Interferons
       OR d.drug ILIKE '%axicabtagene%' 
       OR d.drug ILIKE '%lisocabtagene%'  -- CD19 CAR-T
       OR d.drug ILIKE '%ciltacabtagene%'  -- BCMA CAR-T
       OR d.drug ILIKE '%thalidomide%' 
       OR d.drug ILIKE '%lenalidomide%' 
       OR d.drug ILIKE '%pomalidomide%'  -- Immunomodulators
       OR d.drug ILIKE '%tofacitinib%'  -- JAK inhibitors
       OR d.drug ILIKE '%everolimus%' 
       OR d.drug ILIKE '%sirolimus%'  -- mTOR inhibitors
	   OR d.drug ILIKE '%amivantamab%'
	   OR d.drug ILIKE '%osimertinib%'
	   OR d.drug ILIKE '%alectinib%'
	   OR d.drug ILIKE '%erlotinib%'
	   OR d.drug ILIKE '%dostarlimab%');