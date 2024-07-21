SELECT 
  base_cur, 
  conversion_cur, 
  MAX(date) AS date
FROM forex.rates 
GROUP BY base_cur, conversion_cur