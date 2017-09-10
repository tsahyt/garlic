-- Query to extract ingredients from the USDA database, as provided at
-- https://github.com/alyssaq/usda-sqlite

-- Usage in sqlite3, operating on the provided DB
-- 
-- .mode csv
-- .out usda.csv
-- .headers on
-- .read usda.sql

select
	food.long_desc name,
	food.ref_desc comment,
	"Gram" basicUnit,
	100.0 basicAmount,
	sum(case when nutrient.tagname = "PROCNT" then nutrition.amount end) protein,
	sum(case when nutrient.tagname = "CHOCDF" then nutrition.amount end) carbs,
	sum(case when nutrient.tagname = "SUGAR" then nutrition.amount end) sugars,
	sum(case when nutrient.tagname = "FIBTG" then nutrition.amount end) fibre,
	sum(case when nutrient.tagname = "FAT" then nutrition.amount end) fat,
	sum(case when nutrient.tagname = "FASAT" then nutrition.amount end) satFat,
	sum(case when nutrient.tagname = "FAPU" then nutrition.amount end) polyFat,
    sum(case when nutrient.tagname = "FAMS" then nutrition.amount end) monoFat,
	sum(case when nutrient.tagname = "FATRN" then nutrition.amount end) transFat,
	sum(case when nutrient.tagname = "NA" then nutrition.amount end) sodium,
	sum(case when nutrient.tagname = "CHOLE" then nutrition.amount end) cholesterol
from (food join nutrition on nutrition.food_id = food.id) join
     nutrient on nutrition.nutrient_id = nutrient.id
group by food.id
