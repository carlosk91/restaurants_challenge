WITH 
last_date as (
SELECT date((max(visit_date) - 4*7))
FROM restaurant_visitors
),
daily_visitors as (
SELECT 
DATE_SUB(visit_date, INTERVAL DAYOFWEEK(visit_date)-2 DAY) as visit_week, 
sum(reserve_visitors) as visitors
FROM restaurant_visitors rv
LEFT JOIN date_info di on rv.visit_date = di.calendar_date
GROUP BY visit_week
ORDER BY visit_week
),
weekly_growth as (
(SELECT 
visit_week,  
visitors,
visitors /
lag(visitors, 1) OVER (ORDER BY visit_week) - 1 as weekly_visitors_growth
FROM daily_visitors)
)

SELECT *
FROM weekly_growth
WHERE visit_week >= (select * from last_date)
