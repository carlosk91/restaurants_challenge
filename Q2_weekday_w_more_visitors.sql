select
day_of_week,
avg(visitors) as avg_visitors_by_weekday
FROM
(SELECT 
visit_date, 
day_of_week, 
sum(reserve_visitors) as visitors
FROM restaurant_visitors rv
LEFT JOIN date_info di on rv.visit_date = di.calendar_date
GROUP BY visit_date, day_of_week) vrw
GROUP BY day_of_week
ORDER BY avg_visitors_by_weekday DESC
