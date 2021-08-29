SELECT id as restaurant_id, avg(visitors) as avg_daily_visitors_on_holiday
FROM 
(SELECT id, visit_date, sum(reserve_visitors) as visitors
FROM restaurant_visitors rv
LEFT JOIN date_info di on rv.visit_date = di.calendar_date
WHERE holiday_flg = 1
GROUP BY id, visit_date) hrv
GROUP BY id
ORDER BY avg_daily_visitors_on_holiday desc
LIMIT 5
