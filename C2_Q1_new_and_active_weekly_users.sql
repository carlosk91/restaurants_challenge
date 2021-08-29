WITH 
new_users as (
SELECT 
user_id, 
min(order_date) as order_date
FROM order_info oi
GROUP BY user_id
),
weekly_new_users as (
select 
DATE_SUB(order_date, 
	INTERVAL DAYOFWEEK(order_date)-2 DAY) as order_week, 
count(distinct user_id) as new_users
from new_users
group by order_week
),
weekly_unique_active_users as (
SELECT 
DATE_SUB(order_date, 
	INTERVAL DAYOFWEEK(order_date)-2 DAY) as order_week, 
count(distinct user_id) as active_users
FROM order_info oi
GROUP BY order_week
)

SELECT *
FROM weekly_unique_active_users wau 
LEFT JOIN weekly_new_users wnu USING (order_week)
WHERE order_week between '2019-11-01' and '2020-02-29'
ORDER BY order_week
