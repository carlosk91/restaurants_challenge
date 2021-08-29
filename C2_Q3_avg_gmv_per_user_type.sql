WITH 
user_by_week AS (
SELECT 
*,
lag(order_week, 1) OVER (PARTITION BY user_id ORDER BY user_id) previous_order_week
FROM
(SELECT 
user_id,
DATE_SUB(order_date, 
	INTERVAL DAYOFWEEK(order_date)-2 DAY) as order_week
FROM order_info oi
GROUP BY user_id, order_week
ORDER BY user_id, order_week) upw
),
reengaged_users AS (
SELECT *, floor(DATEDIFF(order_week, previous_order_week)/7) as inactive_weeks
FROM user_by_week ubw
WHERE floor(DATEDIFF(order_week, previous_order_week)/7) > 1
),
weekly_reengaged_users AS (
SELECT order_week, user_id
FROM reengaged_users ru
GROUP BY order_week, user_id
ORDER BY order_week, user_id
),
reengaged_users_orders AS (
SELECT 
wru.order_week, 
avg(gmv) as avg_gmv
FROM weekly_reengaged_users wru
LEFT JOIN order_info oi ON 
	(wru.user_id = oi.user_id AND
		DATE_SUB(oi.order_date, 
			INTERVAL DAYOFWEEK(order_date)-2 DAY) =
		wru.order_week)
GROUP BY wru.order_week
),
new_users_orders AS (
SELECT 
DATE_SUB(oi.order_date, 
	INTERVAL DAYOFWEEK(order_date)-2 DAY) as order_week, 
avg(gmv) as avg_gmv
FROM user_info ui
LEFT JOIN order_info oi ON 
	(ui.first_order = oi.order_id AND oi.user_id = ui.user_id)
GROUP BY order_week
),
active_users_orders as (
SELECT 
DATE_SUB(order_date, 
	INTERVAL DAYOFWEEK(order_date)-2 DAY) as order_week, 
avg(gmv) as avg_gmv
FROM order_info oi
GROUP BY order_week
)

SELECT *, 'New User' AS user_type
FROM new_users_orders nuo
WHERE nuo.order_week between '2019-11-01' and '2020-02-29'
UNION
SELECT *, 'Active User' AS user_type
FROM active_users_orders auo
WHERE auo.order_week between '2019-11-01' and '2020-02-29'
UNION
SELECT *, 'Reengaged User' AS user_type
FROM reengaged_users_orders ruo
WHERE ruo.order_week between '2019-11-01' and '2020-02-29'
