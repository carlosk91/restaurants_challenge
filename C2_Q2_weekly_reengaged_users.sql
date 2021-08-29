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
)

SELECT order_week, count(distinct user_id) as reengaged_users
FROM reengaged_users ru
WHERE order_week between '2019-11-01' and '2020-02-29'
GROUP BY order_week
ORDER BY order_week
