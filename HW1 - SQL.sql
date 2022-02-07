#6
select a.category_name, b.product_name, b.list_price
from categories as a join products as b 
on a.category_id = b.category_id
order by 1,2;

#7
select a.first_name, a.last_name, b.line1, b.city, b.state, b.zip_code
from customers as a join addresses as b 
on a.customer_id = b.customer_id
where a.email_address = "allan.sherwood@yahoo.com";

#8
select a.first_name, a.last_name, b.line1, b.city, b.state, b.zip_code
from customers as a join addresses as b 
on a.customer_id = b.customer_id
where b.address_id = a.shipping_address_id;

#9
select 
a.last_name, a.first_name, 
b.order_date, c.product_name, 
d.item_price, d.discount_amount, d.quantity
from 
customers as a, orders as b, 
products as c, order_items as d
where 
(a.customer_id = b.customer_id) and
(b.order_id = d.order_id) and
(d.product_id = c.product_id)
order by a.last_name, b.order_date, c.product_name;

#10
select product_name, list_price 
from products 
where list_price in  
(select list_price from products 
group by list_price 
having count(list_price) > 1)
order by product_name;

#11
select 
"SHIPPED" as ship_status,
order_id, order_date
from orders
where ship_date is not null
union
select 
"NOT SHIPPED" as ship_status,
order_id, order_date
from orders
where ship_date is null
order by order_date;

#12
select 
c.category_name,
count(p.product_id) as product_cnt,
max(p.list_price) as highest_price
from categories as c, products as p
where c.category_id = p.category_id
group by c.category_id
order by product_cnt desc;

#13
select 
email_address, count(distinct(order_id)),sum(total_amount)
from (select
c.email_address,
o.order_id,
(i.item_price - i.discount_amount)*i.quantity as total_amount
from customers as c, orders as o, order_items as i
where (c.customer_id = o.customer_id) and
(o.order_id = i.order_id)) as a
group by email_address
having count(distinct(order_id)) > 1
order by sum(total_amount) desc;


#14
select
c.email_address,
count(distinct p.product_id)
from customers as c, orders as o, 
order_items as i, products as p
where (c.customer_id = o.customer_id) and
(o.order_id = i.order_id) and
(p.product_id = i.product_id)
group by c.customer_id
having count(distinct p.product_id) > 1;

#15
select product_name, list_price
from products
where list_price > (select avg(list_price) from products)
order by list_price desc;

#16
select category_name from categories
where not exists
(select * from products
where category_id = categories.category_id);

#17
select c.email_address, o.order_id, 
sum((i.item_price - i.discount_amount)*i.quantity) as order_total
from customers as c, orders as o, order_items as i
where (c.customer_id = o.customer_id) and
(o.order_id = i.order_id)
group by i.order_id;

with a as 
(select c.email_address, o.order_id, 
sum((i.item_price - i.discount_amount)*i.quantity) as order_total
from customers as c, orders as o, order_items as i
where (c.customer_id = o.customer_id) and
(o.order_id = i.order_id)
group by i.order_id)
select 
a.email_address, a.order_id as largest_order_id 
from a
join (select email_address, max(order_total) from a group by email_address) as b
on a.order_total = b.`max(order_total)` and a.email_address = b.email_address;


select c.email_address, o.order_id, 
sum((i.item_price - i.discount_amount)*i.quantity) as order_total
from customers as c, orders as o, order_items as i
where (c.customer_id = o.customer_id) and
(o.order_id = i.order_id)
group by i.order_id;
####
with a as 
(select c.email_address, o.order_id, 
sum((i.item_price - i.discount_amount)*i.quantity) as order_total
from customers as c, orders as o, order_items as i
where (c.customer_id = o.customer_id) and
(o.order_id = i.order_id)
group by i.order_id)
select 
a.email_address, a.order_id as largest_order_id 
from a；



#18
select product_name, discount_percent
from products
where discount_percent in 
(select discount_percent from products 
group by discount_percent having count(discount_percent) = 1)
order by product_name;

#19
select
list_price,
format(list_price,1),
convert(list_price,unsigned),
cast(list_price as unsigned),
date_added,
cast(date_added as date),
cast(cast(date_added as date) as char(7)),
cast(date_added as time) from products;

#20
select
card_number,
length(card_number),
SUBSTRING(card_number, -4),
concat('XXXX-XXXX-XXXX-', SUBSTRING(card_number, -4))
from orders;


select sum(order_id) as 'oder a' from orders;

# full record
with a as (select d.name, e.salary
from employees as e, departments as d
where (e.department_id = d.id) and (e.salary >= 100000))

select
sum(salary)/(select sum(salary) from a) as percentage_over_100K,
name as “department name”
count(salary) as “number of employees”
from a group by name
having count(salary) >= 10
order by percentage_over_100K limit 3;

# d and e
with a as (select d.name as department, e.salary
from employees as e, departments as d
where (e.department_id = d.id))
select 
count(b.salary)/c.d_pop as percentage_over_100K,
b.department as 'department name',
count(b.salary) as 'number of employees'
from (select * from a where salary>=100000) as b
join (select department,count(salary)as d_pop from a group by department) as c
on b.department = c.department
group by b.department
having count(b.salary) >= 10
order by percentage_over_100K limit 3;


with a as
(select products.product_id, #categories.category_id,
max(CASE WHEN categories.category_id = 3 THEN 'True' ELSE 'False' end) as hhh
from products,categories
where products.category_id = categories.category_id
group by product_id) 
select avg(product_id),hhh
from a
group by hhh;



select products.product_id, categories.category_id
from products,categories
where products.category_id = categories.category_id;

with k as 
(select ship_amount,max(order_date),min(order_date)
from orders 
group by ship_amount)
select ship_amount from k
where `max(order_date)` != `min(order_date)`;

(select round(ship_amount,1),max(date(order_date)),min(date(order_date))
from orders 
group by ship_amount);

DATE_SUB("2020-11-22", INTERVAL 2 MONTH);


