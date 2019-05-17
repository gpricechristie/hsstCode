SELECT cust.first_name, cust.last_name,
film.title
FROM customer cust
JOIN rental rent on cust.customer_id = rent.customer_id
JOIN inventory inv on rent.inventory_id = inv.inventory_id
JOIN film film on film.film_id = inv.film_id
WHERE film.title ILIKE '%Whale%'
ORDER BY cust.first_name, cust.last_name,
film.title