type 'a priority_queue

val pq_create : 'a priority_queue;;

val pq_add : 'a -> 'a priority_queue -> 'a priority_queue

val pq_min : 'a priority_queue -> 'a option

val pq_rm : 'a priority_queue -> 'a priority_queue

val pq_merge : 'a priority_queue -> 'a priority_queue -> 'a priority_queue