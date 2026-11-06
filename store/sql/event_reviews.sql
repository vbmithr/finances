-- The reviews of the movements whose event IDs are in the JSON array ?1, in listing order.
SELECT event_id, institution, occurred_at, occurred_on, description, override, decision,
  counter_account, reason, card_reference, withdrawal_reversal
FROM reviews r
WHERE event_id IN (SELECT value FROM json_each(?1))
ORDER BY listed_on, occurred_at, event_id
