-- Every bank movement's review. The caller appends the order.
SELECT event_id, institution, occurred_at, occurred_on, description, override, decision,
  counter_account, reason, card_reference, withdrawal_reversal
FROM reviews r
