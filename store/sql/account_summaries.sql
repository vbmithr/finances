-- Each account's movements by event kind: one movement's ID, so the caller can keep the
-- kinds its registry reads as movements, then the count, the first and last booking dates
-- and how many are undecided.
SELECT min(event_id) AS event_id, institution, source, count(*) AS count,
  min(booked_on) AS first, max(booked_on) AS last,
  sum(decision = 'unclassified') AS unclassified
FROM reviews
GROUP BY institution, source, kind
