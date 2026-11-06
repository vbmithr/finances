-- Each account's movements by event kind, so that the caller keeps the kinds its registry
-- reads as movements: the count, the first and last booking dates and how many are
-- undecided.
SELECT kind, institution, source, count(*) AS count,
  min(booked_on) AS first, max(booked_on) AS last,
  sum(decision = 'unclassified') AS unclassified
FROM reviews
GROUP BY institution, source, kind
