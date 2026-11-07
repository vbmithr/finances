-- Every symbol a provider reported on a movement, such as a token contract on a network or
-- an exchange's ticker: how many movements and when, the name its rows most often give it,
-- and what it is mapped to, or whether, unmapped, it is an ISO 4217 code that counts as
-- that currency. A mapping with no movements yet, as a restore can make, is listed too.
WITH reported AS (
  SELECT m.commodity ->> 'provider' AS provider, m.commodity ->> 'code' AS symbol,
    coalesce(substr(e.occurred_at, 1, 10), e.occurred_on) AS day,
    CAST(e.payload AS TEXT) ->> '$.record.details.descriptions[0]' AS name
  FROM operation_movements m
  JOIN source_events e ON e.id = m.anchor_event_id
  WHERE m.commodity ->> 'kind' = 'reported'),
seen AS (
  SELECT provider, symbol, count(*) AS movements, min(day) AS first_seen,
    max(day) AS last_seen
  FROM reported
  GROUP BY provider, symbol),
names AS (
  SELECT provider, symbol, name,
    row_number() OVER (PARTITION BY provider, symbol ORDER BY count(*) DESC, name) AS rank
  FROM reported
  WHERE name IS NOT NULL
  GROUP BY provider, symbol, name)
SELECT s.provider, s.symbol, s.movements, s.first_seen, s.last_seen, n.name,
  a.provider IS NOT NULL AS decided, a.commodity,
  EXISTS (SELECT 1 FROM currencies c WHERE c.code = s.symbol) AS currency
FROM seen s
LEFT JOIN names n ON n.provider = s.provider AND n.symbol = s.symbol AND n.rank = 1
LEFT JOIN asset_mappings a ON a.provider = s.provider AND a.symbol = s.symbol
UNION ALL
SELECT a.provider, a.symbol, 0, NULL, NULL, NULL, 1, a.commodity, 0
FROM asset_mappings a
WHERE NOT EXISTS (SELECT 1 FROM seen s WHERE s.provider = a.provider AND s.symbol = a.symbol)
ORDER BY 1, 2
