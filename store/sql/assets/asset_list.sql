-- Every symbol a provider reported on a movement, such as a token contract on a network or
-- an exchange's ticker: how many movements and when, the name its rows most often state for
-- it, else the one its institution's definition gives,
-- what it is mapped to, and, unmapped, what resolved_movements counts it as by default,
-- such as the currency an ISO 4217 code names. A mapping with no movements yet, as a
-- restore can make, is listed too.
WITH reported AS (
  SELECT m.commodity ->> 'provider' AS provider, m.commodity ->> 'code' AS symbol,
    coalesce(substr(e.occurred_at, 1, 10), e.occurred_on) AS day,
    CAST(e.payload AS TEXT) -> '$.record.details.names' ->> (m.commodity ->> 'code') AS name,
    r.commodity AS resolved
  FROM operation_movements m
  JOIN source_events e ON e.id = m.anchor_event_id
  JOIN resolved_movements r ON r.id = m.id
  WHERE m.commodity ->> 'kind' = 'reported'),
seen AS (
  SELECT provider, symbol, count(*) AS movements, min(day) AS first_seen,
    max(day) AS last_seen, max(resolved) AS resolved
  FROM reported
  GROUP BY provider, symbol),
names AS (
  SELECT provider, symbol, name,
    row_number() OVER (PARTITION BY provider, symbol ORDER BY count(*) DESC, name) AS rank
  FROM reported
  WHERE name IS NOT NULL
  GROUP BY provider, symbol, name)
SELECT s.provider, s.symbol, s.movements, s.first_seen, s.last_seen,
  coalesce(n.name, o.name) AS name,
  a.provider IS NOT NULL AS decided, a.commodity,
  CASE WHEN a.provider IS NULL AND s.resolved ->> 'kind' <> 'reported' THEN s.resolved END
    AS assumed
FROM seen s
LEFT JOIN names n ON n.provider = s.provider AND n.symbol = s.symbol AND n.rank = 1
LEFT JOIN own_symbols o ON o.provider = s.provider AND o.symbol = s.symbol
LEFT JOIN asset_mappings a ON a.provider = s.provider AND a.symbol = s.symbol
UNION ALL
SELECT a.provider, a.symbol, 0, NULL, NULL, NULL, 1, a.commodity, NULL
FROM asset_mappings a
WHERE NOT EXISTS (SELECT 1 FROM seen s WHERE s.provider = a.provider AND s.symbol = a.symbol)
ORDER BY 1, 2
