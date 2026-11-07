-- What an operation movement now says, for everything that reads one.
-- Every connection creates these views when it opens, file by file in name order, so a
-- view comes after the views it reads. They hold no data: changing one needs no rebuild.

-- The ISO 4217 codes, which the connection fills from the library it was built with.
CREATE TEMP TABLE currencies(code TEXT PRIMARY KEY) STRICT;

-- The institutions whose symbols are market tickers, such as exchanges, which the
-- connection fills from the institutions it knows.
CREATE TEMP TABLE ticker_providers(provider TEXT PRIMARY KEY) STRICT;

-- The units institutions define for themselves, such as Kraken's fee credits, and the
-- asset each definition says it is, which the connection fills from the institutions it
-- knows.
CREATE TEMP TABLE own_symbols(provider TEXT NOT NULL, symbol TEXT NOT NULL,
                              asset TEXT NOT NULL, name TEXT NOT NULL,
                              PRIMARY KEY (provider, symbol)) STRICT;

-- Each movement with the account a review gave it, else the imported one, and its
-- commodity: what its provider's symbol is mapped to; else, for a unit its institution
-- defines, the asset its definition names; else, for a symbol that is an ISO 4217 code,
-- that currency; else, for an exchange's ticker that can name an asset, that asset; else
-- the imported one. A symbol mapped to nothing is ignored, as spam is: the
-- movement stays as evidence and never counts.
CREATE TEMP VIEW resolved_movements(id, operation_id, anchor_event_id, role, account,
                                    commodity, quantity, ignored) AS
SELECT m.id, m.operation_id, m.anchor_event_id, m.role, coalesce(r.account, m.account),
  CASE
    WHEN a.provider IS NOT NULL THEN coalesce(a.commodity, m.commodity)
    WHEN o.asset IS NOT NULL THEN json_object('kind', 'asset', 'code', o.asset)
    WHEN c.code IS NOT NULL THEN json_object('kind', 'fiat', 'code', c.code)
    WHEN t.provider IS NOT NULL THEN json_object('kind', 'asset', 'code', m.commodity ->> 'code')
    ELSE m.commodity
  END,
  m.quantity, a.provider IS NOT NULL AND a.commodity IS NULL
FROM operation_movements m
LEFT JOIN operation_movement_resolutions r ON r.movement_id = m.id
LEFT JOIN asset_mappings a
  ON m.commodity ->> 'kind' = 'reported'
  AND a.provider = m.commodity ->> 'provider' AND a.symbol = m.commodity ->> 'code'
LEFT JOIN own_symbols o
  ON m.commodity ->> 'kind' = 'reported'
  AND o.provider = m.commodity ->> 'provider' AND o.symbol = m.commodity ->> 'code'
LEFT JOIN currencies c
  ON m.commodity ->> 'kind' = 'reported' AND c.code = m.commodity ->> 'code'
-- A ticker names an asset only if it could be one (Validation.token); a symbol such as
-- XTZ.S stays reported.
LEFT JOIN ticker_providers t
  ON m.commodity ->> 'kind' = 'reported' AND t.provider = m.commodity ->> 'provider'
  AND m.commodity ->> 'code' NOT GLOB '*[^A-Za-z0-9_-]*';
