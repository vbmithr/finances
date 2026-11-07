-- What an operation movement now says, for everything that reads one.
-- Every connection creates these views when it opens, file by file in name order, so a
-- view comes after the views it reads. They hold no data: changing one needs no rebuild.

-- Each movement with the account a review gave it, else the imported one, and the
-- commodity its provider's symbol is mapped to, else the imported one. A symbol mapped to
-- nothing is ignored, as spam is: the movement stays as evidence and never counts.
CREATE TEMP VIEW resolved_movements(id, operation_id, anchor_event_id, role, account,
                                    commodity, quantity, ignored) AS
SELECT m.id, m.operation_id, m.anchor_event_id, m.role, coalesce(r.account, m.account),
  coalesce(a.commodity, m.commodity), m.quantity,
  a.provider IS NOT NULL AND a.commodity IS NULL
FROM operation_movements m
LEFT JOIN operation_movement_resolutions r ON r.movement_id = m.id
LEFT JOIN asset_mappings a
  ON m.commodity ->> 'kind' = 'reported'
  AND a.provider = m.commodity ->> 'provider' AND a.symbol = m.commodity ->> 'code';
