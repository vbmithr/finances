-- FDM's signed USD amounts describe spending; an undated row is a file total.
config = {
  institution = "fdm",
  identity = "inventory",
  converter = "fin csv fdm",
  version = "1",
  account = { id = "card", bank = "fdm", type = "prepaid-card", currency = "USD" },
  header = { "Merchant", "Category", "Date", "Local currency", "Local amount",
             "Exchange rate", "USD" },
  link_statements = yes
}

function classify(row)
  if row["Date"] == "" then return "statement" end
  return "record"
end

function convert(row, raw, context)
  local usd = { kind = "fiat", code = "USD" }
  if row["Date"] == "" then
    return {
      kind = "statement",
      id = "total-" .. context.source_hash_prefix,
      account = config.account.id,
      total = { quantity = negate(row["USD"]), commodity = usd }
    }
  end

  local descriptions = array()
  local counterparty = nil
  if row["Merchant"] ~= "" then
    descriptions = { raw["Merchant"] }
    counterparty = { name = row["Merchant"] }
  end

  local exchange = nil
  if row["Local currency"] ~= "" then
    local rate = nil
    local target_quantity = nil
    if row["Exchange rate"] ~= "" then
      local parsed = decimal(row["Exchange rate"])
      if sign(parsed) ~= "negative" then rate = parsed end
    end
    if row["Local amount"] ~= "" then
      target_quantity = decimal(row["Local amount"])
    end
    exchange = { source = "USD", target = row["Local currency"],
                 rate = rate, target_quantity = target_quantity }
  end

  return {
    kind = "record",
    id = inventory_id(config.account.id, row["Date"]),
    occurred = row["Date"],
    movements = {
      { role = "primary", quantity = negate(row["USD"]), commodity = usd }
    },
    details = { descriptions = descriptions, counterparty = counterparty,
                exchange = exchange }
  }
end
