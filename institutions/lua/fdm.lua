-- FDM exports can state purchases with either sign; rows without a date are totals.
-- Each export is one card, named by --account-id or else by its file name, 1118.csv.
config = {
  institution = "fdm",
  identity = "inventory",
  converter = "fin csv fdm",
  version = "3",
  account = { id = "card", bank = "fdm", type = "prepaid-card", currency = "USD" },
  header = { "Type", "Merchant", "Category", "Date", "Local currency",
             "Local amount", "Exchange rate", "USD", "Balance after", "Note" },
  account_from_filename = yes
}

-- Exports can start mid-day and repeat a day, so a row is identified by what it
-- says, not by its position in the file. Cells are assigned by index: a nil
-- first element would end the array.
function row_id(row, context)
  local parts = {}
  parts[1] = row["Type"]
  parts[2] = row["Merchant"]
  parts[3] = row["USD"]
  parts[4] = row["Local amount"]
  return content_id(context.account_id, row["Date"], parts)
end

function classify(row)
  if row["Date"] == "" then return "statement" end
  return "record"
end

function convert(row, raw, context)
  local usd = { kind = "fiat", code = "USD" }
  if row["Date"] == "" then
    local total = row["USD"]
    local kind = "total"
    if row["Type"] == "Total spent" then
      kind = "spent"
    elseif row["Type"] == "Total deposited" then
      kind = "deposited"
    end
    return {
      kind = "statement",
      id = kind .. "-" .. context.source_hash_prefix,
      account = context.account_id,
      total = { quantity = total, commodity = usd }
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

  -- Older exports use positive spending, while current exports use signed
  -- quantities. Preserve the sign for other types such as adjustments.
  local amount = decimal(row["USD"])
  if row["Type"] == "Purchase" and sign(amount) == "positive" then
    amount = negate(amount)
  elseif row["Type"] == "Refund" and sign(amount) == "negative" then
    amount = negate(amount)
  end

  -- Balance after is retained among raw fields. These files can omit changes
  -- that affect the displayed balance, so adjacent rows need not reconcile.
  return {
    kind = "record",
    id = row_id(row, context),
    occurred = row["Date"],
    movements = {
      { role = "primary", quantity = amount, commodity = usd }
    },
    details = { descriptions = descriptions, counterparty = counterparty,
                exchange = exchange }
  }
end
