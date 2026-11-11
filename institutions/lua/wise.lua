config = {
  institution = "wise",
  identity = "source",
  converter = "fin csv wise",
  version = "1",
  header = { "TransferWise ID", "Date", "Date Time", "Amount", "Currency",
             "Description", "Payment Reference", "Running Balance", "Exchange From",
             "Exchange To", "Exchange Rate", "Payer Name", "Payee Name",
             "Payee Account Number", "Merchant", "Card Last Four Digits",
             "Card Holder Full Name", "Attachment", "Note", "Total fees",
             "Exchange To Amount", "Transaction Type", "Transaction Details Type" },
  observation = {
    journal_tags = {}, journal_description = "Merchant",
    conflict_columns = { "Total fees", "Exchange From", "Exchange To", "Exchange Rate",
                         "Exchange To Amount" }
  }
}

config.observation.journal_tags["wise-merchant"] = "Merchant"
config.observation.journal_tags["wise-payment-reference"] = "Payment Reference"
config.observation.journal_tags["wise-reported-fees"] = "Total fees"

function account(row)
  return { id = "balance-" .. row["Currency"], bank = "wise", currency = row["Currency"] }
end

function present(value)
  if value ~= "" then return value end
  return nil
end

function convert(row, raw, context)
  local kind = row["Transaction Type"]
  if kind ~= "DEBIT" and kind ~= "CREDIT" then reject("unexpected Transaction Type") end
  if row["TransferWise ID"] == "" then reject("unexpected TransferWise ID") end
  if not matches(row["Date Time"], "^[0-9]{2}-[0-9]{2}-[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}$") then
    reject("unexpected Date Time")
  end
  if row["Date"] == "" or not starts_with(row["Date Time"], row["Date"]) then
    reject("Date disagrees with Date Time")
  end
  local amount = "^-?[0-9]+(\\.[0-9]+)?$"
  if not matches(row["Amount"], amount) then reject("unexpected Amount") end
  if not matches(row["Running Balance"], amount) then reject("unexpected Running Balance") end
  if not matches(row["Total fees"], amount) then reject("unexpected Total fees") end
  if kind == "DEBIT" and sign(row["Amount"]) == "positive" then reject("unexpected Amount") end
  if kind == "CREDIT" and sign(row["Amount"]) == "negative" then reject("unexpected Amount") end

  local reference = normalize_hex(row["TransferWise ID"], row["Merchant"],
                                  "BALANCE_CASHBACK-", "cashback", 32)
  local descriptions = array()
  if row["Description"] ~= "" and row["Merchant"] ~= "" then
    descriptions = { raw["Description"], raw["Merchant"] }
  elseif row["Description"] ~= "" then descriptions = { raw["Description"] }
  elseif row["Merchant"] ~= "" then descriptions = { raw["Merchant"] }
  end
  local name = present(row["Payee Name"])
  if not name then name = present(row["Payer Name"]) end
  local iban = nil
  if row["Payee Account Number"] ~= "" and valid_iban(row["Payee Account Number"]) then
    iban = row["Payee Account Number"]
  end
  local counterparty = nil
  if name or iban then counterparty = { name = name, iban = iban } end
  local exchange = nil
  if row["Exchange From"] ~= "" and row["Exchange To"] ~= "" then
    local rate = nil
    local target = nil
    if row["Exchange Rate"] ~= "" then
      local value = decimal(row["Exchange Rate"])
      if sign(value) ~= "negative" then rate = value end
    end
    if row["Exchange To Amount"] ~= "" then target = decimal(row["Exchange To Amount"]) end
    exchange = { source = row["Exchange From"], target = row["Exchange To"],
                 rate = rate, target_quantity = target }
  end
  return {
    kind = "record", id = reference .. "/" .. row["Date Time"] .. "/" .. kind,
    occurred = day_first(row["Date"]),
    movements = { { role = "primary", quantity = decimal(row["Amount"]),
                    commodity = { kind = "fiat", code = row["Currency"] } } },
    details = { descriptions = descriptions, counterparty = counterparty,
                references = { servicer = reference, payment = present(row["Payment Reference"]) },
                exchange = exchange, balance_after = decimal(row["Running Balance"]) }
  }
end
