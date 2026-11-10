-- Everything specific to this institution lives in this Lua file.
config = {
  institution = "cdc",
  identity = "source",
  converter = "fin csv cdc",
  version = "1",
  account = { id = "app", bank = "cdc" },
  header = {
    "Timestamp (UTC)", "Transaction Description", "Currency", "Amount",
    "To Currency", "To Amount", "Native Currency", "Native Amount",
    "Native Amount (in USD)", "Transaction Kind", "Transaction Hash"
  }
}

incoming = {
  referral_card_cashback = 1, mco_stake_reward = 1, gift_card_reward = 1,
  referral_bonus = 1, referral_gift = 1, crypto_deposit = 1,
  exchange_to_crypto_transfer = 1, reimbursement = 1,
  supercharger_withdrawal = 1, lockup_unlock = 1
}
outgoing = {
  card_top_up = 1, card_cashback_reverted = 1,
  supercharger_deposit = 1, lockup_lock = 1
}
exchanges = {
  crypto_viban_exchange = 1, crypto_exchange = 1, viban_purchase = 1
}
exchanges["trading.crypto_conversion.cash"] = 1

function reported(symbol)
  return { kind = "reported", code = symbol, provider = config.institution }
end

function movement(role, value, symbol)
  return { role = role, quantity = decimal(value), commodity = reported(symbol) }
end

function present(value)
  if value ~= "" then return value end
  return nil
end

function convert(row, raw, context)
  local kind = row["Transaction Kind"]
  local amount = row["Amount"]
  local symbol = row["Currency"]
  local to_currency = row["To Currency"]
  local to_amount = row["To Amount"]
  local movements = array()
  local fallback = nil

  if incoming[kind] and sign(amount) == "positive" and to_currency == "" then
    movements = { movement("primary", amount, symbol) }
  elseif outgoing[kind] and sign(amount) == "negative" and to_currency == "" then
    movements = { movement("primary", amount, symbol) }
  elseif exchanges[kind] and sign(amount) == "negative"
         and sign(to_amount) == "positive" then
    movements = { movement("from", amount, symbol),
                  movement("to", to_amount, to_currency) }
  elseif symbol ~= "" then
    fallback = { quantity = decimal(amount), symbol = symbol }
  end

  local descriptions = array()
  if row["Transaction Description"] ~= "" then
    descriptions = { raw["Transaction Description"] }
  end

  local native = nil
  local usd = nil
  if row["Native Amount"] ~= "" then
    native = { quantity = decimal(row["Native Amount"]),
               commodity = { kind = "fiat", code = row["Native Currency"] } }
  end
  if row["Native Amount (in USD)"] ~= "" then
    usd = { quantity = decimal(row["Native Amount (in USD)"]),
            commodity = { kind = "fiat", code = "USD" } }
  end
  local valuations = array()
  if native and usd then valuations = { native, usd }
  elseif native then valuations = { native }
  elseif usd then valuations = { usd }
  end

  return {
    kind = "record",
    id = context.row_hash,
    occurred = utc_datetime(row["Timestamp (UTC)"]),
    movements = movements,
    details = {
      descriptions = descriptions,
      type = present(kind),
      references = { hash = present(row["Transaction Hash"]) },
      reported = fallback,
      valuations = valuations,
    }
  }
end
