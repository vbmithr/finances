-- Kraken has two ledger headers, but both describe the same rows.
config = {
  institution = "kraken",
  identity = "source",
  converter = "fin csv kraken",
  version = "1",
  headers = {
    { "txid", "refid", "time", "type", "subtype", "aclass", "asset",
      "amount", "fee", "balance" },
    { "txid", "refid", "time", "type", "subtype", "aclass", "subclass",
      "asset", "wallet", "amount", "fee", "balance", "amountusd", "feeusd",
      "balanceusd", "feecurrency" }
  }
}

asset_codes = {
  ZEUR = "EUR", ZUSD = "USD", ZGBP = "GBP", ZCAD = "CAD", ZAUD = "AUD", ZJPY = "JPY",
  XXBT = "BTC", XBT = "BTC", XETH = "ETH", XZEC = "ZEC", XXLM = "XLM", XXRP = "XRP",
  XLTC = "LTC", XXMR = "XMR", XETC = "ETC", XREP = "REP", XMLN = "MLN", XDAO = "DAO",
  KFEE = "FEE"
}

function asset(row)
  return asset_codes[row["asset"]] or row["asset"]
end

function classify(row)
  if row["txid"] == "" then return "skip" end
  return "record"
end

function account(row)
  return { id = asset(row), bank = "kraken" }
end

function convert(row, raw, context)
  if row["aclass"] ~= "currency" then reject("unexpected aclass") end
  local typ = row["type"]
  if typ ~= "trade" and typ ~= "deposit" and typ ~= "withdrawal"
     and typ ~= "transfer" then reject("unexpected type") end

  local code = asset(row)
  local commodity = { kind = "reported", code = code, provider = "kraken" }
  if code == "EUR" or code == "USD" or code == "GBP" or code == "CAD"
     or code == "AUD" or code == "JPY" or code == "CHF" then
    commodity = { kind = "fiat", code = code }
  end
  local fee = nil
  if sign(row["fee"]) ~= "zero" then fee = decimal(row["fee"]) end
  local balance = nil
  if row["balance"] ~= "" then balance = decimal(row["balance"]) end
  local group = nil
  local supporting = nil
  if typ == "trade" and row["refid"] ~= "" then
    group = row["refid"]
    if code == "FEE" then supporting = yes end
  end
  return {
    kind = "record", id = row["txid"], occurred = utc_datetime(row["time"]),
    movements = { { role = "primary", quantity = subtract(row["amount"], row["fee"]),
                    commodity = commodity } },
    fee = fee, group = group, supporting = supporting,
    details = { descriptions = { raw["type"] }, type = typ, balance_after = balance }
  }
end
