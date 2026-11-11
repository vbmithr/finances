-- An explorer's exports of one address, as a wallet account. The wallet is the
-- address found on every row; the export lists what it sent and received.
chain = "ethereum"
native = "ETH"

config = {
  institution = "ethereum",
  identity = "source",
  converter = "fin csv ethereum",
  version = "1",
  address = { "From", "To" },
  headers = {
    -- Transactions. The price columns' names vary with the day.
    { "Transaction Hash", "Blockno", "UnixTimestamp", "DateTime (UTC)", "From", "To",
      "ContractAddress", "Value_IN(ETH)", "Value_OUT(ETH)", "CurrentValue @ *",
      "TxnFee(ETH)", "TxnFee(USD)", "Historical $Price/Eth", "Status", "ErrCode",
      "Method" },
    -- Token transfers (ERC-20).
    { "Transaction Hash", "Blockno", "UnixTimestamp", "DateTime (UTC)", "From", "To",
      "TokenValue", "USDValueDayOfTx", "ContractAddress", "TokenName", "TokenSymbol" }
  }
}

function token_value(row)
  return decimal(remove_text(row["TokenValue"], ","))
end

-- Zero-value token transfers, such as a poisoner's, are not movements.
function classify(row)
  if row["TokenValue"] ~= nil and sign(remove_text(row["TokenValue"], ",")) ~= "positive" then
    return "skip"
  end
  return "record"
end

function account(row, context)
  return { id = context.address, bank = chain, type = "wallet" }
end

function transaction(row, raw, context)
  local me = context.address
  local from = lowercase(row["From"]) == me
  local to = lowercase(row["To"]) == me
  local fee = decimal(row["TxnFee(" .. native .. ")"])
  local out = decimal(row["Value_OUT(" .. native .. ")"])
  local incoming = decimal(row["Value_IN(" .. native .. ")"])
  local quantity = nil
  local paid = nil
  if from then
    paid = fee
    if row["ErrCode"] == "" and sign(out) == "positive" then
      quantity = subtract(negate(out), fee)
    else
      quantity = negate(fee)
    end
  elseif to and sign(incoming) == "positive" and row["ErrCode"] == "" then
    quantity = incoming
  end
  local movements = array()
  if quantity ~= nil then
    movements = { { role = "primary", quantity = quantity,
                    commodity = { kind = "asset", code = native } } }
  end
  if paid ~= nil and sign(paid) == "zero" then paid = nil end
  local descriptions = array()
  local method = nil
  if row["Method"] ~= "" then
    descriptions = { raw["Method"] }
    method = row["Method"]
  end
  local references = nil
  if row["Transaction Hash"] ~= "" then references = { hash = row["Transaction Hash"] } end
  return {
    kind = "record", id = row["Transaction Hash"],
    occurred = utc_datetime(row["DateTime (UTC)"]),
    movements = movements, fee = paid,
    details = { descriptions = descriptions, type = method, references = references }
  }
end

function token_transfer(row, raw, context)
  local me = context.address
  local value = token_value(row)
  local quantity = nil
  if lowercase(row["From"]) == me then
    quantity = negate(value)
  elseif lowercase(row["To"]) == me then
    quantity = value
  end
  local movements = array()
  local names = nil
  local contract = row["ContractAddress"]
  if quantity ~= nil then
    movements = { { role = "primary", quantity = quantity,
                    commodity = { kind = "reported", code = contract, provider = chain } } }
    if row["TokenName"] ~= "" then
      names = {}
      names[contract] = row["TokenName"]
    end
  end
  local descriptions = array()
  if row["TokenName"] ~= "" then descriptions = { raw["TokenName"] } end
  local references = nil
  if row["Transaction Hash"] ~= "" then references = { hash = row["Transaction Hash"] } end
  return {
    kind = "record",
    id = row["Transaction Hash"] .. "/" .. row["From"] .. "/" .. row["To"] .. "/"
         .. contract .. "/" .. row["TokenValue"],
    occurred = utc_datetime(row["DateTime (UTC)"]),
    movements = movements,
    details = { descriptions = descriptions, references = references, names = names }
  }
end

function convert(row, raw, context)
  if row["TokenValue"] ~= nil then return token_transfer(row, raw, context) end
  return transaction(row, raw, context)
end
