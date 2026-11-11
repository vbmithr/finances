config = {
  institution = "revolut",
  identity = "inventory",
  converter = "fin csv revolut",
  version = "1",
  header = { "Type", "Product", "Started Date", "Completed Date", "Description",
             "Amount", "Fee", "Currency", "State", "Balance" }
}

function account(row)
  return { id = "current-" .. row["Currency"], bank = "revolut", number = "personal",
           type = "current-" .. row["Currency"], currency = row["Currency"] }
end

function convert(row, raw, context)
  if row["Product"] ~= "Current" then reject("unexpected Product") end
  if row["State"] ~= "COMPLETED" and row["State"] ~= "REVERTED" then
    reject("unexpected State")
  end
  local clock = "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$"
  local amount = "^-?[0-9]+(\\.[0-9]+)?$"
  if not matches(row["Started Date"], clock) then reject("unexpected Started Date") end
  if not matches(row["Amount"], amount) then reject("unexpected Amount") end
  if not matches(row["Fee"], amount) then reject("unexpected Fee") end

  local occurred = row["Started Date"]
  local movements = array()
  local reported = nil
  local balance = nil
  local fee = nil
  if row["State"] == "COMPLETED" then
    if row["Type"] == "" then reject("unexpected Type") end
    if not matches(row["Completed Date"], clock) then reject("unexpected Completed Date") end
    if row["Completed Date"] < row["Started Date"] then
      reject("Completed Date precedes Started Date")
    end
    if not matches(row["Balance"], amount) then reject("unexpected Balance") end
    if sign(row["Fee"]) == "negative" then reject("a fee cannot be negative") end
    occurred = row["Completed Date"]
    movements = { { role = "primary", quantity = subtract(row["Amount"], row["Fee"]),
                    commodity = { kind = "fiat", code = row["Currency"] } } }
    balance = decimal(row["Balance"])
    if sign(row["Fee"]) ~= "zero" then fee = decimal(row["Fee"]) end
  else
    if row["Completed Date"] ~= "" then reject("unexpected Completed Date") end
    if row["Balance"] ~= "" then reject("unexpected Balance") end
    if row["Currency"] ~= "" then
      reported = { quantity = decimal(row["Amount"]), symbol = row["Currency"] }
    end
  end
  local descriptions = array()
  if row["Description"] ~= "" then descriptions = { raw["Description"] } end
  local date = date_of_datetime(occurred)
  return {
    kind = "record", id = inventory_id("current-" .. row["Currency"], date),
    occurred = date, movements = movements, fee = fee,
    details = { descriptions = descriptions, reported = reported, balance_after = balance }
  }
end
