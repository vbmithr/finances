config = {
  institution = "bunq",
  identity = "inventory",
  converter = "fin csv bunq",
  version = "1",
  header = { "Date", "Interest Date", "Amount", "Account", "Counterparty",
             "Name", "Description" },
  observation = { journal_tags = {} }
}

config.observation.journal_tags["bunq-counterparty"] = "Counterparty"

function account(row)
  if not matches(row["Account"], "^\\S+$") then reject("unexpected Account") end
  return { id = row["Account"], bank = "bunq", currency = "EUR" }
end

function convert(row, raw, context)
  if not matches(row["Amount"], "^-?[0-9][^,.]*,[0-9]{2}$") then
    reject("unexpected Amount")
  end
  local descriptions = array()
  if row["Description"] ~= "" then descriptions = { raw["Description"] } end
  local name = nil
  local iban = nil
  if row["Name"] ~= "" then name = row["Name"] end
  if row["Counterparty"] ~= "" and valid_iban(row["Counterparty"]) then
    iban = row["Counterparty"]
  end
  local counterparty = nil
  if name or iban then counterparty = { name = name, iban = iban } end
  return {
    kind = "record",
    id = observation_id(row["Account"], row["Date"]),
    occurred = row["Date"], value_on = row["Interest Date"],
    movements = { { role = "primary", quantity = decimal_comma(row["Amount"]),
                    commodity = { kind = "fiat", code = "EUR" } } },
    details = { descriptions = descriptions, counterparty = counterparty }
  }
end
