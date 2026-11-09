-- A second institution using the same host, without CDC-specific OCaml.
config = {
  institution = "example-bank",
  identity = "source",
  converter = "fin csv example-bank",
  version = "1",
  account = { id = "checking" },
  header = { "Date", "Description", "Amount" }
}

function convert(row, raw, context)
  local descriptions = array()
  if row["Description"] ~= "" then descriptions = { raw["Description"] } end
  return {
    kind = "record",
    id = context.row_hash,
    account = config.account.id,
    occurred = row["Date"],
    movements = {
      { role = "primary", quantity = decimal(row["Amount"]),
        commodity = { kind = "fiat", code = "EUR" } }
    },
    details = { descriptions = descriptions, fields = context.fields }
  }
end
