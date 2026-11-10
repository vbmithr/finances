config = {
  institution = "boursobank",
  identity = "inventory",
  converter = "fin csv boursobank",
  version = "1",
  separator = ";",
  header = { "Date Opération", "Date Valeur", "Libellé", "Libellé Suggéré",
             "Catégorie", "Catégorie Parente", "Solde", "Commentaire",
             "Numéro Compte", "Libellé Compte", "Solde", "Pointage" },
  observation = {
    journal_tags = {},
    conflict_columns = { "Date Valeur", "Libellé Suggéré", "Catégorie",
                         "Catégorie Parente", "Commentaire" }
  }
}

config.observation.journal_tags["bank-category"] = "Catégorie"
config.observation.journal_tags["bank-parent-category"] = "Catégorie Parente"
config.observation.journal_tags["bank-suggested-description"] = "Libellé Suggéré"

function account(row)
  return { id = row["Numéro Compte"], bank = "boursobank", currency = "EUR" }
end

function convert(row, raw, context)
  local descriptions = array()
  if row["Libellé"] ~= "" and row["Libellé Suggéré"] ~= "" then
    descriptions = { raw["Libellé"], raw["Libellé Suggéré"] }
  elseif row["Libellé"] ~= "" then
    descriptions = { raw["Libellé"] }
  elseif row["Libellé Suggéré"] ~= "" then
    descriptions = { raw["Libellé Suggéré"] }
  end
  local balance = nil
  if row["Solde#2"] ~= "" then
    balance = {
      kind = "balance", type = "day_end", as_of = row["Date Valeur"],
      quantity = decimal(row["Solde#2"]), commodity = { kind = "fiat", code = "EUR" }
    }
  end
  return {
    kind = "record", id = observation_id(row["Numéro Compte"], row["Date Opération"]),
    occurred = row["Date Opération"], value_on = row["Date Valeur"],
    movements = { { role = "primary", quantity = decimal_comma(row["Solde"]),
                    commodity = { kind = "fiat", code = "EUR" } } },
    details = { descriptions = descriptions }
  }, balance
end
