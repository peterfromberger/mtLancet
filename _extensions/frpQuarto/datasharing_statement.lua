return {
  ['datasharing_statement'] = function(args, kwargs, meta) 

    local md = {}

    table.insert(md, "## Data sharing\n")
    if not meta.data_sharing_statement then
        table.insert(md, "No data sharing statement found in metadata.\n")
    else
    table.insert(md, pandoc.utils.stringify(meta.data_sharing_statement) .. "\n")
    end

    -- Markdown-Text in Pandoc-Blöcke parsen, damit die ToC korrekt funktioniert
    local blocks = pandoc.read(table.concat(md, "\n")).blocks
    return blocks
  end
}
