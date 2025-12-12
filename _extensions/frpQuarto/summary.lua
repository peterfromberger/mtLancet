return {
  ['summary'] = function(args, kwargs, meta) 
    if not meta.summary then
        return pandoc.Div({pandoc.Para("No summary metadata found.")})
    end

    local md = {}

    table.insert(md, "## Summary\n")
    table.insert(md, "### Background\n" .. pandoc.utils.stringify(meta.summary.background) .. "\n")
    table.insert(md, "### Methods\n" .. pandoc.utils.stringify(meta.summary.methods) .. "\n")
    table.insert(md, "### Findings\n" .. pandoc.utils.stringify(meta.summary.findings) .. "\n")
    table.insert(md, "### Interpretations\n" .. pandoc.utils.stringify(meta.summary.interpretations) .. "\n")
    table.insert(md, "### Funding\n" .. pandoc.utils.stringify(meta.summary.funding) .. "\n")

    -- Markdown-Text in Pandoc-Blöcke parsen, damit die ToC korrekt funktioniert
    local blocks = pandoc.read(table.concat(md, "\n")).blocks
    return blocks
  end
}
