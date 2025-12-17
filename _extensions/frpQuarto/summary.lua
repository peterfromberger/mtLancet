return {
  ['summary'] = function(args, kwargs, meta)
    if not meta.summary then
        return pandoc.Div({pandoc.Para("No summary metadata found.")})
    end

    local md = {}

    table.insert(md, "## Summary {.summary}\n")
    table.insert(md, "### Background {.unnumbered .unlisted}\n" .. pandoc.utils.stringify(meta.summary.background) .. "\n")
    table.insert(md, "### Methods {.unnumbered .unlisted}\n" .. pandoc.utils.stringify(meta.summary.methods) .. "\n")
    table.insert(md, "### Findings {.unnumbered .unlisted}\n" .. pandoc.utils.stringify(meta.summary.findings) .. "\n")
    table.insert(md, "### Interpretations {.unnumbered .unlisted}\n" .. pandoc.utils.stringify(meta.summary.interpretations) .. "\n")
    table.insert(md, "### Funding {.unnumbered .unlisted}\n" .. pandoc.utils.stringify(meta.summary.funding) .. "\n")
    table.insert(md, "---\n")

    -- Markdown-Text in Pandoc-Blöcke parsen, damit die ToC korrekt funktioniert
    local blocks = pandoc.read(table.concat(md, "\n")).blocks
    return blocks
  end
}
