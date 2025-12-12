return {
  ['acknowledgements'] = function(args, kwargs, meta) 

    local md = {}

    table.insert(md, "## Acknowledgement\n")
    if not meta.acknowledgement then
        table.insert(md, "This work has not been funded.\n")
    else
      table.insert(md, pandoc.utils.stringify(meta.acknowledgement.funding))
      if meta.acknowledgement.thanks then
        table.insert(md, pandoc.utils.stringify(meta.acknowledgement.thanks))
      end
      if meta.acknowledgement.ai_use then
        table.insert(md, pandoc.utils.stringify(meta.acknowledgement.ai_use))
      end
      table.insert(md, "\n")
    end

    -- Markdown-Text in Pandoc-Blöcke parsen, damit die ToC korrekt funktioniert
    local blocks = pandoc.read(table.concat(md, "\n")).blocks
    return blocks
  end
}
