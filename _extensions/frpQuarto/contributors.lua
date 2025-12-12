return {
  ['contributors'] = function(args, kwargs, meta) 

    local md = {}

    table.insert(md, "[Contributors]{.contributors}\n")

    if meta.authors then

      for _, author in ipairs(meta.authors) do
        -- Initialen oder Name
        local initials = ""
        if author.metadata.initials then
          initials = pandoc.utils.stringify(author.metadata.initials)
        else
          initials = "?"
        end

        table.insert(md, initials .. ": ")

        -- Rollen
        if author.roles then
          local role_strings = {}
          for _, role in ipairs(author.roles) do
            table.insert(role_strings, pandoc.utils.stringify(role.role))
          end
          table.insert(md, table.concat(role_strings, ', ') .. '. ')
        end
      end
  

      -- Markdown-Text in Pandoc-Blöcke parsen, damit die ToC korrekt funktioniert
      local blocks = pandoc.read(table.concat(md, "\n")).blocks
      return blocks
    end
  end
}
