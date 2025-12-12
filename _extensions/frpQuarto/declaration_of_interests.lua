return {
  ['declaration_of_interests'] = function(args, kwargs, meta)

    local md = {}
    local declaration_of_interests_strings = {}

    table.insert(md, "## Declaration of interests\n")

    local authors_with_interests = {}
    local total_authors = 0

    if meta.authors then
      for _, author in ipairs(meta.authors) do
        total_authors = total_authors + 1
        if author.metadata.declaration_of_interests then
          local local_strings = {}

          -- Initialen oder Platzhalter
          local initials = "?"
          if author.metadata.initials then
            initials = pandoc.utils.stringify(author.metadata.initials)
          elseif author.name then
            initials = pandoc.utils.stringify(author.name)
          end

          table.insert(local_strings, initials .. ": ")
          table.insert(local_strings, pandoc.utils.stringify(author.metadata.declaration_of_interests))

          -- Füge Autor-Eintrag hinzu
          table.insert(declaration_of_interests_strings, table.concat(local_strings))
          
          -- Speichere Autor mit Interessen
          table.insert(authors_with_interests, initials)
        end
      end
    end

    -- Wenn kein Autor etwas angegeben hat
    if #declaration_of_interests_strings == 0 then
      table.insert(md, "The authors declare no competing interests.\n")
    -- Wenn einige Autoren etwas angegeben haben
    elseif #declaration_of_interests_strings > 0 then
      table.insert(md, table.concat(declaration_of_interests_strings, "\n"))
      
      -- Prüfe, ob es mehr als einen Autor gibt und ob nicht alle Autoren Interessen angegeben haben
      if total_authors > 1 and #authors_with_interests < total_authors then
        table.insert(md, "All other authors declare no competing interests.")
      end
    end

    -- Markdown in Pandoc-Blöcke umwandeln (damit Überschriften korrekt im TOC erscheinen)
    local blocks = pandoc.read(table.concat(md, "\n")).blocks
    return pandoc.Div(blocks, {id = "declaration-of-interests"})
  end
  
}

