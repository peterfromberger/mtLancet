return {
  ['panel'] = function(args, kwargs, meta)

    if not meta.panel then
      return pandoc.Div(
        { pandoc.Para("No panel metadata found.") },
        pandoc.Attr("", {"callout", "callout-note", "research-panel", "panel-missing"})
      )
    end

    local blocks = {}

    -- Haupttitel (Callout-Header-Ersatz)
    table.insert(blocks,
      pandoc.Para(
        pandoc.Span(
          "Research in context",
          pandoc.Attr("", {"panel-main-title"})
        )
      )
    )

    -- Hilfsfunktion für Unterabschnitte
    local function panel_section(title, content, section_class)
      return pandoc.Div(
        {
          pandoc.Para(
            pandoc.Span(
              title,
              pandoc.Attr("", {"panel-section-title", section_class})
            )
          ),
          pandoc.Para(pandoc.utils.stringify(content))
        },
        pandoc.Attr("", {"panel-section"})
      )
    end

    table.insert(blocks,
      panel_section(
        "Evidence before this study",
        meta.panel.evidence_before_this_study,
        "evidence-before"
      )
    )

    table.insert(blocks,
      panel_section(
        "Added value of this study",
        meta.panel.added_value_of_this_study,
        "added-value"
      )
    )

    table.insert(blocks,
      panel_section(
        "Implications of all the available evidence",
        meta.panel.implications_of_all_the_available_evidence,
        "implications"
      )
    )

    -- 🔹 Callout-Hybrid-Container
    return pandoc.Div(
      blocks,
      pandoc.Attr("", {
        "callout",
        "callout-note",
        "research-panel",
        "research-panel-context"
      })
    )
  end
}

