
local function count_words(text)
  io.stderr:write("DEBUG: Lua-Filter wurde ausgeführt\n")
  local count = 0
  for _ in string.gmatch(text, "%S+") do
    count = count + 1
  end
  io.stderr:write("Count: " .. count .. "\n")
  return count
end

function Meta(meta)
  if not meta.summary then return meta end

  local total = 0
  for _, block in pairs(meta.summary) do
    local text = pandoc.utils.stringify(block)
    total = total + count_words(text)
  end

  io.stderr:write("Total: " .. total .. "\n")

  -- WICHTIG: als MetaString
  meta["summary-wordcount"] = pandoc.Str(total)
  io.stderr:write("Count: " .. pandoc.utils.stringify(meta["summary-wordcount"]) .. "\n")
  return meta
end