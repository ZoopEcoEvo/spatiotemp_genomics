--[[
word_frontmatter.lua
Adds the title-block details that the PDF template prints, but Quarto's Word
output leaves out, using the same YAML fields so they are only written once:

  * each author line becomes "Name  Affiliation" (affiliation in italics)
  * the date line becomes three lines: Correspondence, Code, and
    "This draft is current as of <date>"
  * "Keywords: ..." is added as the last line of the abstract

It also makes figure and table captions use the caption styles in
word_template.docx (Quarto otherwise hard-codes their alignment).

Used only for the docx format (see the YAML header of manuscript_name.qmd).
It runs after Quarto's own filters, so author and date metadata are already
normalized and formatted when it sees them.
--]]

local function inlines(x)
  if x == nil then return nil end
  local t = pandoc.utils.type(x)
  if t == "Inlines" then return x end
  if t == "Blocks" then return pandoc.utils.blocks_to_inlines(x) end
  return pandoc.Inlines(pandoc.utils.stringify(x))
end

local function affiliation_text(author)
  local names = {}
  for _, aff in ipairs(author.affiliations or {}) do
    if aff.name then table.insert(names, pandoc.utils.stringify(aff.name)) end
  end
  return table.concat(names, "; ")
end

-- Caption paragraphs ------------------------------------------------------------
-- Quarto writes figure and table captions with a hard-coded left alignment in
-- raw Word XML, which also gives the paragraph two property blocks. Replace
-- that with the plain caption style, so captions follow word_template.docx.
local caption_styles = {ImageCaption = "Image Caption", TableCaption = "Table Caption"}

function Para(p)
  local first = p.content[1]
  if first and first.t == "RawInline" and first.format == "openxml"
     and first.text:match("^%s*<w:pPr>") then
    local id = first.text:match('w:pStyle w:val="(%w+)"')
    if caption_styles[id] then
      p.content:remove(1)
      -- Quarto styles table captions as image captions; give them their own
      if pandoc.utils.stringify(p):match("^Table") then id = "TableCaption" end
      return pandoc.Div({p}, pandoc.Attr("", {}, {["custom-style"] = caption_styles[id]}))
    end
  end
end

-- Quarto wraps each captioned figure and table in an invisible one-cell table
-- whose alignment overrides the caption style. Clear that alignment so the
-- caption follows word_template.docx. Only wrapper tables that hold a caption
-- are changed; the column alignment of your own tables is left alone.
local function has_caption(blocks)
  local found = false
  pandoc.Blocks(blocks):walk({Div = function(d)
    for _, name in pairs(caption_styles) do
      if d.attributes["custom-style"] == name then found = true end
    end
  end})
  return found
end

function Table(t)
  local changed = false
  for _, body in ipairs(t.bodies) do
    for _, row in ipairs(body.body) do
      for _, cell in ipairs(row.cells) do
        if has_caption(cell.contents) then
          cell.alignment = pandoc.AlignDefault
          changed = true
        end
      end
    end
  end
  if changed then
    for i, spec in ipairs(t.colspecs) do t.colspecs[i] = {pandoc.AlignDefault, spec[2]} end
    return t
  end
end

function Meta(meta)
  -- Authors with affiliations ------------------------------------------------
  if meta["by-author"] then
    local authors = pandoc.List()
    for _, a in ipairs(meta["by-author"]) do
      local line = pandoc.Inlines(pandoc.utils.stringify(a.name.literal or a.name))
      local aff = affiliation_text(a)
      if aff ~= "" then
        line:insert(pandoc.Space())
        line:insert(pandoc.Space())
        line:insert(pandoc.Emph(pandoc.Inlines(aff)))
      end
      authors:insert(pandoc.MetaInlines(line))
    end
    meta.author = authors
  end

  -- Correspondence, code, and date --------------------------------------------
  local info = pandoc.Inlines{}
  for _, key in ipairs({"contact", "repo"}) do
    if meta[key] then
      info:extend(inlines(meta[key]))
      info:insert(pandoc.LineBreak())
    end
  end
  if meta.date then
    info:extend(pandoc.Inlines("This draft is current as of "))
    info:extend(inlines(meta.date))
  end
  if #info > 0 then meta.date = pandoc.MetaInlines(info) end

  -- Keywords at the end of the abstract ----------------------------------------
  if meta.keywords and meta.abstract then
    local kw = {}
    for _, k in ipairs(meta.keywords) do table.insert(kw, pandoc.utils.stringify(k)) end
    local para = pandoc.Para({pandoc.Emph(pandoc.Inlines("Keywords")),
                              pandoc.Str(":"), pandoc.Space(),
                              table.unpack(pandoc.Inlines(table.concat(kw, ", ")))})
    local abstract = meta.abstract
    if pandoc.utils.type(abstract) == "Inlines" then
      abstract = pandoc.Blocks{pandoc.Para(abstract)}
    end
    abstract:insert(para)
    meta.abstract = pandoc.MetaBlocks(abstract)
  end

  return meta
end
