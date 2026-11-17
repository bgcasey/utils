-- abmi_crossref.lua
-- pandoc filter used by export_abmi_document(): numbered, labelled captions and
-- cross-references, for docx and LaTeX output.
--
-- 1. A caption can end in a label, {#tbl-name} or {#fig-name}:
--      : Agreement between maps. {#tbl-parity}      (table caption, above or below)
--      ![Study area.](map.png){#fig-map}             (figure)
--    Every captioned table and figure is numbered in document order. In docx the
--    number is written into the caption ("Table 1. ..."); LaTeX numbers its own.
-- 2. @tbl-name or [@tbl-name] (and @fig-...) become "Table 1" / "Figure 1", linked to
--    the caption. This runs before --citeproc, so these never reach the citation
--    processor. A reference with no matching label is left as written, with a warning.
-- 3. An image on a paragraph of its own without a caption is centred (docx: the
--    "Centred Image" paragraph style export_abmi_document() adds to the reference doc).

local is_docx = FORMAT:match('docx') ~= nil
local is_latex = FORMAT:match('latex') ~= nil or FORMAT:match('pdf') ~= nil

local refs = {}   -- label -> "Table 1" / "Figure 2"
local n_tab, n_fig = 0, 0

-- Remove a trailing {#tbl-x} / {#fig-x} from a caption's inlines; return the label.
local function take_label(inlines)
  local last = inlines[#inlines]
  if not last or last.t ~= 'Str' then return nil end
  local id = last.text:match('^{#([%w_%-]+)}$')
  if not id or not (id:match('^tbl%-') or id:match('^fig%-')) then return nil end
  inlines:remove(#inlines)
  while #inlines > 0 and (inlines[#inlines].t == 'Space' or inlines[#inlines].t == 'SoftBreak') do
    inlines:remove(#inlines)
  end
  return id
end

-- The caption's last paragraph (Plain or Para), whose inlines hold the label.
local function caption_inlines(caption)
  local last = caption.long[#caption.long]
  if last and (last.t == 'Plain' or last.t == 'Para') then return last.content end
  return nil
end

local function prefix(caption, label)
  local first = caption.long[1]
  if first and (first.t == 'Plain' or first.t == 'Para') then
    first.content:insert(1, pandoc.Space())
    first.content:insert(1, pandoc.Str(label))
  end
end

local function number_table(tbl)
  local inl = caption_inlines(tbl.caption)
  if not inl or #inl == 0 then return nil end
  local id = take_label(inl) or (tbl.identifier ~= '' and tbl.identifier or nil)
  n_tab = n_tab + 1
  if id then refs[id] = 'Table ' .. n_tab end
  if is_docx then
    prefix(tbl.caption, 'Table ' .. n_tab .. '.')
    -- A Div's id becomes a Word bookmark, the target of the reference links.
    if id then
      tbl.identifier = ''
      return pandoc.Div({ tbl }, pandoc.Attr(id))
    end
  elseif is_latex and id then
    inl:insert(pandoc.RawInline('latex', '\\label{' .. id .. '}'))
  end
  return tbl
end

local function number_figure(fig)
  local inl = caption_inlines(fig.caption)
  if not inl or #inl == 0 then return nil end
  local id = take_label(inl)
  if id then fig.identifier = id end
  id = fig.identifier ~= '' and fig.identifier or nil
  n_fig = n_fig + 1
  if id then refs[id] = 'Figure ' .. n_fig end
  if is_docx then prefix(fig.caption, 'Figure ' .. n_fig .. '.') end
  return fig
end

-- An image alone in its paragraph, without a caption.
local function centre_image(para)
  local content = para.content:filter(function(i) return i.t ~= 'Space' and i.t ~= 'SoftBreak' end)
  if #content ~= 1 or content[1].t ~= 'Image' then return nil end
  if is_docx then
    return pandoc.Div({ para }, pandoc.Attr('', {}, { ['custom-style'] = 'Centred Image' }))
  elseif is_latex then
    return { pandoc.RawBlock('latex', '\\begin{center}'), para, pandoc.RawBlock('latex', '\\end{center}') }
  end
end

local function is_crossref(id) return id:match('^tbl%-') or id:match('^fig%-') end

local function resolve(cite)
  for _, c in ipairs(cite.citations) do
    if not is_crossref(c.id) then return nil end   -- a real citation: leave for citeproc
  end
  local out = pandoc.Inlines({})
  for i, c in ipairs(cite.citations) do
    if i > 1 then out:insert(pandoc.Str(i == #cite.citations and ' and' or ',')); out:insert(pandoc.Space()) end
    local text = refs[c.id]
    if not text then
      io.stderr:write('[export_abmi_document] WARNING: no caption labelled {#' .. c.id .. '}\n')
      out:insert(pandoc.Str('@' .. c.id))
    elseif is_latex then
      out:insert(pandoc.RawInline('latex', '\\hyperref[' .. c.id .. ']{' .. text:gsub(' ', '~') .. '}'))
    else
      out:insert(pandoc.Link(text, '#' .. c.id))
    end
  end
  return out
end

return {
  { Table = number_table, Figure = number_figure, Para = centre_image },
  { Cite = resolve },
}
