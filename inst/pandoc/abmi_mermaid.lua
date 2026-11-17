-- abmi_mermaid.lua
-- pandoc filter used by export_abmi_document(): renders ```mermaid code blocks to PNG
-- with mermaid-cli (mmdc). export_abmi_document() passes its path as the metadata
-- `mermaid-cli` and a folder for the images as `mermaid-dir`; without mmdc the blocks
-- are left as code, with a warning.
--
-- A block can set the diagram's width with a line of its own, which mermaid ignores:
--   %% width: 60%        (of the text width; or a size such as 4in, 10cm, 400px)

local mmdc, outdir
local warned = false

local function meta(m)
  mmdc = m['mermaid-cli'] and pandoc.utils.stringify(m['mermaid-cli']) or nil
  outdir = m['mermaid-dir'] and pandoc.utils.stringify(m['mermaid-dir']) or nil
  if mmdc == '' then mmdc = nil end
end

local function render(block)
  if not block.classes:includes('mermaid') then return nil end
  if not mmdc or not outdir then
    if not warned then
      io.stderr:write('[export_abmi_document] WARNING: mermaid-cli (mmdc) not found; ' ..
        'mermaid diagrams are exported as code. Install it with: npm install -g @mermaid-js/mermaid-cli\n')
      warned = true
    end
    return nil
  end

  local width
  for line in block.text:gmatch('[^\n]+') do
    width = width or line:match('^%s*%%%%%s*[Ww]idth:%s*(%S+)%s*$')
  end

  local stem = outdir .. '/mermaid-' .. pandoc.utils.sha1(block.text)
  local src, png = stem .. '.mmd', stem .. '.png'
  local f = io.open(src, 'w')
  f:write(block.text)
  f:close()
  local ok, err = pcall(pandoc.pipe, mmdc,
    { '-i', src, '-o', png, '-b', 'white', '-s', '3', '-q' }, '')
  if not ok then
    io.stderr:write('[export_abmi_document] WARNING: mmdc failed; diagram exported as code.\n' ..
      tostring(err) .. '\n')
    return nil
  end

  local attr = pandoc.Attr('', {}, width and { width = width } or {})
  return pandoc.Para({ pandoc.Image({}, png, '', attr) })
end

return {
  { Meta = meta },
  { CodeBlock = render },
}
