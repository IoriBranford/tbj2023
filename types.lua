---@meta _

---@class frmdata
---@field i integer tile index
---@field t integer?
---@field w integer? width in tiles
---@field h integer? height in tiles
---@field flpx boolean?
---@field flpy boolean?
---@field palt integer?
---@field pal table<integer,integer>?

---@alias frm integer|frmdata tile index or frame data
---@alias ani frm|frm[] combination of frames list and default frame data

---@class obj
---@field age integer 1 on first update, negative when dead
---@field x number
---@field y number
---@field update fun(o:obj)
---@field draw fun(o:obj)

---@class sprobj:obj
---@field spri integer tile index
---@field w integer width in tiles
---@field h integer height in tiles
---@field flpx boolean
---@field flpy boolean
---@field palt integer?
---@field pal table<integer,integer>?

---@class aniobj:sprobj
---@field ani ani
---@field fi integer frame index
---@field ft integer frame time left
---@field frmflpx boolean copy of frame flpx
---@field frmflpy boolean copy of frame flpy

---@class shapeobj:obj
---@field clr integer color
---@field fill boolean

---@class circobj:shapeobj
---@field rad number radius