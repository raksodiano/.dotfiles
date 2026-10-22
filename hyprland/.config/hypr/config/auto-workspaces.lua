-- Keep one empty workspace always at the end of the list.

local TAIL_EVENTS = {
    "window.open",
    "window.destroy",
    "window.move_to_workspace",
    "workspace.created",
    "workspace.removed",
    "workspace.move_to_monitor",
    "monitor.layout_changed",
}

local tail_id   = nil -- current empty trailing workspace (nil = not yet set)
local tail_rule = nil -- its persistent workspace rule

-- Non-special workspace holding windows with the highest id.
local function highest_occupied()
    local id = 0
    for _, ws in ipairs(hl.get_workspaces()) do
        local count = ws.windows
        if count == nil then count = #hl.get_workspace_windows(ws) end
        if not ws.special and count > 0 and ws.id > id then
            id = ws.id
        end
    end
    return id
end

-- Make `id` the persistent empty workspace at the end of the list.
local function set_tail(id)
    if tail_id == id then return end
    if tail_rule then tail_rule:set_enabled(false) end
    -- tail_id first: enabling the rule may emit workspace.created (reentry).
    tail_id = id
    tail_rule = hl.workspace_rule({ workspace = id, persistent = true })
end

local function update()
    set_tail(highest_occupied() + 1)
end

-- Sync once on config reload/start.
update()

for _, event in ipairs(TAIL_EVENTS) do
    hl.on(event, update)
end

hl.on("hyprland.start", update)
hl.on("config.reloaded", update)
