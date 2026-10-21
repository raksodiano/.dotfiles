-- Auto-generating "tail" workspaces.
--
-- Keeps the invariant: there is always one empty workspace right after the
-- highest-numbered workspace that has windows. When every workspace has at
-- least one window (i.e. the tail got filled), a new workspace numbered
-- max + 1 is spawned at the end.
--
-- Hyprland destroys numeric workspaces as soon as they turn empty and
-- inactive, so a trailing empty workspace cannot just be created. The trick
-- used here is a `persistent = true` workspace rule for the tail id: work
-- covered by it survive while empty. The rule moves along: once the tail
-- receives a window, its rule is disabled and a new persistent rule for
-- tail + 1 is created.

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
    -- Assign tail_id first: enabling the rule may emit workspace.created,
    -- which reenters update() and must not spawn a duplicate rule.
    tail_id = id
    tail_rule = hl.workspace_rule({ workspace = id, persistent = true })
end

local function update()
    set_tail(highest_occupied() + 1)
end

-- Also sync on reload/start: after a config reload there is no window event
-- to kick off the invariant, so run it directly.
update()

for _, event in ipairs(TAIL_EVENTS) do
    hl.on(event, update)
end

hl.on("hyprland.start", update)
hl.on("config.reloaded", update)
