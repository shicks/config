-- This file forked from
--   https://github.com/streetturtle/awesome-wm-widgets/tree/master/volumearc-widget
-- Fixed color for mute, added callable methods and a number in the middle

--[[
The MIT License (MIT)

Copyright (c) 2017

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
]]--

local awful = require("awful")
local beautiful = require("beautiful")
local spawn = require("awful.spawn")
local watch = require("awful.widget.watch")
local wibox = require("wibox")

-- local GET_VOLUME_CMD = 'amixer sget Master'
-- local INC_VOLUME_CMD = 'amixer sset Master 5%+'
-- local DEC_VOLUME_CMD = 'amixer sset Master 5%-'
-- local TOG_VOLUME_CMD = 'amixer sset Master toggle'
local GET_VOLUME_CMD = 'volume get'
local INC_VOLUME_CMD = 'volume up'
local DEC_VOLUME_CMD = 'volume down'
local TOG_VOLUME_CMD = 'volume toggle'

local text = wibox.widget {
    id = "txt",
    font = "Play 7",
    align = "center",
    widget = wibox.widget.textbox
}
local mirrored_text = wibox.container.mirror(text, { horizontal = true })

local volumearc = wibox.widget {
    mirrored_text,
    max_value = 1,
    thickness = 2,
    start_angle = 4.71238898, -- 2pi*3/4
    forced_height = 17,
    forced_width = 17,
    bg = "#ffffff11",
    paddings = 2,
    widget = wibox.container.arcchart,
    set_value = function(self, value)
        self.value = value
    end,
}

volumearc_widget = wibox.container.mirror(volumearc, { horizontal = true })

local update_graphic = function(widget, stdout, _, _, _)
    local mute = string.match(stdout, "off")
    local volume = string.match(stdout, "(%d?%d?%d)%%")
    if volume then
       volume = tonumber(string.format("% 3d", volume))
       widget.value = volume / 100
       if volume == 100 then
          text.font = "Play 4"
       else
          text.font = "Play 5"
       end
       text.text = volume
       if mute then
           widget.colors = { "#ff0000" }
       else
           widget.colors = { "#aaaaaa" }
       end
    else
       text.text = "xx"
       widget.colors = { "#5555ff" }
    end
end


volumearc:connect_signal("button::press", function(_, _, _, button)
    if (button == 4) then awful.spawn(INC_VOLUME_CMD, false)
    elseif (button == 5) then awful.spawn(DEC_VOLUME_CMD, false)
    elseif (button == 1) then awful.spawn(TOG_VOLUME_CMD, false)
    end

    spawn.easy_async(GET_VOLUME_CMD, function(stdout, stderr, exitreason, exitcode)
        update_graphic(volumearc, stdout, stderr, exitreason, exitcode)
    end)
end)

function volumearc_widget.up()
  awful.spawn(INC_VOLUME_CMD, false)
end

function volumearc_widget.down()
  awful.spawn(DEC_VOLUME_CMD, false)
end

function volumearc_widget.toggle()
  awful.spawn(TOG_VOLUME_CMD, false)
end

watch(GET_VOLUME_CMD, 1, update_graphic, volumearc)

return volumearc_widget
