sim.setinput("l", 1)
sim.setinput("r", 1)
sim.setinput("d", 0)
sim.setinput("c", 0)
sim.sleep(50)
sim.setinput("c", 1)
sim.sleep(50)
assert(sim.getoutput("q"):tointeger() == 0, "Error: reset failed")
val = sim.getoutput("q")
for x = 1, 100 do
    local l = math.random(0,1)
    local r = math.random(0,1)
    local i = math.random(0,1)
    sim.setinput("l", l)
    sim.setinput("r", r)
    sim.setinput("i", i)
    local nextval
    if l == 1 and r == 1 then
        nextval = vec.frominteger(math.random(0, 255), 8)
        sim.setinput("d", nextval)
    elseif l == 1 then
        nextval = vec.frominteger(i, 1) .. val(1, 7)
    elseif r == 1 then
        nextval = val(0, 7) .. vec.frominteger(i, 1)
    else
        nextval = val
end
sim.sleep(50)
sim.setinput("c", 0)
sim.sleep(100)
sim.setinput("c", 1)
sim.sleep(50)
local newval = sim.getoutput("q")
assert(newval == nextval,
     "Error: l=" .. l .. " r=" .. r .. " i=" .. i .. " previous=" .. val:tobin() ..
     " expected=" .. nextval:tobin() .. " actual=" .. newval:tobin())
    val = newval
end
print("OK")