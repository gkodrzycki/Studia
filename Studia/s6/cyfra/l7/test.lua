sim.setinput("nrst", 0)
sim.sleep(100)
assert(sim.getoutput("out"):tointeger() == 0, "Error: reset failed")
sim.setinput("nrst", 1)
sim.sleep(100)
sim.wait(sim.posedge("clk"))
sim.sleep(25)
cnt = sim.getoutput("out"):tointeger()
for x = 1, 100 do
    local step = math.random(0,1)
    local down = math.random(0,1)
    local nextcnt = (cnt + (1 - down * 2) * (step + 1)) % 16
    sim.setinput("step", step)
    sim.setinput("down", down)
    sim.wait(sim.posedge("clk"))
    sim.sleep(25)
    local newcnt = sim.getoutput("out"):tointeger()
    assert(newcnt == nextcnt,
        "Error: step=" .. step .. " down=" .. down .. " previous=" .. cnt ..
        " expected=" .. nextcnt .. " actual=" .. newcnt)
    cnt = newcnt
end
print("OK!")