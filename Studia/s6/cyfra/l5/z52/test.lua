for x=0,99 do
    local v = math.random(0, (1<<30)-1) << 2
    v = v + math.random(0, 3)
    local g = v ~ (v >> 1)
    sim.setinput("i", g)
    sim.sleep(50)
    assert(vec(v, 32) == sim.getoutput("o"),
        "Error: i=" .. g .. " o=" .. v)
end
print("OK!")