for x=0,15 do
    local v = vec(x, 4)
    sim.setinput("i", v)
    sim.sleep(10)
    local n = 0
    for p=0,3 do
        n = n + v(p):tointeger()
    end
    assert(sim.getoutput("o"):ishigh() == (n == 2 or n == 3), "Error: v=" .. v:tobin())
end
print("OK!")