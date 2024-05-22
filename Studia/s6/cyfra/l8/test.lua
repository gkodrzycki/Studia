sim.setinput("d", 0)
for sel = 1, 3 do
    sim.setinput("sel", sel)
    sim.wait(sim.posedge("clk"))
end
sim.sleep(10)
for x = 1, 100 do
    sel = math.random(0, 3)
    prevcnt = sim.getoutput("cnt"):tointeger()
    prevtop = sim.getoutput("top"):tointeger()
    newd = math.random(0, 65535)
    sim.setinput("d", newd)
    sim.setinput("sel", sel)
    sim.wait(sim.posedge("clk"))
    sim.sleep(10)
    cnt = sim.getoutput("cnt"):tointeger()
    cmp = sim.getoutput("cmp"):tointeger()
    top = sim.getoutput("top"):tointeger()
    if sel == 3 then
        assert(cnt == newd, "Error: cnt didn't load to " .. newd)
    elseif prevcnt >= prevtop then
        assert(cnt == 0, "Error: cnt didn't roll over to 0")
    else
        assert(cnt == prevcnt + 1, "Error: cnt didn't increment")
    end
    if sel == 2 then
        assert(top == newd, "Error: top didn't load to " .. newd)
    elseif sel == 1 then
        assert(cmp == newd, "Error: cmp didn't load to " .. newd)
    end
    assert(sim.getoutput("out") == vec.frombool(cnt < cmp),
        "Error: PWM output invalid")
end
print("OK!")