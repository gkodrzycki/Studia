sim.setinput("step", 0)
sim.setinput("nrst", 0)
sim.sleep(100)
assert(sim.getoutput("out"):tointeger() == 0
    and sim.getoutput("cnt"):tointeger() == 0, "Error: reset failed")
sim.setinput("nrst", 1)
sim.sleep(100)
ops = {2, 2, '*', '-', 2, '+', ' '}
res = -2
for k, v in ipairs(ops) do
    if type(v) == "number" then
        sim.setinput("push", 1)
        sim.setinput("d", v)
    elseif type(v) == "string" then
        sim.setinput("push", 0)
        if v == ' ' then
            sim.setinput("op", 0)
        elseif v == '-' then
            sim.setinput("op", 1)
        elseif v == '+' then
            sim.setinput("op", 2)
        elseif v == '*' then
            sim.setinput("op", 3)
        end
    end
    sim.sleep(50)
    sim.setinput("step", 1)
    sim.sleep(50)
    sim.setinput("step", 0)
end
sim.sleep(50)
assert(sim.getoutput("out"):tointegersigned() == res
    and sim.getoutput("cnt"):tointeger() == 1, "Error: invalid result")
print("OK!")