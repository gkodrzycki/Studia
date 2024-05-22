function r()
	return math.random(0, 15)
end
for x=0,99 do
  local t = {r(), r(), r(), r()}
  local u = {}
  
  for k, v in pairs(t) do u[k] = vec(v, 4) end
    sim.setinput("i", u[4] .. u[3] .. u[2] .. u[1])
    sim.sleep(50)
    table.sort(t)
    local r = sim.getoutput("o")
    for k in pairs(t) do
      assert(r((k-1)*4, 4):tointeger() == t[k],
      "Error: i=" .. (u[4] .. u[3] .. u[2] .. u[1]):tohex())
     end
end
print("OK!")