a = {[0]={[0]="a",[1]="b"},[1]={[0]="c",[1]="d"}}
for x=0,1 do
  sim.setinput("x", x)
  for y=0,1 do
    sim.setinput("y", y)
    for v=0,1 do
      sim.setinput("a", math.random(0, 1))
      sim.setinput("b", math.random(0, 1))
      sim.setinput("c", math.random(0, 1))
      sim.setinput("d", math.random(0, 1))
      sim.setinput(a[x][y], v)
      sim.sleep(10)
      assert(sim.getoutput("o") == vec(v), "Error: x=" .. x .. " y=" .. y)
    end
  end
end
print("OK!")