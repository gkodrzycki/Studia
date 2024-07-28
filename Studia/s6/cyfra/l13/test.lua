example_g1 = {5, 'g'}
example_g2 = {0, 'g'}
example_g3 = {5, '-', 'g'}
example_sp = {1, 2, 's', 'p'}
example_l1 = {3, 4, 0, 'l', 's', 'p', 's', 'p'}
example_l2 = {3, 4, 1, 'l', 's', 'p', 's', 'p'}

function example_factorial(x)
    return {4, x, 6, 'j', 32, 'j', 0, 'l', 'g', 3, '*',
    14, '+', 'j', 1, 28, 'j', 0, 'l', 27, 2, 'l', 1,
    '-', '+', 6, 'j', '*', 's', 'p', 's', 'j'}
    end

function example_power(x, y)
    return {5, x, y, 7, 'j', 37, 'j', 0, 'l', 'g', 3,
    '*', 15, '+', 'j', 1, 31, 'j', 1, 'l', 30, 3,
    'l', 3, 'l', 1, '-', '+', 7, 'j', '*', 's', 'p',
    's', 'p', 's', 'j'}
    end

function example_gcd(x, y)
    return {5, x, y, 7, 'j', 68, 'j', 1, 'l', 1, 'l',
    '-', '+', 0, 'l', '-', 'g', 's', 'g', '+', 'g', 4,
    '*', 26, '+', 'j', 1, 'l', 62, 'j', 1, 'l', 1, 'l',
    '-', '+', 'g', 9, '*', 42, '+', 'j', 49, 1, 'l', 3,
    'l', 7, 'j', 62, 'j', 62, 2, 'l', 2, 'l', '-', '+',
    2, 'l', 7, 'j', 's', 'p', 's', 'p', 's', 'j'}
    end

instruction_encodings = {
    ['g'] = 0, ['-'] = 1, ['+'] = 2, ['*'] = 3,
    ['s'] = 4, ['l'] = 5, ['p'] = 6, ['j'] = 7
}

function encode_program(arr)
    local out = {}
    local i = 1
    while arr[i] do
    if type(arr[i]) == "number" then
    out[i] = arr[i]
    elseif instruction_encodings[arr[i]] then
    out[i] = 0x8000 + instruction_encodings[arr[i]]
    else
    error("Invalid symbol in program: " .. arr[i])
    end
    i = i + 1
    end
    out[i] = 0xffff
    return out
end

arr = encode_program(example_factorial(3))
sim.setinput("wr", 0)
sim.setinput("start", 0)
sim.setinput("nrst", 0)
sim.sleep(100)
sim.setinput("nrst",1)
sim.sleep(100)
assert(sim.getoutput("ready"):ishigh(), "Reset failed!")
sim.wait(sim.posedge("clk"))
sim.setinput("wr", 1)
for i, v in ipairs(arr) do
    sim.setinput("addr", i-1)
    sim.setinput("datain", v)
    sim.wait(sim.posedge("clk"))
end
sim.setinput("wr", 0)
sim.setinput("start", 1)
sim.wait(sim.posedge("clk"))
sim.setinput("start", 0)
sim.sleep(10)
assert(sim.getoutput("ready"):islow(), "Start failed!")
while sim.getoutput("ready"):islow() do
    sim.wait(sim.posedge("clk"))
    sim.sleep(10)
end
print("Program result: " .. sim.getoutput("out"):tointeger())