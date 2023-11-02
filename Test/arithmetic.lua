local tinip = require "tinip"

show_info = false
--show_info = true

--############################################################################

source = [[
function main() {
  var a = 1;
  var b = 2;
  return a + b;
}
]]
result = tinip.interpreter(source, show_info)
assert(3 == result)


source = [[
function main() {
  var a = 1;
  var b = 2;
  return a - b;
}
]]
result = tinip.interpreter(source, show_info)
assert(-1 == result)


source = [[
function main() {
  var a = 1;
  var b = 2;
  return a * b;
}
]]
result = tinip.interpreter(source, show_info)
assert(2 == result)


source = [[
function main() {
  var a = 1;
  var b = 2;
  return a / b;
}
]]
result = tinip.interpreter(source, show_info)
assert(math.abs(result - 0.5) < 1e-9)


source = [[
function main() {
  var x1 = 1;
  var x2 = 2;
  var x3 = (x1 + x2)*x2 - x1*3;
  # (1 + 2)*2 - 1*3 ==> 6 - 3 ==> 3
  return x3;
}
]]
result = tinip.interpreter(source, show_info)
assert(3 == result)

--############################################################################
-- The execution of this script reached the end of the file.
print("OK")
