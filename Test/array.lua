local tinip = require "tinip"

show_info = false
--show_info = true

--############################################################################

source = [[
function main() {
  var a = new [3];
  a[1] = -1;
  a[2] = -22;
  a[3] = -333;
  return a;
}
]]
result = tinip.interpreter(source, show_info)
assert(-1 == result[1], "[1]")
assert(-22 == result[2], "[2]")
assert(-333 == result[3], "[3]")
assert(3 == result.size, "result.size")


--############################################################################
-- The execution of this script reached the end of the file.
print("OK")
