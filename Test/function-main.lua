local tinip = require "tinip"

show_info = false
--show_info = true

--############################################################################

--[=[
source = [[
function main() {
  return 0
}
]]
result = tinip.interpreter(source, show_info)
assert(0 == result)


source = [[
function main() {
  return 2;
}
]]
result = tinip.interpreter(source, show_info)
assert(2 == result)
--]=]

source = [[
function main() {
  print(k);
  return 1;
}
]]
--result = tinip.interpreter(source, show_info)
result = tinip.interpreter(source, true)
print(result)
--assert(nil == result)

--############################################################################
-- The execution of this script reached the end of the file.
print("OK")
