local tinip = require "tinip"

show_info = false
--show_info = true

--############################################################################

source = [[
function main() {
  var i = 1;
  while (i <= 1) {
    i = i + 1;
  };
  return i;
}
]]
result = tinip.interpreter(source, show_info)
assert(2 == result)


source = [[
function main() {
  var i = 1;
  while (i <= 1000) {
    i = i + 1;
  };
  return i;
}
]]
result = tinip.interpreter(source, show_info)
assert(1001 == result)


--############################################################################
-- The execution of this script reached the end of the file.
print("OK")
