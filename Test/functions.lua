local tinip = require "tinip"

show_info = false
--show_info = true

--############################################################################

source = [[
function sub() {
    return 11;
}
function main() {
  return sub();
}
]]
result = tinip.interpreter(source, show_info)
assert(11 == result)


source = [[
function myfunc(k) {
  return k * 3;
}
function main() {
    var a = -1;
    return myfunc(a);
}
]]
result = tinip.interpreter(source, show_info)
assert(-3 == result)


source = [[
function sub2() {
    return 22;
}
function sub() {
    return sub2();
}
function main() {
  return sub();
}
]]
result = tinip.interpreter(source, show_info)
assert(22 == result)

--############################################################################
-- The execution of this script reached the end of the file.
print("OK")
