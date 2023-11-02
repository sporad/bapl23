local tinip = require "tinip"

show_info = false
--show_info = true

--############################################################################

source = [[
function main() {
  var a = 1;
  var b = 2;
  if a == b {
    return 11;
  } else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(22 == result)


source = [[
function main() {
  var a = 1;
  var b = 2;
  if a ~= b {
    return 11;
  } else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(11 == result)


source = [[
function main() {
  var a = 1;
  var b = 2;
  if a > b {
    return 11;
  } else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(22 == result)


source = [[
function main() {
  var a = 1;
  var b = 2;
  if a >= b {
    return 11;
  } else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(22 == result)


source = [[
function main() {
  var a = 1;
  var b = 2;
  if a < b {
    return 11;
  } else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(11 == result)


source = [[
function main() {
  var a = 1;
  var b = 2;
  if a <= b {
    return 11;
  } else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(11 == result)

--############################################################################
-- The execution of this script reached the end of the file.
print("OK")
