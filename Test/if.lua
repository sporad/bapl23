local tinip = require "tinip"

show_info = false
--show_info = true

--############################################################################

source = [[
function main() {
  if 0 {
    return 11;
  }
  else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(22 == result)


source = [[
function main() {
  if 1 {
    return 11;
  }
  else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(11 == result)


source = [[
function main() {
  if 2 {
    return 11;
  }
  else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(11 == result)



source = [[
function main() {
  if 0.1 {
    return 11;
  }
  else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(11 == result)


source = [[
function main() {
  var k = 1;
  if k {
    return 11;
  }
  else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(11 == result)


source = [[
function main() {
  var k = 1;
  var s = -2;
  if k && s {
    return 11;
  }
  else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(11 == result)


source = [[
function main() {
  var k = 0;
  var s = -2;
  if k && s {
    return 11;
  }
  else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(22 == result)


source = [[
function main() {
  var k = 1;
  var s = 0;
  if k && s {
    return 11;
  }
  else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(22 == result)


source = [[
function main() {
  var k = 1;
  var s = 0;
  if k || s {
    return 11;
  }
  else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(11 == result)


source = [[
function main() {
  var k = 0;
  var s = 1;
  if k || s {
    return 11;
  }
  else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(11 == result)


source = [[
function main() {
  var k = 1;
  var s = 1;
  if k || s {
    return 11;
  }
  else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(11 == result)


source = [[
function main() {
  var k = 0;
  var s = 0;
  if k || s {
    return 11;
  }
  else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(22 == result)


source = [[
function main() {
  var a = 0;
  var b = 1;
  var c = 1;
  var d = 1;
  if (a && b) || (c && d) {
    return 11;
  }
  else {
    return 22;
  }
}
]]
result = tinip.interpreter(source, show_info)
assert(11 == result)

--############################################################################
-- The execution of this script reached the end of the file.
print("OK")
