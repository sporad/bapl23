-- Tinip, Tiny Interpreter
--
-- Copyright 2023 Sporadic Interlude, ihi.
-- Original copyright by Classpert.
--
-- See LICENSE

local lpeg = require "lpeg"

-- print table module
local pt = require "pt"

--##################################################
--##################################################
--##################################################
-- Parser

-- "Identity" function
-- Use this function at a place within a pattern or a grammar
-- where you want to print a message.
local function I (msg)
  -- When lpeg.P (pattern) is given a function like below,
  -- the function is always evaluated during match-time
  -- regardless of the result of match.
  return lpeg.P( function ()
    print(msg)
    return true
  end )
end

-- Parser uses this `node` function to build AST.
-- This was introduced in lesson 5.
--
-- node("number", "val")
-- {tag="number", val=val}
local function node (tag, ...)
  -- table.pack(...) returns a new table with all arguments
  -- stored into keys 1, 2, etc. and
  -- with a field "n" with the total number of arguments.
  local labels = table.pack(...)

  local params = table.concat(labels, ", ")
  local fields = string.gsub(params, "(%w+)", "%1 = %1")
  local code = string.format(
    "return function (%s) return {tag = '%s', %s} end",
    params, tag, fields)
  return assert(load(code))()
end

--[==[
-- The above `node` function replaces the following functions
-- in a sophisticated manner.
-- The `node` function was introduced in lesson 5.
-- The functions below were used in lessons 2, 3, and 4.
-- The `node` function dynamically builds functions like below.
-- While the `node` function can replace these functions,
-- it hides what is actually being built.
-- The functions below may look clumsy, but
-- they are far easier to understand.

-- number
local function nodeNum (num)
  return {tag = "number", val = tonumber(num)}
end

-- variable
local function nodeVar (var)
  return {tag = "variable", var = var}
end

-- assignment
local function nodeAssgn (id, exp)
  return {tag = "assgn", id = id, exp = exp}
end

-- return
local function nodeRet (exp)
  return {tag = "ret", exp = exp}
end

--]==]

-- Sequence ... "seq" tag in AST
-- This is not replaced by the `node` function.
local function nodeSeq (stmt1, stmt2)
  if stmt2 == nil then
    return stmt1
  else
    return {tag = "seq", stmt1 = stmt1, stmt2 = stmt2}
  end
end

local alpha = lpeg.R("AZ", "az")
local digit = lpeg.R("09")
local alphanum = alpha + digit

local comment = "#" * (lpeg.P(1) - "\n")^0

local maxmatch = 0

-- lpeg.V defines non-terminal symbol, which in this cade is "space".
-- Non-terminals must be reduced to terminals in the grammar definition.
local space = lpeg.V"space"

-- {tag = "number", val = tonumber(num)}
--local numeral = lpeg.R("09")^1 / tonumber /
--                     node("number", "val") * space
--
-- Accept unary plus or minus.
-- Support integers and floats.
local numeral = 
  lpeg.S("+-")^-1 * lpeg.R("09")^1 * (lpeg.P(".") * lpeg.R("09")^1)^-1
  / tonumber / node("number", "val") * space
 
--=====================================
-- Reserved words
local reserved = {
  lpeg.P "function",
  lpeg.P "return",
  lpeg.P "while",
  lpeg.P "else",
  lpeg.P "new",
  lpeg.P "var",
  lpeg.P "if"
  }
-- lpeg.P(false) below initializes the `excluded` variable as
-- an empty lpeg object. It eliminates the need to write lpeg.P in
-- the above `reserved` array elements.
-- However, lpeg.P is used above to make it obvious that
-- they are lpeg patterns.
local excluded = lpeg.P(false)
for i = 1, #reserved do
  excluded = excluded + reserved[i]
end
-- Adding "* -alphanum" makes sure that words like "if1", "returnK", etc.
-- are not treated as reserved words.
excluded = excluded * -alphanum

-- ID = Identifier
local ID = lpeg.V"ID"
-- var = variable ... for AST
local var = ID / node("variable", "var")


-- Terminal symbol
-- Examples:
--   T"(" .. open paren
--   T")" .. close paren
local function T (sym)
  return sym * space
end


-- Reserved word
local function Rw (t)
  assert(excluded:match(t))
  return t * space
end


-- Additional operators
-- + for addition
-- - for subtraction
local opA = lpeg.C(lpeg.S"+-") * space
  + lpeg.C(lpeg.P"==") * space
  + lpeg.C(lpeg.P"~=") * space
  + lpeg.C(lpeg.P"<=") * space
  + lpeg.C(lpeg.P">=") * space
  + lpeg.C(lpeg.P"<") * space
  + lpeg.C(lpeg.P">") * space
  + lpeg.C(lpeg.P"&&") * space  -- short-cut AND
  + lpeg.C(lpeg.P"||") * space  -- short-cut OR

-- Multiplicative operators
-- * for multiplication
-- / for division
local opM = lpeg.C(lpeg.S"*/") * space

-- For binary operators
-- Convert a list {n1, "+", n2, "+", n3, ...} into a tree
-- {...{ op = "+", e1 = {op = "+", e1 = n1, n2 = n2}, e2 = n3}...}
local function foldBin (lst)
  local tree = lst[1]
  for i = 2, #lst, 2 do
    tree = { tag = "binop", e1 = tree, op = lst[i], e2 = lst[i + 1] }
  end
  return tree
end


-- Array
local function foldIndex (lst)
  local tree = lst[1]
  for i = 2, #lst do
    tree = { tag = "indexed", array = tree, index = lst[i] }
  end
  return tree
end


----------------------------------------------------------------
-- lpeg.V for non-terminals.
-- A grammar definition must define grammar rules for lpeg.V.
local lhs     = lpeg.V "lhs"      -- Substitution
local call    = lpeg.V "call"     -- Function call
local factor  = lpeg.V "factor"
local term    = lpeg.V "term"
local exp     = lpeg.V "exp"      -- Expression
local stat    = lpeg.V "stat"     -- Statement
local stats   = lpeg.V "stats"    -- Statements
local block   = lpeg.V "block"    -- Code block ... defines variable scope
local funcDec = lpeg.V "funcDec"  -- Function declaration
local args    = lpeg.V "args"     -- Arguments .. things passed in function calls
local params  = lpeg.V "params"   -- Parameters .. things declared in function declaration

-- Grammar defintion
-- lpeg.P (for "Pattern") can take a table.
-- Table must contain grammar for non-terminals.
grammar = lpeg.P {

  "prog", -- The `prog` non-terminal symbol is the entry point of the parser.

  -- Ct for capture in table.
  -- This line defines that function declaration is the basic unit of program.
  prog = space * lpeg.Ct(funcDec^1) * -1,

  -- function declaration
  funcDec = Rw"function" * ID * T"(" * params * T")" * block
              / node("function", "name", "params", "body"),

  -- function parameters, sepcified in function declaration
  params = lpeg.Ct((ID * (T"," * ID)^0)^-1),

  -- statements
  stats = stat * (T";" * stats)^-1 / nodeSeq,

  -- code block
  block = T"{" * stats * T";"^-1 * T"}" / node("block", "body"),

  -- statement
  stat = block
       + Rw"var" * ID * T"=" * exp
          / node("local", "name", "init")
       + Rw"if" * exp * block * (Rw"else" * block)^-1
          / node("if1", "cond", "th", "el")
       + Rw"while" * exp * block
          / node("while1", "cond", "body")
       + call
       + lhs * T"=" * exp
          / node("assgn", "lhs", "exp")
       + Rw"return" * exp
          / node("ret", "exp"),

  lhs = lpeg.Ct(var * (T"[" * exp * T"]")^0)
          / foldIndex,

  -- function call
  call = ID * T"(" * args * T")"
           / node("call", "fname", "args"),

  -- arguments ... things passed to function in function call
  args = lpeg.Ct((exp * (T"," * exp)^0)^-1),

  factor = Rw"new" * T"[" * exp * T"]"
             / node("new", "size")
         + numeral
         + T"(" * exp * T")"
         + call
         + lhs,

  term = lpeg.Ct(factor * (opM * factor)^0) / foldBin,

  -- expression
  exp = lpeg.Ct(term * (opA * term)^0) / foldBin,

  -- space
  space = (lpeg.S(" \t\n") + comment)^0
              -- Passing a function to lpeg.P defines a function which is
              -- always evaluated regardless of the result of match attempt.
            * lpeg.P(function (_, p) -- 1st arg: whole subject, 2nd arg: current position
                       -- Update the `maxmatch` cursor which indicates
                       -- the current parser position.
                       maxmatch = math.max(maxmatch, p);
                       return true
                     end),

  -- identifier
  ID = (lpeg.C(alpha * alphanum^0) - excluded) * space
}


local function current_line_num (subject, pos)
  if pos == 1 then return 1, 1 end
  local current_line_str, num_newlines = subject:sub(1, pos):gsub("[^\n]*\n", "")
  local current_line = num_newlines + 1
  local current_column = #current_line_str
  return current_line, current_column
end


-- Reporting syntax error
local function syntaxError (input, max_pos)
  local L, col = current_line_num(input, max_pos)

  io.stderr:write("syntax error :: line " .. L .. " :: column " .. col .. "\n")
  io.stderr:write(string.sub(input, max_pos - 10, max_pos - 1),
        "|", string.sub(input, max_pos, max_pos + 11), "\n")
end


local function parse (input)
  local res = grammar:match(input)
  if (not res) then
    syntaxError(input, maxmatch)
    os.exit(1)
  end
  return res
end

--##################################################
--##################################################
--##################################################
-- Compiler
-- Takes AST and converts to instructions.

local Compiler = { funcs = {}, vars = {}, nvars = 0, locals = {} }

function Compiler:addCode (op)
  local code = self.code
  code[#code + 1] = op
end


local ops = {["+"] = "add", ["-"] = "sub",
             ["*"] = "mul", ["/"] = "div", 
             -- [isaac] Added:
             ["&&"] = "AND",  -- short-cut and
             ["||"] = "OR",   -- short-cut or
             ["=="] = "eq",
             ["~="] = "ne",
             ["<="] = "le",
             [">="] = "ge",
             ["<"] = "lt",
             [">"] = "gt" }


-- In instructions which compiler generates, variables in program source
-- appear just as numbers.
-- This function assigns numbers starting from 1 to variables as they appear
-- in the program source.
-- For example, if the source has "var a; var b;" then
-- a is 1 and b is 2 in the corresponding instructions.
function Compiler:var2num (id)
  local num = self.vars[id]
  if not num then
    num = self.nvars + 1
    self.nvars = num
    self.vars[id] = num
  end
  return num
end


function Compiler:currentPosition ()
  return #self.code
end


function Compiler:codeJmpB (op, label)
  self:addCode(op)
  self:addCode(label)
end


function Compiler:codeJmpF (op)
  self:addCode(op)
  self:addCode(0)
  return self:currentPosition()
end


function Compiler:fixJmp2here (jmp)
  self.code[jmp] = self:currentPosition()
end


function Compiler:findLocal (name)
  -- First, search local variables.
  local vars = self.locals
  for i = #vars, 1, -1 do
    if name == vars[i] then
      return i
    end
  end
  -- Second, search function parameters.
  local params = self.params
  for i = 1, #params do
    if name == params[i] then
      -- Functino parameters are indexed in the negative domain
      -- against the base index.
      return -(#params - i)
    end
  end
  return false   -- not found
end


-- Function call
function Compiler:codeCall (ast)
  local func = self.funcs[ast.fname]
  if not func then
    error("undefined function " .. ast.fname)
  end
  local args = ast.args
  if #args ~= #func.params then
    error("wrong number of arguments calling " .. ast.fname)
  end
  -- Put arguments in the stack.
  for i = 1, #args do
    self:codeExp(args[i])
  end
  self:addCode("call")
  self:addCode(func.code)
end


-- Build insttructions for expressions
function Compiler:codeExp (ast)
  if ast.tag == "number" then
    self:addCode("push")
    self:addCode(ast.val)
  elseif ast.tag == "call" then
    self:codeCall(ast)
  elseif ast.tag == "variable" then
    -- Look for a local variable
    local idx = self:findLocal(ast.var)
    if idx then
      self:addCode("loadL")
      self:addCode(idx)
    else
      -- It's a global var
      self:addCode("load")
      self:addCode(self:var2num(ast.var))
    end
  elseif ast.tag == "indexed" then
    self:codeExp(ast.array)
    self:codeExp(ast.index)
    self:addCode("getarray")
  elseif ast.tag == "new" then
    self:codeExp(ast.size)
    self:addCode("newarray")
  elseif ast.tag == "binop" then
    self:codeExp(ast.e1)
    self:codeExp(ast.e2)
    self:addCode(ops[ast.op])
  else error("invalid tree")
  end
end


-- Build instructions for assignment
function Compiler:codeAssgn (ast)
  local lhs = ast.lhs

  if lhs.tag == "variable" then
    -- Scalar variable
    self:codeExp(ast.exp)
    local idx = self:findLocal(lhs.var)
    if idx then
      -- Store data in the local memory area which will be a stack.
      self:addCode("storeL")
      self:addCode(idx)
    else
      -- Store data in the global memory area.
      -- [TODO] Memory area can be separated into variables and constants.
      self:addCode("store")
      self:addCode(self:var2num(lhs.var))
    end

  elseif lhs.tag == "indexed" then
    -- Array variable
    self:codeExp(lhs.array)
    self:codeExp(lhs.index)
    self:codeExp(ast.exp)
    self:addCode("setarray")
  else error("unkown tag")
  end
end


function Compiler:codeBlock (ast)
  local oldlevel = #self.locals
  self:codeStat(ast.body)
  local n = #self.locals - oldlevel   -- number of new local variables
  if n > 0 then
    for i = 1, n do table.remove(self.locals) end
    self:addCode("pop")
    self:addCode(n)
  end
end


-- Build instructions for statement
function Compiler:codeStat (ast)

  if ast.tag == "assgn" then
    self:codeAssgn(ast)

  elseif ast.tag == "local" then
    self:codeExp(ast.init)
    self.locals[#self.locals + 1] = ast.name

  elseif ast.tag == "call" then
    self:codeCall(ast)
    -- Remove the result of function call from stack top
    -- because this is statement.
    self:addCode("pop")
    self:addCode(1)

  elseif ast.tag == "block" then
    self:codeBlock(ast)

  elseif ast.tag == "seq" then
    self:codeStat(ast.stmt1)
    self:codeStat(ast.stmt2)

  elseif ast.tag == "ret" then
    self:codeExp(ast.exp)
    self:addCode("ret")
    self:addCode(#self.locals + #self.params)

  elseif ast.tag == "while1" then
    local ilabel = self:currentPosition()
    self:codeExp(ast.cond)
    local jmp = self:codeJmpF("jmpZ")
    self:codeStat(ast.body)
    self:codeJmpB("jmp", ilabel)
    self:fixJmp2here(jmp)

  elseif ast.tag == "if1" then
    -- Condition expression.
    self:codeExp(ast.cond)

    -- Now add a conditional jump instruction, jmpZ (jump zero).
    -- If the above condition was true,
    -- the instruction right after jmpZ is skipped.
    -- If false, it is executed, and it contains the position to jump to skip the "then" block.
    self:addCode("jmpZ")  -- jump zero is conditional jump.

    -- Add an instruction: code[current_position] = 0
    self:addCode(0)
    -- The value zero here is temporary and must be updated later with
    -- the program position to jump to if the condition was false.
    -- Right now, the position to jump to is unknown because
    -- we still don't know the instructions for the "then" block yet.

    -- Remember the current position.
    -- We will update the instruction `code[pos1]` after
    -- all the instructions for the "then" block are added.
    local pos1 = self:currentPosition()

    -- Instructions for the "then" block.
    self:codeStat(ast.th)

    if ast.el == nil then
      -- There is no "else" block.
      -- The position to jump from pos1 is here.
      self.code[pos1] = self:currentPosition()

    else
      -- There is "else" block.

      -- Unconditional jump to skip the "else" block.
      -- Execution comes here only when the "then" block was executed.
      self:addCode("jmp")

      -- Similar to the conditional jump (jmpZ) before,
      -- 0 is set to code[current_position] for now.
      self:addCode(0)
      local pos2 = self:currentPosition()
    
      -- Here is the position to jump from pos1
      -- if the "then" block was skipped.
      -- This position must come after the unconditional jump.
      self.code[pos1] = self:currentPosition()

      -- Instructions for the "else" block.
      self:codeStat(ast.el)

      -- Here is the position we want to jump from pos2
      -- to skip the "else" block.
      self.code[pos2] = self:currentPosition()
    end

  else error("invalid tree")
  end
end


-- Build instructions for a function call
function Compiler:codeFunction (ast)
  local code = {}
  self.funcs[ast.name] = {code = code, params = ast.params}
  self.code = code
  self.params = ast.params

  self:codeStat(ast.body)

  -- End the instructions for function call
  -- with "push", 0, "ret".
  -- This is used as an idiom to mark the end of function call.
  self:addCode("push")
  self:addCode(0)
  self:addCode("ret")

  -- Store the number of local variables and function parameters
  -- at the end of the instructions for a function.
  -- This is used to properly adjust the VM's stack top
  -- when returning from a function. Very important.
  self:addCode(#self.locals + #self.params)
end


function compile (ast, func_name)
  for i = 1, #ast do
    Compiler:codeFunction(ast[i])
  end
  if func_name == nil then
    func_name = "main"
  end
  local f = Compiler.funcs[func_name]
  if f == nil then
    error("no function '" .. func_name .. "'")
  end
  return f.code
end


--##################################################
--##################################################
--##################################################
-- Runner / Virtual machine
-- Takes instructions and executes them.

function run (code, mem, stack, top, print_info)
  local pc = 1
  local base = top
  while true do
    if print_info then
      -- Display the stack.
      io.write("--> ")
      for i = 1, top do
         io.write(stack[i], " ")
      end
      io.write("\n", code[pc], "\n")
    end
    if code[pc] == "ret" then
      -- Return from a function.
      --
      -- Funciton's parameters and local variables are
      -- on the top of the stack.
      -- We must remove those local variable
      -- when returning from a function.
      --
      -- In the instructions array (`code`),
      -- the "ret" directive is followed by an integer
      -- indicating the number of local variables.
      local num_local_vars = code[pc + 1]    -- number of active local variables
      stack[top - num_local_vars] = stack[top]
      top = top - num_local_vars
      return top

    elseif code[pc] == "call" then
      -- Call a function
      pc = pc + 1
      local code = code[pc]
      -- Recursive call of run()
      top = run(code, mem, stack, top)

    elseif code[pc] == "pop" then
      pc = pc + 1
      top = top - code[pc]
    elseif code[pc] == "push" then
      pc = pc + 1
      top = top + 1
      stack[top] = code[pc]
    elseif code[pc] == "add" then
      stack[top - 1] = stack[top - 1] + stack[top]
      top = top - 1
    elseif code[pc] == "sub" then
      stack[top - 1] = stack[top - 1] - stack[top]
      top = top - 1
    elseif code[pc] == "mul" then
      stack[top - 1] = stack[top - 1] * stack[top]
      top = top - 1
    elseif code[pc] == "div" then
      stack[top - 1] = stack[top - 1] / stack[top]
      top = top - 1
    elseif code[pc] == "loadL" then
      -- local variable
      pc = pc + 1
      local num_local_vars = code[pc]
      top = top + 1
      stack[top] = stack[base + num_local_vars]
    elseif code[pc] == "storeL" then
      pc = pc + 1
      local num_local_vars = code[pc]
      stack[base + num_local_vars] = stack[top]
      top = top - 1
    elseif code[pc] == "load" then
      -- Load data from the global memory to the stack top.
      pc = pc + 1
      local id = code[pc]
      top = top + 1
      stack[top] = mem[id]
    elseif code[pc] == "store" then
      -- Store a variable at the stack top to the global memory.
      pc = pc + 1
      local id = code[pc]
      mem[id] = stack[top]
      top = top - 1
    elseif code[pc] == "newarray" then
      local size = stack[top]
      stack[top] = { size = size }
    elseif code[pc] == "getarray" then
      local array = stack[top - 1]
      local index = stack[top]
      stack[top - 1] = array[index]
      top = top - 1
    elseif code[pc] == "setarray" then
      local array = stack[top - 2]
      local index = stack[top - 1]
      local value = stack[top]
      array[index] = value
      top = top - 3

    elseif code[pc] == "jmp" then
      -- Unconditional jump
      pc = code[pc + 1]

    elseif code[pc] == "jmpZ" then
      -- Jump zero ... Conditional jump ... jump if previous condition was zero.
      -- Video: week 5, lecture 5.
      pc = pc + 1
      if stack[top] == 0 or stack[top] == nil then
        pc = code[pc]
      end
      top = top - 1

    -- [isaac] Added
    elseif code[pc] == "eq" then
      if stack[top - 1] == stack[top] then
        stack[top - 1] = 1
      else
        stack[top - 1] = 0
      end
      top = top - 1
    -- [isaac] Added
    elseif code[pc] == "ne" then
      if stack[top - 1] ~= stack[top] then
        stack[top - 1] = 1
      else
        stack[top - 1] = 0
      end
      top = top - 1
    -- [isaac] Added
    elseif code[pc] == "le" then
      if stack[top - 1] <= stack[top] then
        stack[top - 1] = 1
      else
        stack[top - 1] = 0
      end
      top = top - 1
    -- [isaac] Added
    elseif code[pc] == "ge" then
      if stack[top - 1] >= stack[top] then
        stack[top - 1] = 1
      else
        stack[top - 1] = 0
      end
      top = top - 1
    -- [isaac] Added
    elseif code[pc] == "lt" then
      if stack[top - 1] < stack[top] then
        stack[top - 1] = 1
      else
        stack[top - 1] = 0
      end
      top = top - 1
    -- [isaac] Added
    elseif code[pc] == "gt" then
      if stack[top - 1] > stack[top] then
        stack[top - 1] = 1
      else
        stack[top - 1] = 0
      end
      top = top - 1
    -- [isaac] Added
    elseif code[pc] == "AND" then
      -- There are no boolean values. 1 for true, 0 for false.
      if (stack[top - 1] ~= 0) and (stack[top] ~= 0) then
        stack[top - 1] = 1
      else
        stack[top - 1] = 0
      end
      top = top - 1
    -- [isaac] Added
    elseif code[pc] == "OR" then
      -- There are no boolean values. 1 for true, 0 for false.
      if (stack[top - 1] ~= 0) or (stack[top] ~= 0) then
        stack[top - 1] = 1
      else
        stack[top - 1] = 0
      end
      top = top - 1
    else error("unknown instruction " .. code[pc])
    end
    pc = pc + 1
  end
end

--##################################################
--##################################################
--##################################################
-- Read program source, parse, compile, and run.

local function interpreter (input, print_info)

  local ast = parse(input)
  if print_info then
    print(pt.pt(ast))
  end

  local code = compile(ast)
  if print_info then
    print(pt.pt(code))
  end

  local stack = {}
  local mem = {}
  run(code, mem, stack, 0, print_info)

  local r = stack[1]
  if print_info then
    print(stack[1])
  end
  return r
end

return {interpreter=interpreter}