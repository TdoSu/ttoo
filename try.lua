-- DONE: 用 lua 重写 ok
-- DONE: 每个词的起止位置, 改写字符的结构, 把每个字符的位置冗余到字符身上 ok
-- DONE: 添加每个词是什么的信息 ok
-- DONE: 将分词数组构建成一棵树 s-expression tree ok
-- DONE: 遍历树, 对里面的 identifier 进行细分类 ok
-- DONE: 将树改为带有详细分类的数组 ok
-- DONE: 引入 nvim 添加事件监听, 和代码根据分类上色 ok
-- DONE: 性能提升, 0.01 秒以内完成高亮, 监听光标移动高亮而不卡顿 ok

-- TODO: 添加性能测试用例和功能测试用例, 方便后面改写.
-- TODO: 提高容错性, 避免因为括号配对等情况崩溃.
-- TODO: scheme 没有语法提升所以出现在 define 前面的变量求值应该是红色.
-- TODO: 支持 let*, letrec, 命名 let, 另外 let 本身声明的变量, 声明区域是不能读取的.
-- TODO: 实现定义跳转, 变量重命名, 符号大纲.

-- S-EXPRESSION
-- 注释 COMMENT                                               -- 灰色
-- 数字 NUMBER, 字符串 STRING, 字符 CHARACTER, 布尔值 BOOLEAN -- 黄色
-- 符号 SYMBOL, 引用序列 QUOTE_LIST                           -- 绿色
-- 自定义标识符 IDENTIFIER-CUSTOM                             -- 蓝色
-- R5RS 标识符 IDENTIFIER-R5RS                                -- 紫色
-- 未定义标识符 IDENTIFIER-UNDEFINED                          -- 红色
-- 其他的                                                     -- 白色

-- 先用注释描述需求, 写一些使用的例子, 设计 api,
-- 然后把描述转化为 scheme 语言,
-- 然后将 scheme 描述中的 "红色" 标识符一步步转化为 "蓝色" 和 "紫色".
-- 然后改写并测试.

------------------
-- utils
------------------

-- split("abcd", "") --> { "a", "b", "c", "d" }
local split = function (str, ch)
  -- 要在结尾补充一个分隔符, 否则最后一个元素不会被加入表中
  str = str .. ch
  local t = {}
  local sub_str = ''
  for i = 1, #str do
    if (ch == '') then
      table.insert(t, string.sub(str, i, i))
    elseif (string.sub(str, i, i) == ch) then
      table.insert(t, sub_str)
      sub_str = ''
    else
      local current_char = string.sub(str, i, i)
      sub_str = sub_str .. current_char
    end
  end
  return t
end

-- map({ 1, 2, 3, 4 }, square) --> { 1, 4, 9, 14 }
local map = function (old_table, operate)
  local new_table = {}
  for i = 1, #old_table do
    table.insert(new_table, operate(old_table[i], i))
  end
  return new_table
end

-- for_each({ 1, 2, 3, 4 }, print)
local for_each = function (t, operate)
  for i = 1, #t do
    operate(t[i])
  end
end

-- find_index({ 1, 3, 5, 7 }, function (x) return x == 5 end) --> 3
local find_index = function (t, start, pred)
  for i = start,#t do
    if (pred(t[i])) then
      return i
    end
  end
  return -1
end

-- flat({ 1, 2, { 3, 4 } }) --> { 1, 2, 3, 4 }
local flat = function (t)
  local r = {}
  for i = 1, #t do
    local sub_t = t[i]
    for j = 1, #sub_t do
      table.insert(r, sub_t[j])
    end
  end
  return r
end

local slice_count = 0

-- slice({ 1, 2, 3, 4 }, 2, 3) --> { 2, 3 }
local slice = function (t, s, e)
  local sub_t = {}
  for i = 1, #t do
    if (i >= s and i <= e) then
      table.insert(sub_t, t[i])
    end
  end
  return sub_t
end

-- concat({ 1, 2, 3 }, { 4, 5 }) --> { 1, 2, 3, 4, 5 }
local concat = function (t1, t2)
  local r = {}
  for i = 1, #t1 do
    table.insert(r, t1[i])
  end
  for i = 1, #t2 do
    table.insert(r, t2[i])
  end
  return r
end

-- join({ "1", "2", "3" }, "-") --> "1-2-3"
local join = function (t, ch)
  local str = ''
  for i = 1, #t do
    if (i == #t) then
      str = str .. t[i]
    else
      str = str .. t[i] .. ch
    end
  end
  return str
end

local some = function (t, pred)
  for i = 1, #t do
    if (pred(t[i])) then
      return true
    end
  end
  return false
end

local filter = function (t, pred)
  local r = {}
  for i = 1, #t do
    local current = t[i]
    if (pred(current)) then
      table.insert(r, current)
    end
  end
  return r
end

-- 这个是左折叠
function Reduce(t, combin, init)
  local r = init
  for i = 1, #t do
    r = combin(r, t[i])
  end
  return r
end

local top_stack = function (stack)
  --
  return stack[#stack]
end

local push_stack = function (stack, value)
  --
  return table.insert(stack, value)
end

local pop_stack = function (stack)
  local tmp = stack[#stack]
  table.remove(stack)
  return tmp
end

------------------
-- tools
------------------

local is_white_space = function (char)
  return char == '' or char == ' ' or char == '\n'
end

local text2type = function (text)
  if (text == '#t' or text == '#f') then
    return 'BOOLEAN'
  elseif (#text > 2 and string.sub(text, 1, 2) == [[#\]]) then
    return 'CHARACTER'
  elseif (string.sub(text, 1, 1) == [[']]) then
    return 'SYMBOL'
  elseif (tonumber(text) ~= nil) then
    return 'NUMBER'
  else
    return 'IDENTIFIER'
  end
end

local chars_with_position = function (str)
  local t =
    flat(
      map(
        map(
          split(str, '\n'),
          function (line, _) return line .. '\n' end
        ),
        function (line, i)
          return map(
            split(line, ''),
            function (ch, j)
              local o = {
                row = i + 1,
                column = j + 1,
                char = ch,
              }
              return o
            end
          )
        end
      )
    )
  return t
end

------------------
-- lexer and parser
------------------

local is_combination_expression = function (expression)
  --
  return expression.type == 'combination_expression'
end

local is_identifier = function (expression)
  --
  return expression.type == 'IDENTIFIER'
end

-- TODO 对 let 和 lambda 的处理还有些问题,
-- 比如 let 后面的列表读取的是 let 环境而不是外部环境,
-- 还有些情况没处理, 比如 (lambda _ 3), let*, letrec, 命名 let
local get_env = function (current, parent)
  return {
    local_tokens = Reduce(
      -- 从当前表达式的子表达式中过滤出 define let lambda 表达式
      -- 它能产生新环境
      filter(
        current.children,
        function (child)
          return (child and child.type == 'combination_expression') and
            (
              child.children[1] and
              (
                child.children[1].text == 'define' or
                child.children[1].text == 'let' or
                child.children[1].text == 'lambda'
              )
            )
        end
      ),
      -- 处理子表达式中的 define 变量, 它能改变当前环境
      function (r, c)
        local op = c.children[1].text
        local first_operand = c.children[2]
        if (op == 'define') then
          -- 定义过程
          if (first_operand.type == 'combination_expression') then
            return concat(r, { first_operand.children[1] })
          -- 定义变量
          else
            return concat(r, { first_operand })
          end
        else
          return r
        end
      end,
      -- 初始值 - 过程的参数 (define 定义过程, let, lambda)
      (function ()
        -- define 定义的过程参数
        if (current.children[1] and
          current.children[2] and
          current.children[1].text == 'define' and
          current.children[2].type == 'combination_expression'
          ) then
          -- 去掉 define 的过程名, 只获取参数名
          local paramaters = current.children[2].children
          return slice(paramaters, 2, #paramaters)
        -- let 定义的过程参数
        elseif (current.children[1] and
          current.children[1].text == 'let') then
          return map(current.children[2].children, function (item)
            return item.children[1]
          end)
        -- lambda 定义的过程参数
        elseif (current.children[1] and
          current.children[1].text == 'lambda') then
          return current.children[2].children
        else
          return {}
        end
      end)()
    ),
    parent = parent,
  }
end

local generate_expression_tree = function (tokens)
  local stack = {
    { type = 'global', children = {}, env = nil, text = '' }
  }
  for_each(tokens, function (token)
    local text = token.text
    if (text == '(') then
      push_stack(stack, {
        type = 'combination_expression',
        children = {}
      })
    elseif (text == ')') then
      local o = pop_stack(stack)
      o.env = get_env(o, top_stack(stack))
      table.insert(top_stack(stack).children, o)
    else
      table.insert(top_stack(stack).children, token)
    end
  end)
  stack[1].env = get_env(stack[1], nil)
  return stack[1]
end

-- child { text }
-- parent { env = { local_tokens = {}, parent = {} } }
--  local_tokens = { { text } }
function Is_custom_identifier(child, parent)
  return (
    parent and
    parent.env and
    -- 当前作用域包含该标识定义
    some(
      parent.env.local_tokens,
      function(token) return token.text == child.text end)
    ) or
    -- 判断父级作用域是否包含该标识符定义
    (parent and parent.env and Is_custom_identifier(child, parent.env.parent))
end

local is_r5rs_identifier = function (token)
  -- 下面的表来自 R5RS 标准, 并且在 chez scheme 和 guile 环境测试过
  local r5rs_tokens = {
    -- 常量表达式
    'quote',
    -- 过程
    'lambda',
    -- 条件表达式
    'if',
    -- 赋值
    'set!',
    -- 派生表达式
    'cond', 'case', 'else', 'and', 'or',
    -- 绑定
    'let', 'let*', 'letrec',
    -- 顺序结构
    'begin',
    -- 迭代
    'do',
    -- 延迟求值
    'delay',
    -- 准引用
    'quasiquote',
    -- 宏
    'let-syntax', 'letrec-syntax',
    -- 定义
    'define',
    -- ps: 上面这些都是语法结构, 不能用 procedure? 去判断
    --------------------------------------------------------------
    -- 相等谓词 equal
    'eqv?', 'eq?', 'equal?',
    -- 数值运算 number
    'number?', 'complex?', 'real?', 'rational?', 'integer?',
    'exact?', 'inexact?',
    '=', '<', '>', '<=', '>=',
    'zero?', 'positive?', 'negative?', 'odd?', 'even?',
    'max', 'min',
    '+', '-', '*', '/',
    'abs', 'quotient', 'remainder', 'modulo',
    'gcd', 'lcm',
    'numerator', 'denominator',
    'floor', 'ceiling', 'truncate', 'round', 'rationalize',
    'exp', 'log', 'sin', 'cos', 'tan', 'asin', 'acos', 'atan',
    'sqrt', 'expt',
    'make-rectangular', 'make-polar', 'real-part', 'imag-part', 'magnitude', 'angle',
    'exact->inexact', 'inexact->exact',
    'number->string', 'string->number',
    -- 布尔值 boolean
    'not', 'boolean?',
    -- pair 和 list
    'pair?', 'cons', 'car', 'cdr', 'set-car!', 'set-cdr!',
    'caar', 'cadr', 'cdar', 'cddr', 'caddr', 'caadr', 'cadar', 'caaar',
    'cdadr', 'cdaar', 'cddar', 'cdddr', 'cadddr', 'caaddr', 'cadadr',
    'caddar', 'caaadr', 'cadaar', 'caadar', 'caaaar', 'cdaddr', 'cdaadr',
    'cdaaar', 'cddadr', 'cddaar', 'cdddar', 'cddddr',
    'null?', 'list?',
    'list', 'length', 'append', 'reverse', 'list-tail', 'list-ref',
    'memq', 'memv', 'member',
    'assq', 'assv', 'assoc',
    -- 符号 symbol
    'symbol?', 'symbol->string', 'string->symbol',
    -- 字符 character
    'char?', 'char=?', 'char<?', 'char>?', 'char<=?', 'char>=?',
    'char-ci=?', 'char-ci<?', 'char-ci>?', 'char-ci<=?', 'char-ci>=?',
    'char-alphabetic?', 'char-numeric?', 'char-whitespace?',
    'char-upper-case?', 'char-lower-case?',
    'char->integer', 'integer->char', 'char-upcase', 'char-downcase',
    -- 字符串 string
    'make-string', 'string', 'string-length', 'string-ref', 'string-set!',
    'string=?', 'string-ci=?', 'string<?', 'string>?', 'string<=?', 'string>=?',
    'string-ci<?', 'string-ci>?', 'string-ci<=?', 'string-ci>=?',
    'substring', 'string-append', 'string->list', 'list->string',
    'string-copy', 'string-fill!',
    -- 向量 vector
    'vector?', 'make-vector', 'vector', 'vector-length',
    'vector-ref', 'vector-set!', 'vector->list', 'list->vector', 'vector-fill!',
    -- 控制 control
    'procedure?', 'apply', 'map', 'for-each', 'force',
    'call-with-current-continuation', 'values', 'call-with-values', 'dynamic-wind',
    -- 求值 evaluation
    'eval',
    -- 'scheme-report-enviroment', 'null-enviroment', 'interaction-enviroment',
    -- ps: 上面这三个过程 chez 和 guile 都没有实现
    -- 输入输出 input and output
    'call-with-input-file', 'call-with-output-file',
    'input-port?', 'output-port?', 'current-input-port',
    'current-output-port', 'with-input-from-file',
    'with-output-to-file', 'open-input-file', 'open-output-file',
    'close-input-port', 'close-output-port', 'read', 'read-char', 'peek-char',
    'eof-object?', 'char-ready?', 'write', 'display', 'newline', 'write-char',
    -- 系统接口
    'load',
    -- 'transcript-on', 'transcript-off',
    -- ps: 这两个过程 guile 没有实现
  }
  return some(r5rs_tokens, function (t) return t == token.text end)
end

local add_identifier_type = function (tokens)
  function Identifier_iter(expression_tree)
    if (expression_tree.type == 'global'
      or is_combination_expression(expression_tree)) then
      -- 遍历每个子表达式进行处理
      for_each(expression_tree.children, function (child_expression)
        -- 对标识符添加分类标识
        if (is_identifier(child_expression)) then
          if (Is_custom_identifier(child_expression, expression_tree)) then
            child_expression.type = 'IDENTIFIER_CUSTOM'
          elseif (is_r5rs_identifier(child_expression)) then
            child_expression.type = 'IDENTIFIER_R5RS'
          else
            child_expression.type = 'IDENTIFIER_UNDEFINED'
          end
        -- 对子组合表达式进行递归处理
        elseif (is_combination_expression(child_expression)) then
          Identifier_iter(child_expression)
        end
      end)
    end
  end
  Identifier_iter(generate_expression_tree(tokens))
end

-- 这样写当词太多时性能会有问题, slice 会遍历后续内容, 6000 词 5-6 秒.
-- 传一个开始位置, 优化到 2 秒.
-- 改写 find_index 相关方法, 使用统一的 code, 通过传递位置避免 slice 遍历,
-- 改写 组合的方式, 通过每次修改 result, 避免反复使用 concat 组合结果,
-- 将处理 400 行, 6000+ tokens 的性能提升到 0.01 秒以内.
function Lexer(code, start, result)
  local t = os.clock()
  if (code == '' or code == nil) then
    return {}
  else
    local index = start
    local current_char = code[index].char
    -- white space
    if (is_white_space(current_char)) then
      local new_index = find_index(
        code,
        index,
        function (item)
          return not is_white_space(item.char)
        end
      )
      if (new_index == -1) then
        return {}
      end
      Lexer(code, new_index, result)
    -- string
    elseif (current_char == '"') then
      local end_index = find_index(
        code,
        -- 跳过当前的 " 字符
        index + 1,
        function (item) return item.char == '"' end
      )
      -- 合并字符串
      local text = ''
      for i = index, end_index do
        local c = code[i].char
        text = text .. c
      end
      -- local text = join(
      --   map(
      --     slice(
      --       code,
      --       index,
      --       end_index
      --     ),
      --     function (item, _) return item.char end
      --   ),
      --   ''
      -- )
      table.insert(result, 
          {
            type = 'STRING',
            text = text,
            start_position = {
              row = code[index].row,
              column = code[index].column,
            },
            end_position = {
              row = code[end_index].row,
              column = code[end_index].column,
            },
          }
      )
      -- 跳过结尾 "
      Lexer(code, end_index + 1, result)
    -- comment
    elseif (current_char == ';') then
      local end_index = find_index(
        code,
        index,
        function (item) return item.char == '\n' end
      )
      -- 兼容注释出现在最后一行的情况
      if (end_index >= #code) then
        end_index = #code - 1
      end
      -- 合并注释
      -- local text = join(
      --   map(
      --     slice(
      --       code,
      --       index,
      --       -- 去掉换行符
      --       end_index - 1
      --     ),
      --     function (item, _) return item.char end
      --   ),
      --   ''
      -- )
      local text = ''
      for i = index, (end_index - 1) do
        local c = code[i].char
        text = text .. c
      end
      table.insert(result, 
          {
            type = 'COMMENT',
            text = text,
            start_position = {
              row = code[index].row,
              column = code[index].column,
            },
            end_position = {
              -- 不计算换行符
              row = code[end_index - 1].row,
              column = code[end_index - 1].column,
            },
          }
      )
      -- 跳过换行符
      Lexer(code, end_index + 1, result)
    -- list '(1 2 (3 4))
    elseif (((current_char == '\'') or (current_char == '`'))
      and code[index + 1].char == '(') then
      function Find_end_index(str, count, sub_index, start_index)
        if (count == 0) then
          return sub_index
        elseif (str[start_index].char == '(') then
          return Find_end_index(code, count + 1, sub_index + 1, start_index + 1)
        elseif (str[start_index].char == ')') then
          return Find_end_index(code, count - 1, sub_index + 1, start_index + 1)
        else
          return Find_end_index(code, count, sub_index + 1, start_index + 1)
        end
      end
      -- 跳过开头的 '( 两个字符
      local end_index = index + Find_end_index(code, 1, 1, index + 2)
      -- 合并 TODO
      local expression = join(
        map(
          -- TODO 这个加 1 要测一下
          slice(code, index, end_index),
          function (o, _) return o.char end
        ),
        ''
      )
      table.insert(result,
          {
            type = 'QUOTE_LIST',
            text = expression,
            start_position = {
              row = code[index].row,
              column =  code[index].column,
            },
            end_position = {
              row = code[end_index].row,
              column =  code[end_index].column,
            },
          }
      )
      -- 跳过结尾括号
      Lexer(code, end_index + 1, result)
    -- S-expression
    elseif (current_char == '(' or current_char == ')') then
      table.insert(result,
          {
            type = 'S-EXPRESSION',
            text = current_char,
            start_position = {
              row = code[index].row,
              column = code[index].column,
            },
            end_position = {
              row = code[index].row,
              column = code[index].column,
            },
          }
      )
      Lexer(code, index + 1, result)
    -- number character boolean symbol identifier
    else
      local end_index = find_index(
        code,
        index,
        function (item)
          return is_white_space(item.char) or item.char == ')'
        end
      )
      local text = ''
      for i = index, (end_index -1) do
        local c = code[i].char
        text = text .. c
      end
      -- local text = join(
      --   map(
      --     slice(
      --       code,
      --       index,
      --       -- 排除结尾空白或者 )
      --       end_index - 1
      --     ),
      --     function (item, _) return item.char end
      --   ),
      --   ''
      -- )
      table.insert(result,
          {
            type = text2type(text),
            text = text,
            start_position = {
              row = code[index].row,
              column = code[index].column,
            },
            end_position = {
              row = code[end_index - 1].row,
              column = code[end_index - 1].column,
            },
          }
      )
      Lexer(code, end_index, result)
    end
  end
end

local parser = function (code)
  -- 这里使用修改引用的方式提升性能
  local tokens = {}
  Lexer(code, 1, tokens)
  add_identifier_type(tokens)
  return tokens
end

local get_current_buffer_number = function ()
  -- 0 表示当前 bufer
  return 0
end

-- 非 vim 环境测试时, 可以在这里返回测试文本
local get_current_buffer_context = function ()
  --
  return join(vim.api.nvim_buf_get_text(0, 0, 0, -1, -1, {}), '\n')
end

local type2color = function (type)
  local color = {
    COMMENT               = 'GREY',
    NUMBER                = 'ORANGE',
    STRING                = 'ORANGE',
    CHARACTER             = 'ORANGE',
    BOOLEAN               = 'ORANGE',
    SYMBOL                = 'GREEN',
    QUOTE_LIST            = 'GREEN',
    IDENTIFIER            = 'WHITE',
    IDENTIFIER_CUSTOM     = 'BLUE',
    IDENTIFIER_R5RS       = 'PURPLE',
    IDENTIFIER_UNDEFINED  = 'RED',
  }
  return color[type]
end

-- 这里主要是为了挂载统一的高亮命名空间, 方便每次刷新时重置.
-- 非 vim 环境运行时, 要注释掉这里.
local M = {}
M.ns = vim.api.nvim_create_namespace('')

function _G.set_scheme_token_color()
  vim.cmd [[
    " alacritty 中的 neovim 使用的是 gui
    highlight RED     ctermfg=204 ctermbg=NONE  guifg=#ff5370 guibg=NONE 
    highlight ORANGE  ctermfg=173 ctermbg=NONE  guifg=#F78C6C guibg=NONE 
    highlight YELLOW  ctermfg=180 ctermbg=NONE  guifg=#ffcb6b guibg=NONE 
    highlight GREEN   ctermfg=114 ctermbg=NONE  guifg=#C3E88D guibg=NONE 
    highlight CYAN    ctermfg=38  ctermbg=NONE  guifg=#89DDFF guibg=NONE 
    highlight BLUE    ctermfg=39  ctermbg=NONE  guifg=#82b1ff guibg=NONE 
    highlight PURPLE  ctermfg=170 ctermbg=NONE  guifg=#c792ea guibg=NONE 
    highlight GREY    ctermfg=59  ctermbg=NONE  guifg=#697098 guibg=NONE 
    highlight WHITE   ctermfg=145 ctermbg=NONE  guifg=#bfc7d5 guibg=NONE 
    highlight BLUE_PURPLE  ctermfg=39 ctermbg=NONE  guifg=#939ede guibg=NONE
  ]]
  --
  local current_buffer_number = get_current_buffer_number()
  local text = get_current_buffer_context()
  local tokens = parser(chars_with_position(text))
  -- 先清除, 避免颜色残留
  vim.api.nvim_buf_clear_namespace(0, M.ns, 0, -1)
  for_each(tokens, function (token)
    local type = token.type
    local start_position = {
      token.start_position.row - 2,
      token.start_position.column - 2
    }
    local end_position = {
      token.end_position.row - 2,
      token.end_position.column - 1
    }
    --
    local highlight_group = type2color(type)
    if (highlight_group) then
      vim.highlight.range(
        current_buffer_number,
        M.ns,
        highlight_group,
        start_position,
        end_position
      )
    end
  end)
end

-- 手动调用 :lua _G.set_scheme_token_color()
local add_events = function ()
  vim.cmd [[
    " 打开文件时
    autocmd BufRead *.scm :silent! lua _G.set_scheme_token_color()
    " 光标移动时
    autocmd CursorMoved *.scm :silent! lua _G.set_scheme_token_color()
    autocmd CursorMovedI *.scm :silent! lua _G.set_scheme_token_color()
    " :w 写入 buffer 时
    " autocmd BufModifiedSet *.scm :silent! lua _G.set_scheme_token_color()
    " esc 离开输入模式时
    " autocmd InsertLeave *.scm :silent! lua _G.set_scheme_token_color()
  ]]
end

-- 非 vim 环境, 注释掉这里
-- 添加事件触发 set_scheme_token_colors()
add_events()

-- local M = {}
-- M.tt = os.clock()
-- function Ooo(s)
--   print(s, os.clock() - M.tt)
--   M.tt = os.clock()
-- end

local test = function ()
  local text = get_current_buffer_context()
  parser(chars_with_position(text))
end

-- test()
