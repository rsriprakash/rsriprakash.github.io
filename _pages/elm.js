(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// DECODER

var _File_decoder = _Json_decodePrim(function(value) {
	// NOTE: checks if `File` exists in case this is run on node
	return (typeof File !== 'undefined' && value instanceof File)
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FILE', value);
});


// METADATA

function _File_name(file) { return file.name; }
function _File_mime(file) { return file.type; }
function _File_size(file) { return file.size; }

function _File_lastModified(file)
{
	return $elm$time$Time$millisToPosix(file.lastModified);
}


// DOWNLOAD

var _File_downloadNode;

function _File_getDownloadNode()
{
	return _File_downloadNode || (_File_downloadNode = document.createElement('a'));
}

var _File_download = F3(function(name, mime, content)
{
	return _Scheduler_binding(function(callback)
	{
		var blob = new Blob([content], {type: mime});

		// for IE10+
		if (navigator.msSaveOrOpenBlob)
		{
			navigator.msSaveOrOpenBlob(blob, name);
			return;
		}

		// for HTML5
		var node = _File_getDownloadNode();
		var objectUrl = URL.createObjectURL(blob);
		node.href = objectUrl;
		node.download = name;
		_File_click(node);
		URL.revokeObjectURL(objectUrl);
	});
});

function _File_downloadUrl(href)
{
	return _Scheduler_binding(function(callback)
	{
		var node = _File_getDownloadNode();
		node.href = href;
		node.download = '';
		node.origin === location.origin || (node.target = '_blank');
		_File_click(node);
	});
}


// IE COMPATIBILITY

function _File_makeBytesSafeForInternetExplorer(bytes)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/10
	// all other browsers can just run `new Blob([bytes])` directly with no problem
	//
	return new Uint8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength);
}

function _File_click(node)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/11
	// all other browsers have MouseEvent and do not need this conditional stuff
	//
	if (typeof MouseEvent === 'function')
	{
		node.dispatchEvent(new MouseEvent('click'));
	}
	else
	{
		var event = document.createEvent('MouseEvents');
		event.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
		document.body.appendChild(node);
		node.dispatchEvent(event);
		document.body.removeChild(node);
	}
}


// UPLOAD

var _File_node;

function _File_uploadOne(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			callback(_Scheduler_succeed(event.target.files[0]));
		});
		_File_click(_File_node);
	});
}

function _File_uploadOneOrMore(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.multiple = true;
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			var elmFiles = _List_fromArray(event.target.files);
			callback(_Scheduler_succeed(_Utils_Tuple2(elmFiles.a, elmFiles.b)));
		});
		_File_click(_File_node);
	});
}


// CONTENT

function _File_toString(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsText(blob);
		return function() { reader.abort(); };
	});
}

function _File_toBytes(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(new DataView(reader.result)));
		});
		reader.readAsArrayBuffer(blob);
		return function() { reader.abort(); };
	});
}

function _File_toUrl(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsDataURL(blob);
		return function() { reader.abort(); };
	});
}




var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Main$AutoDetect = {$: 'AutoDetect'};
var $author$project$Student$FailGrade = function (a) {
	return {$: 'FailGrade', a: a};
};
var $author$project$Student$PassGrade = F2(
	function (a, b) {
		return {$: 'PassGrade', a: a, b: b};
	});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Main$init = function (_v0) {
	return _Utils_Tuple2(
		{
			column_names: _List_Nil,
			content: '',
			grades: _List_fromArray(
				[
					A2($author$project$Student$PassGrade, 'S', 90.0),
					A2($author$project$Student$PassGrade, 'A', 80.0),
					A2($author$project$Student$PassGrade, 'B', 70.0),
					A2($author$project$Student$PassGrade, 'C', 60.0),
					A2($author$project$Student$PassGrade, 'D', 50.0),
					A2($author$project$Student$PassGrade, 'E', 30.0),
					$author$project$Student$FailGrade('U')
				]),
			histogramBins: 100,
			separator: $author$project$Main$AutoDetect,
			sortEntries: true,
			students: _List_Nil
		},
		$elm$core$Platform$Cmd$none);
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Main$subscriptions = function (model) {
	return $elm$core$Platform$Sub$none;
};
var $author$project$Main$Comma = {$: 'Comma'};
var $author$project$Main$Tab = {$: 'Tab'};
var $author$project$Main$UploadLoaded = function (a) {
	return {$: 'UploadLoaded', a: a};
};
var $author$project$Main$UploadSelected = function (a) {
	return {$: 'UploadSelected', a: a};
};
var $author$project$GradeSuggestions$mean = function (markList) {
	return function (n) {
		return n / $elm$core$List$length(markList);
	}(
		A3($elm$core$List$foldl, $elm$core$Basics$add, 0.0, markList));
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$round = _Basics_round;
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $elm$core$Basics$pow = _Basics_pow;
var $author$project$GradeSuggestions$variance = function (markList) {
	var mu = $author$project$GradeSuggestions$mean(markList);
	return function (n) {
		return n - (mu * mu);
	}(
		function (n) {
			return n / $elm$core$List$length(markList);
		}(
			A3(
				$elm$core$List$foldl,
				$elm$core$Basics$add,
				0.0,
				A2(
					$elm$core$List$map,
					function (n) {
						return A2($elm$core$Basics$pow, n, 2);
					},
					markList))));
};
var $author$project$GradeSuggestions$autoPartition = F2(
	function (nPartitions, markList) {
		var sigma = $elm$core$Basics$sqrt(
			$author$project$GradeSuggestions$variance(markList));
		var mu = $author$project$GradeSuggestions$mean(markList);
		return (nPartitions < 2) ? _List_Nil : ((!(nPartitions % 2)) ? A2(
			$elm$core$List$map,
			function (n) {
				return (n > 100) ? 100 : ((n < 0) ? 0 : n);
			},
			A2(
				$elm$core$List$map,
				function (n) {
					return n / 2;
				},
				A2(
					$elm$core$List$map,
					function (n) {
						return $elm$core$Basics$round(n);
					},
					A2(
						$elm$core$List$map,
						function (n) {
							return n * 2;
						},
						A2(
							$elm$core$List$map,
							function (n) {
								return n + mu;
							},
							A2(
								$elm$core$List$map,
								function (n) {
									return n * (sigma / 2);
								},
								A2(
									$elm$core$List$map,
									function (n) {
										return n - ((nPartitions - 1) / 2);
									},
									A2($elm$core$List$range, 0, nPartitions - 1)))))))) : A2(
			$elm$core$List$map,
			function (n) {
				return (n > 100) ? 100 : ((n < 0) ? 0 : n);
			},
			A2(
				$elm$core$List$map,
				function (n) {
					return n / 2;
				},
				A2(
					$elm$core$List$map,
					function (n) {
						return $elm$core$Basics$round(n);
					},
					A2(
						$elm$core$List$map,
						function (n) {
							return n * 2;
						},
						A2(
							$elm$core$List$map,
							function (n) {
								return n + mu;
							},
							A2(
								$elm$core$List$map,
								function (n) {
									return n * (sigma / 2);
								},
								A2($elm$core$List$range, -(((nPartitions - 1) / 2) | 0), ((nPartitions - 1) / 2) | 0))))))));
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$file$File$Download$string = F3(
	function (name, mime, content) {
		return A2(
			$elm$core$Task$perform,
			$elm$core$Basics$never,
			A3(_File_download, name, mime, content));
	});
var $author$project$Main$download = function (ascData) {
	return A3($elm$file$File$Download$string, 'ascdata.txt', 'text/plain', ascData);
};
var $elm$file$File$Select$file = F2(
	function (mimes, toMsg) {
		return A2(
			$elm$core$Task$perform,
			toMsg,
			_File_uploadOne(mimes));
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$Student$getStudentGrade = function (s) {
	switch (s.$) {
		case 'InvalidStudent':
			return $author$project$Student$FailGrade('INVALID');
		case 'UngradedStudent':
			return $author$project$Student$FailGrade('UNGRADED');
		default:
			var g = s.d;
			return g;
	}
};
var $author$project$Student$gradeToStr = function (grade) {
	if (grade.$ === 'PassGrade') {
		var g = grade.a;
		return g;
	} else {
		var g = grade.a;
		return g;
	}
};
var $author$project$Student$getStudentGradeStr = function (s) {
	return $author$project$Student$gradeToStr(
		$author$project$Student$getStudentGrade(s));
};
var $elm$core$String$toUpper = _String_toUpper;
var $author$project$Student$getStudentRollno = function (s) {
	switch (s.$) {
		case 'InvalidStudent':
			return 'INVALID';
		case 'UngradedStudent':
			var r = s.a;
			return $elm$core$String$toUpper(r);
		default:
			var r = s.a;
			return $elm$core$String$toUpper(r);
	}
};
var $author$project$Student$getStudentScore = function (s) {
	switch (s.$) {
		case 'InvalidStudent':
			return -2.0;
		case 'UngradedStudent':
			var score = s.c;
			return score;
		default:
			var score = s.c;
			return score;
	}
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Student$isGradedStudent = function (s) {
	switch (s.$) {
		case 'InvalidStudent':
			return false;
		case 'UngradedStudent':
			var score = s.c;
			return false;
		default:
			var score = s.c;
			return true;
	}
};
var $author$project$Student$GradedStudent = F4(
	function (a, b, c, d) {
		return {$: 'GradedStudent', a: a, b: b, c: c, d: d};
	});
var $author$project$Student$InvalidStudent = {$: 'InvalidStudent'};
var $author$project$Student$UngradedStudent = F3(
	function (a, b, c) {
		return {$: 'UngradedStudent', a: a, b: b, c: c};
	});
var $author$project$Student$getGradeBoundary = function (grade) {
	if (grade.$ === 'PassGrade') {
		var b = grade.b;
		return b;
	} else {
		return 0.0;
	}
};
var $author$project$Student$assignStudentGrade = F2(
	function (grade_boundaries, student) {
		switch (student.$) {
			case 'InvalidStudent':
				return $author$project$Student$InvalidStudent;
			case 'UngradedStudent':
				var rollno = student.a;
				var data = student.b;
				var score = student.c;
				var _v1 = $elm$core$List$head(
					A2(
						$elm$core$List$filter,
						function (n) {
							return $author$project$Student$getGradeBoundary(n) >= 0;
						},
						A2(
							$elm$core$List$map,
							function (n) {
								return A2(
									$author$project$Student$PassGrade,
									$author$project$Student$gradeToStr(n),
									score - $author$project$Student$getGradeBoundary(n));
							},
							grade_boundaries)));
				if (_v1.$ === 'Nothing') {
					return A3($author$project$Student$UngradedStudent, rollno, data, score);
				} else {
					var x = _v1.a;
					return A4($author$project$Student$GradedStudent, rollno, data, score, x);
				}
			default:
				return student;
		}
	});
var $author$project$Main$getSeparator = function (sep) {
	switch (sep.$) {
		case 'Comma':
			return ',';
		case 'Tab':
			return '\t';
		default:
			return '\t';
	}
};
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$String$toFloat = _String_toFloat;
var $author$project$Student$rowToUngradedStudent = function (stringList) {
	if (!stringList.b) {
		return $author$project$Student$InvalidStudent;
	} else {
		if (!stringList.b.b) {
			var a = stringList.a;
			return $author$project$Student$InvalidStudent;
		} else {
			if (!stringList.b.b.b) {
				var a = stringList.a;
				var _v1 = stringList.b;
				var b = _v1.a;
				var score = function () {
					var _v2 = $elm$core$String$toFloat(b);
					if (_v2.$ === 'Nothing') {
						return -2.0;
					} else {
						var x = _v2.a;
						return x;
					}
				}();
				return (score < 0) ? $author$project$Student$InvalidStudent : A3($author$project$Student$UngradedStudent, a, _List_Nil, score);
			} else {
				var a = stringList.a;
				var rest = stringList.b;
				var realRest = $elm$core$List$reverse(rest);
				var scoreStr = function () {
					var _v5 = $elm$core$List$head(realRest);
					if (_v5.$ === 'Nothing') {
						return '-3.0';
					} else {
						var x = _v5.a;
						return x;
					}
				}();
				var score = function () {
					var _v4 = $elm$core$String$toFloat(scoreStr);
					if (_v4.$ === 'Nothing') {
						return -3.0;
					} else {
						var x = _v4.a;
						return x;
					}
				}();
				var data = function () {
					var _v3 = $elm$core$List$tail(realRest);
					if (_v3.$ === 'Nothing') {
						return _List_Nil;
					} else {
						var x = _v3.a;
						return $elm$core$List$reverse(x);
					}
				}();
				return (score < 0) ? $author$project$Student$InvalidStudent : A3($author$project$Student$UngradedStudent, a, data, score);
			}
		}
	}
};
var $elm$core$String$trim = _String_trim;
var $author$project$Main$parseStudentRows = F3(
	function (separator, content, grade_boundaries) {
		var contentHead = function () {
			var _v5 = $elm$core$List$head(
				A2(
					$elm$core$String$split,
					'\n',
					$elm$core$String$trim(content)));
			if (_v5.$ === 'Just') {
				var a = _v5.a;
				return a;
			} else {
				return '';
			}
		}();
		var detectedSeparator = (A2($elm$core$String$contains, ',', contentHead) && (!A2($elm$core$String$contains, '\t', contentHead))) ? ',' : ((A2($elm$core$String$contains, '\t', contentHead) && (!A2($elm$core$String$contains, ',', contentHead))) ? '\t' : '\t');
		var separatorStr = function () {
			if (separator.$ === 'AutoDetect') {
				return detectedSeparator;
			} else {
				return $author$project$Main$getSeparator(separator);
			}
		}();
		var firstRowHeader = function () {
			var _v2 = $elm$core$List$head(
				$elm$core$List$reverse(
					A2($elm$core$String$split, separatorStr, contentHead)));
			if (_v2.$ === 'Just') {
				var isScore = _v2.a;
				var _v3 = $elm$core$String$toFloat(isScore);
				if (_v3.$ === 'Just') {
					return false;
				} else {
					return true;
				}
			} else {
				return false;
			}
		}();
		var column_names = function () {
			var first_row = A2($elm$core$String$split, separatorStr, contentHead);
			return firstRowHeader ? first_row : $elm$core$List$concat(
				_List_fromArray(
					[
						_List_fromArray(
						['Roll number']),
						A2(
						$elm$core$List$map,
						function (n) {
							return 'Column ' + $elm$core$String$fromInt(n);
						},
						A2(
							$elm$core$List$range,
							2,
							$elm$core$List$length(first_row)))
					]));
		}();
		return _Utils_Tuple2(
			A2(
				$elm$core$List$filter,
				function (n) {
					if (n.$ === 'GradedStudent') {
						return true;
					} else {
						return false;
					}
				},
				A2(
					$elm$core$List$map,
					$author$project$Student$assignStudentGrade(grade_boundaries),
					A2(
						$elm$core$List$map,
						$author$project$Student$rowToUngradedStudent,
						A2(
							$elm$core$List$filter,
							function (n) {
								var _v0 = $elm$core$List$head(n);
								if (_v0.$ === 'Nothing') {
									return false;
								} else {
									var x = _v0.a;
									return !$elm$core$String$isEmpty(x);
								}
							},
							A2(
								$elm$core$List$filter,
								function (n) {
									return $elm$core$List$length(n) > 1;
								},
								A2(
									$elm$core$List$map,
									$elm$core$String$split(separatorStr),
									A2(
										$elm$core$String$split,
										'\n',
										$elm$core$String$trim(content)))))))),
			column_names);
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$file$File$toString = _File_toString;
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'ToggleSort':
				var sortEntries = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{sortEntries: sortEntries}),
					$elm$core$Platform$Cmd$none);
			case 'ChangeSeparator':
				var sep = msg.a;
				var separator = function () {
					switch (sep) {
						case ',':
							return $author$project$Main$Comma;
						case '\t':
							return $author$project$Main$Tab;
						default:
							return $author$project$Main$AutoDetect;
					}
				}();
				var parsed_rows = A3($author$project$Main$parseStudentRows, separator, model.content, model.grades);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{column_names: parsed_rows.b, separator: separator, students: parsed_rows.a}),
					$elm$core$Platform$Cmd$none);
			case 'ContentChange':
				var textAreaContent = msg.a;
				var parsed_rows = A3($author$project$Main$parseStudentRows, model.separator, textAreaContent, model.grades);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{column_names: parsed_rows.b, content: textAreaContent, students: parsed_rows.a}),
					$elm$core$Platform$Cmd$none);
			case 'NewGradeBoundary':
				var grade = msg.a;
				var newBoundaryStr = msg.b;
				var newBoundary = function () {
					var _v3 = $elm$core$String$toFloat(newBoundaryStr);
					if (_v3.$ === 'Nothing') {
						return 0.0;
					} else {
						var x = _v3.a;
						return x;
					}
				}();
				var newGradeBoundaries = A2(
					$elm$core$List$map,
					function (n) {
						if (n.$ === 'PassGrade') {
							var grade_ = n.a;
							var boundary = n.b;
							return (_Utils_eq(grade_, grade) && (newBoundary >= 0.0)) ? A2($author$project$Student$PassGrade, grade, newBoundary) : n;
						} else {
							return n;
						}
					},
					model.grades);
				var parsed_rows = A3($author$project$Main$parseStudentRows, model.separator, model.content, newGradeBoundaries);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{column_names: parsed_rows.b, grades: newGradeBoundaries, students: parsed_rows.a}),
					$elm$core$Platform$Cmd$none);
			case 'HistogramBinChange':
				var newBins = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							histogramBins: function () {
								var _v4 = $elm$core$String$toInt(newBins);
								if (_v4.$ === 'Nothing') {
									return model.histogramBins;
								} else {
									var x = _v4.a;
									return x;
								}
							}()
						}),
					$elm$core$Platform$Cmd$none);
			case 'ExportData':
				return _Utils_Tuple2(
					model,
					$author$project$Main$download(
						A2(
							$elm$core$String$join,
							'\u000D\n',
							A2(
								$elm$core$List$map,
								function (n) {
									return $author$project$Student$getStudentRollno(n) + ('\t' + $author$project$Student$getStudentGradeStr(n));
								},
								A2(
									$elm$core$List$filter,
									function (n) {
										return $author$project$Student$isGradedStudent(n);
									},
									model.students)))));
			case 'SuggestPartitions':
				var newGradeBoundaries = $elm$core$List$concat(
					_List_fromArray(
						[
							A3(
							$elm$core$List$map2,
							F2(
								function (x, y) {
									return A2($author$project$Student$PassGrade, x, y);
								}),
							A2($elm$core$List$map, $author$project$Student$gradeToStr, model.grades),
							$elm$core$List$reverse(
								A2(
									$author$project$GradeSuggestions$autoPartition,
									$elm$core$List$length(model.grades) - 1,
									A2($elm$core$List$map, $author$project$Student$getStudentScore, model.students)))),
							_List_fromArray(
							[
								function () {
								var _v5 = $elm$core$List$head(
									$elm$core$List$reverse(model.grades));
								if (_v5.$ === 'Nothing') {
									return $author$project$Student$FailGrade('FR');
								} else {
									var x = _v5.a;
									return x;
								}
							}()
							])
						]));
				var parsed_rows = A3($author$project$Main$parseStudentRows, model.separator, model.content, newGradeBoundaries);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{column_names: parsed_rows.b, grades: newGradeBoundaries, students: parsed_rows.a}),
					$elm$core$Platform$Cmd$none);
			case 'UploadRequested':
				return _Utils_Tuple2(
					model,
					A2(
						$elm$file$File$Select$file,
						_List_fromArray(
							['text/csv', 'text/tsv']),
						$author$project$Main$UploadSelected));
			case 'UploadSelected':
				var file = msg.a;
				return _Utils_Tuple2(
					model,
					A2(
						$elm$core$Task$perform,
						$author$project$Main$UploadLoaded,
						$elm$file$File$toString(file)));
			default:
				var content = msg.a;
				var parsed_rows = A3($author$project$Main$parseStudentRows, model.separator, content, model.grades);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{column_names: parsed_rows.b, content: content, students: parsed_rows.a}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $author$project$Main$ChangeSeparator = function (a) {
	return {$: 'ChangeSeparator', a: a};
};
var $author$project$Main$ContentChange = function (a) {
	return {$: 'ContentChange', a: a};
};
var $author$project$Main$UploadRequested = {$: 'UploadRequested'};
var $elm$html$Html$br = _VirtualDom_node('br');
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $author$project$RandomStudentData$data = _List_fromArray(
	[69.57, 45.48, 78.81, 52.14, 54.91, 35.77, 48.87, 66.6, 59.84, 68.13, 96.44, 56.66, 60.41, 58.46, 39.64, 61.39, 74.18, 45.42, 44.63, 30.37, 70.55, 61.22, 85.79, 64.72, 74.23, 35.09, 60.23, 39.57, 45.65, 42.62, 43.5, 44.71, 64.04, 40.4, 7.95, 48.46, 35.34, 34.87, 67.89, 52.11, 60.75, 46.28, 35.99, 65.5, 63.29, 78.04, 37.8, 46.19, 63.38, 37.05, 55.88, 61.08, 73.02, 60.8, 70.02, 58.15, 42.55, 67.71, 64.43, 74.97, 30.32, 42.02, 66.75, 55.3, 52.1, 75.37, 44.34, 36.04, 60.54, 56.24, 52.56, 39.22, 63.88, 64.7, 61.19, 33.28, 61.46, 62.19, 43.53, 48.94, 51.74, 28.22, 62.83, 61.22, 89.76, 25.49, 58.21, 59.08, 29.83, 55.4, 72.89, 31.56, 24.16, 52.51, 75.33, 26.23, 62.39, 25.53, 64.9, 45.64, 44.9, 34.93, 25.73, 53.73, 65.08, 60.69, 7.13, 70.85, 47.27, 56.67, 68.92, 35.83, 33.83, 69.3, 47.22, 68.76, 50.19, 42.77, 31.41, 42.19, 46.9, 27.8, 63.51, 57.79, 63.47, 49.36, 61.18, 64.75, 42.09, 42.02, 63.91, 53.01, 15.83, 54.02, 42.09, 47.37, 61.62, 54.95, 47.12, 65.38, 70.36, 58.13, 58.27, 51.34, 43.04, 44.54, 64.03, 65.31, 32.54, 42.47, 41.35, 47.78, 48.63, 24.77, 62.35, 44.96, 38.5, 65.49, 71.63, 40.69, 46.84, 42.5, 28.24, 26.81, 51.29, 30.99, 47.87, 62.48, 63.94, 54.27, 35.0, 69.68, 28.33, 51.15, 64.4, 60.18, 57.17, 41.25, 48.48, 39.28, 40.94, 72.25, 18.63, 32.66, 29.66, 59.6, 69.81, 60.25, 47.35, 32.54, 43.85, 39.65, 59.03, 74.34, 49.72, 31.39, 55.7, 46.07, 35.37, 68.7]);
var $elm$core$String$fromFloat = _String_fromNumber;
var $author$project$RandomStudentData$generateRandomData = F2(
	function (num, separator) {
		return A2(
			$elm$core$String$join,
			'\n',
			A2(
				$elm$core$List$map,
				$elm$core$String$join(separator),
				A3(
					$elm$core$List$map2,
					$elm$core$Basics$append,
					A2(
						$elm$core$List$map,
						function (n) {
							return _List_fromArray(
								[
									'rollno' + $elm$core$String$fromInt(n),
									'Student ' + $elm$core$String$fromInt(n)
								]);
						},
						A2($elm$core$List$range, 1, num)),
					A2(
						$elm$core$List$map,
						function (n) {
							return _List_fromArray(
								[
									$elm$core$String$fromFloat(n)
								]);
						},
						$author$project$RandomStudentData$data))));
	});
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$option = _VirtualDom_node('option');
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$pre = _VirtualDom_node('pre');
var $elm$html$Html$select = _VirtualDom_node('select');
var $elm$html$Html$Attributes$selected = $elm$html$Html$Attributes$boolProperty('selected');
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Main$dataEntryView = function (model) {
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$h2,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Step 1: Data Entry')
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Choose your data separator:'),
					A2(
					$elm$html$Html$select,
					_List_fromArray(
						[
							$elm$html$Html$Events$onInput($author$project$Main$ChangeSeparator)
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$option,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$value(''),
									$elm$html$Html$Attributes$selected(true)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Auto-detect')
								])),
							A2(
							$elm$html$Html$option,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$value('\t')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Tab')
								])),
							A2(
							$elm$html$Html$option,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$value(',')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Comma')
								]))
						]))
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Paste your CSV/Tab separated grades here. The first column is considered as the roll number, and the last one as the score out of 100.')
				])),
			A2(
			$elm$html$Html$textarea,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$placeholder('Paste data here.'),
					$elm$html$Html$Events$onInput($author$project$Main$ContentChange),
					$elm$html$Html$Attributes$value(model.content),
					A2($elm$html$Html$Attributes$style, 'width', '40em')
				]),
			_List_Nil),
			A2($elm$html$Html$br, _List_Nil, _List_Nil),
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('btn btn-primary'),
					$elm$html$Html$Events$onClick($author$project$Main$UploadRequested)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Upload data...')
				])),
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$disabled(model.content !== ''),
					$elm$html$Html$Attributes$class('btn btn-primary'),
					$elm$html$Html$Events$onClick(
					$author$project$Main$ContentChange(
						A2(
							$author$project$RandomStudentData$generateRandomData,
							100,
							$author$project$Main$getSeparator(model.separator))))
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Insert sample data')
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Example (Tab separated data):')
				])),
			A2(
			$elm$html$Html$pre,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'background-color', '#eee')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('rollno1\tName 1\t90.4\nrollno2\tName 2\t45.6\nrollno3\tName 3\t75.5\n…')
				]))
		]);
};
var $elm$html$Html$div = _VirtualDom_node('div');
var $author$project$Main$SuggestPartitions = {$: 'SuggestPartitions'};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $author$project$Main$NewGradeBoundary = F2(
	function (a, b) {
		return {$: 'NewGradeBoundary', a: a, b: b};
	});
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$html$Html$Attributes$name = $elm$html$Html$Attributes$stringProperty('name');
var $elm$html$Html$Attributes$step = function (n) {
	return A2($elm$html$Html$Attributes$stringProperty, 'step', n);
};
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$Main$gradeBoundaryInputs = function (gradeBoundary) {
	if (gradeBoundary.$ === 'PassGrade') {
		var grade = gradeBoundary.a;
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$label,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'for', 'grade' + grade)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(grade + ': ')
					])),
				A2(
				$elm$html$Html$input,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$placeholder(grade),
						$elm$html$Html$Attributes$name('grade' + grade),
						$elm$html$Html$Attributes$type_('number'),
						$elm$html$Html$Attributes$step('0.5'),
						A2($elm$html$Html$Attributes$style, 'width', '4em'),
						$elm$html$Html$Events$onInput(
						$author$project$Main$NewGradeBoundary(grade)),
						$elm$html$Html$Attributes$value(
						$elm$core$String$fromFloat(
							$author$project$Student$getGradeBoundary(gradeBoundary)))
					]),
				_List_Nil),
				A2($elm$html$Html$br, _List_Nil, _List_Nil)
			]);
	} else {
		var grade = gradeBoundary.a;
		return _List_fromArray(
			[
				$elm$html$Html$text(grade + ': 0')
			]);
	}
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$Main$gradeBoundaryView = function (model) {
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$h2,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Step 2: Specify the grade boundaries:')
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Hint: use the mouse scroll button or arrow keys to change inputs in the input boxes for fine grained control.')
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_Nil,
					$elm$core$List$concat(
						_List_fromArray(
							[
								_List_fromArray(
								[
									$elm$html$Html$text('Grade boundaries')
								]),
								_List_fromArray(
								[
									A2($elm$html$Html$br, _List_Nil, _List_Nil)
								]),
								A2($elm$core$List$concatMap, $author$project$Main$gradeBoundaryInputs, model.grades)
							])))
				])),
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick($author$project$Main$SuggestPartitions),
					$elm$html$Html$Attributes$disabled(
					$elm$core$List$length(model.students) < 15),
					$elm$html$Html$Attributes$class('btn btn-primary')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Suggest partitions')
				]))
		]);
};
var $author$project$Main$ExportData = {$: 'ExportData'};
var $author$project$Main$ToggleSort = function (a) {
	return {$: 'ToggleSort', a: a};
};
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $author$project$Student$getStudentData = function (s) {
	switch (s.$) {
		case 'InvalidStudent':
			return _List_Nil;
		case 'UngradedStudent':
			var d = s.b;
			return d;
		default:
			var d = s.b;
			return d;
	}
};
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$html$Html$Events$targetChecked = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'checked']),
	$elm$json$Json$Decode$bool);
var $elm$html$Html$Events$onCheck = function (tagger) {
	return A2(
		$elm$html$Html$Events$on,
		'change',
		A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetChecked));
};
var $elm$html$Html$Attributes$scope = $elm$html$Html$Attributes$stringProperty('scope');
var $elm$core$List$sortBy = _List_sortBy;
var $elm$core$List$sort = function (xs) {
	return A2($elm$core$List$sortBy, $elm$core$Basics$identity, xs);
};
var $elm$html$Html$table = _VirtualDom_node('table');
var $elm$html$Html$tbody = _VirtualDom_node('tbody');
var $elm$html$Html$td = _VirtualDom_node('td');
var $elm$html$Html$th = _VirtualDom_node('th');
var $elm$html$Html$thead = _VirtualDom_node('thead');
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $author$project$Main$gradedRowsView = function (model) {
	var listEntries = model.sortEntries ? A2(
		$elm$core$List$sortBy,
		function (n) {
			return -$author$project$Student$getStudentScore(n);
		},
		model.students) : model.students;
	var dataCols = function () {
		var _v0 = $elm$core$List$head(model.students);
		if (_v0.$ === 'Nothing') {
			return -1;
		} else {
			var x = _v0.a;
			return $elm$core$List$length(
				$author$project$Student$getStudentData(x));
		}
	}();
	var fallback_column_names = $elm$core$List$concat(
		_List_fromArray(
			[
				_List_fromArray(
				['Roll number']),
				A2(
				$elm$core$List$map,
				function (n) {
					return 'Column ' + $elm$core$String$fromInt(n);
				},
				A2($elm$core$List$range, 1, dataCols)),
				_List_fromArray(
				['Score'])
			]));
	var column_names = _Utils_eq(
		$elm$core$List$length(model.column_names),
		$elm$core$List$length(fallback_column_names)) ? model.column_names : fallback_column_names;
	var boundaries = A2($elm$core$List$map, $author$project$Student$getGradeBoundary, model.grades);
	return _Utils_eq(
		$elm$core$List$reverse(boundaries),
		$elm$core$List$sort(boundaries)) ? _List_fromArray(
		[
			A2(
			$elm$html$Html$h2,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Step 4: Verify grades')
				])),
			$elm$html$Html$text('Sort by score?'),
			A2(
			$elm$html$Html$input,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$type_('checkbox'),
					$elm$html$Html$Attributes$checked(model.sortEntries),
					$elm$html$Html$Events$onCheck($author$project$Main$ToggleSort)
				]),
			_List_Nil),
			A2($elm$html$Html$br, _List_Nil, _List_Nil),
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('btn btn-primary'),
					$elm$html$Html$Events$onClick($author$project$Main$ExportData),
					$elm$html$Html$Attributes$disabled(
					!$elm$core$List$length(model.students))
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Export for ASC upload')
				])),
			A2($elm$html$Html$br, _List_Nil, _List_Nil),
			A2(
			$elm$html$Html$table,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('table')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$thead,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$tr,
							_List_Nil,
							$elm$core$List$concat(
								_List_fromArray(
									[
										A2(
										$elm$core$List$map,
										function (n) {
											return A2(
												$elm$html$Html$th,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$scope('col')
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(n)
													]));
										},
										column_names),
										_List_fromArray(
										[
											A2(
											$elm$html$Html$th,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$scope('col')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('Grade')
												]))
										])
									])))
						])),
					A2(
					$elm$html$Html$tbody,
					_List_Nil,
					$elm$core$List$concat(
						A2(
							$elm$core$List$map,
							function (n) {
								return _List_fromArray(
									[
										A2(
										$elm$html$Html$tr,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class(
												'grade' + $author$project$Student$getStudentGradeStr(n))
											]),
										$elm$core$List$concat(
											_List_fromArray(
												[
													_List_fromArray(
													[
														A2(
														$elm$html$Html$th,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$scope('row')
															]),
														_List_fromArray(
															[
																$elm$html$Html$text(
																$author$project$Student$getStudentRollno(n))
															]))
													]),
													A2(
													$elm$core$List$map,
													function (m) {
														return A2(
															$elm$html$Html$td,
															_List_Nil,
															_List_fromArray(
																[
																	$elm$html$Html$text(m)
																]));
													},
													$author$project$Student$getStudentData(n)),
													_List_fromArray(
													[
														A2(
														$elm$html$Html$td,
														_List_Nil,
														_List_fromArray(
															[
																$elm$html$Html$text(
																$elm$core$String$fromFloat(
																	$author$project$Student$getStudentScore(n)))
															]))
													]),
													_List_fromArray(
													[
														A2(
														$elm$html$Html$td,
														_List_Nil,
														_List_fromArray(
															[
																$elm$html$Html$text(
																$author$project$Student$getStudentGradeStr(n))
															]))
													])
												])))
									]);
							},
							listEntries)))
				]))
		]) : _List_fromArray(
		[
			A2(
			$elm$html$Html$span,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'color', 'red')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Grade ordering incorrect!')
				]))
		]);
};
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $author$project$Main$HistogramBinChange = function (a) {
	return {$: 'HistogramBinChange', a: a};
};
var $author$project$Main$countGradeDist = F2(
	function (studentList, gradeBoundaries) {
		return A2(
			$elm$core$List$map,
			function (n) {
				return {
					count: $elm$core$List$length(
						A2(
							$elm$core$List$filter,
							function (m) {
								return _Utils_eq(
									$author$project$Student$getStudentGradeStr(m),
									n);
							},
							studentList)),
					grade: n
				};
			},
			A2(
				$elm$core$List$map,
				function (n) {
					if (n.$ === 'PassGrade') {
						var g = n.a;
						return g;
					} else {
						var g = n.a;
						return g;
					}
				},
				gradeBoundaries));
	});
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $elm$html$Html$hr = _VirtualDom_node('hr');
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm_community$typed_svg$TypedSvg$Core$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm_community$typed_svg$TypedSvg$Attributes$class = function (names) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'class',
		A2($elm$core$String$join, ' ', names));
};
var $elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString = function (length) {
	switch (length.$) {
		case 'Cm':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'cm';
		case 'Em':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'em';
		case 'Ex':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'ex';
		case 'In':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'in';
		case 'Mm':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'mm';
		case 'Num':
			var x = length.a;
			return $elm$core$String$fromFloat(x);
		case 'Pc':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'pc';
		case 'Percent':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + '%';
		case 'Pt':
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'pt';
		default:
			var x = length.a;
			return $elm$core$String$fromFloat(x) + 'px';
	}
};
var $elm_community$typed_svg$TypedSvg$Attributes$dy = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'dy',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Types$Em = function (a) {
	return {$: 'Em', a: a};
};
var $elm_community$typed_svg$TypedSvg$Types$em = $elm_community$typed_svg$TypedSvg$Types$Em;
var $elm_community$typed_svg$TypedSvg$Attributes$InEm$dy = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$dy(
		$elm_community$typed_svg$TypedSvg$Types$em(value));
};
var $elm_community$typed_svg$TypedSvg$Attributes$fontSize = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'font-size',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InEm$fontSize = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$fontSize(
		$elm_community$typed_svg$TypedSvg$Types$em(value));
};
var $elm$virtual_dom$VirtualDom$nodeNS = F2(
	function (namespace, tag) {
		return A2(
			_VirtualDom_nodeNS,
			namespace,
			_VirtualDom_noScript(tag));
	});
var $elm_community$typed_svg$TypedSvg$Core$node = $elm$virtual_dom$VirtualDom$nodeNS('http://www.w3.org/2000/svg');
var $elm_community$typed_svg$TypedSvg$g = $elm_community$typed_svg$TypedSvg$Core$node('g');
var $elm_community$typed_svg$TypedSvg$Attributes$height = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'height',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Types$Px = function (a) {
	return {$: 'Px', a: a};
};
var $elm_community$typed_svg$TypedSvg$Types$px = $elm_community$typed_svg$TypedSvg$Types$Px;
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$height = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$height(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $elm_community$typed_svg$TypedSvg$rect = $elm_community$typed_svg$TypedSvg$Core$node('rect');
var $elm_community$typed_svg$TypedSvg$Core$text = $elm$virtual_dom$VirtualDom$text;
var $elm_community$typed_svg$TypedSvg$text_ = $elm_community$typed_svg$TypedSvg$Core$node('text');
var $elm_community$typed_svg$TypedSvg$Attributes$width = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'width',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$width = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$width(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $elm_community$typed_svg$TypedSvg$Attributes$x = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'x',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$x = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$x(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $elm_community$typed_svg$TypedSvg$Attributes$y = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'y',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$y = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$y(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $author$project$GradeBarChart$drawBar = F4(
	function (barWidth, barHeight, barY, barLabel) {
		return A2(
			$elm_community$typed_svg$TypedSvg$g,
			_List_fromArray(
				[
					$elm_community$typed_svg$TypedSvg$Attributes$class(
					_List_fromArray(
						['bar']))
				]),
			_List_fromArray(
				[
					A2(
					$elm_community$typed_svg$TypedSvg$rect,
					_List_fromArray(
						[
							$elm_community$typed_svg$TypedSvg$Attributes$InPx$width(barWidth),
							$elm_community$typed_svg$TypedSvg$Attributes$InPx$height(barHeight),
							$elm_community$typed_svg$TypedSvg$Attributes$InPx$y(barY)
						]),
					_List_Nil),
					A2(
					$elm_community$typed_svg$TypedSvg$text_,
					_List_fromArray(
						[
							$elm_community$typed_svg$TypedSvg$Attributes$InPx$x(barWidth + 5),
							$elm_community$typed_svg$TypedSvg$Attributes$InPx$y(barY + 4.5),
							$elm_community$typed_svg$TypedSvg$Attributes$InEm$dy(0.35),
							$elm_community$typed_svg$TypedSvg$Attributes$InEm$fontSize(0.7)
						]),
					_List_fromArray(
						[
							$elm_community$typed_svg$TypedSvg$Core$text(barLabel)
						]))
				]));
	});
var $author$project$GradeBarChart$h = 100;
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm_community$typed_svg$TypedSvg$svg = $elm_community$typed_svg$TypedSvg$Core$node('svg');
var $elm_community$typed_svg$TypedSvg$Attributes$viewBox = F4(
	function (minX, minY, vWidth, vHeight) {
		return A2(
			$elm_community$typed_svg$TypedSvg$Core$attribute,
			'viewBox',
			A2(
				$elm$core$String$join,
				' ',
				A2(
					$elm$core$List$map,
					$elm$core$String$fromFloat,
					_List_fromArray(
						[minX, minY, vWidth, vHeight]))));
	});
var $author$project$GradeBarChart$w = 450;
var $author$project$GradeBarChart$view = function (model) {
	var maxNMaybe = $elm$core$List$maximum(
		A2($elm$core$List$map, $elm$core$Tuple$second, model));
	var maxN = function () {
		if (maxNMaybe.$ === 'Nothing') {
			return 0;
		} else {
			var x = maxNMaybe.a;
			return x;
		}
	}();
	var widthScaling = ($author$project$GradeBarChart$w / maxN) * 0.8;
	return (maxN < 1) ? A2($elm_community$typed_svg$TypedSvg$svg, _List_Nil, _List_Nil) : A2(
		$elm_community$typed_svg$TypedSvg$svg,
		_List_fromArray(
			[
				$elm_community$typed_svg$TypedSvg$Attributes$class(
				_List_fromArray(
					['chart'])),
				A4($elm_community$typed_svg$TypedSvg$Attributes$viewBox, 0, 0, $author$project$GradeBarChart$w, $author$project$GradeBarChart$h)
			]),
		A2(
			$elm$core$List$map,
			function (n) {
				var occurrences = n.b.b;
				var index = n.a;
				var grade = n.b.a;
				return A4(
					$author$project$GradeBarChart$drawBar,
					widthScaling * occurrences,
					10,
					(index * 11) + 5,
					grade + (': ' + $elm$core$String$fromInt(occurrences)));
			},
			A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, model)));
};
var $elm_community$typed_svg$TypedSvg$Types$Scale = F2(
	function (a, b) {
		return {$: 'Scale', a: a, b: b};
	});
var $elm_community$typed_svg$TypedSvg$Types$Translate = F2(
	function (a, b) {
		return {$: 'Translate', a: a, b: b};
	});
var $gampleman$elm_visualization$Scale$convert = F2(
	function (_v0, value) {
		var scale = _v0.a;
		return A3(scale.convert, scale.domain, scale.range, value);
	});
var $author$project$GradeHistogram$h = 150;
var $elm_community$typed_svg$TypedSvg$line = $elm_community$typed_svg$TypedSvg$Core$node('line');
var $author$project$GradeHistogram$padding = 30;
var $avh4$elm_color$Color$RgbaSpace = F4(
	function (a, b, c, d) {
		return {$: 'RgbaSpace', a: a, b: b, c: c, d: d};
	});
var $avh4$elm_color$Color$scaleFrom255 = function (c) {
	return c / 255;
};
var $avh4$elm_color$Color$rgb255 = F3(
	function (r, g, b) {
		return A4(
			$avh4$elm_color$Color$RgbaSpace,
			$avh4$elm_color$Color$scaleFrom255(r),
			$avh4$elm_color$Color$scaleFrom255(g),
			$avh4$elm_color$Color$scaleFrom255(b),
			1.0);
	});
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $avh4$elm_color$Color$toCssString = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	var roundTo = function (x) {
		return $elm$core$Basics$round(x * 1000) / 1000;
	};
	var pct = function (x) {
		return $elm$core$Basics$round(x * 10000) / 100;
	};
	return $elm$core$String$concat(
		_List_fromArray(
			[
				'rgba(',
				$elm$core$String$fromFloat(
				pct(r)),
				'%,',
				$elm$core$String$fromFloat(
				pct(g)),
				'%,',
				$elm$core$String$fromFloat(
				pct(b)),
				'%,',
				$elm$core$String$fromFloat(
				roundTo(a)),
				')'
			]));
};
var $elm_community$typed_svg$TypedSvg$Attributes$stroke = function (col) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'stroke',
		$avh4$elm_color$Color$toCssString(col));
};
var $elm_community$typed_svg$TypedSvg$Attributes$strokeDasharray = $elm_community$typed_svg$TypedSvg$Core$attribute('stroke-dasharray');
var $elm_community$typed_svg$TypedSvg$Attributes$strokeWidth = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'stroke-width',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$strokeWidth = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$strokeWidth(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $elm_community$typed_svg$TypedSvg$Attributes$x1 = function (position) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'x1',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(position));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$x1 = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$x1(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $elm_community$typed_svg$TypedSvg$Attributes$x2 = function (position) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'x2',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(position));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$x2 = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$x2(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $gampleman$elm_visualization$Scale$Scale = function (a) {
	return {$: 'Scale', a: a};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $gampleman$elm_visualization$Scale$Continuous$normalize = F2(
	function (a, b) {
		var c = b - a;
		return (!c) ? $elm$core$Basics$always(0.5) : ($elm$core$Basics$isNaN(c) ? $elm$core$Basics$always(0 / 0) : function (x) {
			return (x - a) / c;
		});
	});
var $gampleman$elm_visualization$Scale$Continuous$bimap = F3(
	function (_v0, _v1, interpolate) {
		var d0 = _v0.a;
		var d1 = _v0.b;
		var r0 = _v1.a;
		var r1 = _v1.b;
		var _v2 = (_Utils_cmp(d1, d0) < 0) ? _Utils_Tuple2(
			A2($gampleman$elm_visualization$Scale$Continuous$normalize, d1, d0),
			A2(interpolate, r1, r0)) : _Utils_Tuple2(
			A2($gampleman$elm_visualization$Scale$Continuous$normalize, d0, d1),
			A2(interpolate, r0, r1));
		var de = _v2.a;
		var re = _v2.b;
		return A2($elm$core$Basics$composeL, re, de);
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $gampleman$elm_visualization$Scale$Continuous$convertTransform = F4(
	function (transform, interpolate, _v0, range) {
		var d0 = _v0.a;
		var d1 = _v0.b;
		return A2(
			$elm$core$Basics$composeR,
			transform,
			A3(
				$gampleman$elm_visualization$Scale$Continuous$bimap,
				_Utils_Tuple2(
					transform(d0),
					transform(d1)),
				range,
				interpolate));
	});
var $gampleman$elm_visualization$Interpolation$float = F2(
	function (a, to) {
		var b = to - a;
		return function (t) {
			return a + (b * t);
		};
	});
var $gampleman$elm_visualization$Scale$Continuous$invertTransform = F4(
	function (transform, untransform, _v0, range) {
		var d0 = _v0.a;
		var d1 = _v0.b;
		return A2(
			$elm$core$Basics$composeR,
			A3(
				$gampleman$elm_visualization$Scale$Continuous$bimap,
				range,
				_Utils_Tuple2(
					transform(d0),
					transform(d1)),
				$gampleman$elm_visualization$Interpolation$float),
			untransform);
	});
var $gampleman$elm_visualization$Scale$Continuous$fixPoint = F3(
	function (maxIterations, initialInput, fn) {
		var helper = F2(
			function (iters, _v0) {
				helper:
				while (true) {
					var a = _v0.a;
					var b = _v0.b;
					if (_Utils_cmp(iters + 1, maxIterations) > -1) {
						return b;
					} else {
						var _v1 = fn(b);
						var outA = _v1.a;
						var outB = _v1.b;
						if (_Utils_eq(outA, a)) {
							return b;
						} else {
							if (!outA) {
								return b;
							} else {
								var $temp$iters = iters + 1,
									$temp$_v0 = _Utils_Tuple2(outA, outB);
								iters = $temp$iters;
								_v0 = $temp$_v0;
								continue helper;
							}
						}
					}
				}
			});
		return A2(
			helper,
			1,
			fn(initialInput));
	});
var $elm$core$Basics$e = _Basics_e;
var $gampleman$elm_visualization$Scale$Continuous$e10 = $elm$core$Basics$sqrt(50);
var $gampleman$elm_visualization$Scale$Continuous$e2 = $elm$core$Basics$sqrt(2);
var $gampleman$elm_visualization$Scale$Continuous$e5 = $elm$core$Basics$sqrt(10);
var $gampleman$elm_visualization$Scale$Continuous$ln10 = A2($elm$core$Basics$logBase, $elm$core$Basics$e, 10);
var $gampleman$elm_visualization$Scale$Continuous$tickIncrement = F3(
	function (start, stop, count) {
		var step = (stop - start) / A2($elm$core$Basics$max, 0, count);
		var powr = $elm$core$Basics$floor(
			A2($elm$core$Basics$logBase, $elm$core$Basics$e, step) / $gampleman$elm_visualization$Scale$Continuous$ln10);
		var error = step / A2($elm$core$Basics$pow, 10, powr);
		var order = (_Utils_cmp(error, $gampleman$elm_visualization$Scale$Continuous$e10) > -1) ? 10 : ((_Utils_cmp(error, $gampleman$elm_visualization$Scale$Continuous$e5) > -1) ? 5 : ((_Utils_cmp(error, $gampleman$elm_visualization$Scale$Continuous$e2) > -1) ? 2 : 1));
		return (powr >= 0) ? (order * A2($elm$core$Basics$pow, 10, powr)) : ((-A2($elm$core$Basics$pow, 10, -powr)) / order);
	});
var $gampleman$elm_visualization$Scale$Continuous$withNormalizedDomain = F2(
	function (fn, _v0) {
		var a = _v0.a;
		var b = _v0.b;
		if (_Utils_cmp(a, b) < 0) {
			return fn(
				_Utils_Tuple2(a, b));
		} else {
			var _v1 = fn(
				_Utils_Tuple2(b, a));
			var d = _v1.a;
			var c = _v1.b;
			return _Utils_Tuple2(c, d);
		}
	});
var $gampleman$elm_visualization$Scale$Continuous$nice = F2(
	function (domain, count) {
		var computation = function (_v0) {
			var start = _v0.a;
			var stop = _v0.b;
			var step = A3($gampleman$elm_visualization$Scale$Continuous$tickIncrement, start, stop, count);
			return _Utils_Tuple2(
				step,
				(step > 0) ? _Utils_Tuple2(
					$elm$core$Basics$floor(start / step) * step,
					$elm$core$Basics$ceiling(stop / step) * step) : ((step < 0) ? _Utils_Tuple2(
					$elm$core$Basics$ceiling(start * step) / step,
					$elm$core$Basics$floor(stop * step) / step) : _Utils_Tuple2(start, stop)));
		};
		return A2(
			$gampleman$elm_visualization$Scale$Continuous$withNormalizedDomain,
			function (dmn) {
				return A3($gampleman$elm_visualization$Scale$Continuous$fixPoint, 10, dmn, computation);
			},
			domain);
	});
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $gampleman$elm_visualization$Scale$Continuous$exponent = function (num) {
	var helper = F2(
		function (soFar, x) {
			helper:
			while (true) {
				if (!x) {
					return soFar;
				} else {
					if (x < 1) {
						var $temp$soFar = 1 + soFar,
							$temp$x = x * 10;
						soFar = $temp$soFar;
						x = $temp$x;
						continue helper;
					} else {
						return soFar;
					}
				}
			}
		});
	return A2(helper, 0, num);
};
var $gampleman$elm_visualization$Scale$Continuous$precisionFixed = function (step) {
	return A2(
		$elm$core$Basics$max,
		0,
		$gampleman$elm_visualization$Scale$Continuous$exponent(
			$elm$core$Basics$abs(step)));
};
var $gampleman$elm_visualization$Statistics$tickStep = F3(
	function (start, stop, count) {
		var step0 = $elm$core$Basics$abs(stop - start) / A2($elm$core$Basics$max, 0, count);
		var step1 = A2(
			$elm$core$Basics$pow,
			10,
			$elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Basics$e, step0) / A2($elm$core$Basics$logBase, $elm$core$Basics$e, 10)));
		var error = step0 / step1;
		var step2 = (_Utils_cmp(
			error,
			$elm$core$Basics$sqrt(50)) > -1) ? (step1 * 10) : ((_Utils_cmp(
			error,
			$elm$core$Basics$sqrt(10)) > -1) ? (step1 * 5) : ((_Utils_cmp(
			error,
			$elm$core$Basics$sqrt(2)) > -1) ? (step1 * 2) : step1));
		return (_Utils_cmp(stop, start) < 0) ? (-step2) : step2;
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padRight = F3(
	function (n, _char, string) {
		return _Utils_ap(
			string,
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)));
	});
var $gampleman$elm_visualization$Scale$Continuous$toFixed = F2(
	function (precision, value) {
		var power_ = A2($elm$core$Basics$pow, 10, precision);
		var pad = function (num) {
			_v0$2:
			while (true) {
				if (num.b) {
					if (num.b.b) {
						if (!num.b.b.b) {
							var x = num.a;
							var _v1 = num.b;
							var y = _v1.a;
							return _List_fromArray(
								[
									x,
									A3(
									$elm$core$String$padRight,
									precision,
									_Utils_chr('0'),
									y)
								]);
						} else {
							break _v0$2;
						}
					} else {
						var val = num.a;
						return (precision > 0) ? _List_fromArray(
							[
								val,
								A3(
								$elm$core$String$padRight,
								precision,
								_Utils_chr('0'),
								'')
							]) : _List_fromArray(
							[val]);
					}
				} else {
					break _v0$2;
				}
			}
			var val = num;
			return val;
		};
		return A2(
			$elm$core$String$join,
			'.',
			pad(
				A2(
					$elm$core$String$split,
					'.',
					$elm$core$String$fromFloat(
						$elm$core$Basics$round(value * power_) / power_))));
	});
var $gampleman$elm_visualization$Scale$Continuous$tickFormat = F2(
	function (_v0, count) {
		var start = _v0.a;
		var stop = _v0.b;
		return $gampleman$elm_visualization$Scale$Continuous$toFixed(
			$gampleman$elm_visualization$Scale$Continuous$precisionFixed(
				A3($gampleman$elm_visualization$Statistics$tickStep, start, stop, count)));
	});
var $elmcraft$core_extra$Float$Extra$range = F3(
	function (start, stop, step) {
		if (!step) {
			return _List_Nil;
		} else {
			var n = A2(
				$elm$core$Basics$max,
				0,
				$elm$core$Basics$ceiling((stop - start) / step));
			var helper = F2(
				function (i, list) {
					helper:
					while (true) {
						if (i >= 0) {
							var $temp$i = i - 1,
								$temp$list = A2($elm$core$List$cons, start + (step * i), list);
							i = $temp$i;
							list = $temp$list;
							continue helper;
						} else {
							return list;
						}
					}
				});
			return A2(helper, n - 1, _List_Nil);
		}
	});
var $gampleman$elm_visualization$Statistics$range = $elmcraft$core_extra$Float$Extra$range;
var $gampleman$elm_visualization$Statistics$ticks = F3(
	function (start, stop, count) {
		var step = A3($gampleman$elm_visualization$Statistics$tickStep, start, stop, count);
		var end = ($elm$core$Basics$floor(stop / step) * step) + (step / 2);
		var beg = $elm$core$Basics$ceiling(start / step) * step;
		return A3($gampleman$elm_visualization$Statistics$range, beg, end, step);
	});
var $gampleman$elm_visualization$Scale$Continuous$ticks = F2(
	function (_v0, count) {
		var start = _v0.a;
		var end = _v0.b;
		return A3($gampleman$elm_visualization$Statistics$ticks, start, end, count);
	});
var $gampleman$elm_visualization$Scale$Continuous$scaleWithTransform = F4(
	function (transform, untransform, range_, domain_) {
		return {
			convert: A2($gampleman$elm_visualization$Scale$Continuous$convertTransform, transform, $gampleman$elm_visualization$Interpolation$float),
			domain: domain_,
			invert: A2($gampleman$elm_visualization$Scale$Continuous$invertTransform, transform, untransform),
			nice: $gampleman$elm_visualization$Scale$Continuous$nice,
			range: range_,
			rangeExtent: F2(
				function (_v0, r) {
					return r;
				}),
			tickFormat: $gampleman$elm_visualization$Scale$Continuous$tickFormat,
			ticks: $gampleman$elm_visualization$Scale$Continuous$ticks
		};
	});
var $gampleman$elm_visualization$Scale$Continuous$linear = A2($gampleman$elm_visualization$Scale$Continuous$scaleWithTransform, $elm$core$Basics$identity, $elm$core$Basics$identity);
var $gampleman$elm_visualization$Scale$linear = F2(
	function (range_, domain_) {
		return $gampleman$elm_visualization$Scale$Scale(
			A2($gampleman$elm_visualization$Scale$Continuous$linear, range_, domain_));
	});
var $author$project$GradeHistogram$w = 450;
var $author$project$GradeHistogram$xScale = A2(
	$gampleman$elm_visualization$Scale$linear,
	_Utils_Tuple2(0, $author$project$GradeHistogram$w - (2 * $author$project$GradeHistogram$padding)),
	_Utils_Tuple2(0, 100));
var $elm_community$typed_svg$TypedSvg$Attributes$y1 = function (position) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'y1',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(position));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$y1 = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$y1(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $elm_community$typed_svg$TypedSvg$Attributes$y2 = function (position) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'y2',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(position));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InPx$y2 = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$y2(
		$elm_community$typed_svg$TypedSvg$Types$px(value));
};
var $author$project$GradeHistogram$boundaryLine = function (x) {
	return A2(
		$elm_community$typed_svg$TypedSvg$line,
		_List_fromArray(
			[
				$elm_community$typed_svg$TypedSvg$Attributes$InPx$x1(
				A2($gampleman$elm_visualization$Scale$convert, $author$project$GradeHistogram$xScale, x)),
				$elm_community$typed_svg$TypedSvg$Attributes$InPx$y1(0.0),
				$elm_community$typed_svg$TypedSvg$Attributes$InPx$x2(
				A2($gampleman$elm_visualization$Scale$convert, $author$project$GradeHistogram$xScale, x)),
				$elm_community$typed_svg$TypedSvg$Attributes$InPx$y2($author$project$GradeHistogram$h - (2 * $author$project$GradeHistogram$padding)),
				$elm_community$typed_svg$TypedSvg$Attributes$InPx$strokeWidth(1),
				$elm_community$typed_svg$TypedSvg$Attributes$stroke(
				A3($avh4$elm_color$Color$rgb255, 0, 0, 0)),
				$elm_community$typed_svg$TypedSvg$Attributes$strokeDasharray('5 5')
			]),
		_List_Nil);
};
var $elm_community$typed_svg$TypedSvg$Types$Fill = function (a) {
	return {$: 'Fill', a: a};
};
var $elm_community$typed_svg$TypedSvg$TypesToStrings$fillToString = function (fill) {
	if (fill.$ === 'Fill') {
		var color = fill.a;
		return $avh4$elm_color$Color$toCssString(color);
	} else {
		return 'none';
	}
};
var $elm_community$typed_svg$TypedSvg$Attributes$fill = A2(
	$elm$core$Basics$composeL,
	$elm_community$typed_svg$TypedSvg$Core$attribute('fill'),
	$elm_community$typed_svg$TypedSvg$TypesToStrings$fillToString);
var $author$project$GradeHistogram$column = F2(
	function (yScale, _v0) {
		var length = _v0.length;
		var x0 = _v0.x0;
		var x1 = _v0.x1;
		return A2(
			$elm_community$typed_svg$TypedSvg$rect,
			_List_fromArray(
				[
					$elm_community$typed_svg$TypedSvg$Attributes$InPx$x(
					A2($gampleman$elm_visualization$Scale$convert, $author$project$GradeHistogram$xScale, x0)),
					$elm_community$typed_svg$TypedSvg$Attributes$InPx$y(
					A2($gampleman$elm_visualization$Scale$convert, yScale, length)),
					$elm_community$typed_svg$TypedSvg$Attributes$InPx$width(
					A2($gampleman$elm_visualization$Scale$convert, $author$project$GradeHistogram$xScale, x1) - A2($gampleman$elm_visualization$Scale$convert, $author$project$GradeHistogram$xScale, x0)),
					$elm_community$typed_svg$TypedSvg$Attributes$InPx$height(
					($author$project$GradeHistogram$h - A2($gampleman$elm_visualization$Scale$convert, yScale, length)) - (2 * $author$project$GradeHistogram$padding)),
					$elm_community$typed_svg$TypedSvg$Attributes$fill(
					$elm_community$typed_svg$TypedSvg$Types$Fill(
						A3($avh4$elm_color$Color$rgb255, 46, 118, 149)))
				]),
			_List_Nil);
	});
var $elm_community$typed_svg$TypedSvg$Attributes$dx = function (length) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'dx',
		$elm_community$typed_svg$TypedSvg$TypesToStrings$lengthToString(length));
};
var $elm_community$typed_svg$TypedSvg$Attributes$InEm$dx = function (value) {
	return $elm_community$typed_svg$TypedSvg$Attributes$dx(
		$elm_community$typed_svg$TypedSvg$Types$em(value));
};
var $author$project$GradeHistogram$gradeText = function (labelStr) {
	var xval = labelStr.a;
	var gradeStr = labelStr.b;
	return A2(
		$elm_community$typed_svg$TypedSvg$text_,
		_List_fromArray(
			[
				$elm_community$typed_svg$TypedSvg$Attributes$InPx$x(
				A2($gampleman$elm_visualization$Scale$convert, $author$project$GradeHistogram$xScale, xval)),
				$elm_community$typed_svg$TypedSvg$Attributes$InPx$y(0.0),
				$elm_community$typed_svg$TypedSvg$Attributes$InEm$dx(-0.1),
				$elm_community$typed_svg$TypedSvg$Attributes$InEm$dy(-0.3),
				$elm_community$typed_svg$TypedSvg$Attributes$InEm$fontSize(0.75)
			]),
		_List_fromArray(
			[
				$elm_community$typed_svg$TypedSvg$Core$text(gradeStr)
			]));
};
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $gampleman$elm_visualization$Histogram$Array$bisectRight = F3(
	function (item, array, extent) {
		var _v0 = A2($elm$core$Array$get, 0, array);
		if (_v0.$ === 'Nothing') {
			return 0;
		} else {
			var _default = _v0.a;
			var get = function (index) {
				return A2(
					$elm$core$Maybe$withDefault,
					_default,
					A2($elm$core$Array$get, index, array));
			};
			var helper = F2(
				function (lo, hi) {
					helper:
					while (true) {
						if (_Utils_cmp(lo, hi) < 0) {
							var mid = (lo + hi) >>> 1;
							if (_Utils_cmp(
								get(mid),
								item) > 0) {
								var $temp$lo = lo,
									$temp$hi = mid;
								lo = $temp$lo;
								hi = $temp$hi;
								continue helper;
							} else {
								var $temp$lo = mid + 1,
									$temp$hi = hi;
								lo = $temp$lo;
								hi = $temp$hi;
								continue helper;
							}
						} else {
							return lo;
						}
					}
				});
			if (extent.$ === 'Just') {
				var _v2 = extent.a;
				var lo = _v2.a;
				var hi = _v2.b;
				return A2(
					helper,
					A2($elm$core$Basics$max, 0, lo),
					A2(
						$elm$core$Basics$min,
						hi,
						$elm$core$Array$length(array)));
			} else {
				return A2(
					helper,
					0,
					$elm$core$Array$length(array));
			}
		}
	});
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $elm$core$Elm$JsArray$foldl = _JsArray_foldl;
var $elm$core$Elm$JsArray$indexedMap = _JsArray_indexedMap;
var $elm$core$Array$indexedMap = F2(
	function (func, _v0) {
		var len = _v0.a;
		var tree = _v0.c;
		var tail = _v0.d;
		var initialBuilder = {
			nodeList: _List_Nil,
			nodeListSize: 0,
			tail: A3(
				$elm$core$Elm$JsArray$indexedMap,
				func,
				$elm$core$Array$tailIndex(len),
				tail)
		};
		var helper = F2(
			function (node, builder) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldl, helper, builder, subTree);
				} else {
					var leaf = node.a;
					var offset = builder.nodeListSize * $elm$core$Array$branchFactor;
					var mappedLeaf = $elm$core$Array$Leaf(
						A3($elm$core$Elm$JsArray$indexedMap, func, offset, leaf));
					return {
						nodeList: A2($elm$core$List$cons, mappedLeaf, builder.nodeList),
						nodeListSize: builder.nodeListSize + 1,
						tail: builder.tail
					};
				}
			});
		return A2(
			$elm$core$Array$builderToArray,
			true,
			A3($elm$core$Elm$JsArray$foldl, helper, initialBuilder, tree));
	});
var $elm$core$Elm$JsArray$push = _JsArray_push;
var $elm$core$Elm$JsArray$singleton = _JsArray_singleton;
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$insertTailInTree = F4(
	function (shift, index, tail, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		if (_Utils_cmp(
			pos,
			$elm$core$Elm$JsArray$length(tree)) > -1) {
			if (shift === 5) {
				return A2(
					$elm$core$Elm$JsArray$push,
					$elm$core$Array$Leaf(tail),
					tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, $elm$core$Elm$JsArray$empty));
				return A2($elm$core$Elm$JsArray$push, newSub, tree);
			}
		} else {
			var value = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (value.$ === 'SubTree') {
				var subTree = value.a;
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, subTree));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4(
						$elm$core$Array$insertTailInTree,
						shift - $elm$core$Array$shiftStep,
						index,
						tail,
						$elm$core$Elm$JsArray$singleton(value)));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var $elm$core$Array$unsafeReplaceTail = F2(
	function (newTail, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var originalTailLen = $elm$core$Elm$JsArray$length(tail);
		var newTailLen = $elm$core$Elm$JsArray$length(newTail);
		var newArrayLen = len + (newTailLen - originalTailLen);
		if (_Utils_eq(newTailLen, $elm$core$Array$branchFactor)) {
			var overflow = _Utils_cmp(newArrayLen >>> $elm$core$Array$shiftStep, 1 << startShift) > 0;
			if (overflow) {
				var newShift = startShift + $elm$core$Array$shiftStep;
				var newTree = A4(
					$elm$core$Array$insertTailInTree,
					newShift,
					len,
					newTail,
					$elm$core$Elm$JsArray$singleton(
						$elm$core$Array$SubTree(tree)));
				return A4($elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, $elm$core$Elm$JsArray$empty);
			} else {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					newArrayLen,
					startShift,
					A4($elm$core$Array$insertTailInTree, startShift, len, newTail, tree),
					$elm$core$Elm$JsArray$empty);
			}
		} else {
			return A4($elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
		}
	});
var $elm$core$Array$push = F2(
	function (a, array) {
		var tail = array.d;
		return A2(
			$elm$core$Array$unsafeReplaceTail,
			A2($elm$core$Elm$JsArray$push, a, tail),
			array);
	});
var $elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (_v0.$ === 'SubTree') {
			var subTree = _v0.a;
			var newSub = A4($elm$core$Array$setHelp, shift - $elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _v0.a;
			var newLeaf = A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, values);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var $elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, tail)) : A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4($elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var $gampleman$elm_visualization$Histogram$tupleMap = F2(
	function (f, _v0) {
		var a1 = _v0.a;
		var a2 = _v0.b;
		return _Utils_Tuple2(
			f(a1),
			f(a2));
	});
var $gampleman$elm_visualization$Histogram$compute = F2(
	function (list, _v0) {
		var value = _v0.a.value;
		var threshold = _v0.a.threshold;
		var domain = _v0.a.domain;
		if (!list.b) {
			return _List_Nil;
		} else {
			var x = list.a;
			var defaultValue = value(x);
			var computedDomain = A2(
				$elm$core$Maybe$withDefault,
				_Utils_Tuple2(x, x),
				domain(list));
			var _v2 = A2($gampleman$elm_visualization$Histogram$tupleMap, value, computedDomain);
			var x0 = _v2.a;
			var x1 = _v2.b;
			var thresholds = $elm$core$Array$fromList(
				A2(
					$elm$core$List$filter,
					function (t) {
						return (_Utils_cmp(t, x0) > -1) && (_Utils_cmp(t, x1) < 0);
					},
					A3(threshold, value, list, computedDomain)));
			var thresholdsCount = $elm$core$Array$length(thresholds);
			var initBin = F2(
				function (i, thresh) {
					return {
						length: 0,
						values: _List_Nil,
						x0: (i > 0) ? A2(
							$elm$core$Maybe$withDefault,
							defaultValue,
							A2($elm$core$Array$get, i - 1, thresholds)) : x0,
						x1: (_Utils_cmp(i, thresholdsCount) < 0) ? thresh : x1
					};
				});
			var defaultBins = A2(
				$elm$core$Array$push,
				A2(
					initBin,
					thresholdsCount,
					A2(
						$elm$core$Maybe$withDefault,
						defaultValue,
						A2($elm$core$Array$get, thresholdsCount - 1, thresholds))),
				A2($elm$core$Array$indexedMap, initBin, thresholds));
			var fromMaybe = $elm$core$Maybe$withDefault(
				A2(initBin, 0, defaultValue));
			var binify = F2(
				function (item, bins) {
					var threshIndex = A3(
						$gampleman$elm_visualization$Histogram$Array$bisectRight,
						value(item),
						thresholds,
						$elm$core$Maybe$Just(
							_Utils_Tuple2(0, thresholdsCount)));
					var oldBin = fromMaybe(
						A2($elm$core$Array$get, threshIndex, bins));
					var newBin = _Utils_update(
						oldBin,
						{
							length: oldBin.length + 1,
							values: A2($elm$core$List$cons, item, oldBin.values)
						});
					return A3($elm$core$Array$set, threshIndex, newBin, bins);
				});
			return $elm$core$Array$toList(
				A3($elm$core$List$foldl, binify, defaultBins, list));
		}
	});
var $gampleman$elm_visualization$Histogram$H = function (a) {
	return {$: 'H', a: a};
};
var $gampleman$elm_visualization$Statistics$extentBy = F2(
	function (fn, list) {
		var min = F2(
			function (a, b) {
				return (_Utils_cmp(
					fn(a),
					fn(b)) < 0) ? a : b;
			});
		var max = F2(
			function (a, b) {
				return (_Utils_cmp(
					fn(a),
					fn(b)) > 0) ? a : b;
			});
		var helper = F2(
			function (l, _v0) {
				helper:
				while (true) {
					var mini = _v0.a;
					var maxi = _v0.b;
					if (!l.b) {
						return _Utils_Tuple2(mini, maxi);
					} else {
						var x = l.a;
						var xs = l.b;
						var $temp$l = xs,
							$temp$_v0 = _Utils_Tuple2(
							A2(min, mini, x),
							A2(max, maxi, x));
						l = $temp$l;
						_v0 = $temp$_v0;
						continue helper;
					}
				}
			});
		if (!list.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var x = list.a;
			var xs = list.b;
			return $elm$core$Maybe$Just(
				A2(
					helper,
					xs,
					_Utils_Tuple2(x, x)));
		}
	});
var $gampleman$elm_visualization$Histogram$custom = F2(
	function (thresh, _function) {
		return $gampleman$elm_visualization$Histogram$H(
			{
				domain: $gampleman$elm_visualization$Statistics$extentBy(_function),
				threshold: thresh,
				value: _function
			});
	});
var $author$project$GradeHistogram$separations = F4(
	function (nBins, fn, list, domain) {
		return A2(
			$elm$core$List$map,
			function (n) {
				return n / (nBins / 100.0);
			},
			A2(
				$elm$core$List$map,
				$elm$core$Basics$toFloat,
				A2($elm$core$List$range, 1, nBins - 1)));
	});
var $gampleman$elm_visualization$Histogram$limitedBy = F2(
	function (domain, _v0) {
		var cfg = _v0.a;
		return $gampleman$elm_visualization$Histogram$H(
			_Utils_update(
				cfg,
				{domain: domain}));
	});
var $gampleman$elm_visualization$Histogram$withDomain = A2(
	$elm$core$Basics$composeR,
	$elm$core$Maybe$Just,
	A2($elm$core$Basics$composeR, $elm$core$Basics$always, $gampleman$elm_visualization$Histogram$limitedBy));
var $author$project$GradeHistogram$histogram = F2(
	function (model, nBins) {
		return A2(
			$gampleman$elm_visualization$Histogram$compute,
			model,
			A2(
				$gampleman$elm_visualization$Histogram$withDomain,
				_Utils_Tuple2(0, 100),
				A2(
					$gampleman$elm_visualization$Histogram$custom,
					$author$project$GradeHistogram$separations(nBins),
					function (n) {
						return n;
					})));
	});
var $author$project$GradeHistogram$scaleFactor = 1.0;
var $elm_community$typed_svg$TypedSvg$TypesToStrings$transformToString = function (xform) {
	var tr = F2(
		function (name, args) {
			return $elm$core$String$concat(
				_List_fromArray(
					[
						name,
						'(',
						A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, $elm$core$String$fromFloat, args)),
						')'
					]));
		});
	switch (xform.$) {
		case 'Matrix':
			var a = xform.a;
			var b = xform.b;
			var c = xform.c;
			var d = xform.d;
			var e = xform.e;
			var f = xform.f;
			return A2(
				tr,
				'matrix',
				_List_fromArray(
					[a, b, c, d, e, f]));
		case 'Rotate':
			var a = xform.a;
			var x = xform.b;
			var y = xform.c;
			return A2(
				tr,
				'rotate',
				_List_fromArray(
					[a, x, y]));
		case 'Scale':
			var x = xform.a;
			var y = xform.b;
			return A2(
				tr,
				'scale',
				_List_fromArray(
					[x, y]));
		case 'SkewX':
			var x = xform.a;
			return A2(
				tr,
				'skewX',
				_List_fromArray(
					[x]));
		case 'SkewY':
			var y = xform.a;
			return A2(
				tr,
				'skewY',
				_List_fromArray(
					[y]));
		default:
			var x = xform.a;
			var y = xform.b;
			return A2(
				tr,
				'translate',
				_List_fromArray(
					[x, y]));
	}
};
var $elm_community$typed_svg$TypedSvg$Attributes$transform = function (transforms) {
	return A2(
		$elm_community$typed_svg$TypedSvg$Core$attribute,
		'transform',
		A2(
			$elm$core$String$join,
			' ',
			A2($elm$core$List$map, $elm_community$typed_svg$TypedSvg$TypesToStrings$transformToString, transforms)));
};
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $gampleman$elm_visualization$Scale$tickFormat = function (_v0) {
	var opts = _v0.a;
	return opts.tickFormat(opts.domain);
};
var $gampleman$elm_visualization$Scale$ticks = F2(
	function (_v0, count) {
		var scale = _v0.a;
		return A2(scale.ticks, scale.domain, count);
	});
var $gampleman$elm_visualization$Axis$computeOptions = F2(
	function (attrs, scale) {
		var _v0 = A3(
			$elm$core$List$foldl,
			F2(
				function (attr, _v1) {
					var babyOpts = _v1.a;
					var post = _v1.b;
					switch (attr.$) {
						case 'TickCount':
							var val = attr.a;
							return _Utils_Tuple2(
								_Utils_update(
									babyOpts,
									{tickCount: val}),
								post);
						case 'TickSizeInner':
							var val = attr.a;
							return _Utils_Tuple2(
								_Utils_update(
									babyOpts,
									{tickSizeInner: val}),
								post);
						case 'TickSizeOuter':
							var val = attr.a;
							return _Utils_Tuple2(
								_Utils_update(
									babyOpts,
									{tickSizeOuter: val}),
								post);
						case 'TickPadding':
							var val = attr.a;
							return _Utils_Tuple2(
								_Utils_update(
									babyOpts,
									{tickPadding: val}),
								post);
						default:
							return _Utils_Tuple2(
								babyOpts,
								A2($elm$core$List$cons, attr, post));
					}
				}),
			_Utils_Tuple2(
				{tickCount: 10, tickPadding: 3, tickSizeInner: 6, tickSizeOuter: 6},
				_List_Nil),
			attrs);
		var opts = _v0.a;
		var postList = _v0.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (attr, options) {
					switch (attr.$) {
						case 'Ticks':
							var val = attr.a;
							return _Utils_update(
								options,
								{ticks: val});
						case 'TickFormat':
							var val = attr.a;
							return _Utils_update(
								options,
								{tickFormat: val});
						default:
							return options;
					}
				}),
			{
				tickCount: opts.tickCount,
				tickFormat: A2($gampleman$elm_visualization$Scale$tickFormat, scale, opts.tickCount),
				tickPadding: opts.tickPadding,
				tickSizeInner: opts.tickSizeInner,
				tickSizeOuter: opts.tickSizeOuter,
				ticks: A2($gampleman$elm_visualization$Scale$ticks, scale, opts.tickCount)
			},
			postList);
	});
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$Attributes$dy = _VirtualDom_attribute('dy');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$Attributes$fontFamily = _VirtualDom_attribute('font-family');
var $elm$svg$Svg$Attributes$fontSize = _VirtualDom_attribute('font-size');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $elm$svg$Svg$line = $elm$svg$Svg$trustedNode('line');
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $gampleman$elm_visualization$Scale$rangeExtent = function (_v0) {
	var options = _v0.a;
	return A2(options.rangeExtent, options.domain, options.range);
};
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$text = $elm$virtual_dom$VirtualDom$text;
var $elm$svg$Svg$Attributes$textAnchor = _VirtualDom_attribute('text-anchor');
var $elm$svg$Svg$text_ = $elm$svg$Svg$trustedNode('text');
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $gampleman$elm_visualization$Axis$element = F4(
	function (_v0, k, displacement, textAnchorPosition) {
		var x = _v0.x;
		var y = _v0.y;
		var x2 = _v0.x2;
		var y1 = _v0.y1;
		var y2 = _v0.y2;
		var translate = _v0.translate;
		var horizontal = _v0.horizontal;
		return F2(
			function (attrs, scale) {
				var rangeExtent = $gampleman$elm_visualization$Scale$rangeExtent(scale);
				var range1 = rangeExtent.b + 0.5;
				var range0 = rangeExtent.a + 0.5;
				var position = $gampleman$elm_visualization$Scale$convert(scale);
				var opts = A2($gampleman$elm_visualization$Axis$computeOptions, attrs, scale);
				var spacing = A2($elm$core$Basics$max, opts.tickSizeInner, 0) + opts.tickPadding;
				var drawTick = function (tick) {
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$class('tick'),
								$elm$svg$Svg$Attributes$transform(
								translate(
									position(tick)))
							]),
						_List_fromArray(
							[
								A2(
								$elm$svg$Svg$line,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$stroke('#000'),
										x2(k * opts.tickSizeInner),
										y1(0.5),
										y2(0.5)
									]),
								_List_Nil),
								A2(
								$elm$svg$Svg$text_,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$fill('#000'),
										x(k * spacing),
										y(0.5),
										$elm$svg$Svg$Attributes$dy(displacement)
									]),
								_List_fromArray(
									[
										$elm$svg$Svg$text(
										opts.tickFormat(tick))
									]))
							]));
				};
				var domainLine = horizontal ? ('M' + ($elm$core$String$fromFloat(k * opts.tickSizeOuter) + (',' + ($elm$core$String$fromFloat(range0) + ('H0.5V' + ($elm$core$String$fromFloat(range1) + ('H' + $elm$core$String$fromFloat(k * opts.tickSizeOuter)))))))) : ('M' + ($elm$core$String$fromFloat(range0) + (',' + ($elm$core$String$fromFloat(k * opts.tickSizeOuter) + ('V0.5H' + ($elm$core$String$fromFloat(range1) + ('V' + $elm$core$String$fromFloat(k * opts.tickSizeOuter))))))));
				return A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$fill('none'),
							$elm$svg$Svg$Attributes$fontSize('10'),
							$elm$svg$Svg$Attributes$fontFamily('sans-serif'),
							$elm$svg$Svg$Attributes$textAnchor(textAnchorPosition)
						]),
					A2(
						$elm$core$List$cons,
						A2(
							$elm$svg$Svg$path,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$class('domain'),
									$elm$svg$Svg$Attributes$stroke('#000'),
									$elm$svg$Svg$Attributes$d(domainLine)
								]),
							_List_Nil),
						A2($elm$core$List$map, drawTick, opts.ticks)));
			});
	});
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var $elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var $elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var $gampleman$elm_visualization$Axis$verticalAttrs = {
	horizontal: false,
	translate: function (x) {
		return 'translate(' + ($elm$core$String$fromFloat(x) + ', 0)');
	},
	x: A2($elm$core$Basics$composeL, $elm$svg$Svg$Attributes$y, $elm$core$String$fromFloat),
	x1: A2($elm$core$Basics$composeL, $elm$svg$Svg$Attributes$y1, $elm$core$String$fromFloat),
	x2: A2($elm$core$Basics$composeL, $elm$svg$Svg$Attributes$y2, $elm$core$String$fromFloat),
	y: A2($elm$core$Basics$composeL, $elm$svg$Svg$Attributes$x, $elm$core$String$fromFloat),
	y1: A2($elm$core$Basics$composeL, $elm$svg$Svg$Attributes$x1, $elm$core$String$fromFloat),
	y2: A2($elm$core$Basics$composeL, $elm$svg$Svg$Attributes$x2, $elm$core$String$fromFloat)
};
var $gampleman$elm_visualization$Axis$bottom = A4($gampleman$elm_visualization$Axis$element, $gampleman$elm_visualization$Axis$verticalAttrs, 1, '0.71em', 'middle');
var $author$project$GradeHistogram$xAxis = function (model) {
	return A2($gampleman$elm_visualization$Axis$bottom, _List_Nil, $author$project$GradeHistogram$xScale);
};
var $gampleman$elm_visualization$Axis$horizontalAttrs = {
	horizontal: true,
	translate: function (y) {
		return 'translate(0, ' + ($elm$core$String$fromFloat(y) + ')');
	},
	x: A2($elm$core$Basics$composeL, $elm$svg$Svg$Attributes$x, $elm$core$String$fromFloat),
	x1: A2($elm$core$Basics$composeL, $elm$svg$Svg$Attributes$x1, $elm$core$String$fromFloat),
	x2: A2($elm$core$Basics$composeL, $elm$svg$Svg$Attributes$x2, $elm$core$String$fromFloat),
	y: A2($elm$core$Basics$composeL, $elm$svg$Svg$Attributes$y, $elm$core$String$fromFloat),
	y1: A2($elm$core$Basics$composeL, $elm$svg$Svg$Attributes$y1, $elm$core$String$fromFloat),
	y2: A2($elm$core$Basics$composeL, $elm$svg$Svg$Attributes$y2, $elm$core$String$fromFloat)
};
var $gampleman$elm_visualization$Axis$left = A4($gampleman$elm_visualization$Axis$element, $gampleman$elm_visualization$Axis$horizontalAttrs, -1, '0.32em', 'end');
var $gampleman$elm_visualization$Axis$TickCount = function (a) {
	return {$: 'TickCount', a: a};
};
var $gampleman$elm_visualization$Axis$tickCount = $gampleman$elm_visualization$Axis$TickCount;
var $author$project$GradeHistogram$yScaleFromBins = function (bins) {
	return A2(
		$gampleman$elm_visualization$Scale$linear,
		_Utils_Tuple2($author$project$GradeHistogram$h - (2 * $author$project$GradeHistogram$padding), 0),
		A2(
			$elm$core$Tuple$pair,
			0,
			A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$List$maximum(
					A2(
						$elm$core$List$map,
						function ($) {
							return $.length;
						},
						bins)))));
};
var $author$project$GradeHistogram$yAxis = function (bins) {
	return A2(
		$gampleman$elm_visualization$Axis$left,
		_List_fromArray(
			[
				$gampleman$elm_visualization$Axis$tickCount(5)
			]),
		$author$project$GradeHistogram$yScaleFromBins(bins));
};
var $author$project$GradeHistogram$view = F3(
	function (model, boundaries, nBins) {
		var bins = A2($author$project$GradeHistogram$histogram, model, nBins);
		return A2(
			$elm_community$typed_svg$TypedSvg$svg,
			_List_fromArray(
				[
					A4($elm_community$typed_svg$TypedSvg$Attributes$viewBox, 0, 0, $author$project$GradeHistogram$w, $author$project$GradeHistogram$h)
				]),
			_List_fromArray(
				[
					A2(
					$elm_community$typed_svg$TypedSvg$g,
					_List_fromArray(
						[
							$elm_community$typed_svg$TypedSvg$Attributes$transform(
							_List_fromArray(
								[
									A2($elm_community$typed_svg$TypedSvg$Types$Scale, $author$project$GradeHistogram$scaleFactor, $author$project$GradeHistogram$scaleFactor),
									A2($elm_community$typed_svg$TypedSvg$Types$Translate, $author$project$GradeHistogram$padding - 1, $author$project$GradeHistogram$h - $author$project$GradeHistogram$padding)
								]))
						]),
					_List_fromArray(
						[
							$author$project$GradeHistogram$xAxis(model)
						])),
					A2(
					$elm_community$typed_svg$TypedSvg$g,
					_List_fromArray(
						[
							$elm_community$typed_svg$TypedSvg$Attributes$transform(
							_List_fromArray(
								[
									A2($elm_community$typed_svg$TypedSvg$Types$Scale, $author$project$GradeHistogram$scaleFactor, $author$project$GradeHistogram$scaleFactor),
									A2($elm_community$typed_svg$TypedSvg$Types$Translate, $author$project$GradeHistogram$padding - 1, $author$project$GradeHistogram$padding)
								]))
						]),
					_List_fromArray(
						[
							$author$project$GradeHistogram$yAxis(bins)
						])),
					A2(
					$elm_community$typed_svg$TypedSvg$g,
					_List_fromArray(
						[
							$elm_community$typed_svg$TypedSvg$Attributes$transform(
							_List_fromArray(
								[
									A2($elm_community$typed_svg$TypedSvg$Types$Scale, $author$project$GradeHistogram$scaleFactor, $author$project$GradeHistogram$scaleFactor),
									A2($elm_community$typed_svg$TypedSvg$Types$Translate, $author$project$GradeHistogram$padding, $author$project$GradeHistogram$padding)
								])),
							$elm_community$typed_svg$TypedSvg$Attributes$class(
							_List_fromArray(
								['series']))
						]),
					A2(
						$elm$core$List$map,
						$author$project$GradeHistogram$column(
							$author$project$GradeHistogram$yScaleFromBins(bins)),
						bins)),
					A2(
					$elm_community$typed_svg$TypedSvg$g,
					_List_fromArray(
						[
							$elm_community$typed_svg$TypedSvg$Attributes$transform(
							_List_fromArray(
								[
									A2($elm_community$typed_svg$TypedSvg$Types$Scale, $author$project$GradeHistogram$scaleFactor, $author$project$GradeHistogram$scaleFactor),
									A2($elm_community$typed_svg$TypedSvg$Types$Translate, $author$project$GradeHistogram$padding, $author$project$GradeHistogram$padding)
								])),
							$elm_community$typed_svg$TypedSvg$Attributes$class(
							_List_fromArray(
								['series']))
						]),
					A2(
						$elm$core$List$map,
						$author$project$GradeHistogram$boundaryLine,
						A2($elm$core$List$map, $elm$core$Tuple$first, boundaries))),
					A2(
					$elm_community$typed_svg$TypedSvg$g,
					_List_fromArray(
						[
							$elm_community$typed_svg$TypedSvg$Attributes$transform(
							_List_fromArray(
								[
									A2($elm_community$typed_svg$TypedSvg$Types$Scale, $author$project$GradeHistogram$scaleFactor, $author$project$GradeHistogram$scaleFactor),
									A2($elm_community$typed_svg$TypedSvg$Types$Translate, $author$project$GradeHistogram$padding, $author$project$GradeHistogram$padding)
								])),
							$elm_community$typed_svg$TypedSvg$Attributes$class(
							_List_fromArray(
								['series']))
						]),
					A2($elm$core$List$map, $author$project$GradeHistogram$gradeText, boundaries))
				]));
	});
var $author$project$Main$histogramView = F3(
	function (nBins, studentList, grade_boundaries) {
		var grade_counts = A2($author$project$Main$countGradeDist, studentList, grade_boundaries);
		var gradeLabels = A2($elm$core$List$map, $author$project$Student$gradeToStr, grade_boundaries);
		var data = A2($elm$core$List$map, $author$project$Student$getStudentScore, studentList);
		var boundaries = A2($elm$core$List$map, $author$project$Student$getGradeBoundary, grade_boundaries);
		return _Utils_eq(
			$elm$core$List$reverse(boundaries),
			$elm$core$List$sort(boundaries)) ? $elm$core$List$concat(
			_List_fromArray(
				[
					_List_fromArray(
					[
						A2(
						$elm$html$Html$h2,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Step 3: Histogram')
							]))
					]),
					A2(
					$elm$core$List$map,
					$elm$html$Html$text,
					A2(
						$elm$core$List$map,
						function (n) {
							return n.grade + (': ' + ($elm$core$String$fromInt(n.count) + ' '));
						},
						grade_counts)),
					_List_fromArray(
					[
						A3(
						$author$project$GradeHistogram$view,
						A2(
							$elm$core$List$filter,
							function (n) {
								return n >= 0;
							},
							data),
						A3($elm$core$List$map2, $elm$core$Tuple$pair, boundaries, gradeLabels),
						nBins),
						A2($elm$html$Html$br, _List_Nil, _List_Nil),
						$elm$html$Html$text('Number of histogram bins: ')
					]),
					_List_fromArray(
					[
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$placeholder('Bins'),
								$elm$html$Html$Attributes$type_('number'),
								$elm$html$Html$Attributes$step('1'),
								$elm$html$Html$Events$onInput($author$project$Main$HistogramBinChange),
								$elm$html$Html$Attributes$value(
								$elm$core$String$fromInt(nBins))
							]),
						_List_Nil)
					]),
					_List_fromArray(
					[
						A2($elm$html$Html$br, _List_Nil, _List_Nil)
					]),
					_List_fromArray(
					[
						A2($elm$html$Html$hr, _List_Nil, _List_Nil)
					]),
					_List_fromArray(
					[
						A2(
						$elm$html$Html$h3,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Grade distribution')
							]))
					]),
					_List_fromArray(
					[
						$author$project$GradeBarChart$view(
						A2(
							$elm$core$List$map,
							function (n) {
								return _Utils_Tuple2(n.grade, n.count);
							},
							grade_counts))
					])
				])) : _List_fromArray(
			[
				A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'color', 'red')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Grade ordering incorrect!')
					]))
			]);
	});
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('container')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h1,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Grading Helper')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('col-sm border')
							]),
						$author$project$Main$dataEntryView(model))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('col-sm-3 border')
							]),
						$author$project$Main$gradeBoundaryView(model)),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('col-sm border')
							]),
						A3($author$project$Main$histogramView, model.histogramBins, model.students, model.grades))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('row')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('col-sm border')
							]),
						$author$project$Main$gradedRowsView(model))
					]))
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{init: $author$project$Main$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));