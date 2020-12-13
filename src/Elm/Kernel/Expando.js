/*

import Debugger.Expando as Expando exposing (S, Primitive, Sequence, Dictionary, Record, Constructor, ListSeq, SetSeq, ArraySeq)
import Elm.Kernel.List exposing (Cons, Nil)
import Elm.Kernel.Utils exposing (Tuple2)
import List exposing (map, reverse)
import Maybe exposing (Just, Nothing)
import Set exposing (foldr)
import Dict exposing (foldr, empty, insert)
import Array exposing (foldr)

*/



function _Expando_init(value)
{
	if (typeof value === 'boolean')
	{
		return A3(__Expando_Constructor, __Maybe_Just(value ? 'True' : 'False'), true, __List_Nil);
	}

	if (typeof value === 'number')
	{
		return __Expando_Primitive(value + '');
	}

	if (typeof value === 'string')
	{
		return __Expando_S('"' + _Expando_addSlashes(value, false) + '"');
	}

	if (value instanceof String)
	{
		return __Expando_S("'" + _Expando_addSlashes(value, true) + "'");
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (tag === '::' || tag === '[]')
		{
			return A3(__Expando_Sequence, __Expando_ListSeq, true,
				A2(__List_map, _Expando_init, value)
			);
		}

		if (tag === 'Set_elm_builtin')
		{
			return A3(__Expando_Sequence, __Expando_SetSeq, true,
				A3(__Set_foldr, _Expando_initCons, __List_Nil, value)
			);
		}

		if (tag === 'RBNode_elm_builtin' || tag == 'RBEmpty_elm_builtin')
		{
			return A2(__Expando_Dictionary, true,
				A3(__Dict_foldr, _Expando_initKeyValueCons, __List_Nil, value)
			);
		}

		if (tag === 'Array_elm_builtin')
		{
			return A3(__Expando_Sequence, __Expando_ArraySeq, true,
				A3(__Array_foldr, _Expando_initCons, __List_Nil, value)
			);
		}

		if (typeof tag === 'number')
		{
			return __Expando_Primitive('<internals>');
		}

		var char = tag.charCodeAt(0);
		if (char === 35 || 65 <= char && char <= 90)
		{
			var list = __List_Nil;
			for (var i in value)
			{
				if (i === '$') continue;
				list = __List_Cons(_Expando_init(value[i]), list);
			}
			return A3(__Expando_Constructor, char === 35 ? __Maybe_Nothing : __Maybe_Just(tag), true, __List_reverse(list));
		}

		return __Expando_Primitive('<internals>');
	}

	if (typeof value === 'object')
	{
		var dict = __Dict_empty;
		for (var i in value)
		{
			dict = A3(__Dict_insert, i, _Expando_init(value[i]), dict);
		}
		return A2(__Expando_Record, true, dict);
	}

	return __Expando_Primitive('<internals>');
}

var _Expando_initCons = F2(function initConsHelp(value, list)
{
	return __List_Cons(_Expando_init(value), list);
});

var _Expando_initKeyValueCons = F3(function(key, value, list)
{
	return __List_Cons(
		__Utils_Tuple2(_Expando_init(key), _Expando_init(value)),
		list
	);
});

function _Expando_addSlashes(str, isChar)
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
	}}
