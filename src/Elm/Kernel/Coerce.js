/*

import Tuple exposing (first, second)
import Reader.TraceData as TraceData exposing (Thunk)

*/


function _Coerce_decodeFrameThunk(valAndId) {
  var val = __Tuple_first(valAndId);
  var id = __Tuple_second(valAndId);
  return A2(__TraceData_Thunk, id, val.a.thunk.func);
}

