/*

import Elm.Kernel.Utils exposing (Tuple0)
import Elm.Kernel.List exposing (Cons, Nil)
import Elm.Kernel.JsArray exposing (initializeFromList)
import Elm.Kernel.Json exposing (wrap)
import Tuple exposing (pair, first)
import List exposing (reverse, length)
import Platform.Sub as Sub exposing (none)
import Platform.Cmd as Cmd exposing (none)

import Reader exposing (ModeBrowse, ModeDebug, update, view, parseConfig, parseSourceMap, parseFrame)

*/

var _Reader_impl = function (debugData) {
  var programData = Object.assign({}, debugData, {traces: _Reader_contextJSON()});
  console.info("Program data:", programData);
  _Reader_initSourceMaps(debugData.source_map);
  return {
    init: function () {
      return A2(__Tuple_pair,
        A3(__Reader_parseConfig, __Reader_ModeBrowse, _Reader_sourceMapsElm, __Json_wrap(programData)),
        __Cmd_none);
    },
    update: __Reader_update,
    view: __Reader_view,
    subscriptions: function () { return __Sub_none; }
  };
};


var _Reader_thunkExec = F3(function (func, thunk, childFrameId) {
  _Reader_contextDepth = 0;
  // this wrapper context "emulates" the parent frame from which `func` was called
  var wrapperContext = {
    $: __1_NON_INSTRUMENTED_FRAME,
    __childFrames: [],
    __runtimeId: childFrameId.b, // take the cdr of childFrameId to get its parent
  };
  _Reader_context = wrapperContext;

  // set the UID of the thunk's frame, whether it is instrumented or not.
  _Reader_nextFrameId.specify(childFrameId.a);
  // TODO: is this `.a` reliable? Replace with Elm function call?
  // Same for `.b` above.

  // if the thunk's frame isn't instrumented, set up the non-instrumented-frame context to
  // capture traces within it
  if (!func.elmReaderInstrumented) {
    _Reader_context = {
      $: __1_NON_INSTRUMENTED_FRAME,
      __childFrames: [],
      __runtimeId: _Reader_nextFrameId(_Reader_context.__runtimeId),
    };
  }
  thunk(__Utils_Tuple0); // intentionally ignore the result, which was already recorded
  if (!func.elmReaderInstrumented) {
    wrapperContext.__childFrames = [{
      $: __2_NON_INSTRUMENTED,
      __childFrames: _Reader_context.__childFrames,
      __runtimeId: _Reader_context.__runtimeId,
    }];
    _Reader_context = wrapperContext;
  }

  // get the frame information we want from _Reader_context
  var contextPrepared = _Reader_contextJSON();
  if (contextPrepared.length !== 1) {
    throw new Error("_Reader_thunkExec: _Reader_context got: "
      + contextPrepared.length
      + " frames");
  }
  return __Reader_parseFrame(__Json_wrap(contextPrepared[0]));
});

// Reader.updateExec (see Reader.elm for type signature)
var _Reader_updateExec = F3(function (updateFn, msg, model) {
  _Reader_nextFrameId.reset();
  _Reader_contextDepth = 0;
  _Reader_context = {
    $: __1_NON_INSTRUMENTED_FRAME,
    __childFrames: [],
    __runtimeId: _Reader_nextFrameId(),
  };
  var result = A2(updateFn, msg, model);
  console.log("Result of updateFn(", msg, ", ", model, "):\n    ", result);
  var newUserModel = __Tuple_first(result);
  if (!_Reader_sourceMapsElm) {
    console.error("_Reader_sourceMapsElm uninitialized.");
  }
  var debugMetadata = {
    traces: _Reader_contextJSON(),
  };
  //FIXME console.log("debugMetadata: ", debugMetadata);
  var readerModel = A3(__Reader_parseConfig, __Reader_ModeDebug, _Reader_sourceMapsElm, __Json_wrap(debugMetadata));
  return A2(__Tuple_pair, newUserModel, readerModel);
});

var _Reader_sourceMaps = null;
var _Reader_sourceMapsElm = null;
function _Reader_initSourceMaps(srcMaps) {
  console.log("initializing source maps to: ", srcMaps);
  _Reader_sourceMaps = srcMaps;
  var maybeSrcMaps = __Reader_parseSourceMap(__Json_wrap(_Reader_sourceMaps));
  if (maybeSrcMaps.$ !== 'Ok') {
    console.error("failed to parse elm source maps! -- ", maybeSrcMaps);
  } else {
    _Reader_sourceMapsElm = maybeSrcMaps.a;
  }
}

/*
Types:
type context
  = __1_NON_INSTRUMENTED_FRAME (__childFrames: Array of frames, runtimeId)
  | __1_INSTRUMENTED_FRAME (__exprs: object representing map from expr IDs to exprs, runtimeId)
  | __1_CALL (__childFrame: frame resulting from call, runtimeId)
  | __1_NO_RECORD (__thunk: function returning the frame, __id: the runtime id of the child frame)

type frame
  = __2_NON_INSTRUMENTED (__childFrames: Array of frames, __runtimeId: runtime ID of frame)
  | __2_INSTRUMENTED (__id: ID of source map frame, __exprs, __runtimeId)
  | __2_THUNK (__childId: id of the frame, __thunk: function returning the frame's trace)

type expr = { __val: value of expression, __childFrame: frame that returned it }
*/

var _Reader_nextFrameId = (function () {
  var nextUid = 0;
  var nextIdForced = -1;

  // Construct a linked list as each stack frame's ID,
  // with the first element being that frame's unique
  // integer ID.
  var nextFrameId = function (parent) {
    var uid;
    if (nextIdForced !== -1) {
      uid = nextIdForced;
      nextIdForced = -1;
    } else {
      uid = nextUid;
      nextUid += 1
    }
    return __List_Cons(uid, parent || __List_Nil);
  };
  nextFrameId.reset = function () {
    nextUid = 0;
  };
  nextFrameId.specify = function (idnum) {
    nextIdForced = idnum;
  };
  return nextFrameId;
}());

var _Reader_context = {
  $: __1_NON_INSTRUMENTED_FRAME,
  __childFrames: [],
  __runtimeId: _Reader_nextFrameId(),
};

var _Reader_contextDepth = 0;

var _Reader_recordExpr = F2(function (exprId, val) {
  if (_Reader_context.$ === __1_NO_RECORD) {
    return val;
  }
  if (_Reader_context.$ === __1_INSTRUMENTED_FRAME) {
    _Reader_context.__exprs[exprId] = {
      __val: val,
      __childFrame: null,
    };
    return val;
  }
  console.warn(
    'Elm Reader: Someone tried to record an expression from a non-instrumented context.',
    'expression id: ', exprId,
    'value: ', val
  );
  return val;
});

var _Reader_log = F2(function (msg, val) {
  // Differs from built-in log because it puts the actual JS value to the console,
  // enabling in-console inspection.
  console.log(msg, val);
  return val;
});

var _Reader_recordCall = F3(function (exprId, func, body) {
  if (!window.recordCallNumInvocations) {
    window.recordCallNumInvocations = 1;
  } else {
    window.recordCallNumInvocations++;
  }
  if (_Reader_context.$ == __1_NO_RECORD) {
    return body(__Utils_Tuple0);
  }
  if (_Reader_context.$ !== __1_INSTRUMENTED_FRAME) {
    var result = body(__Utils_Tuple0);
    console.warn(
      'Elm Reader: Someone tried to record a function call from a non-instrumented context.',
      'expression id: ', exprId,
      'value: ', result
    );
    return result;
  }

  if (!func.elmReaderInstrumented) {
    var newContext;
    if (_Reader_contextDepth > 0) {
      var childFrameId = _Reader_nextFrameId(_Reader_context.__runtimeId);
      newContext = {
        $: __1_NO_RECORD,
        __id: childFrameId,
        __thunk: function () {
          return A3(_Reader_thunkExec, func, body, childFrameId);
        },
      };
    }
    else {
      newContext = {
        $: __1_NON_INSTRUMENTED_FRAME,
        __childFrames: [],
        __runtimeId: _Reader_nextFrameId(_Reader_context.__runtimeId),
      };
    }

    var oldContext = _Reader_context;
    _Reader_context = newContext;
    _Reader_contextDepth += 1;
    var result = body(__Utils_Tuple0);
    _Reader_context = oldContext;
    _Reader_contextDepth -= 1;

    var childFrame;
    if (newContext.$ === __1_NO_RECORD) {
      childFrame = {
        $: __2_THUNK,
        __childId: newContext.__id,
        __thunk: newContext.__thunk,
      };
    } else {
      childFrame = {
        $: __2_NON_INSTRUMENTED,
        __childFrames: newContext.__childFrames,
        __runtimeId: newContext.__runtimeId,
      };
    }

    _Reader_context.__exprs[exprId] = {
      __val: result,
      __childFrame: childFrame,
    };

    return result;
  }

  var newContext;
  if (_Reader_contextDepth > 0) {
    var childFrameId = _Reader_nextFrameId(_Reader_context.__runtimeId);
    newContext = {
      $: __1_NO_RECORD,
      __id: childFrameId,
      __thunk: function () {
        return A3(_Reader_thunkExec, func, body, childFrameId);
      },
    };
  } else {
    newContext = {
      $: __1_CALL,
      __childFrame: null,
      __runtimeId: _Reader_nextFrameId(_Reader_context.__runtimeId),
    };
  }

  var oldContext = _Reader_context;
  _Reader_context = newContext;
  _Reader_contextDepth++;
  var result = body(__Utils_Tuple0);
  _Reader_context = oldContext;
  _Reader_contextDepth--;

  var childFrame = newContext.__childFrame;
  if (newContext.$ === __1_NO_RECORD) {
    childFrame = {
      $: __2_THUNK,
      __childId: newContext.__id,
      __thunk: newContext.__thunk,
    };
  }
  _Reader_context.__exprs[exprId] = {
    __val: result,
    __childFrame: childFrame,
  };

  return result;
});

var _Reader_recordFrame = F2(function (frameIdRaw, body) {
  if (_Reader_context.$ === __1_NO_RECORD) {
    return body(__Utils_Tuple0);
  }

  var newContext = {
    $: __1_INSTRUMENTED_FRAME,
    __exprs: {},
    __runtimeId:
      (_Reader_context.$ === __1_CALL
        ? _Reader_context.__runtimeId
        : _Reader_nextFrameId(_Reader_context.__runtimeId)),
  };

  var oldContext = _Reader_context;
  _Reader_context = newContext;
  var result = body(__Utils_Tuple0);
  _Reader_context = oldContext;

  var newFrame = {
    $: __2_INSTRUMENTED,
    __id: JSON.parse(frameIdRaw),
    __exprs: newContext.__exprs,
    __runtimeId: newContext.__runtimeId,
  };

  if (_Reader_context.$ === __1_CALL) {
    _Reader_context.__childFrame = newFrame;
  }
  else if (_Reader_context.$ === __1_NON_INSTRUMENTED_FRAME) {
    _Reader_context.__childFrames.push(newFrame);
  }

  return result;
});

var _Reader_markInstrumented = function (func) {
  func.elmReaderInstrumented = true;
  return func;
};

var _Reader_seq = F2(function (sideEffect, val) {
  return val;
});

var _Reader_contextJSON = function () {
  return _Reader_frameToJSON(_Reader_context).child_frames;
};

var _Reader_runtimeIdToJson = function (runtimeId) {
  var uid = runtimeId.a;
  var idPathList = __List_reverse(runtimeId);
  var ids = __Tuple_first(
    A2(__JsArray_initializeFromList, __List_length(idPathList) - 1, idPathList)
  );
  return {
    uid: uid,
    id_path: ids,
  };
};

function _Reader_frameToJSON(frame) {
  if (frame.$ === __2_INSTRUMENTED) {
    var readableExprs = [];
    Object.keys(frame.__exprs).forEach(function (id) {
      var expr = frame.__exprs[id];
      var val =
        (typeof expr.__val === "function"
          ? { "#<function>": {} }
          : expr.__val);
      readableExprs.push({
        id: +id,
        expr: {
          val: val,
          child_frame: expr.__childFrame && _Reader_frameToJSON(expr.__childFrame),
        }
      });
    });
    return {
      tag: 'Instrumented',
      is_thunk: false,
      source_map_id: frame.__id,
      runtime_id: _Reader_runtimeIdToJson(frame.__runtimeId),
      exprs: readableExprs
    };
  }
  if (frame.$ === __2_NON_INSTRUMENTED) {
    console.log("Serializing frame -- ", frame, " -- with runtime ID: ", frame.__runtimeId);
    return {
      tag: 'NonInstrumented',
      is_thunk: false,
      child_frames: frame.__childFrames.map(_Reader_frameToJSON),
      runtime_id: _Reader_runtimeIdToJson(frame.__runtimeId),
    };
  }
  // otherwise, frame.$ is __2_THUNK
  return {
    tag: 'Thunk',
    is_thunk: true,
    thunk: { func: frame.__thunk },
    runtime_id: _Reader_runtimeIdToJson(frame.__childId),
  };
};

// Tools for debugging the debugger from the JavaScript console

// Need to export as global to be available from the console
window.elmReaderRoot = function () {
  return _Reader_frameToJSON(_Reader_context);
};
