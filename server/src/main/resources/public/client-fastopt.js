'use strict';
var $fileLevelThis = this;
var $getOwnPropertyDescriptors = (Object.getOwnPropertyDescriptors || (() => {
  var ownKeysFun;
  if ((((typeof Reflect) !== "undefined") && Reflect.ownKeys)) {
    ownKeysFun = Reflect.ownKeys;
  } else {
    var getOwnPropertySymbols = (Object.getOwnPropertySymbols || ((o) => []));
    ownKeysFun = ((o) => Object.getOwnPropertyNames(o).concat(getOwnPropertySymbols(o)));
  }
  return ((o) => {
    var ownKeys = ownKeysFun(o);
    var descriptors = ({});
    var len = (ownKeys.length | 0);
    var i = 0;
    while ((i !== len)) {
      var key = ownKeys[i];
      Object.defineProperty(descriptors, key, ({
        "configurable": true,
        "enumerable": true,
        "writable": true,
        "value": Object.getOwnPropertyDescriptor(o, key)
      }));
      i = ((i + 1) | 0);
    }
    return descriptors;
  });
})());
var $L0;
function $Char(c) {
  this.c = c;
}
$Char.prototype.toString = (function() {
  return String.fromCharCode(this.c);
});
function $valueDescription(arg0) {
  return (((typeof arg0) === "number") ? (((arg0 === 0) && ((1 / arg0) < 0)) ? "number(-0)" : (("number(" + arg0) + ")")) : ((arg0 instanceof $c_RTLong) ? "long" : ((arg0 instanceof $Char) ? "char" : ((!(!(arg0 && arg0.$classData))) ? arg0.$classData.name : (typeof arg0)))));
}
function $throwClassCastException(arg0, arg1) {
  throw new $c_Lorg_scalajs_linker_runtime_UndefinedBehaviorError(new $c_jl_ClassCastException((($valueDescription(arg0) + " cannot be cast to ") + arg1)));
}
function $throwArrayCastException(arg0, arg1, arg2) {
  while ((--arg2)) {
    arg1 = ("[" + arg1);
  }
  $throwClassCastException(arg0, arg1);
}
function $throwArrayIndexOutOFBoundsException(arg0) {
  throw new $c_Lorg_scalajs_linker_runtime_UndefinedBehaviorError(new $c_jl_ArrayIndexOutOfBoundsException(((arg0 === null) ? null : ("" + arg0))));
}
function $throwArrayStoreException(arg0) {
  throw new $c_Lorg_scalajs_linker_runtime_UndefinedBehaviorError(new $c_jl_ArrayStoreException(((arg0 === null) ? null : $valueDescription(arg0))));
}
function $throwNegativeArraySizeException() {
  throw new $c_Lorg_scalajs_linker_runtime_UndefinedBehaviorError(new $c_jl_NegativeArraySizeException());
}
function $throwNullPointerException() {
  throw new $c_Lorg_scalajs_linker_runtime_UndefinedBehaviorError(new $c_jl_NullPointerException());
}
function $n(arg0) {
  if ((arg0 === null)) {
    $throwNullPointerException();
  }
  return arg0;
}
function $noIsInstance(arg0) {
  throw new TypeError("Cannot call isInstance() on a Class representing a JS trait/object");
}
function $objectClone(arg0) {
  return Object.create(Object.getPrototypeOf(arg0), $getOwnPropertyDescriptors(arg0));
}
function $objectOrArrayClone(arg0) {
  return (arg0.$classData.isArrayClass ? arg0.clone__O() : $objectClone(arg0));
}
function $objectGetClass(arg0) {
  switch ((typeof arg0)) {
    case "string": {
      return $d_T.getClassOf();
    }
    case "number": {
      if ($isInt(arg0)) {
        if ((((arg0 << 24) >> 24) === arg0)) {
          return $d_jl_Byte.getClassOf();
        } else if ((((arg0 << 16) >> 16) === arg0)) {
          return $d_jl_Short.getClassOf();
        } else {
          return $d_jl_Integer.getClassOf();
        }
      } else if ($isFloat(arg0)) {
        return $d_jl_Float.getClassOf();
      } else {
        return $d_jl_Double.getClassOf();
      }
    }
    case "boolean": {
      return $d_jl_Boolean.getClassOf();
    }
    case "undefined": {
      return $d_jl_Void.getClassOf();
    }
    default: {
      if ((arg0 instanceof $c_RTLong)) {
        return $d_jl_Long.getClassOf();
      } else if ((arg0 instanceof $Char)) {
        return $d_jl_Character.getClassOf();
      } else if ((!(!(arg0 && arg0.$classData)))) {
        return arg0.$classData.getClassOf();
      } else {
        return null;
      }
    }
  }
}
function $objectClassName(arg0) {
  switch ((typeof arg0)) {
    case "string": {
      return "java.lang.String";
    }
    case "number": {
      if ($isInt(arg0)) {
        if ((((arg0 << 24) >> 24) === arg0)) {
          return "java.lang.Byte";
        } else if ((((arg0 << 16) >> 16) === arg0)) {
          return "java.lang.Short";
        } else {
          return "java.lang.Integer";
        }
      } else if ($isFloat(arg0)) {
        return "java.lang.Float";
      } else {
        return "java.lang.Double";
      }
    }
    case "boolean": {
      return "java.lang.Boolean";
    }
    case "undefined": {
      return "java.lang.Void";
    }
    default: {
      if ((arg0 instanceof $c_RTLong)) {
        return "java.lang.Long";
      } else if ((arg0 instanceof $Char)) {
        return "java.lang.Character";
      } else if ((!(!(arg0 && arg0.$classData)))) {
        return arg0.$classData.name;
      } else {
        return $throwNullPointerException();
      }
    }
  }
}
function $dp_equals__O__Z(instance, x0) {
  switch ((typeof instance)) {
    case "string": {
      return $f_T__equals__O__Z(instance, x0);
    }
    case "number": {
      return $f_jl_Double__equals__O__Z(instance, x0);
    }
    case "boolean": {
      return $f_jl_Boolean__equals__O__Z(instance, x0);
    }
    case "undefined": {
      return $f_jl_Void__equals__O__Z(instance, x0);
    }
    default: {
      if (((!(!(instance && instance.$classData))) || (instance === null))) {
        return instance.equals__O__Z(x0);
      } else if ((instance instanceof $c_RTLong)) {
        return $f_jl_Long__equals__O__Z(instance, x0);
      } else if ((instance instanceof $Char)) {
        return $f_jl_Character__equals__O__Z($uC(instance), x0);
      } else {
        return $c_O.prototype.equals__O__Z.call(instance, x0);
      }
    }
  }
}
function $dp_hashCode__I(instance) {
  switch ((typeof instance)) {
    case "string": {
      return $f_T__hashCode__I(instance);
    }
    case "number": {
      return $f_jl_Double__hashCode__I(instance);
    }
    case "boolean": {
      return $f_jl_Boolean__hashCode__I(instance);
    }
    case "undefined": {
      return $f_jl_Void__hashCode__I(instance);
    }
    default: {
      if (((!(!(instance && instance.$classData))) || (instance === null))) {
        return instance.hashCode__I();
      } else if ((instance instanceof $c_RTLong)) {
        return $f_jl_Long__hashCode__I(instance);
      } else if ((instance instanceof $Char)) {
        return $f_jl_Character__hashCode__I($uC(instance));
      } else {
        return $c_O.prototype.hashCode__I.call(instance);
      }
    }
  }
}
function $dp_toString__T(instance) {
  return ((instance === (void 0)) ? "undefined" : instance.toString());
}
function $intDiv(arg0, arg1) {
  if ((arg1 === 0)) {
    throw new $c_jl_ArithmeticException("/ by zero");
  } else {
    return ((arg0 / arg1) | 0);
  }
}
function $intMod(arg0, arg1) {
  if ((arg1 === 0)) {
    throw new $c_jl_ArithmeticException("/ by zero");
  } else {
    return ((arg0 % arg1) | 0);
  }
}
function $doubleToInt(arg0) {
  return ((arg0 > 2147483647) ? 2147483647 : ((arg0 < (-2147483648)) ? (-2147483648) : (arg0 | 0)));
}
function $cToS(arg0) {
  return String.fromCharCode(arg0);
}
function $charAt(arg0, arg1) {
  var r = arg0.charCodeAt(arg1);
  if ((r !== r)) {
    throw new $c_Lorg_scalajs_linker_runtime_UndefinedBehaviorError($ct_jl_StringIndexOutOfBoundsException__I__(new $c_jl_StringIndexOutOfBoundsException(), arg1));
  } else {
    return r;
  }
}
function $resolveSuperRef(arg0, arg1) {
  var getPrototypeOf = Object.getPrototyeOf;
  var getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
  var superProto = arg0.prototype;
  while ((superProto !== null)) {
    var desc = getOwnPropertyDescriptor(superProto, arg1);
    if ((desc !== (void 0))) {
      return desc;
    }
    superProto = getPrototypeOf(superProto);
  }
}
function $superGet(arg0, arg1, arg2) {
  var desc = $resolveSuperRef(arg0, arg2);
  if ((desc !== (void 0))) {
    var getter = desc.get;
    return ((getter !== (void 0)) ? getter.call(arg1) : getter.value);
  }
}
function $superSet(arg0, arg1, arg2, arg3) {
  var desc = $resolveSuperRef(arg0, arg2);
  if ((desc !== (void 0))) {
    var setter = desc.set;
    if ((setter !== (void 0))) {
      setter.call(arg1, arg3);
      return (void 0);
    }
  }
  throw new TypeError((("super has no setter '" + arg2) + "'."));
}
function $arraycopyCheckBounds(arg0, arg1, arg2, arg3, arg4) {
  if ((((((arg1 < 0) || (arg3 < 0)) || (arg4 < 0)) || (arg1 > ((arg0 - arg4) | 0))) || (arg3 > ((arg2 - arg4) | 0)))) {
    $throwArrayIndexOutOFBoundsException(null);
  }
}
function $arraycopyGeneric(arg0, arg1, arg2, arg3, arg4) {
  $arraycopyCheckBounds(arg0.length, arg1, arg2.length, arg3, arg4);
  if ((((arg0 !== arg2) || (arg3 < arg1)) || (((arg1 + arg4) | 0) < arg3))) {
    for (var i = 0; (i < arg4); i = ((i + 1) | 0)) {
      arg2[((arg3 + i) | 0)] = arg0[((arg1 + i) | 0)];
    }
  } else {
    for (var i = ((arg4 - 1) | 0); (i >= 0); i = ((i - 1) | 0)) {
      arg2[((arg3 + i) | 0)] = arg0[((arg1 + i) | 0)];
    }
  }
}
function $systemArraycopy(arg0, arg1, arg2, arg3, arg4) {
  arg0.copyTo(arg1, arg2, arg3, arg4);
}
function $systemArraycopyRefs(arg0, arg1, arg2, arg3, arg4) {
  if (arg2.$classData.isAssignableFrom(arg0.$classData)) {
    $arraycopyGeneric(arg0.u, arg1, arg2.u, arg3, arg4);
  } else {
    var srcArray = arg0.u;
    $arraycopyCheckBounds(srcArray.length, arg1, arg2.u.length, arg3, arg4);
    for (var i = 0; (i < arg4); i = ((i + 1) | 0)) {
      arg2.set(((arg3 + i) | 0), srcArray[((arg1 + i) | 0)]);
    }
  }
}
function $systemArraycopyFull(arg0, arg1, arg2, arg3, arg4) {
  var srcData = (arg0 && arg0.$classData);
  if ((srcData === (arg2 && arg2.$classData))) {
    if ((srcData && srcData.isArrayClass)) {
      $systemArraycopy(arg0, arg1, arg2, arg3, arg4);
    } else {
      $throwArrayStoreException(null);
    }
  } else if (((arg0 instanceof $ac_O) && (arg2 instanceof $ac_O))) {
    $systemArraycopyRefs(arg0, arg1, arg2, arg3, arg4);
  } else {
    $throwArrayStoreException(null);
  }
}
var $lastIDHash = 0;
var $idHashCodeMap = new WeakMap();
function $systemIdentityHashCode(obj) {
  switch ((typeof obj)) {
    case "string": {
      return $f_T__hashCode__I(obj);
    }
    case "number": {
      return $f_jl_Double__hashCode__I(obj);
    }
    case "bigint": {
      var biHash = 0;
      if ((obj < BigInt(0))) {
        obj = (~obj);
      }
      while ((obj !== BigInt(0))) {
        biHash = (biHash ^ Number(BigInt.asIntN(32, obj)));
        obj = (obj >> BigInt(32));
      }
      return biHash;
    }
    case "boolean": {
      return (obj ? 1231 : 1237);
    }
    case "undefined": {
      return 0;
    }
    case "symbol": {
      var description = obj.description;
      return ((description === (void 0)) ? 0 : $f_T__hashCode__I(description));
    }
    default: {
      if ((obj === null)) {
        return 0;
      } else {
        var hash = $idHashCodeMap.get(obj);
        if ((hash === (void 0))) {
          hash = (($lastIDHash + 1) | 0);
          $lastIDHash = hash;
          $idHashCodeMap.set(obj, hash);
        }
        return hash;
      }
    }
  }
}
function $isByte(arg0) {
  return ((((typeof arg0) === "number") && (((arg0 << 24) >> 24) === arg0)) && ((1 / arg0) !== (1 / (-0))));
}
function $isShort(arg0) {
  return ((((typeof arg0) === "number") && (((arg0 << 16) >> 16) === arg0)) && ((1 / arg0) !== (1 / (-0))));
}
function $isInt(arg0) {
  return ((((typeof arg0) === "number") && ((arg0 | 0) === arg0)) && ((1 / arg0) !== (1 / (-0))));
}
function $isFloat(arg0) {
  return (((typeof arg0) === "number") && ((arg0 !== arg0) || (Math.fround(arg0) === arg0)));
}
function $bC(arg0) {
  return new $Char(arg0);
}
var $bC0 = $bC(0);
function $uV(arg0) {
  return (((arg0 === (void 0)) || (arg0 === null)) ? (void 0) : $throwClassCastException(arg0, "java.lang.Void"));
}
function $uZ(arg0) {
  return ((((typeof arg0) === "boolean") || (arg0 === null)) ? (!(!arg0)) : $throwClassCastException(arg0, "java.lang.Boolean"));
}
function $uC(arg0) {
  return (((arg0 instanceof $Char) || (arg0 === null)) ? ((arg0 === null) ? 0 : arg0.c) : $throwClassCastException(arg0, "java.lang.Character"));
}
function $uB(arg0) {
  return (($isByte(arg0) || (arg0 === null)) ? (arg0 | 0) : $throwClassCastException(arg0, "java.lang.Byte"));
}
function $uS(arg0) {
  return (($isShort(arg0) || (arg0 === null)) ? (arg0 | 0) : $throwClassCastException(arg0, "java.lang.Short"));
}
function $uI(arg0) {
  return (($isInt(arg0) || (arg0 === null)) ? (arg0 | 0) : $throwClassCastException(arg0, "java.lang.Integer"));
}
function $uJ(arg0) {
  return (((arg0 instanceof $c_RTLong) || (arg0 === null)) ? ((arg0 === null) ? $L0 : arg0) : $throwClassCastException(arg0, "java.lang.Long"));
}
function $uF(arg0) {
  return (($isFloat(arg0) || (arg0 === null)) ? (+arg0) : $throwClassCastException(arg0, "java.lang.Float"));
}
function $uD(arg0) {
  return ((((typeof arg0) === "number") || (arg0 === null)) ? (+arg0) : $throwClassCastException(arg0, "java.lang.Double"));
}
function $uT(arg0) {
  return ((((typeof arg0) === "string") || (arg0 === null)) ? ((arg0 === null) ? "" : arg0) : $throwClassCastException(arg0, "java.lang.String"));
}
/** @constructor */
function $c_O() {
}
$c_O.prototype.constructor = $c_O;
/** @constructor */
function $h_O() {
}
$h_O.prototype = $c_O.prototype;
$c_O.prototype.hashCode__I = (function() {
  return $systemIdentityHashCode(this);
});
$c_O.prototype.equals__O__Z = (function(that) {
  return (this === that);
});
$c_O.prototype.toString__T = (function() {
  var i = this.hashCode__I();
  return (($objectClassName(this) + "@") + $as_T($uD((i >>> 0.0)).toString(16)));
});
$c_O.prototype.toString = (function() {
  return this.toString__T();
});
function $ac_O(arg) {
  if (((typeof arg) === "number")) {
    if ((arg < 0)) {
      $throwNegativeArraySizeException();
    }
    this.u = new Array(arg);
    for (var i = 0; (i < arg); (i++)) {
      this.u[i] = null;
    }
  } else {
    this.u = arg;
  }
}
$ac_O.prototype = new $h_O();
$ac_O.prototype.constructor = $ac_O;
$ac_O.prototype.get = (function(i) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  return this.u[i];
});
$ac_O.prototype.set = (function(i, v) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  this.u[i] = v;
});
$ac_O.prototype.copyTo = (function(srcPos, dest, destPos, length) {
  $arraycopyGeneric(this.u, srcPos, dest.u, destPos, length);
});
$ac_O.prototype.clone__O = (function() {
  return new $ac_O(this.u.slice());
});
function $ah_O() {
}
$ah_O.prototype = $ac_O.prototype;
function $ac_Z(arg) {
  if (((typeof arg) === "number")) {
    if ((arg < 0)) {
      $throwNegativeArraySizeException();
    }
    this.u = new Array(arg);
    for (var i = 0; (i < arg); (i++)) {
      this.u[i] = false;
    }
  } else {
    this.u = arg;
  }
}
$ac_Z.prototype = new $h_O();
$ac_Z.prototype.constructor = $ac_Z;
$ac_Z.prototype.get = (function(i) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  return this.u[i];
});
$ac_Z.prototype.set = (function(i, v) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  this.u[i] = v;
});
$ac_Z.prototype.copyTo = (function(srcPos, dest, destPos, length) {
  $arraycopyGeneric(this.u, srcPos, dest.u, destPos, length);
});
$ac_Z.prototype.clone__O = (function() {
  return new $ac_Z(this.u.slice());
});
function $ac_C(arg) {
  if (((typeof arg) === "number")) {
    if ((arg < 0)) {
      $throwNegativeArraySizeException();
    }
    this.u = new Uint16Array(arg);
  } else {
    this.u = arg;
  }
}
$ac_C.prototype = new $h_O();
$ac_C.prototype.constructor = $ac_C;
$ac_C.prototype.get = (function(i) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  return this.u[i];
});
$ac_C.prototype.set = (function(i, v) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  this.u[i] = v;
});
$ac_C.prototype.copyTo = (function(srcPos, dest, destPos, length) {
  $arraycopyCheckBounds(this.u.length, srcPos, dest.u.length, destPos, length);
  dest.u.set(this.u.subarray(srcPos, ((srcPos + length) | 0)), destPos);
});
$ac_C.prototype.clone__O = (function() {
  return new $ac_C(this.u.slice());
});
function $ac_B(arg) {
  if (((typeof arg) === "number")) {
    if ((arg < 0)) {
      $throwNegativeArraySizeException();
    }
    this.u = new Int8Array(arg);
  } else {
    this.u = arg;
  }
}
$ac_B.prototype = new $h_O();
$ac_B.prototype.constructor = $ac_B;
$ac_B.prototype.get = (function(i) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  return this.u[i];
});
$ac_B.prototype.set = (function(i, v) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  this.u[i] = v;
});
$ac_B.prototype.copyTo = (function(srcPos, dest, destPos, length) {
  $arraycopyCheckBounds(this.u.length, srcPos, dest.u.length, destPos, length);
  dest.u.set(this.u.subarray(srcPos, ((srcPos + length) | 0)), destPos);
});
$ac_B.prototype.clone__O = (function() {
  return new $ac_B(this.u.slice());
});
function $ac_S(arg) {
  if (((typeof arg) === "number")) {
    if ((arg < 0)) {
      $throwNegativeArraySizeException();
    }
    this.u = new Int16Array(arg);
  } else {
    this.u = arg;
  }
}
$ac_S.prototype = new $h_O();
$ac_S.prototype.constructor = $ac_S;
$ac_S.prototype.get = (function(i) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  return this.u[i];
});
$ac_S.prototype.set = (function(i, v) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  this.u[i] = v;
});
$ac_S.prototype.copyTo = (function(srcPos, dest, destPos, length) {
  $arraycopyCheckBounds(this.u.length, srcPos, dest.u.length, destPos, length);
  dest.u.set(this.u.subarray(srcPos, ((srcPos + length) | 0)), destPos);
});
$ac_S.prototype.clone__O = (function() {
  return new $ac_S(this.u.slice());
});
function $ac_I(arg) {
  if (((typeof arg) === "number")) {
    if ((arg < 0)) {
      $throwNegativeArraySizeException();
    }
    this.u = new Int32Array(arg);
  } else {
    this.u = arg;
  }
}
$ac_I.prototype = new $h_O();
$ac_I.prototype.constructor = $ac_I;
$ac_I.prototype.get = (function(i) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  return this.u[i];
});
$ac_I.prototype.set = (function(i, v) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  this.u[i] = v;
});
$ac_I.prototype.copyTo = (function(srcPos, dest, destPos, length) {
  $arraycopyCheckBounds(this.u.length, srcPos, dest.u.length, destPos, length);
  dest.u.set(this.u.subarray(srcPos, ((srcPos + length) | 0)), destPos);
});
$ac_I.prototype.clone__O = (function() {
  return new $ac_I(this.u.slice());
});
function $ac_J(arg) {
  if (((typeof arg) === "number")) {
    if ((arg < 0)) {
      $throwNegativeArraySizeException();
    }
    this.u = new Array(arg);
    for (var i = 0; (i < arg); (i++)) {
      this.u[i] = $L0;
    }
  } else {
    this.u = arg;
  }
}
$ac_J.prototype = new $h_O();
$ac_J.prototype.constructor = $ac_J;
$ac_J.prototype.get = (function(i) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  return this.u[i];
});
$ac_J.prototype.set = (function(i, v) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  this.u[i] = v;
});
$ac_J.prototype.copyTo = (function(srcPos, dest, destPos, length) {
  $arraycopyGeneric(this.u, srcPos, dest.u, destPos, length);
});
$ac_J.prototype.clone__O = (function() {
  return new $ac_J(this.u.slice());
});
function $ac_F(arg) {
  if (((typeof arg) === "number")) {
    if ((arg < 0)) {
      $throwNegativeArraySizeException();
    }
    this.u = new Float32Array(arg);
  } else {
    this.u = arg;
  }
}
$ac_F.prototype = new $h_O();
$ac_F.prototype.constructor = $ac_F;
$ac_F.prototype.get = (function(i) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  return this.u[i];
});
$ac_F.prototype.set = (function(i, v) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  this.u[i] = v;
});
$ac_F.prototype.copyTo = (function(srcPos, dest, destPos, length) {
  $arraycopyCheckBounds(this.u.length, srcPos, dest.u.length, destPos, length);
  dest.u.set(this.u.subarray(srcPos, ((srcPos + length) | 0)), destPos);
});
$ac_F.prototype.clone__O = (function() {
  return new $ac_F(this.u.slice());
});
function $ac_D(arg) {
  if (((typeof arg) === "number")) {
    if ((arg < 0)) {
      $throwNegativeArraySizeException();
    }
    this.u = new Float64Array(arg);
  } else {
    this.u = arg;
  }
}
$ac_D.prototype = new $h_O();
$ac_D.prototype.constructor = $ac_D;
$ac_D.prototype.get = (function(i) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  return this.u[i];
});
$ac_D.prototype.set = (function(i, v) {
  if (((i < 0) || (i >= this.u.length))) {
    $throwArrayIndexOutOFBoundsException(i);
  }
  this.u[i] = v;
});
$ac_D.prototype.copyTo = (function(srcPos, dest, destPos, length) {
  $arraycopyCheckBounds(this.u.length, srcPos, dest.u.length, destPos, length);
  dest.u.set(this.u.subarray(srcPos, ((srcPos + length) | 0)), destPos);
});
$ac_D.prototype.clone__O = (function() {
  return new $ac_D(this.u.slice());
});
function $TypeData() {
  this.constr = (void 0);
  this.ancestors = null;
  this.componentData = null;
  this.arrayBase = null;
  this.arrayDepth = 0;
  this.zero = null;
  this.arrayEncodedName = "";
  this._classOf = (void 0);
  this._arrayOf = (void 0);
  this.isAssignableFromFun = (void 0);
  this.wrapArray = (void 0);
  this.isJSType = false;
  this.name = "";
  this.isPrimitive = false;
  this.isInterface = false;
  this.isArrayClass = false;
  this.isInstance = (void 0);
}
$TypeData.prototype.initPrim = (function(zero, arrayEncodedName, displayName, arrayClass, typedArrayClass) {
  this.ancestors = ({});
  this.zero = zero;
  this.arrayEncodedName = arrayEncodedName;
  var self = this;
  this.isAssignableFromFun = ((that) => (that === self));
  this.name = displayName;
  this.isPrimitive = true;
  this.isInstance = ((obj) => false);
  if ((arrayClass !== (void 0))) {
    this._arrayOf = new $TypeData().initSpecializedArray(this, arrayClass, typedArrayClass);
  }
  return this;
});
$TypeData.prototype.initClass = (function(kindOrCtor, fullName, ancestors, isInstance) {
  var internalName = Object.getOwnPropertyNames(ancestors)[0];
  this.ancestors = ancestors;
  this.arrayEncodedName = (("L" + fullName) + ";");
  this.isAssignableFromFun = ((that) => (!(!that.ancestors[internalName])));
  this.isJSType = (kindOrCtor === 2);
  this.name = fullName;
  this.isInterface = (kindOrCtor === 1);
  this.isInstance = (isInstance || ((obj) => (!(!((obj && obj.$classData) && obj.$classData.ancestors[internalName])))));
  if (((typeof kindOrCtor) !== "number")) {
    kindOrCtor.prototype.$classData = this;
  }
  return this;
});
$TypeData.prototype.initSpecializedArray = (function(componentData, arrayClass, typedArrayClass, isAssignableFromFun) {
  arrayClass.prototype.$classData = this;
  var name = ("[" + componentData.arrayEncodedName);
  this.constr = arrayClass;
  this.ancestors = ({
    jl_Cloneable: 1,
    Ljava_io_Serializable: 1
  });
  this.componentData = componentData;
  this.arrayBase = componentData;
  this.arrayDepth = 1;
  this.arrayEncodedName = name;
  this.name = name;
  this.isArrayClass = true;
  var self = this;
  this.isAssignableFromFun = (isAssignableFromFun || ((that) => (self === that)));
  this.wrapArray = (typedArrayClass ? ((array) => new arrayClass(new typedArrayClass(array))) : ((array) => new arrayClass(array)));
  this.isInstance = ((obj) => (obj instanceof arrayClass));
  return this;
});
$TypeData.prototype.initArray = (function(componentData) {
  function ArrayClass(arg) {
    if (((typeof arg) === "number")) {
      if ((arg < 0)) {
        $throwNegativeArraySizeException();
      }
      this.u = new Array(arg);
      for (var i = 0; (i < arg); (i++)) {
        this.u[i] = null;
      }
    } else {
      this.u = arg;
    }
  }
  ArrayClass.prototype = new $ah_O();
  ArrayClass.prototype.constructor = ArrayClass;
  ArrayClass.prototype.set = (function(i, v) {
    if (((i < 0) || (i >= this.u.length))) {
      $throwArrayIndexOutOFBoundsException(i);
    }
    if ((((v !== null) && (!componentData.isJSType)) && (!componentData.isInstance(v)))) {
      $throwArrayStoreException(v);
    }
    this.u[i] = v;
  });
  ArrayClass.prototype.copyTo = (function(srcPos, dest, destPos, length) {
    $arraycopyGeneric(this.u, srcPos, dest.u, destPos, length);
  });
  ArrayClass.prototype.clone__O = (function() {
    return new ArrayClass(this.u.slice());
  });
  ArrayClass.prototype.$classData = this;
  var arrayBase = (componentData.arrayBase || componentData);
  var arrayDepth = (componentData.arrayDepth + 1);
  var name = ("[" + componentData.arrayEncodedName);
  this.constr = ArrayClass;
  this.ancestors = ({
    jl_Cloneable: 1,
    Ljava_io_Serializable: 1
  });
  this.componentData = componentData;
  this.arrayBase = arrayBase;
  this.arrayDepth = arrayDepth;
  this.arrayEncodedName = name;
  this.name = name;
  this.isArrayClass = true;
  var isAssignableFromFun = ((that) => {
    var thatDepth = that.arrayDepth;
    return ((thatDepth === arrayDepth) ? arrayBase.isAssignableFromFun(that.arrayBase) : ((thatDepth > arrayDepth) && (arrayBase === $d_O)));
  });
  this.isAssignableFromFun = isAssignableFromFun;
  this.wrapArray = ((array) => new ArrayClass(array));
  var self = this;
  this.isInstance = ((obj) => {
    var data = (obj && obj.$classData);
    return ((!(!data)) && ((data === self) || isAssignableFromFun(data)));
  });
  return this;
});
$TypeData.prototype.getArrayOf = (function() {
  if ((!this._arrayOf)) {
    this._arrayOf = new $TypeData().initArray(this);
  }
  return this._arrayOf;
});
$TypeData.prototype.getClassOf = (function() {
  if ((!this._classOf)) {
    this._classOf = new $c_jl_Class(this);
  }
  return this._classOf;
});
$TypeData.prototype.isAssignableFrom = (function(that) {
  return ((this === that) || this.isAssignableFromFun(that));
});
$TypeData.prototype.cast = (function(obj) {
  if ((((obj !== null) && (!this.isJSType)) && (!this.isInstance(obj)))) {
    $throwClassCastException(obj, this.name);
  }
  return obj;
});
$TypeData.prototype.getSuperclass = (function() {
  return (this.parentData ? this.parentData.getClassOf() : null);
});
$TypeData.prototype.getComponentType = (function() {
  return (this.componentData ? this.componentData.getClassOf() : null);
});
function $isArrayOf_O(obj, depth) {
  var data = (obj && obj.$classData);
  if ((!data)) {
    return false;
  } else {
    var arrayDepth = data.arrayDepth;
    return ((arrayDepth === depth) ? (!data.arrayBase.isPrimitive) : (arrayDepth > depth));
  }
}
function $isArrayOf_Z(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && (obj.$classData.arrayBase === $d_Z))));
}
function $isArrayOf_C(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && (obj.$classData.arrayBase === $d_C))));
}
function $isArrayOf_B(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && (obj.$classData.arrayBase === $d_B))));
}
function $isArrayOf_S(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && (obj.$classData.arrayBase === $d_S))));
}
function $isArrayOf_I(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && (obj.$classData.arrayBase === $d_I))));
}
function $isArrayOf_J(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && (obj.$classData.arrayBase === $d_J))));
}
function $isArrayOf_F(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && (obj.$classData.arrayBase === $d_F))));
}
function $isArrayOf_D(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && (obj.$classData.arrayBase === $d_D))));
}
function $asArrayOf_O(obj, depth) {
  if (($isArrayOf_O(obj, depth) || (obj === null))) {
    return obj;
  } else {
    $throwArrayCastException(obj, "Ljava.lang.Object;", depth);
  }
}
function $asArrayOf_Z(obj, depth) {
  if (($isArrayOf_Z(obj, depth) || (obj === null))) {
    return obj;
  } else {
    $throwArrayCastException(obj, "Z", depth);
  }
}
function $asArrayOf_C(obj, depth) {
  if (($isArrayOf_C(obj, depth) || (obj === null))) {
    return obj;
  } else {
    $throwArrayCastException(obj, "C", depth);
  }
}
function $asArrayOf_B(obj, depth) {
  if (($isArrayOf_B(obj, depth) || (obj === null))) {
    return obj;
  } else {
    $throwArrayCastException(obj, "B", depth);
  }
}
function $asArrayOf_S(obj, depth) {
  if (($isArrayOf_S(obj, depth) || (obj === null))) {
    return obj;
  } else {
    $throwArrayCastException(obj, "S", depth);
  }
}
function $asArrayOf_I(obj, depth) {
  if (($isArrayOf_I(obj, depth) || (obj === null))) {
    return obj;
  } else {
    $throwArrayCastException(obj, "I", depth);
  }
}
function $asArrayOf_J(obj, depth) {
  if (($isArrayOf_J(obj, depth) || (obj === null))) {
    return obj;
  } else {
    $throwArrayCastException(obj, "J", depth);
  }
}
function $asArrayOf_F(obj, depth) {
  if (($isArrayOf_F(obj, depth) || (obj === null))) {
    return obj;
  } else {
    $throwArrayCastException(obj, "F", depth);
  }
}
function $asArrayOf_D(obj, depth) {
  if (($isArrayOf_D(obj, depth) || (obj === null))) {
    return obj;
  } else {
    $throwArrayCastException(obj, "D", depth);
  }
}
var $d_O = new $TypeData();
$d_O.ancestors = ({});
$d_O.arrayEncodedName = "Ljava.lang.Object;";
$d_O.isAssignableFromFun = ((that) => (!that.isPrimitive));
$d_O.name = "java.lang.Object";
$d_O.isInstance = ((obj) => (obj !== null));
$d_O._arrayOf = new $TypeData().initSpecializedArray($d_O, $ac_O, (void 0), ((that) => {
  var thatDepth = that.arrayDepth;
  return ((thatDepth === 1) ? (!that.arrayBase.isPrimitive) : (thatDepth > 1));
}));
$c_O.prototype.$classData = $d_O;
var $d_V = new $TypeData().initPrim((void 0), "V", "void", (void 0), (void 0));
var $d_Z = new $TypeData().initPrim(false, "Z", "boolean", $ac_Z, (void 0));
var $d_C = new $TypeData().initPrim(0, "C", "char", $ac_C, Uint16Array);
var $d_B = new $TypeData().initPrim(0, "B", "byte", $ac_B, Int8Array);
var $d_S = new $TypeData().initPrim(0, "S", "short", $ac_S, Int16Array);
var $d_I = new $TypeData().initPrim(0, "I", "int", $ac_I, Int32Array);
var $d_J = new $TypeData().initPrim(null, "J", "long", $ac_J, (void 0));
var $d_F = new $TypeData().initPrim(0.0, "F", "float", $ac_F, Float32Array);
var $d_D = new $TypeData().initPrim(0.0, "D", "double", $ac_D, Float64Array);
function $s_LMain__main__AT__V(args) {
  var this$1 = $m_LMain$();
  $f_s_App__main__AT__V(this$1, args);
}
function $f_Lcom_raquo_airstream_core_Named__defaultDisplayName__T($thiz) {
  return (($objectGetClass($thiz).getSimpleName__T() + "@") + $systemIdentityHashCode($thiz));
}
function $f_Lcom_raquo_airstream_core_Named__displayName__T($thiz) {
  var x = $thiz.Lcom_raquo_airstream_core_Observer$$anon$8__f_maybeDisplayName;
  return $as_T(((x === (void 0)) ? $f_Lcom_raquo_airstream_core_Named__defaultDisplayName__T($thiz) : x));
}
/** @constructor */
function $c_Lcom_raquo_airstream_core_Observer$() {
  $n_Lcom_raquo_airstream_core_Observer$ = this;
  var this$2 = $m_Lcom_raquo_airstream_core_Observer$();
  var onNext = new $c_sjsr_AnonFunction1(((_$1) => (void 0)));
  this$2.withRecover__F1__s_PartialFunction__Z__Lcom_raquo_airstream_core_Observer(onNext, $m_s_PartialFunction$().s_PartialFunction$__f_empty_pf, true);
}
$c_Lcom_raquo_airstream_core_Observer$.prototype = new $h_O();
$c_Lcom_raquo_airstream_core_Observer$.prototype.constructor = $c_Lcom_raquo_airstream_core_Observer$;
/** @constructor */
function $h_Lcom_raquo_airstream_core_Observer$() {
}
$h_Lcom_raquo_airstream_core_Observer$.prototype = $c_Lcom_raquo_airstream_core_Observer$.prototype;
$c_Lcom_raquo_airstream_core_Observer$.prototype.withRecover__F1__s_PartialFunction__Z__Lcom_raquo_airstream_core_Observer = (function(onNext, onError, handleObserverErrors) {
  return new $c_Lcom_raquo_airstream_core_Observer$$anon$8(onNext, handleObserverErrors, onError, this);
});
var $d_Lcom_raquo_airstream_core_Observer$ = new $TypeData().initClass($c_Lcom_raquo_airstream_core_Observer$, "com.raquo.airstream.core.Observer$", ({
  Lcom_raquo_airstream_core_Observer$: 1
}));
var $n_Lcom_raquo_airstream_core_Observer$;
function $m_Lcom_raquo_airstream_core_Observer$() {
  if ((!$n_Lcom_raquo_airstream_core_Observer$)) {
    $n_Lcom_raquo_airstream_core_Observer$ = new $c_Lcom_raquo_airstream_core_Observer$();
  }
  return $n_Lcom_raquo_airstream_core_Observer$;
}
/** @constructor */
function $c_Lcom_raquo_airstream_core_Transaction(code) {
  this.Lcom_raquo_airstream_core_Transaction__f_code = null;
  this.Lcom_raquo_airstream_core_Transaction__f_maybePendingObservables = null;
  this.Lcom_raquo_airstream_core_Transaction__f_depth = 0;
  this.Lcom_raquo_airstream_core_Transaction__f_code = code;
  this.Lcom_raquo_airstream_core_Transaction__f_maybePendingObservables = (void 0);
  var x = $m_Lcom_raquo_airstream_core_Transaction$pendingTransactions$().peekStack__O();
  if ((x === (void 0))) {
    var $x_1 = 1;
  } else {
    var _$1 = $as_Lcom_raquo_airstream_core_Transaction(x);
    var $x_1 = ((1 + $n(_$1).Lcom_raquo_airstream_core_Transaction__f_depth) | 0);
  }
  this.Lcom_raquo_airstream_core_Transaction__f_depth = $x_1;
  if ((($m_Lcom_raquo_airstream_core_Transaction$().Lcom_raquo_airstream_core_Transaction$__f_maxDepth === (-1)) || (this.Lcom_raquo_airstream_core_Transaction__f_depth > $m_Lcom_raquo_airstream_core_Transaction$().Lcom_raquo_airstream_core_Transaction$__f_maxDepth))) {
    var $x_2 = $m_Lcom_raquo_airstream_core_AirstreamError$();
    var depth = $m_Lcom_raquo_airstream_core_Transaction$().Lcom_raquo_airstream_core_Transaction$__f_maxDepth;
    $x_2.sendUnhandledError__jl_Throwable__V(new $c_Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded(this, depth));
  } else if ($m_Lcom_raquo_airstream_core_Transaction$onStart$().Lcom_raquo_airstream_core_Transaction$onStart$__f_isSharedStart) {
    $uI($m_Lcom_raquo_airstream_core_Transaction$onStart$().Lcom_raquo_airstream_core_Transaction$onStart$__f_postStartTransactions.push(this));
  } else {
    $m_Lcom_raquo_airstream_core_Transaction$pendingTransactions$().add__Lcom_raquo_airstream_core_Transaction__V(this);
  }
}
$c_Lcom_raquo_airstream_core_Transaction.prototype = new $h_O();
$c_Lcom_raquo_airstream_core_Transaction.prototype.constructor = $c_Lcom_raquo_airstream_core_Transaction;
/** @constructor */
function $h_Lcom_raquo_airstream_core_Transaction() {
}
$h_Lcom_raquo_airstream_core_Transaction.prototype = $c_Lcom_raquo_airstream_core_Transaction.prototype;
function $as_Lcom_raquo_airstream_core_Transaction(obj) {
  return (((obj instanceof $c_Lcom_raquo_airstream_core_Transaction) || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.airstream.core.Transaction"));
}
function $isArrayOf_Lcom_raquo_airstream_core_Transaction(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_airstream_core_Transaction)));
}
function $asArrayOf_Lcom_raquo_airstream_core_Transaction(obj, depth) {
  return (($isArrayOf_Lcom_raquo_airstream_core_Transaction(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.airstream.core.Transaction;", depth));
}
var $d_Lcom_raquo_airstream_core_Transaction = new $TypeData().initClass($c_Lcom_raquo_airstream_core_Transaction, "com.raquo.airstream.core.Transaction", ({
  Lcom_raquo_airstream_core_Transaction: 1
}));
/** @constructor */
function $c_Lcom_raquo_airstream_core_Transaction$() {
  this.Lcom_raquo_airstream_core_Transaction$__f_maxDepth = 0;
  this.Lcom_raquo_airstream_core_Transaction$__f_com$raquo$airstream$core$Transaction$$$throwDeadTrxError = null;
  $n_Lcom_raquo_airstream_core_Transaction$ = this;
  this.Lcom_raquo_airstream_core_Transaction$__f_maxDepth = 1000;
  this.Lcom_raquo_airstream_core_Transaction$__f_com$raquo$airstream$core$Transaction$$$throwDeadTrxError = new $c_sjsr_AnonFunction1(((trx) => {
    var trx$1 = $as_Lcom_raquo_airstream_core_Transaction(trx);
    throw $ct_jl_Exception__T__(new $c_jl_Exception(), (("Attempted to run Transaction " + trx$1) + " after it was already executed."));
  }));
}
$c_Lcom_raquo_airstream_core_Transaction$.prototype = new $h_O();
$c_Lcom_raquo_airstream_core_Transaction$.prototype.constructor = $c_Lcom_raquo_airstream_core_Transaction$;
/** @constructor */
function $h_Lcom_raquo_airstream_core_Transaction$() {
}
$h_Lcom_raquo_airstream_core_Transaction$.prototype = $c_Lcom_raquo_airstream_core_Transaction$.prototype;
$c_Lcom_raquo_airstream_core_Transaction$.prototype.com$raquo$airstream$core$Transaction$$$run__Lcom_raquo_airstream_core_Transaction__V = (function(transaction) {
  try {
    $n($n(transaction).Lcom_raquo_airstream_core_Transaction__f_code).apply__O__O(transaction);
    var this$1 = $n(transaction);
    var x = this$1.Lcom_raquo_airstream_core_Transaction__f_maybePendingObservables;
    if ((x !== (void 0))) {
      var pendingObservables = $as_Lcom_raquo_airstream_util_JsPriorityQueue(x);
      while ($n(pendingObservables).nonEmpty__Z()) {
        $n($as_Lcom_raquo_airstream_core_SyncObservable($n(pendingObservables).dequeue__O())).syncFire__Lcom_raquo_airstream_core_Transaction__V(this$1);
      }
    }
  } catch (e) {
    var e$2 = ((e instanceof $c_jl_Throwable) ? e : new $c_sjs_js_JavaScriptException(e));
    $m_Lcom_raquo_airstream_core_AirstreamError$().sendUnhandledError__jl_Throwable__V(e$2);
  }
});
var $d_Lcom_raquo_airstream_core_Transaction$ = new $TypeData().initClass($c_Lcom_raquo_airstream_core_Transaction$, "com.raquo.airstream.core.Transaction$", ({
  Lcom_raquo_airstream_core_Transaction$: 1
}));
var $n_Lcom_raquo_airstream_core_Transaction$;
function $m_Lcom_raquo_airstream_core_Transaction$() {
  if ((!$n_Lcom_raquo_airstream_core_Transaction$)) {
    $n_Lcom_raquo_airstream_core_Transaction$ = new $c_Lcom_raquo_airstream_core_Transaction$();
  }
  return $n_Lcom_raquo_airstream_core_Transaction$;
}
function $p_Lcom_raquo_airstream_core_Transaction$onStart$__resolve__V($thiz) {
  if (($uI($thiz.Lcom_raquo_airstream_core_Transaction$onStart$__f_pendingCallbacks.length) === 0)) {
    if (($uI($thiz.Lcom_raquo_airstream_core_Transaction$onStart$__f_postStartTransactions.length) > 0)) {
      var code = new $c_sjsr_AnonFunction1(((_$3) => {
        $as_Lcom_raquo_airstream_core_Transaction(_$3);
        while (($uI($thiz.Lcom_raquo_airstream_core_Transaction$onStart$__f_postStartTransactions.length) > 0)) {
          $m_Lcom_raquo_airstream_core_Transaction$pendingTransactions$().add__Lcom_raquo_airstream_core_Transaction__V($as_Lcom_raquo_airstream_core_Transaction($thiz.Lcom_raquo_airstream_core_Transaction$onStart$__f_postStartTransactions.shift()));
        }
      }));
      new $c_Lcom_raquo_airstream_core_Transaction(code);
    }
  } else {
    var code$1 = new $c_sjsr_AnonFunction1(((trx) => {
      var trx$1 = $as_Lcom_raquo_airstream_core_Transaction(trx);
      while (($uI($thiz.Lcom_raquo_airstream_core_Transaction$onStart$__f_pendingCallbacks.length) > 0)) {
        var callback = $as_F1($thiz.Lcom_raquo_airstream_core_Transaction$onStart$__f_pendingCallbacks.shift());
        try {
          $n(callback).apply__O__O(trx$1);
        } catch (e) {
          var e$2 = ((e instanceof $c_jl_Throwable) ? e : new $c_sjs_js_JavaScriptException(e));
          $m_Lcom_raquo_airstream_core_AirstreamError$().sendUnhandledError__jl_Throwable__V(e$2);
        }
      }
      while (($uI($thiz.Lcom_raquo_airstream_core_Transaction$onStart$__f_postStartTransactions.length) > 0)) {
        var _trx = $as_Lcom_raquo_airstream_core_Transaction($thiz.Lcom_raquo_airstream_core_Transaction$onStart$__f_postStartTransactions.shift());
        $m_Lcom_raquo_airstream_core_Transaction$pendingTransactions$().add__Lcom_raquo_airstream_core_Transaction__V(_trx);
      }
    }));
    new $c_Lcom_raquo_airstream_core_Transaction(code$1);
  }
}
/** @constructor */
function $c_Lcom_raquo_airstream_core_Transaction$onStart$() {
  this.Lcom_raquo_airstream_core_Transaction$onStart$__f_isSharedStart = false;
  this.Lcom_raquo_airstream_core_Transaction$onStart$__f_pendingCallbacks = null;
  this.Lcom_raquo_airstream_core_Transaction$onStart$__f_postStartTransactions = null;
  $n_Lcom_raquo_airstream_core_Transaction$onStart$ = this;
  this.Lcom_raquo_airstream_core_Transaction$onStart$__f_isSharedStart = false;
  this.Lcom_raquo_airstream_core_Transaction$onStart$__f_pendingCallbacks = $m_Lcom_raquo_ew_JsArray$().apply__sci_Seq__Lcom_raquo_ew_JsArray($m_sr_ScalaRunTime$().wrapRefArray__AO__sci_ArraySeq(new ($d_F1.getArrayOf().constr)([])));
  this.Lcom_raquo_airstream_core_Transaction$onStart$__f_postStartTransactions = $m_Lcom_raquo_ew_JsArray$().apply__sci_Seq__Lcom_raquo_ew_JsArray($m_sr_ScalaRunTime$().wrapRefArray__AO__sci_ArraySeq(new ($d_Lcom_raquo_airstream_core_Transaction.getArrayOf().constr)([])));
}
$c_Lcom_raquo_airstream_core_Transaction$onStart$.prototype = new $h_O();
$c_Lcom_raquo_airstream_core_Transaction$onStart$.prototype.constructor = $c_Lcom_raquo_airstream_core_Transaction$onStart$;
/** @constructor */
function $h_Lcom_raquo_airstream_core_Transaction$onStart$() {
}
$h_Lcom_raquo_airstream_core_Transaction$onStart$.prototype = $c_Lcom_raquo_airstream_core_Transaction$onStart$.prototype;
var $d_Lcom_raquo_airstream_core_Transaction$onStart$ = new $TypeData().initClass($c_Lcom_raquo_airstream_core_Transaction$onStart$, "com.raquo.airstream.core.Transaction$onStart$", ({
  Lcom_raquo_airstream_core_Transaction$onStart$: 1
}));
var $n_Lcom_raquo_airstream_core_Transaction$onStart$;
function $m_Lcom_raquo_airstream_core_Transaction$onStart$() {
  if ((!$n_Lcom_raquo_airstream_core_Transaction$onStart$)) {
    $n_Lcom_raquo_airstream_core_Transaction$onStart$ = new $c_Lcom_raquo_airstream_core_Transaction$onStart$();
  }
  return $n_Lcom_raquo_airstream_core_Transaction$onStart$;
}
function $p_Lcom_raquo_airstream_core_Transaction$pendingTransactions$__maybeChildrenFor__Lcom_raquo_airstream_core_Transaction__O($thiz, transaction) {
  return $thiz.Lcom_raquo_airstream_core_Transaction$pendingTransactions$__f_children.get(transaction);
}
function $p_Lcom_raquo_airstream_core_Transaction$pendingTransactions$__pushToStack__Lcom_raquo_airstream_core_Transaction__V($thiz, transaction) {
  $thiz.Lcom_raquo_airstream_core_Transaction$pendingTransactions$__f_stack.unshift(transaction);
}
function $p_Lcom_raquo_airstream_core_Transaction$pendingTransactions$__popStack__O($thiz) {
  return $thiz.Lcom_raquo_airstream_core_Transaction$pendingTransactions$__f_stack.shift();
}
function $p_Lcom_raquo_airstream_core_Transaction$pendingTransactions$__enqueueChild__Lcom_raquo_airstream_core_Transaction__Lcom_raquo_airstream_core_Transaction__V($thiz, parent, newChild) {
  var maybeChildren = $p_Lcom_raquo_airstream_core_Transaction$pendingTransactions$__maybeChildrenFor__Lcom_raquo_airstream_core_Transaction__O($thiz, parent);
  var noChildrenFound = (maybeChildren === (void 0));
  var newChildren = ((maybeChildren === (void 0)) ? $m_Lcom_raquo_ew_JsArray$().apply__sci_Seq__Lcom_raquo_ew_JsArray($m_sr_ScalaRunTime$().wrapRefArray__AO__sci_ArraySeq(new ($d_Lcom_raquo_airstream_core_Transaction.getArrayOf().constr)([]))) : maybeChildren);
  newChildren.push(newChild);
  if (noChildrenFound) {
    $thiz.Lcom_raquo_airstream_core_Transaction$pendingTransactions$__f_children.set(parent, newChildren);
  }
}
function $p_Lcom_raquo_airstream_core_Transaction$pendingTransactions$__dequeueChild__Lcom_raquo_airstream_core_Transaction__O($thiz, parent) {
  var maybeParentChildren = $p_Lcom_raquo_airstream_core_Transaction$pendingTransactions$__maybeChildrenFor__Lcom_raquo_airstream_core_Transaction__O($thiz, parent);
  var x = (((maybeParentChildren === (void 0)) || ($uI(maybeParentChildren.length) > 0)) ? maybeParentChildren : (void 0));
  if ((x === (void 0))) {
    return (void 0);
  } else {
    var nextChild = $as_Lcom_raquo_airstream_core_Transaction(x.shift());
    if (($uI(x.length) === 0)) {
      $uZ($thiz.Lcom_raquo_airstream_core_Transaction$pendingTransactions$__f_children.delete(parent));
    }
    return nextChild;
  }
}
/** @constructor */
function $c_Lcom_raquo_airstream_core_Transaction$pendingTransactions$() {
  this.Lcom_raquo_airstream_core_Transaction$pendingTransactions$__f_stack = null;
  this.Lcom_raquo_airstream_core_Transaction$pendingTransactions$__f_children = null;
  $n_Lcom_raquo_airstream_core_Transaction$pendingTransactions$ = this;
  this.Lcom_raquo_airstream_core_Transaction$pendingTransactions$__f_stack = $m_Lcom_raquo_ew_JsArray$().apply__sci_Seq__Lcom_raquo_ew_JsArray($m_sr_ScalaRunTime$().wrapRefArray__AO__sci_ArraySeq(new ($d_Lcom_raquo_airstream_core_Transaction.getArrayOf().constr)([])));
  this.Lcom_raquo_airstream_core_Transaction$pendingTransactions$__f_children = new Map();
}
$c_Lcom_raquo_airstream_core_Transaction$pendingTransactions$.prototype = new $h_O();
$c_Lcom_raquo_airstream_core_Transaction$pendingTransactions$.prototype.constructor = $c_Lcom_raquo_airstream_core_Transaction$pendingTransactions$;
/** @constructor */
function $h_Lcom_raquo_airstream_core_Transaction$pendingTransactions$() {
}
$h_Lcom_raquo_airstream_core_Transaction$pendingTransactions$.prototype = $c_Lcom_raquo_airstream_core_Transaction$pendingTransactions$.prototype;
$c_Lcom_raquo_airstream_core_Transaction$pendingTransactions$.prototype.add__Lcom_raquo_airstream_core_Transaction__V = (function(newTransaction) {
  var x = this.peekStack__O();
  if ((x === (void 0))) {
    $p_Lcom_raquo_airstream_core_Transaction$pendingTransactions$__pushToStack__Lcom_raquo_airstream_core_Transaction__V(this, newTransaction);
    $m_Lcom_raquo_airstream_core_Transaction$().com$raquo$airstream$core$Transaction$$$run__Lcom_raquo_airstream_core_Transaction__V(newTransaction);
    this.done__Lcom_raquo_airstream_core_Transaction__V(newTransaction);
  } else {
    var currentTransaction = $as_Lcom_raquo_airstream_core_Transaction(x);
    $p_Lcom_raquo_airstream_core_Transaction$pendingTransactions$__enqueueChild__Lcom_raquo_airstream_core_Transaction__Lcom_raquo_airstream_core_Transaction__V(this, currentTransaction, newTransaction);
  }
});
$c_Lcom_raquo_airstream_core_Transaction$pendingTransactions$.prototype.done__Lcom_raquo_airstream_core_Transaction__V = (function(transaction) {
  var transaction$tailLocal1 = transaction;
  while (true) {
    var x = this.peekStack__O();
    var elem = transaction$tailLocal1;
    if ((!((x !== (void 0)) && $m_sr_BoxesRunTime$().equals__O__O__Z(elem, x)))) {
      throw $ct_jl_Exception__T__(new $c_jl_Exception(), "Transaction queue error: Completed transaction is not the first in stack. This is a bug in Airstream.");
    }
    this.putNextTransactionOnStack__Lcom_raquo_airstream_core_Transaction__V(transaction$tailLocal1);
    $n(transaction$tailLocal1).Lcom_raquo_airstream_core_Transaction__f_code = $m_Lcom_raquo_airstream_core_Transaction$().Lcom_raquo_airstream_core_Transaction$__f_com$raquo$airstream$core$Transaction$$$throwDeadTrxError;
    var maybeNextTransaction = this.peekStack__O();
    if ($m_sr_BoxesRunTime$().equals__O__O__Z(maybeNextTransaction, (void 0))) {
      if (($uI(this.Lcom_raquo_airstream_core_Transaction$pendingTransactions$__f_children.size) > 0)) {
        var numChildren = new $c_sr_IntRef(0);
        this.Lcom_raquo_airstream_core_Transaction$pendingTransactions$__f_children.forEach(((numChildren) => ((transactions, _$4) => {
          $as_Lcom_raquo_airstream_core_Transaction(_$4);
          var ev$12 = ((numChildren.sr_IntRef__f_elem + $uI(transactions.length)) | 0);
          numChildren.sr_IntRef__f_elem = ev$12;
        }))(numChildren));
        throw $ct_jl_Exception__T__(new $c_jl_Exception(), (((("Transaction queue error: Stack cleared, but a total of " + numChildren.sr_IntRef__f_elem) + " children for ") + $uI(this.Lcom_raquo_airstream_core_Transaction$pendingTransactions$__f_children.size)) + " transactions remain. This is a bug in Airstream."));
      } else {
        return (void 0);
      }
    } else {
      var nextTransaction = $as_Lcom_raquo_airstream_core_Transaction(maybeNextTransaction);
      $m_Lcom_raquo_airstream_core_Transaction$().com$raquo$airstream$core$Transaction$$$run__Lcom_raquo_airstream_core_Transaction__V(nextTransaction);
      transaction$tailLocal1 = nextTransaction;
    }
  }
});
$c_Lcom_raquo_airstream_core_Transaction$pendingTransactions$.prototype.putNextTransactionOnStack__Lcom_raquo_airstream_core_Transaction__V = (function(doneTransaction) {
  var doneTransaction$tailLocal1 = doneTransaction;
  while (true) {
    var maybeNextChildTrx = $p_Lcom_raquo_airstream_core_Transaction$pendingTransactions$__dequeueChild__Lcom_raquo_airstream_core_Transaction__O(this, doneTransaction$tailLocal1);
    if ($m_sr_BoxesRunTime$().equals__O__O__Z(maybeNextChildTrx, (void 0))) {
      $p_Lcom_raquo_airstream_core_Transaction$pendingTransactions$__popStack__O(this);
      var maybeParentTransaction = this.peekStack__O();
      if ((!$m_sr_BoxesRunTime$().equals__O__O__Z(maybeParentTransaction, (void 0)))) {
        var parentTransaction = $as_Lcom_raquo_airstream_core_Transaction(maybeParentTransaction);
        doneTransaction$tailLocal1 = parentTransaction;
      } else {
        return (void 0);
      }
    } else {
      var nextChildTrx = $as_Lcom_raquo_airstream_core_Transaction(maybeNextChildTrx);
      $p_Lcom_raquo_airstream_core_Transaction$pendingTransactions$__pushToStack__Lcom_raquo_airstream_core_Transaction__V(this, nextChildTrx);
      return (void 0);
    }
  }
});
$c_Lcom_raquo_airstream_core_Transaction$pendingTransactions$.prototype.peekStack__O = (function() {
  return this.Lcom_raquo_airstream_core_Transaction$pendingTransactions$__f_stack[0];
});
var $d_Lcom_raquo_airstream_core_Transaction$pendingTransactions$ = new $TypeData().initClass($c_Lcom_raquo_airstream_core_Transaction$pendingTransactions$, "com.raquo.airstream.core.Transaction$pendingTransactions$", ({
  Lcom_raquo_airstream_core_Transaction$pendingTransactions$: 1
}));
var $n_Lcom_raquo_airstream_core_Transaction$pendingTransactions$;
function $m_Lcom_raquo_airstream_core_Transaction$pendingTransactions$() {
  if ((!$n_Lcom_raquo_airstream_core_Transaction$pendingTransactions$)) {
    $n_Lcom_raquo_airstream_core_Transaction$pendingTransactions$ = new $c_Lcom_raquo_airstream_core_Transaction$pendingTransactions$();
  }
  return $n_Lcom_raquo_airstream_core_Transaction$pendingTransactions$;
}
function $p_Lcom_raquo_airstream_ownership_DynamicOwner__removeSubscriptionNow__Lcom_raquo_airstream_ownership_DynamicSubscription__V($thiz, subscription) {
  var index = $uI($thiz.Lcom_raquo_airstream_ownership_DynamicOwner__f_subscriptions.indexOf(subscription));
  if ((index !== (-1))) {
    $thiz.Lcom_raquo_airstream_ownership_DynamicOwner__f_subscriptions.splice(index, 1);
    var this$1 = $n($thiz.Lcom_raquo_airstream_ownership_DynamicOwner__f__maybeCurrentOwner);
    if ((!this$1.isEmpty__Z())) {
      $n(subscription).onDeactivate__V();
    }
  } else {
    throw $ct_jl_Exception__T__(new $c_jl_Exception(), "Can not remove DynamicSubscription from DynamicOwner: subscription not found. Did you already kill it?");
  }
}
function $p_Lcom_raquo_airstream_ownership_DynamicOwner__removePendingSubscriptionsNow__V($thiz) {
  while (($uI($thiz.Lcom_raquo_airstream_ownership_DynamicOwner__f_pendingSubscriptionRemovals.length) > 0)) {
    var subscriptionToRemove = $as_Lcom_raquo_airstream_ownership_DynamicSubscription($thiz.Lcom_raquo_airstream_ownership_DynamicOwner__f_pendingSubscriptionRemovals.shift());
    $p_Lcom_raquo_airstream_ownership_DynamicOwner__removeSubscriptionNow__Lcom_raquo_airstream_ownership_DynamicSubscription__V($thiz, subscriptionToRemove);
  }
}
/** @constructor */
function $c_Lcom_raquo_airstream_ownership_DynamicOwner(onAccessAfterKilled) {
  this.Lcom_raquo_airstream_ownership_DynamicOwner__f_onAccessAfterKilled = null;
  this.Lcom_raquo_airstream_ownership_DynamicOwner__f_subscriptions = null;
  this.Lcom_raquo_airstream_ownership_DynamicOwner__f_isSafeToRemoveSubscription = false;
  this.Lcom_raquo_airstream_ownership_DynamicOwner__f_pendingSubscriptionRemovals = null;
  this.Lcom_raquo_airstream_ownership_DynamicOwner__f__maybeCurrentOwner = null;
  this.Lcom_raquo_airstream_ownership_DynamicOwner__f_numPrependedSubs = 0;
  this.Lcom_raquo_airstream_ownership_DynamicOwner__f_onAccessAfterKilled = onAccessAfterKilled;
  this.Lcom_raquo_airstream_ownership_DynamicOwner__f_subscriptions = $m_Lcom_raquo_ew_JsArray$().apply__sci_Seq__Lcom_raquo_ew_JsArray($m_sr_ScalaRunTime$().wrapRefArray__AO__sci_ArraySeq(new ($d_Lcom_raquo_airstream_ownership_DynamicSubscription.getArrayOf().constr)([])));
  this.Lcom_raquo_airstream_ownership_DynamicOwner__f_isSafeToRemoveSubscription = true;
  this.Lcom_raquo_airstream_ownership_DynamicOwner__f_pendingSubscriptionRemovals = $m_Lcom_raquo_ew_JsArray$().apply__sci_Seq__Lcom_raquo_ew_JsArray($m_sr_ScalaRunTime$().wrapRefArray__AO__sci_ArraySeq(new ($d_Lcom_raquo_airstream_ownership_DynamicSubscription.getArrayOf().constr)([])));
  this.Lcom_raquo_airstream_ownership_DynamicOwner__f__maybeCurrentOwner = $m_s_None$();
  this.Lcom_raquo_airstream_ownership_DynamicOwner__f_numPrependedSubs = 0;
}
$c_Lcom_raquo_airstream_ownership_DynamicOwner.prototype = new $h_O();
$c_Lcom_raquo_airstream_ownership_DynamicOwner.prototype.constructor = $c_Lcom_raquo_airstream_ownership_DynamicOwner;
/** @constructor */
function $h_Lcom_raquo_airstream_ownership_DynamicOwner() {
}
$h_Lcom_raquo_airstream_ownership_DynamicOwner.prototype = $c_Lcom_raquo_airstream_ownership_DynamicOwner.prototype;
$c_Lcom_raquo_airstream_ownership_DynamicOwner.prototype.activate__V = (function() {
  var this$1 = $n(this.Lcom_raquo_airstream_ownership_DynamicOwner__f__maybeCurrentOwner);
  if ((!(!this$1.isEmpty__Z()))) {
    var this$4 = $m_Lcom_raquo_airstream_core_Transaction$onStart$();
    var f = (() => {
      var newOwner = new $c_Lcom_raquo_airstream_ownership_OneTimeOwner(this.Lcom_raquo_airstream_ownership_DynamicOwner__f_onAccessAfterKilled);
      this.Lcom_raquo_airstream_ownership_DynamicOwner__f__maybeCurrentOwner = new $c_s_Some(newOwner);
      this.Lcom_raquo_airstream_ownership_DynamicOwner__f_isSafeToRemoveSubscription = false;
      this.Lcom_raquo_airstream_ownership_DynamicOwner__f_numPrependedSubs = 0;
      var i = 0;
      var originalNumSubs = $uI(this.Lcom_raquo_airstream_ownership_DynamicOwner__f_subscriptions.length);
      while ((i < originalNumSubs)) {
        var ix = ((i + this.Lcom_raquo_airstream_ownership_DynamicOwner__f_numPrependedSubs) | 0);
        var sub = $as_Lcom_raquo_airstream_ownership_DynamicSubscription(this.Lcom_raquo_airstream_ownership_DynamicOwner__f_subscriptions[ix]);
        $n(sub).onActivate__Lcom_raquo_airstream_ownership_Owner__V(newOwner);
        i = ((1 + i) | 0);
      }
      $p_Lcom_raquo_airstream_ownership_DynamicOwner__removePendingSubscriptionsNow__V(this);
      this.Lcom_raquo_airstream_ownership_DynamicOwner__f_isSafeToRemoveSubscription = true;
      this.Lcom_raquo_airstream_ownership_DynamicOwner__f_numPrependedSubs = 0;
    });
    $m_Lcom_raquo_airstream_core_Transaction$onStart$();
    var when = true;
    if ((this$4.Lcom_raquo_airstream_core_Transaction$onStart$__f_isSharedStart || (!when))) {
      f();
    } else {
      this$4.Lcom_raquo_airstream_core_Transaction$onStart$__f_isSharedStart = true;
      try {
        f();
      } finally {
        this$4.Lcom_raquo_airstream_core_Transaction$onStart$__f_isSharedStart = false;
        $p_Lcom_raquo_airstream_core_Transaction$onStart$__resolve__V(this$4);
      }
    }
  } else {
    throw $ct_jl_Exception__T__(new $c_jl_Exception(), (("Can not activate " + this) + ": it is already active"));
  }
});
$c_Lcom_raquo_airstream_ownership_DynamicOwner.prototype.deactivate__V = (function() {
  var this$1 = $n(this.Lcom_raquo_airstream_ownership_DynamicOwner__f__maybeCurrentOwner);
  if ((!this$1.isEmpty__Z())) {
    this.Lcom_raquo_airstream_ownership_DynamicOwner__f_isSafeToRemoveSubscription = false;
    var arr = this.Lcom_raquo_airstream_ownership_DynamicOwner__f_subscriptions;
    var i = 0;
    var len = $uI(arr.length);
    while ((i < len)) {
      var _$1 = arr[i];
      var _$1$1 = $as_Lcom_raquo_airstream_ownership_DynamicSubscription(_$1);
      $n(_$1$1).onDeactivate__V();
      i = ((1 + i) | 0);
    }
    $p_Lcom_raquo_airstream_ownership_DynamicOwner__removePendingSubscriptionsNow__V(this);
    var this$4 = $n(this.Lcom_raquo_airstream_ownership_DynamicOwner__f__maybeCurrentOwner);
    if ((!this$4.isEmpty__Z())) {
      var arg1 = this$4.get__O();
      var _$2 = $as_Lcom_raquo_airstream_ownership_Owner(arg1);
      var this$5 = $n(_$2);
      this$5.killSubscriptions__V();
    }
    $p_Lcom_raquo_airstream_ownership_DynamicOwner__removePendingSubscriptionsNow__V(this);
    this.Lcom_raquo_airstream_ownership_DynamicOwner__f_isSafeToRemoveSubscription = true;
    this.Lcom_raquo_airstream_ownership_DynamicOwner__f__maybeCurrentOwner = $m_s_None$();
  } else {
    throw $ct_jl_Exception__T__(new $c_jl_Exception(), "Can not deactivate DynamicOwner: it is not active");
  }
});
$c_Lcom_raquo_airstream_ownership_DynamicOwner.prototype.addSubscription__Lcom_raquo_airstream_ownership_DynamicSubscription__Z__V = (function(subscription, prepend) {
  if (prepend) {
    this.Lcom_raquo_airstream_ownership_DynamicOwner__f_numPrependedSubs = ((1 + this.Lcom_raquo_airstream_ownership_DynamicOwner__f_numPrependedSubs) | 0);
    this.Lcom_raquo_airstream_ownership_DynamicOwner__f_subscriptions.unshift(subscription);
  } else {
    this.Lcom_raquo_airstream_ownership_DynamicOwner__f_subscriptions.push(subscription);
  }
  var this$1 = $n(this.Lcom_raquo_airstream_ownership_DynamicOwner__f__maybeCurrentOwner);
  if ((!this$1.isEmpty__Z())) {
    var arg1 = this$1.get__O();
    var o = $as_Lcom_raquo_airstream_ownership_Owner(arg1);
    $n(subscription).onActivate__Lcom_raquo_airstream_ownership_Owner__V(o);
  }
});
$c_Lcom_raquo_airstream_ownership_DynamicOwner.prototype.removeSubscription__Lcom_raquo_airstream_ownership_DynamicSubscription__V = (function(subscription) {
  if (this.Lcom_raquo_airstream_ownership_DynamicOwner__f_isSafeToRemoveSubscription) {
    $p_Lcom_raquo_airstream_ownership_DynamicOwner__removeSubscriptionNow__Lcom_raquo_airstream_ownership_DynamicSubscription__V(this, subscription);
  } else {
    this.Lcom_raquo_airstream_ownership_DynamicOwner__f_pendingSubscriptionRemovals.push(subscription);
  }
});
function $as_Lcom_raquo_airstream_ownership_DynamicOwner(obj) {
  return (((obj instanceof $c_Lcom_raquo_airstream_ownership_DynamicOwner) || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.airstream.ownership.DynamicOwner"));
}
function $isArrayOf_Lcom_raquo_airstream_ownership_DynamicOwner(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_airstream_ownership_DynamicOwner)));
}
function $asArrayOf_Lcom_raquo_airstream_ownership_DynamicOwner(obj, depth) {
  return (($isArrayOf_Lcom_raquo_airstream_ownership_DynamicOwner(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.airstream.ownership.DynamicOwner;", depth));
}
var $d_Lcom_raquo_airstream_ownership_DynamicOwner = new $TypeData().initClass($c_Lcom_raquo_airstream_ownership_DynamicOwner, "com.raquo.airstream.ownership.DynamicOwner", ({
  Lcom_raquo_airstream_ownership_DynamicOwner: 1
}));
/** @constructor */
function $c_Lcom_raquo_airstream_ownership_DynamicSubscription(dynamicOwner, activate, prepend) {
  this.Lcom_raquo_airstream_ownership_DynamicSubscription__f_dynamicOwner = null;
  this.Lcom_raquo_airstream_ownership_DynamicSubscription__f_activate = null;
  this.Lcom_raquo_airstream_ownership_DynamicSubscription__f_maybeCurrentSubscription = null;
  this.Lcom_raquo_airstream_ownership_DynamicSubscription__f_dynamicOwner = dynamicOwner;
  this.Lcom_raquo_airstream_ownership_DynamicSubscription__f_activate = activate;
  this.Lcom_raquo_airstream_ownership_DynamicSubscription__f_maybeCurrentSubscription = $m_s_None$();
  $n(dynamicOwner).addSubscription__Lcom_raquo_airstream_ownership_DynamicSubscription__Z__V(this, prepend);
}
$c_Lcom_raquo_airstream_ownership_DynamicSubscription.prototype = new $h_O();
$c_Lcom_raquo_airstream_ownership_DynamicSubscription.prototype.constructor = $c_Lcom_raquo_airstream_ownership_DynamicSubscription;
/** @constructor */
function $h_Lcom_raquo_airstream_ownership_DynamicSubscription() {
}
$h_Lcom_raquo_airstream_ownership_DynamicSubscription.prototype = $c_Lcom_raquo_airstream_ownership_DynamicSubscription.prototype;
$c_Lcom_raquo_airstream_ownership_DynamicSubscription.prototype.kill__V = (function() {
  $n(this.Lcom_raquo_airstream_ownership_DynamicSubscription__f_dynamicOwner).removeSubscription__Lcom_raquo_airstream_ownership_DynamicSubscription__V(this);
});
$c_Lcom_raquo_airstream_ownership_DynamicSubscription.prototype.onActivate__Lcom_raquo_airstream_ownership_Owner__V = (function(owner) {
  var this$2 = $m_Lcom_raquo_airstream_core_Transaction$onStart$();
  var f = (() => {
    this.Lcom_raquo_airstream_ownership_DynamicSubscription__f_maybeCurrentSubscription = $as_s_Option($n(this.Lcom_raquo_airstream_ownership_DynamicSubscription__f_activate).apply__O__O(owner));
  });
  $m_Lcom_raquo_airstream_core_Transaction$onStart$();
  var when = true;
  if ((this$2.Lcom_raquo_airstream_core_Transaction$onStart$__f_isSharedStart || (!when))) {
    f();
  } else {
    this$2.Lcom_raquo_airstream_core_Transaction$onStart$__f_isSharedStart = true;
    try {
      f();
    } finally {
      this$2.Lcom_raquo_airstream_core_Transaction$onStart$__f_isSharedStart = false;
      $p_Lcom_raquo_airstream_core_Transaction$onStart$__resolve__V(this$2);
    }
  }
});
$c_Lcom_raquo_airstream_ownership_DynamicSubscription.prototype.onDeactivate__V = (function() {
  var this$1 = $n(this.Lcom_raquo_airstream_ownership_DynamicSubscription__f_maybeCurrentSubscription);
  if ((!this$1.isEmpty__Z())) {
    var arg1 = this$1.get__O();
    var currentSubscription = $as_Lcom_raquo_airstream_ownership_Subscription(arg1);
    $n(currentSubscription).kill__V();
    this.Lcom_raquo_airstream_ownership_DynamicSubscription__f_maybeCurrentSubscription = $m_s_None$();
  }
});
function $as_Lcom_raquo_airstream_ownership_DynamicSubscription(obj) {
  return (((obj instanceof $c_Lcom_raquo_airstream_ownership_DynamicSubscription) || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.airstream.ownership.DynamicSubscription"));
}
function $isArrayOf_Lcom_raquo_airstream_ownership_DynamicSubscription(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_airstream_ownership_DynamicSubscription)));
}
function $asArrayOf_Lcom_raquo_airstream_ownership_DynamicSubscription(obj, depth) {
  return (($isArrayOf_Lcom_raquo_airstream_ownership_DynamicSubscription(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.airstream.ownership.DynamicSubscription;", depth));
}
var $d_Lcom_raquo_airstream_ownership_DynamicSubscription = new $TypeData().initClass($c_Lcom_raquo_airstream_ownership_DynamicSubscription, "com.raquo.airstream.ownership.DynamicSubscription", ({
  Lcom_raquo_airstream_ownership_DynamicSubscription: 1
}));
/** @constructor */
function $c_Lcom_raquo_airstream_ownership_DynamicSubscription$() {
}
$c_Lcom_raquo_airstream_ownership_DynamicSubscription$.prototype = new $h_O();
$c_Lcom_raquo_airstream_ownership_DynamicSubscription$.prototype.constructor = $c_Lcom_raquo_airstream_ownership_DynamicSubscription$;
/** @constructor */
function $h_Lcom_raquo_airstream_ownership_DynamicSubscription$() {
}
$h_Lcom_raquo_airstream_ownership_DynamicSubscription$.prototype = $c_Lcom_raquo_airstream_ownership_DynamicSubscription$.prototype;
$c_Lcom_raquo_airstream_ownership_DynamicSubscription$.prototype.unsafe__Lcom_raquo_airstream_ownership_DynamicOwner__F1__Z__Lcom_raquo_airstream_ownership_DynamicSubscription = (function(dynamicOwner, activate, prepend) {
  return new $c_Lcom_raquo_airstream_ownership_DynamicSubscription(dynamicOwner, new $c_sjsr_AnonFunction1(((owner) => {
    var owner$1 = $as_Lcom_raquo_airstream_ownership_Owner(owner);
    var value = $n(activate).apply__O__O(owner$1);
    return new $c_s_Some(value);
  })), prepend);
});
$c_Lcom_raquo_airstream_ownership_DynamicSubscription$.prototype.subscribeCallback__Lcom_raquo_airstream_ownership_DynamicOwner__F1__Z__Lcom_raquo_airstream_ownership_DynamicSubscription = (function(dynamicOwner, activate, prepend) {
  return new $c_Lcom_raquo_airstream_ownership_DynamicSubscription(dynamicOwner, new $c_sjsr_AnonFunction1(((owner) => {
    var owner$1 = $as_Lcom_raquo_airstream_ownership_Owner(owner);
    $n(activate).apply__O__O(owner$1);
    return $m_s_None$();
  })), prepend);
});
var $d_Lcom_raquo_airstream_ownership_DynamicSubscription$ = new $TypeData().initClass($c_Lcom_raquo_airstream_ownership_DynamicSubscription$, "com.raquo.airstream.ownership.DynamicSubscription$", ({
  Lcom_raquo_airstream_ownership_DynamicSubscription$: 1
}));
var $n_Lcom_raquo_airstream_ownership_DynamicSubscription$;
function $m_Lcom_raquo_airstream_ownership_DynamicSubscription$() {
  if ((!$n_Lcom_raquo_airstream_ownership_DynamicSubscription$)) {
    $n_Lcom_raquo_airstream_ownership_DynamicSubscription$ = new $c_Lcom_raquo_airstream_ownership_DynamicSubscription$();
  }
  return $n_Lcom_raquo_airstream_ownership_DynamicSubscription$;
}
function $f_Lcom_raquo_airstream_ownership_Owner__$init$__V($thiz) {
  $thiz.Lcom_raquo_airstream_ownership_OneTimeOwner__f_subscriptions = $m_Lcom_raquo_ew_JsArray$().apply__sci_Seq__Lcom_raquo_ew_JsArray($m_sr_ScalaRunTime$().wrapRefArray__AO__sci_ArraySeq(new ($d_Lcom_raquo_airstream_ownership_Subscription.getArrayOf().constr)([])));
}
function $f_Lcom_raquo_airstream_ownership_Owner__killSubscriptions__V($thiz) {
  var arr = $thiz.Lcom_raquo_airstream_ownership_OneTimeOwner__f_subscriptions;
  var i = 0;
  var len = $uI(arr.length);
  while ((i < len)) {
    var _$1 = arr[i];
    var _$1$1 = $as_Lcom_raquo_airstream_ownership_Subscription(_$1);
    var this$3 = $n(_$1$1);
    $p_Lcom_raquo_airstream_ownership_Subscription__safeCleanup__V(this$3);
    i = ((1 + i) | 0);
  }
  $thiz.Lcom_raquo_airstream_ownership_OneTimeOwner__f_subscriptions.length = 0;
}
function $f_Lcom_raquo_airstream_ownership_Owner__onKilledExternally__Lcom_raquo_airstream_ownership_Subscription__V($thiz, subscription) {
  var index = $uI($thiz.Lcom_raquo_airstream_ownership_OneTimeOwner__f_subscriptions.indexOf(subscription));
  if ((index !== (-1))) {
    $thiz.Lcom_raquo_airstream_ownership_OneTimeOwner__f_subscriptions.splice(index, 1);
  } else {
    throw $ct_jl_Exception__T__(new $c_jl_Exception(), "Can not remove Subscription from Owner: subscription not found.");
  }
}
function $f_Lcom_raquo_airstream_ownership_Owner__own__Lcom_raquo_airstream_ownership_Subscription__V($thiz, subscription) {
  $thiz.Lcom_raquo_airstream_ownership_OneTimeOwner__f_subscriptions.push(subscription);
}
function $is_Lcom_raquo_airstream_ownership_Owner(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lcom_raquo_airstream_ownership_Owner)));
}
function $as_Lcom_raquo_airstream_ownership_Owner(obj) {
  return (($is_Lcom_raquo_airstream_ownership_Owner(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.airstream.ownership.Owner"));
}
function $isArrayOf_Lcom_raquo_airstream_ownership_Owner(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_airstream_ownership_Owner)));
}
function $asArrayOf_Lcom_raquo_airstream_ownership_Owner(obj, depth) {
  return (($isArrayOf_Lcom_raquo_airstream_ownership_Owner(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.airstream.ownership.Owner;", depth));
}
function $p_Lcom_raquo_airstream_ownership_Subscription__safeCleanup__V($thiz) {
  if ((!$thiz.Lcom_raquo_airstream_ownership_Subscription__f__isKilled)) {
    $n($thiz.Lcom_raquo_airstream_ownership_Subscription__f_cleanup).apply__O();
    $thiz.Lcom_raquo_airstream_ownership_Subscription__f__isKilled = true;
  } else {
    throw $ct_jl_Exception__T__(new $c_jl_Exception(), "Can not kill Subscription: it was already killed.");
  }
}
/** @constructor */
function $c_Lcom_raquo_airstream_ownership_Subscription(owner, cleanup) {
  this.Lcom_raquo_airstream_ownership_Subscription__f_owner = null;
  this.Lcom_raquo_airstream_ownership_Subscription__f_cleanup = null;
  this.Lcom_raquo_airstream_ownership_Subscription__f__isKilled = false;
  this.Lcom_raquo_airstream_ownership_Subscription__f_owner = owner;
  this.Lcom_raquo_airstream_ownership_Subscription__f_cleanup = cleanup;
  this.Lcom_raquo_airstream_ownership_Subscription__f__isKilled = false;
  $n(owner).own__Lcom_raquo_airstream_ownership_Subscription__V(this);
}
$c_Lcom_raquo_airstream_ownership_Subscription.prototype = new $h_O();
$c_Lcom_raquo_airstream_ownership_Subscription.prototype.constructor = $c_Lcom_raquo_airstream_ownership_Subscription;
/** @constructor */
function $h_Lcom_raquo_airstream_ownership_Subscription() {
}
$h_Lcom_raquo_airstream_ownership_Subscription.prototype = $c_Lcom_raquo_airstream_ownership_Subscription.prototype;
$c_Lcom_raquo_airstream_ownership_Subscription.prototype.kill__V = (function() {
  $p_Lcom_raquo_airstream_ownership_Subscription__safeCleanup__V(this);
  var this$1 = $n(this.Lcom_raquo_airstream_ownership_Subscription__f_owner);
  $f_Lcom_raquo_airstream_ownership_Owner__onKilledExternally__Lcom_raquo_airstream_ownership_Subscription__V(this$1, this);
});
function $as_Lcom_raquo_airstream_ownership_Subscription(obj) {
  return (((obj instanceof $c_Lcom_raquo_airstream_ownership_Subscription) || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.airstream.ownership.Subscription"));
}
function $isArrayOf_Lcom_raquo_airstream_ownership_Subscription(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_airstream_ownership_Subscription)));
}
function $asArrayOf_Lcom_raquo_airstream_ownership_Subscription(obj, depth) {
  return (($isArrayOf_Lcom_raquo_airstream_ownership_Subscription(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.airstream.ownership.Subscription;", depth));
}
var $d_Lcom_raquo_airstream_ownership_Subscription = new $TypeData().initClass($c_Lcom_raquo_airstream_ownership_Subscription, "com.raquo.airstream.ownership.Subscription", ({
  Lcom_raquo_airstream_ownership_Subscription: 1
}));
/** @constructor */
function $c_Lcom_raquo_airstream_ownership_TransferableSubscription(activate, deactivate) {
  this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_activate = null;
  this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_deactivate = null;
  this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_maybeSubscription = null;
  this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_isLiveTransferInProgress = false;
  this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_activate = activate;
  this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_deactivate = deactivate;
  this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_maybeSubscription = $m_s_None$();
  this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_isLiveTransferInProgress = false;
}
$c_Lcom_raquo_airstream_ownership_TransferableSubscription.prototype = new $h_O();
$c_Lcom_raquo_airstream_ownership_TransferableSubscription.prototype.constructor = $c_Lcom_raquo_airstream_ownership_TransferableSubscription;
/** @constructor */
function $h_Lcom_raquo_airstream_ownership_TransferableSubscription() {
}
$h_Lcom_raquo_airstream_ownership_TransferableSubscription.prototype = $c_Lcom_raquo_airstream_ownership_TransferableSubscription.prototype;
$c_Lcom_raquo_airstream_ownership_TransferableSubscription.prototype.isCurrentOwnerActive__Z = (function() {
  var this$1 = $n(this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_maybeSubscription);
  if ((!this$1.isEmpty__Z())) {
    var arg1 = this$1.get__O();
    var _$1 = $as_Lcom_raquo_airstream_ownership_DynamicSubscription(arg1);
    var this$2 = $n(_$1);
    var this$3 = $n(this$2.Lcom_raquo_airstream_ownership_DynamicSubscription__f_dynamicOwner);
    var this$4 = $n(this$3.Lcom_raquo_airstream_ownership_DynamicOwner__f__maybeCurrentOwner);
    return (!this$4.isEmpty__Z());
  } else {
    return false;
  }
});
$c_Lcom_raquo_airstream_ownership_TransferableSubscription.prototype.setOwner__Lcom_raquo_airstream_ownership_DynamicOwner__V = (function(nextOwner) {
  if (this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_isLiveTransferInProgress) {
    throw $ct_jl_Exception__T__(new $c_jl_Exception(), "Unable to set owner on DynamicTransferableSubscription while a transfer on this subscription is already in progress.");
  }
  var this$1 = $n(this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_maybeSubscription);
  if ((!this$1.isEmpty__Z())) {
    var arg1 = this$1.get__O();
    var _$2 = $as_Lcom_raquo_airstream_ownership_DynamicSubscription(arg1);
    var this$2 = $n(_$2);
    var x$2 = this$2.Lcom_raquo_airstream_ownership_DynamicSubscription__f_dynamicOwner;
    if ((nextOwner === null)) {
      var $x_1 = (x$2 === null);
    } else {
      var this$3 = $n(nextOwner);
      var $x_1 = (this$3 === x$2);
    }
  } else {
    var $x_1 = false;
  }
  if ((!$x_1)) {
    if (this.isCurrentOwnerActive__Z()) {
      var this$4 = $n(nextOwner);
      var this$5 = $n(this$4.Lcom_raquo_airstream_ownership_DynamicOwner__f__maybeCurrentOwner);
      var $x_2 = (!this$5.isEmpty__Z());
    } else {
      var $x_2 = false;
    }
    if ($x_2) {
      this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_isLiveTransferInProgress = true;
    }
    var this$6 = $n(this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_maybeSubscription);
    if ((!this$6.isEmpty__Z())) {
      var arg1$1 = this$6.get__O();
      var subscription = $as_Lcom_raquo_airstream_ownership_DynamicSubscription(arg1$1);
      $n(subscription).kill__V();
      this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_maybeSubscription = $m_s_None$();
    }
    var newPilotSubscription = $m_Lcom_raquo_airstream_ownership_DynamicSubscription$().unsafe__Lcom_raquo_airstream_ownership_DynamicOwner__F1__Z__Lcom_raquo_airstream_ownership_DynamicSubscription(nextOwner, new $c_sjsr_AnonFunction1(((parentOwner) => {
      var parentOwner$1 = $as_Lcom_raquo_airstream_ownership_Owner(parentOwner);
      if ((!this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_isLiveTransferInProgress)) {
        $n(this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_activate).apply__O();
      }
      return new $c_Lcom_raquo_airstream_ownership_Subscription(parentOwner$1, new $c_sjsr_AnonFunction0((() => {
        if ((!this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_isLiveTransferInProgress)) {
          $n(this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_deactivate).apply__O();
        }
      })));
    })), false);
    this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_maybeSubscription = new $c_s_Some(newPilotSubscription);
    this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_isLiveTransferInProgress = false;
  }
});
$c_Lcom_raquo_airstream_ownership_TransferableSubscription.prototype.clearOwner__V = (function() {
  if (this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_isLiveTransferInProgress) {
    throw $ct_jl_Exception__T__(new $c_jl_Exception(), "Unable to clear owner on DynamicTransferableSubscription while a transfer on this subscription is already in progress.");
  }
  var this$1 = $n(this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_maybeSubscription);
  if ((!this$1.isEmpty__Z())) {
    var arg1 = this$1.get__O();
    var subscription = $as_Lcom_raquo_airstream_ownership_DynamicSubscription(arg1);
    $n(subscription).kill__V();
  }
  this.Lcom_raquo_airstream_ownership_TransferableSubscription__f_maybeSubscription = $m_s_None$();
});
var $d_Lcom_raquo_airstream_ownership_TransferableSubscription = new $TypeData().initClass($c_Lcom_raquo_airstream_ownership_TransferableSubscription, "com.raquo.airstream.ownership.TransferableSubscription", ({
  Lcom_raquo_airstream_ownership_TransferableSubscription: 1
}));
function $as_Lcom_raquo_airstream_util_JsPriorityQueue(obj) {
  return ((false || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.airstream.util.JsPriorityQueue"));
}
function $isArrayOf_Lcom_raquo_airstream_util_JsPriorityQueue(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_airstream_util_JsPriorityQueue)));
}
function $asArrayOf_Lcom_raquo_airstream_util_JsPriorityQueue(obj, depth) {
  return (($isArrayOf_Lcom_raquo_airstream_util_JsPriorityQueue(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.airstream.util.JsPriorityQueue;", depth));
}
/** @constructor */
function $c_Lcom_raquo_ew_JsArray$() {
}
$c_Lcom_raquo_ew_JsArray$.prototype = new $h_O();
$c_Lcom_raquo_ew_JsArray$.prototype.constructor = $c_Lcom_raquo_ew_JsArray$;
/** @constructor */
function $h_Lcom_raquo_ew_JsArray$() {
}
$h_Lcom_raquo_ew_JsArray$.prototype = $c_Lcom_raquo_ew_JsArray$.prototype;
$c_Lcom_raquo_ew_JsArray$.prototype.apply__sci_Seq__Lcom_raquo_ew_JsArray = (function(items) {
  return [...$m_sjsr_Compat$().toJSVarArgsImpl__sci_Seq__sjs_js_Array(items)];
});
var $d_Lcom_raquo_ew_JsArray$ = new $TypeData().initClass($c_Lcom_raquo_ew_JsArray$, "com.raquo.ew.JsArray$", ({
  Lcom_raquo_ew_JsArray$: 1
}));
var $n_Lcom_raquo_ew_JsArray$;
function $m_Lcom_raquo_ew_JsArray$() {
  if ((!$n_Lcom_raquo_ew_JsArray$)) {
    $n_Lcom_raquo_ew_JsArray$ = new $c_Lcom_raquo_ew_JsArray$();
  }
  return $n_Lcom_raquo_ew_JsArray$;
}
/** @constructor */
function $c_Lcom_raquo_laminar_DomApi$() {
  this.Lcom_raquo_laminar_DomApi$__f_classNamesSeparatorRegex = null;
  $n_Lcom_raquo_laminar_DomApi$ = this;
  document.createElement("template");
  this.createSvgElement__Lcom_raquo_laminar_tags_SvgTag__Lorg_scalajs_dom_SVGElement($n($n($m_Lcom_raquo_laminar_api_package$().Lcom_raquo_laminar_api_package$__f_L).svg__Lcom_raquo_laminar_api_Laminar$svg$()).svg__Lcom_raquo_laminar_tags_SvgTag());
  this.Lcom_raquo_laminar_DomApi$__f_classNamesSeparatorRegex = new RegExp(" ", "g");
}
$c_Lcom_raquo_laminar_DomApi$.prototype = new $h_O();
$c_Lcom_raquo_laminar_DomApi$.prototype.constructor = $c_Lcom_raquo_laminar_DomApi$;
/** @constructor */
function $h_Lcom_raquo_laminar_DomApi$() {
}
$h_Lcom_raquo_laminar_DomApi$.prototype = $c_Lcom_raquo_laminar_DomApi$.prototype;
$c_Lcom_raquo_laminar_DomApi$.prototype.appendChild__Lorg_scalajs_dom_Node__Lorg_scalajs_dom_Node__Z = (function(parent, child) {
  try {
    parent.appendChild(child);
    return true;
  } catch (e) {
    var e$2 = ((e instanceof $c_jl_Throwable) ? e : new $c_sjs_js_JavaScriptException(e));
    if (((e$2 instanceof $c_sjs_js_JavaScriptException) && $uZ(($n($as_sjs_js_JavaScriptException(e$2)).sjs_js_JavaScriptException__f_exception instanceof DOMException)))) {
      return false;
    }
    throw ((e$2 instanceof $c_sjs_js_JavaScriptException) ? e$2.sjs_js_JavaScriptException__f_exception : e$2);
  }
});
$c_Lcom_raquo_laminar_DomApi$.prototype.isDescendantOf__Lorg_scalajs_dom_Node__Lorg_scalajs_dom_Node__Z = (function(node, ancestor) {
  var node$tailLocal1 = node;
  while (true) {
    if ((node$tailLocal1.parentNode !== null)) {
      var effectiveParentNode = node$tailLocal1.parentNode;
    } else {
      var maybeShadowHost = node$tailLocal1.host;
      var this$2 = $m_s_$less$colon$less$();
      var ev = this$2.s_$less$colon$less$__f_singleton;
      var effectiveParentNode = ((maybeShadowHost === (void 0)) ? ($n(ev), null) : maybeShadowHost);
    }
    if ((effectiveParentNode === null)) {
      return false;
    }
    if ($m_sr_BoxesRunTime$().equals__O__O__Z(ancestor, effectiveParentNode)) {
      return true;
    }
    node$tailLocal1 = effectiveParentNode;
  }
});
$c_Lcom_raquo_laminar_DomApi$.prototype.createHtmlElement__Lcom_raquo_laminar_tags_HtmlTag__Lorg_scalajs_dom_HTMLElement = (function(tag) {
  return document.createElement($n(tag).Lcom_raquo_laminar_tags_HtmlTag__f_name);
});
$c_Lcom_raquo_laminar_DomApi$.prototype.getHtmlAttribute__Lcom_raquo_laminar_nodes_ReactiveHtmlElement__Lcom_raquo_laminar_keys_HtmlAttr__O = (function(element, attr) {
  var x = this.getHtmlAttributeRaw__Lcom_raquo_laminar_nodes_ReactiveHtmlElement__Lcom_raquo_laminar_keys_HtmlAttr__O(element, attr);
  if ((x === (void 0))) {
    return (void 0);
  } else {
    var domValue = $as_T(x);
    var a = $n($n(attr).Lcom_raquo_laminar_keys_HtmlAttr__f_codec).decode__O__O(domValue);
    return a;
  }
});
$c_Lcom_raquo_laminar_DomApi$.prototype.getHtmlAttributeRaw__Lcom_raquo_laminar_nodes_ReactiveHtmlElement__Lcom_raquo_laminar_keys_HtmlAttr__O = (function(element, attr) {
  var domValue = $as_T($n(element).Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_ref.getAttributeNS(null, $n(attr).Lcom_raquo_laminar_keys_HtmlAttr__f_name));
  return ((domValue !== null) ? domValue : (void 0));
});
$c_Lcom_raquo_laminar_DomApi$.prototype.setHtmlAttribute__Lcom_raquo_laminar_nodes_ReactiveHtmlElement__Lcom_raquo_laminar_keys_HtmlAttr__O__V = (function(element, attr, value) {
  var domValue = $as_T($n($n(attr).Lcom_raquo_laminar_keys_HtmlAttr__f_codec).encode__O__O(value));
  this.setHtmlAttributeRaw__Lcom_raquo_laminar_nodes_ReactiveHtmlElement__Lcom_raquo_laminar_keys_HtmlAttr__T__V(element, attr, domValue);
});
$c_Lcom_raquo_laminar_DomApi$.prototype.setHtmlAttributeRaw__Lcom_raquo_laminar_nodes_ReactiveHtmlElement__Lcom_raquo_laminar_keys_HtmlAttr__T__V = (function(element, attr, domValue) {
  if ((domValue === null)) {
    this.removeHtmlAttribute__Lcom_raquo_laminar_nodes_ReactiveHtmlElement__Lcom_raquo_laminar_keys_HtmlAttr__V(element, attr);
  } else {
    $n(element).Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_ref.setAttribute($n(attr).Lcom_raquo_laminar_keys_HtmlAttr__f_name, domValue);
  }
});
$c_Lcom_raquo_laminar_DomApi$.prototype.removeHtmlAttribute__Lcom_raquo_laminar_nodes_ReactiveHtmlElement__Lcom_raquo_laminar_keys_HtmlAttr__V = (function(element, attr) {
  $n(element).Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_ref.removeAttribute($n(attr).Lcom_raquo_laminar_keys_HtmlAttr__f_name);
});
$c_Lcom_raquo_laminar_DomApi$.prototype.createSvgElement__Lcom_raquo_laminar_tags_SvgTag__Lorg_scalajs_dom_SVGElement = (function(tag) {
  return document.createElementNS("http://www.w3.org/2000/svg", $n(tag).Lcom_raquo_laminar_tags_SvgTag__f_name);
});
$c_Lcom_raquo_laminar_DomApi$.prototype.getSvgAttribute__Lcom_raquo_laminar_nodes_ReactiveSvgElement__Lcom_raquo_laminar_keys_SvgAttr__O = (function(element, attr) {
  var x = this.getSvgAttributeRaw__Lcom_raquo_laminar_nodes_ReactiveSvgElement__Lcom_raquo_laminar_keys_SvgAttr__O(element, attr);
  if ((x === (void 0))) {
    return (void 0);
  } else {
    var domValue = $as_T(x);
    var a = $n($n(attr).Lcom_raquo_laminar_keys_SvgAttr__f_codec).decode__O__O(domValue);
    return a;
  }
});
$c_Lcom_raquo_laminar_DomApi$.prototype.getSvgAttributeRaw__Lcom_raquo_laminar_nodes_ReactiveSvgElement__Lcom_raquo_laminar_keys_SvgAttr__O = (function(element, attr) {
  var $x_2 = $n(element).ref__Lorg_scalajs_dom_SVGElement();
  var this$2 = $n($n(attr).Lcom_raquo_laminar_keys_SvgAttr__f_namespaceUri);
  var this$1 = $m_s_$less$colon$less$();
  var ev = this$1.s_$less$colon$less$__f_singleton;
  var $x_1 = $x_2.getAttributeNS($as_T((this$2.isEmpty__Z() ? ($n(ev), null) : this$2.get__O())), $n(attr).Lcom_raquo_laminar_keys_SvgAttr__f_localName);
  var domValue = $as_T($x_1);
  return ((domValue !== null) ? domValue : (void 0));
});
$c_Lcom_raquo_laminar_DomApi$.prototype.setSvgAttribute__Lcom_raquo_laminar_nodes_ReactiveSvgElement__Lcom_raquo_laminar_keys_SvgAttr__O__V = (function(element, attr, value) {
  var domValue = $as_T($n($n(attr).Lcom_raquo_laminar_keys_SvgAttr__f_codec).encode__O__O(value));
  this.setSvgAttributeRaw__Lcom_raquo_laminar_nodes_ReactiveSvgElement__Lcom_raquo_laminar_keys_SvgAttr__T__V(element, attr, domValue);
});
$c_Lcom_raquo_laminar_DomApi$.prototype.setSvgAttributeRaw__Lcom_raquo_laminar_nodes_ReactiveSvgElement__Lcom_raquo_laminar_keys_SvgAttr__T__V = (function(element, attr, domValue) {
  if ((domValue === null)) {
    this.removeSvgAttribute__Lcom_raquo_laminar_nodes_ReactiveSvgElement__Lcom_raquo_laminar_keys_SvgAttr__V(element, attr);
  } else {
    var this$1 = $n($n(attr).Lcom_raquo_laminar_keys_SvgAttr__f_namespaceUri);
    if (this$1.isEmpty__Z()) {
      $n(element).ref__Lorg_scalajs_dom_SVGElement().setAttribute($n(attr).Lcom_raquo_laminar_keys_SvgAttr__f_name, domValue);
    } else {
      var arg1 = this$1.get__O();
      var namespaceUri = $as_T(arg1);
      $n(element).ref__Lorg_scalajs_dom_SVGElement().setAttributeNS(namespaceUri, $n(attr).Lcom_raquo_laminar_keys_SvgAttr__f_name, domValue);
    }
  }
});
$c_Lcom_raquo_laminar_DomApi$.prototype.removeSvgAttribute__Lcom_raquo_laminar_nodes_ReactiveSvgElement__Lcom_raquo_laminar_keys_SvgAttr__V = (function(element, attr) {
  var $x_1 = $n(element).ref__Lorg_scalajs_dom_SVGElement();
  var this$2 = $n($n(attr).Lcom_raquo_laminar_keys_SvgAttr__f_namespaceUri);
  var this$1 = $m_s_$less$colon$less$();
  var ev = this$1.s_$less$colon$less$__f_singleton;
  $x_1.removeAttributeNS($as_T((this$2.isEmpty__Z() ? ($n(ev), null) : this$2.get__O())), $n(attr).Lcom_raquo_laminar_keys_SvgAttr__f_localName);
});
$c_Lcom_raquo_laminar_DomApi$.prototype.createTextNode__T__Lorg_scalajs_dom_Text = (function(text) {
  return document.createTextNode(text);
});
$c_Lcom_raquo_laminar_DomApi$.prototype.debugPath__Lorg_scalajs_dom_Node__sci_List__sci_List = (function(element, initial) {
  var initial$tailLocal1 = initial;
  var element$tailLocal1 = element;
  while (true) {
    var x54 = element$tailLocal1;
    if ((x54 === null)) {
      return initial$tailLocal1;
    }
    var element$tailLocal1$tmp1 = element$tailLocal1.parentNode;
    var elem$2 = this.debugNodeDescription__Lorg_scalajs_dom_Node__T(element$tailLocal1);
    var this$1 = $n(initial$tailLocal1);
    var initial$tailLocal1$tmp1 = new $c_sci_$colon$colon(elem$2, this$1);
    element$tailLocal1 = element$tailLocal1$tmp1;
    initial$tailLocal1 = initial$tailLocal1$tmp1;
  }
});
$c_Lcom_raquo_laminar_DomApi$.prototype.debugNodeDescription__Lorg_scalajs_dom_Node__T = (function(node) {
  if ($uZ((node instanceof HTMLElement))) {
    var id = $as_T(node.id);
    var this$3 = $n(id);
    if ((!(this$3 === ""))) {
      var suffixStr = ("#" + id);
    } else {
      var classes = $as_T(node.className);
      var this$6 = $n(classes);
      if ((!(this$6 === ""))) {
        var self = classes.replace(this.Lcom_raquo_laminar_DomApi$__f_classNamesSeparatorRegex, ".");
        var suffixStr = ("." + $as_T(self));
      } else {
        var suffixStr = "";
      }
    }
    var str = $as_T(node.tagName);
    var self$1 = str.toLowerCase();
    return ($as_T(self$1) + suffixStr);
  } else {
    return $as_T(node.nodeName);
  }
});
var $d_Lcom_raquo_laminar_DomApi$ = new $TypeData().initClass($c_Lcom_raquo_laminar_DomApi$, "com.raquo.laminar.DomApi$", ({
  Lcom_raquo_laminar_DomApi$: 1
}));
var $n_Lcom_raquo_laminar_DomApi$;
function $m_Lcom_raquo_laminar_DomApi$() {
  if ((!$n_Lcom_raquo_laminar_DomApi$)) {
    $n_Lcom_raquo_laminar_DomApi$ = new $c_Lcom_raquo_laminar_DomApi$();
  }
  return $n_Lcom_raquo_laminar_DomApi$;
}
function $f_Lcom_raquo_laminar_api_AirstreamAliases__$init$__V($thiz) {
  $m_Lcom_raquo_airstream_core_Observer$();
  $m_Lcom_raquo_airstream_core_AirstreamError$();
}
function $f_Lcom_raquo_laminar_api_LaminarAliases__$init$__V($thiz) {
  $thiz.Lcom_raquo_laminar_api_package$$anon$1__f_Modifier = $m_Lcom_raquo_laminar_modifiers_Modifier$();
}
function $f_Lcom_raquo_laminar_api_MountHooks__$init$__V($thiz) {
  var fn = new $c_sjsr_AnonFunction1(((_$1) => {
    var _$1$1 = $as_Lcom_raquo_laminar_lifecycle_MountContext(_$1);
    $n($as_Lcom_raquo_laminar_nodes_ReactiveHtmlElement($n(_$1$1).Lcom_raquo_laminar_lifecycle_MountContext__f_thisNode)).Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_ref.focus();
  }));
  $f_Lcom_raquo_laminar_api_MountHooks__onMountCallback__F1__Lcom_raquo_laminar_modifiers_Modifier($thiz, fn);
}
function $f_Lcom_raquo_laminar_api_MountHooks__onMountCallback__F1__Lcom_raquo_laminar_modifiers_Modifier($thiz, fn) {
  var this$6 = $m_Lcom_raquo_laminar_modifiers_Modifier$();
  var f = new $c_sjsr_AnonFunction1(((element) => {
    var element$1 = $as_Lcom_raquo_laminar_nodes_ReactiveElement(element);
    var this$2 = $n($n(element$1).Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_dynamicOwner);
    var this$3 = $n(this$2.Lcom_raquo_airstream_ownership_DynamicOwner__f__maybeCurrentOwner);
    var elem = (!this$3.isEmpty__Z());
    var ignoreNextActivation = new $c_sr_BooleanRef(elem);
    var activate = new $c_sjsr_AnonFunction1(((c) => {
      var c$1 = $as_Lcom_raquo_laminar_lifecycle_MountContext(c);
      if (ignoreNextActivation.sr_BooleanRef__f_elem) {
        var ev$5 = false;
        ignoreNextActivation.sr_BooleanRef__f_elem = ev$5;
      } else {
        $n(fn).apply__O__O(c$1);
      }
    }));
    $m_Lcom_raquo_airstream_ownership_DynamicSubscription$().subscribeCallback__Lcom_raquo_airstream_ownership_DynamicOwner__F1__Z__Lcom_raquo_airstream_ownership_DynamicSubscription($n(element$1).Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_dynamicOwner, new $c_sjsr_AnonFunction1(((owner) => {
      var owner$1 = $as_Lcom_raquo_airstream_ownership_Owner(owner);
      activate.apply__O__O(new $c_Lcom_raquo_laminar_lifecycle_MountContext(element$1, owner$1));
    })), false);
  }));
  return new $c_Lcom_raquo_laminar_modifiers_Modifier$$anon$2(f, this$6);
}
/** @constructor */
function $c_Lcom_raquo_laminar_codecs_package$() {
  this.Lcom_raquo_laminar_codecs_package$__f_StringAsIsCodec = null;
  $n_Lcom_raquo_laminar_codecs_package$ = this;
  var this$1 = $m_Lcom_raquo_laminar_codecs_package$();
  this.Lcom_raquo_laminar_codecs_package$__f_StringAsIsCodec = new $c_Lcom_raquo_laminar_codecs_package$$anon$2(this$1);
  var this$2 = $m_Lcom_raquo_laminar_codecs_package$();
  new $c_Lcom_raquo_laminar_codecs_package$$anon$2(this$2);
  var this$3 = $m_Lcom_raquo_laminar_codecs_package$();
  new $c_Lcom_raquo_laminar_codecs_package$$anon$2(this$3);
}
$c_Lcom_raquo_laminar_codecs_package$.prototype = new $h_O();
$c_Lcom_raquo_laminar_codecs_package$.prototype.constructor = $c_Lcom_raquo_laminar_codecs_package$;
/** @constructor */
function $h_Lcom_raquo_laminar_codecs_package$() {
}
$h_Lcom_raquo_laminar_codecs_package$.prototype = $c_Lcom_raquo_laminar_codecs_package$.prototype;
var $d_Lcom_raquo_laminar_codecs_package$ = new $TypeData().initClass($c_Lcom_raquo_laminar_codecs_package$, "com.raquo.laminar.codecs.package$", ({
  Lcom_raquo_laminar_codecs_package$: 1
}));
var $n_Lcom_raquo_laminar_codecs_package$;
function $m_Lcom_raquo_laminar_codecs_package$() {
  if ((!$n_Lcom_raquo_laminar_codecs_package$)) {
    $n_Lcom_raquo_laminar_codecs_package$ = new $c_Lcom_raquo_laminar_codecs_package$();
  }
  return $n_Lcom_raquo_laminar_codecs_package$;
}
function $f_Lcom_raquo_laminar_defs_complex_ComplexHtmlKeys__$init$__V($thiz) {
  $thiz.Lcom_raquo_laminar_api_package$$anon$1__f_className = $f_Lcom_raquo_laminar_defs_complex_ComplexHtmlKeys__stringCompositeHtmlAttr__T__T__Lcom_raquo_laminar_keys_CompositeKey($thiz, "class", " ");
}
function $f_Lcom_raquo_laminar_defs_complex_ComplexHtmlKeys__stringCompositeHtmlAttr__T__T__Lcom_raquo_laminar_keys_CompositeKey($thiz, name, separator) {
  var attr = new $c_Lcom_raquo_laminar_keys_HtmlAttr(name, $m_Lcom_raquo_laminar_codecs_package$().Lcom_raquo_laminar_codecs_package$__f_StringAsIsCodec);
  return new $c_Lcom_raquo_laminar_keys_CompositeKey(attr.Lcom_raquo_laminar_keys_HtmlAttr__f_name, new $c_sjsr_AnonFunction1(((el) => {
    var el$1 = $as_Lcom_raquo_laminar_nodes_ReactiveHtmlElement(el);
    var x = $m_Lcom_raquo_laminar_DomApi$().getHtmlAttribute__Lcom_raquo_laminar_nodes_ReactiveHtmlElement__Lcom_raquo_laminar_keys_HtmlAttr__O(el$1, attr);
    return $as_T(((x === (void 0)) ? "" : x));
  })), new $c_sjsr_AnonFunction2(((el$2, value) => {
    var el$3 = $as_Lcom_raquo_laminar_nodes_ReactiveHtmlElement(el$2);
    var value$1 = $as_T(value);
    $m_Lcom_raquo_laminar_DomApi$().setHtmlAttribute__Lcom_raquo_laminar_nodes_ReactiveHtmlElement__Lcom_raquo_laminar_keys_HtmlAttr__O__V(el$3, attr, value$1);
  })), separator);
}
function $f_Lcom_raquo_laminar_defs_complex_ComplexSvgKeys__$init$__V($thiz) {
  $thiz.Lcom_raquo_laminar_api_Laminar$svg$__f_className = $f_Lcom_raquo_laminar_defs_complex_ComplexSvgKeys__stringCompositeSvgAttr__T__T__Lcom_raquo_laminar_keys_CompositeKey($thiz, "class", " ");
}
function $f_Lcom_raquo_laminar_defs_complex_ComplexSvgKeys__stringCompositeSvgAttr__T__T__Lcom_raquo_laminar_keys_CompositeKey($thiz, name, separator) {
  var attr = new $c_Lcom_raquo_laminar_keys_SvgAttr(name, $m_Lcom_raquo_laminar_codecs_package$().Lcom_raquo_laminar_codecs_package$__f_StringAsIsCodec, $m_s_None$());
  return new $c_Lcom_raquo_laminar_keys_CompositeKey(attr.Lcom_raquo_laminar_keys_SvgAttr__f_name, new $c_sjsr_AnonFunction1(((el) => {
    var el$1 = $as_Lcom_raquo_laminar_nodes_ReactiveSvgElement(el);
    var x = $m_Lcom_raquo_laminar_DomApi$().getSvgAttribute__Lcom_raquo_laminar_nodes_ReactiveSvgElement__Lcom_raquo_laminar_keys_SvgAttr__O(el$1, attr);
    return $as_T(((x === (void 0)) ? "" : x));
  })), new $c_sjsr_AnonFunction2(((el$2, value) => {
    var el$3 = $as_Lcom_raquo_laminar_nodes_ReactiveSvgElement(el$2);
    var value$1 = $as_T(value);
    $m_Lcom_raquo_laminar_DomApi$().setSvgAttribute__Lcom_raquo_laminar_nodes_ReactiveSvgElement__Lcom_raquo_laminar_keys_SvgAttr__O__V(el$3, attr, value$1);
  })), separator);
}
function $as_Lcom_raquo_laminar_inserters_InserterHooks(obj) {
  return ((false || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.laminar.inserters.InserterHooks"));
}
function $isArrayOf_Lcom_raquo_laminar_inserters_InserterHooks(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_laminar_inserters_InserterHooks)));
}
function $asArrayOf_Lcom_raquo_laminar_inserters_InserterHooks(obj, depth) {
  return (($isArrayOf_Lcom_raquo_laminar_inserters_InserterHooks(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.laminar.inserters.InserterHooks;", depth));
}
/** @constructor */
function $c_Lcom_raquo_laminar_keys_Key() {
}
$c_Lcom_raquo_laminar_keys_Key.prototype = new $h_O();
$c_Lcom_raquo_laminar_keys_Key.prototype.constructor = $c_Lcom_raquo_laminar_keys_Key;
/** @constructor */
function $h_Lcom_raquo_laminar_keys_Key() {
}
$h_Lcom_raquo_laminar_keys_Key.prototype = $c_Lcom_raquo_laminar_keys_Key.prototype;
/** @constructor */
function $c_Lcom_raquo_laminar_keys_SvgAttr$() {
  this.Lcom_raquo_laminar_keys_SvgAttr$__f_svgNamespaceUri = null;
  this.Lcom_raquo_laminar_keys_SvgAttr$__f_xlinkNamespaceUri = null;
  this.Lcom_raquo_laminar_keys_SvgAttr$__f_xmlNamespaceUri = null;
  this.Lcom_raquo_laminar_keys_SvgAttr$__f_xmlnsNamespaceUri = null;
  this.Lcom_raquo_laminar_keys_SvgAttr$__f_svgNamespaceUri = "http://www.w3.org/2000/svg";
  this.Lcom_raquo_laminar_keys_SvgAttr$__f_xlinkNamespaceUri = "http://www.w3.org/1999/xlink";
  this.Lcom_raquo_laminar_keys_SvgAttr$__f_xmlNamespaceUri = "http://www.w3.org/XML/1998/namespace";
  this.Lcom_raquo_laminar_keys_SvgAttr$__f_xmlnsNamespaceUri = "http://www.w3.org/2000/xmlns/";
}
$c_Lcom_raquo_laminar_keys_SvgAttr$.prototype = new $h_O();
$c_Lcom_raquo_laminar_keys_SvgAttr$.prototype.constructor = $c_Lcom_raquo_laminar_keys_SvgAttr$;
/** @constructor */
function $h_Lcom_raquo_laminar_keys_SvgAttr$() {
}
$h_Lcom_raquo_laminar_keys_SvgAttr$.prototype = $c_Lcom_raquo_laminar_keys_SvgAttr$.prototype;
$c_Lcom_raquo_laminar_keys_SvgAttr$.prototype.namespaceUri__T__T = (function(namespace) {
  switch (namespace) {
    case "svg": {
      return "http://www.w3.org/2000/svg";
      break;
    }
    case "xlink": {
      return "http://www.w3.org/1999/xlink";
      break;
    }
    case "xml": {
      return "http://www.w3.org/XML/1998/namespace";
      break;
    }
    case "xmlns": {
      return "http://www.w3.org/2000/xmlns/";
      break;
    }
    default: {
      throw new $c_s_MatchError(namespace);
    }
  }
});
var $d_Lcom_raquo_laminar_keys_SvgAttr$ = new $TypeData().initClass($c_Lcom_raquo_laminar_keys_SvgAttr$, "com.raquo.laminar.keys.SvgAttr$", ({
  Lcom_raquo_laminar_keys_SvgAttr$: 1
}));
var $n_Lcom_raquo_laminar_keys_SvgAttr$;
function $m_Lcom_raquo_laminar_keys_SvgAttr$() {
  if ((!$n_Lcom_raquo_laminar_keys_SvgAttr$)) {
    $n_Lcom_raquo_laminar_keys_SvgAttr$ = new $c_Lcom_raquo_laminar_keys_SvgAttr$();
  }
  return $n_Lcom_raquo_laminar_keys_SvgAttr$;
}
/** @constructor */
function $c_Lcom_raquo_laminar_lifecycle_MountContext(thisNode, owner) {
  this.Lcom_raquo_laminar_lifecycle_MountContext__f_thisNode = null;
  this.Lcom_raquo_laminar_lifecycle_MountContext__f_thisNode = thisNode;
}
$c_Lcom_raquo_laminar_lifecycle_MountContext.prototype = new $h_O();
$c_Lcom_raquo_laminar_lifecycle_MountContext.prototype.constructor = $c_Lcom_raquo_laminar_lifecycle_MountContext;
/** @constructor */
function $h_Lcom_raquo_laminar_lifecycle_MountContext() {
}
$h_Lcom_raquo_laminar_lifecycle_MountContext.prototype = $c_Lcom_raquo_laminar_lifecycle_MountContext.prototype;
function $as_Lcom_raquo_laminar_lifecycle_MountContext(obj) {
  return (((obj instanceof $c_Lcom_raquo_laminar_lifecycle_MountContext) || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.laminar.lifecycle.MountContext"));
}
function $isArrayOf_Lcom_raquo_laminar_lifecycle_MountContext(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_laminar_lifecycle_MountContext)));
}
function $asArrayOf_Lcom_raquo_laminar_lifecycle_MountContext(obj, depth) {
  return (($isArrayOf_Lcom_raquo_laminar_lifecycle_MountContext(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.laminar.lifecycle.MountContext;", depth));
}
var $d_Lcom_raquo_laminar_lifecycle_MountContext = new $TypeData().initClass($c_Lcom_raquo_laminar_lifecycle_MountContext, "com.raquo.laminar.lifecycle.MountContext", ({
  Lcom_raquo_laminar_lifecycle_MountContext: 1
}));
function $is_Lcom_raquo_laminar_modifiers_Modifier(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lcom_raquo_laminar_modifiers_Modifier)));
}
function $as_Lcom_raquo_laminar_modifiers_Modifier(obj) {
  return (($is_Lcom_raquo_laminar_modifiers_Modifier(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.laminar.modifiers.Modifier"));
}
function $isArrayOf_Lcom_raquo_laminar_modifiers_Modifier(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_laminar_modifiers_Modifier)));
}
function $asArrayOf_Lcom_raquo_laminar_modifiers_Modifier(obj, depth) {
  return (($isArrayOf_Lcom_raquo_laminar_modifiers_Modifier(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.laminar.modifiers.Modifier;", depth));
}
var $d_Lcom_raquo_laminar_modifiers_Modifier = new $TypeData().initClass(1, "com.raquo.laminar.modifiers.Modifier", ({
  Lcom_raquo_laminar_modifiers_Modifier: 1
}));
/** @constructor */
function $c_Lcom_raquo_laminar_modifiers_Modifier$() {
  this.Lcom_raquo_laminar_modifiers_Modifier$__f_empty = null;
  $n_Lcom_raquo_laminar_modifiers_Modifier$ = this;
  this.Lcom_raquo_laminar_modifiers_Modifier$__f_empty = new $c_Lcom_raquo_laminar_modifiers_Modifier$$anon$1();
}
$c_Lcom_raquo_laminar_modifiers_Modifier$.prototype = new $h_O();
$c_Lcom_raquo_laminar_modifiers_Modifier$.prototype.constructor = $c_Lcom_raquo_laminar_modifiers_Modifier$;
/** @constructor */
function $h_Lcom_raquo_laminar_modifiers_Modifier$() {
}
$h_Lcom_raquo_laminar_modifiers_Modifier$.prototype = $c_Lcom_raquo_laminar_modifiers_Modifier$.prototype;
var $d_Lcom_raquo_laminar_modifiers_Modifier$ = new $TypeData().initClass($c_Lcom_raquo_laminar_modifiers_Modifier$, "com.raquo.laminar.modifiers.Modifier$", ({
  Lcom_raquo_laminar_modifiers_Modifier$: 1
}));
var $n_Lcom_raquo_laminar_modifiers_Modifier$;
function $m_Lcom_raquo_laminar_modifiers_Modifier$() {
  if ((!$n_Lcom_raquo_laminar_modifiers_Modifier$)) {
    $n_Lcom_raquo_laminar_modifiers_Modifier$ = new $c_Lcom_raquo_laminar_modifiers_Modifier$();
  }
  return $n_Lcom_raquo_laminar_modifiers_Modifier$;
}
/** @constructor */
function $c_Lcom_raquo_laminar_modifiers_RenderableText$() {
  this.Lcom_raquo_laminar_modifiers_RenderableText$__f_stringRenderable = null;
  $n_Lcom_raquo_laminar_modifiers_RenderableText$ = this;
  var this$3 = $m_Lcom_raquo_laminar_modifiers_RenderableText$();
  var render = new $c_sjsr_AnonFunction1(((x) => {
    var x$1 = $as_T(x);
    return x$1;
  }));
  this.Lcom_raquo_laminar_modifiers_RenderableText$__f_stringRenderable = new $c_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1(render, this$3);
  var this$5 = $m_Lcom_raquo_laminar_modifiers_RenderableText$();
  var render$1 = new $c_sjsr_AnonFunction1(((_$1) => {
    var _$1$1 = $uI(_$1);
    return ("" + _$1$1);
  }));
  new $c_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1(render$1, this$5);
  var this$7 = $m_Lcom_raquo_laminar_modifiers_RenderableText$();
  var render$2 = new $c_sjsr_AnonFunction1(((_$2) => {
    var _$2$1 = $uD(_$2);
    return ("" + _$2$1);
  }));
  new $c_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1(render$2, this$7);
  var this$9 = $m_Lcom_raquo_laminar_modifiers_RenderableText$();
  var render$3 = new $c_sjsr_AnonFunction1(((_$3) => {
    var _$3$1 = $uZ(_$3);
    return ("" + _$3$1);
  }));
  new $c_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1(render$3, this$9);
  var this$10 = $m_Lcom_raquo_laminar_modifiers_RenderableText$();
  var render$4 = new $c_sjsr_AnonFunction1(((_$4) => {
    var _$4$1 = $as_Lcom_raquo_laminar_nodes_TextNode(_$4);
    return $n(_$4$1).text__T();
  }));
  new $c_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1(render$4, this$10);
}
$c_Lcom_raquo_laminar_modifiers_RenderableText$.prototype = new $h_O();
$c_Lcom_raquo_laminar_modifiers_RenderableText$.prototype.constructor = $c_Lcom_raquo_laminar_modifiers_RenderableText$;
/** @constructor */
function $h_Lcom_raquo_laminar_modifiers_RenderableText$() {
}
$h_Lcom_raquo_laminar_modifiers_RenderableText$.prototype = $c_Lcom_raquo_laminar_modifiers_RenderableText$.prototype;
var $d_Lcom_raquo_laminar_modifiers_RenderableText$ = new $TypeData().initClass($c_Lcom_raquo_laminar_modifiers_RenderableText$, "com.raquo.laminar.modifiers.RenderableText$", ({
  Lcom_raquo_laminar_modifiers_RenderableText$: 1
}));
var $n_Lcom_raquo_laminar_modifiers_RenderableText$;
function $m_Lcom_raquo_laminar_modifiers_RenderableText$() {
  if ((!$n_Lcom_raquo_laminar_modifiers_RenderableText$)) {
    $n_Lcom_raquo_laminar_modifiers_RenderableText$ = new $c_Lcom_raquo_laminar_modifiers_RenderableText$();
  }
  return $n_Lcom_raquo_laminar_modifiers_RenderableText$;
}
/** @constructor */
function $c_Lcom_raquo_laminar_nodes_ParentNode$() {
}
$c_Lcom_raquo_laminar_nodes_ParentNode$.prototype = new $h_O();
$c_Lcom_raquo_laminar_nodes_ParentNode$.prototype.constructor = $c_Lcom_raquo_laminar_nodes_ParentNode$;
/** @constructor */
function $h_Lcom_raquo_laminar_nodes_ParentNode$() {
}
$h_Lcom_raquo_laminar_nodes_ParentNode$.prototype = $c_Lcom_raquo_laminar_nodes_ParentNode$.prototype;
$c_Lcom_raquo_laminar_nodes_ParentNode$.prototype.appendChild__Lcom_raquo_laminar_nodes_ParentNode__Lcom_raquo_laminar_nodes_ChildNode__O__Z = (function(parent, child, hooks) {
  var nextParent = new $c_s_Some(parent);
  $n(child).willSetParent__s_Option__V(nextParent);
  if ((hooks !== (void 0))) {
    var _$1 = $as_Lcom_raquo_laminar_inserters_InserterHooks(hooks);
    $n(_$1).onWillInsertNode__Lcom_raquo_laminar_nodes_ParentNode__Lcom_raquo_laminar_nodes_ChildNode__V(parent, child);
  }
  var appended = $m_Lcom_raquo_laminar_DomApi$().appendChild__Lorg_scalajs_dom_Node__Lorg_scalajs_dom_Node__Z($n(parent).ref__Lorg_scalajs_dom_Node(), $n(child).ref__Lorg_scalajs_dom_Node());
  if (appended) {
    $n(child).setParent__s_Option__V(nextParent);
  }
  return appended;
});
var $d_Lcom_raquo_laminar_nodes_ParentNode$ = new $TypeData().initClass($c_Lcom_raquo_laminar_nodes_ParentNode$, "com.raquo.laminar.nodes.ParentNode$", ({
  Lcom_raquo_laminar_nodes_ParentNode$: 1
}));
var $n_Lcom_raquo_laminar_nodes_ParentNode$;
function $m_Lcom_raquo_laminar_nodes_ParentNode$() {
  if ((!$n_Lcom_raquo_laminar_nodes_ParentNode$)) {
    $n_Lcom_raquo_laminar_nodes_ParentNode$ = new $c_Lcom_raquo_laminar_nodes_ParentNode$();
  }
  return $n_Lcom_raquo_laminar_nodes_ParentNode$;
}
/** @constructor */
function $c_jl_FloatingPointBits$() {
  this.jl_FloatingPointBits$__f_java$lang$FloatingPointBits$$_areTypedArraysSupported = false;
  this.jl_FloatingPointBits$__f_arrayBuffer = null;
  this.jl_FloatingPointBits$__f_int32Array = null;
  this.jl_FloatingPointBits$__f_float64Array = null;
  this.jl_FloatingPointBits$__f_areTypedArraysBigEndian = false;
  this.jl_FloatingPointBits$__f_highOffset = 0;
  this.jl_FloatingPointBits$__f_lowOffset = 0;
  this.jl_FloatingPointBits$__f_java$lang$FloatingPointBits$$doublePowsOf2 = null;
  $n_jl_FloatingPointBits$ = this;
  this.jl_FloatingPointBits$__f_java$lang$FloatingPointBits$$_areTypedArraysSupported = true;
  this.jl_FloatingPointBits$__f_arrayBuffer = new ArrayBuffer(8);
  this.jl_FloatingPointBits$__f_int32Array = new Int32Array(this.jl_FloatingPointBits$__f_arrayBuffer, 0, 2);
  new Float32Array(this.jl_FloatingPointBits$__f_arrayBuffer, 0, 2);
  this.jl_FloatingPointBits$__f_float64Array = new Float64Array(this.jl_FloatingPointBits$__f_arrayBuffer, 0, 1);
  this.jl_FloatingPointBits$__f_int32Array[0] = 16909060;
  this.jl_FloatingPointBits$__f_areTypedArraysBigEndian = ($uB(new Int8Array(this.jl_FloatingPointBits$__f_arrayBuffer, 0, 8)[0]) === 1);
  this.jl_FloatingPointBits$__f_highOffset = (this.jl_FloatingPointBits$__f_areTypedArraysBigEndian ? 0 : 1);
  this.jl_FloatingPointBits$__f_lowOffset = (this.jl_FloatingPointBits$__f_areTypedArraysBigEndian ? 1 : 0);
  this.jl_FloatingPointBits$__f_java$lang$FloatingPointBits$$doublePowsOf2 = null;
}
$c_jl_FloatingPointBits$.prototype = new $h_O();
$c_jl_FloatingPointBits$.prototype.constructor = $c_jl_FloatingPointBits$;
/** @constructor */
function $h_jl_FloatingPointBits$() {
}
$h_jl_FloatingPointBits$.prototype = $c_jl_FloatingPointBits$.prototype;
$c_jl_FloatingPointBits$.prototype.numberHashCode__D__I = (function(value) {
  var iv = $uI((value | 0.0));
  if (((iv === value) && ((1.0 / value) !== (-Infinity)))) {
    return iv;
  } else {
    this.jl_FloatingPointBits$__f_float64Array[0] = value;
    return ($uI(this.jl_FloatingPointBits$__f_int32Array[0]) ^ $uI(this.jl_FloatingPointBits$__f_int32Array[1]));
  }
});
var $d_jl_FloatingPointBits$ = new $TypeData().initClass($c_jl_FloatingPointBits$, "java.lang.FloatingPointBits$", ({
  jl_FloatingPointBits$: 1
}));
var $n_jl_FloatingPointBits$;
function $m_jl_FloatingPointBits$() {
  if ((!$n_jl_FloatingPointBits$)) {
    $n_jl_FloatingPointBits$ = new $c_jl_FloatingPointBits$();
  }
  return $n_jl_FloatingPointBits$;
}
function $p_jl_StackTrace$__normalizedLinesToStackTrace__O__Ajl_StackTraceElement($thiz, lines) {
  var NormalizedFrameLine = $m_jl_StackTrace$StringRE$().re$extension0__T__O("^([^@]*)@(.*?):([0-9]+)(?::([0-9]+))?$");
  var trace = [];
  var i = 0;
  while ((i < $uI(lines.length))) {
    var line = $as_T(lines[i]);
    var this$1 = $n(line);
    if ((!(this$1 === ""))) {
      var mtch = NormalizedFrameLine.exec(line);
      if ((mtch !== null)) {
        var x = mtch[1];
        var classAndMethodName = $p_jl_StackTrace$__extractClassMethod__T__O($thiz, $as_T(x));
        var $x_6 = $as_T(classAndMethodName[0]);
        var $x_5 = $as_T(classAndMethodName[1]);
        var x$1 = mtch[2];
        var $x_4 = $as_T(x$1);
        var x$2 = mtch[3];
        var s = $as_T(x$2);
        var $x_3 = $uI(parseInt(s));
        var x$3 = mtch[4];
        if ((x$3 !== (void 0))) {
          var t = $as_T(x$3);
          var $x_2 = $uI(parseInt(t));
        } else {
          var $x_2 = (-1);
        }
        var $x_1 = trace.push(new $c_jl_StackTraceElement($x_6, $x_5, $x_4, $x_3, $x_2));
        $uI($x_1);
      } else {
        $uI(trace.push(new $c_jl_StackTraceElement("<jscode>", line, null, (-1), (-1))));
      }
    }
    i = ((1 + i) | 0);
  }
  var len = $uI(trace.length);
  var result = new ($d_jl_StackTraceElement.getArrayOf().constr)(len);
  i = 0;
  while ((i < len)) {
    result.set(i, $as_jl_StackTraceElement(trace[i]));
    i = ((1 + i) | 0);
  }
  return result;
}
function $p_jl_StackTrace$__extractClassMethod__T__O($thiz, functionName) {
  var PatBC = $m_jl_StackTrace$StringRE$().re$extension0__T__O("^(?:Object\\.|\\[object Object\\]\\.|Module\\.)?\\$[bc]_([^\\.]+)(?:\\.prototype)?\\.([^\\.]+)$");
  var PatS = $m_jl_StackTrace$StringRE$().re$extension0__T__O("^(?:Object\\.|\\[object Object\\]\\.|Module\\.)?\\$(?:ps?|s|f)_((?:_[^_]|[^_])+)__([^\\.]+)$");
  var PatCT = $m_jl_StackTrace$StringRE$().re$extension0__T__O("^(?:Object\\.|\\[object Object\\]\\.|Module\\.)?\\$ct_((?:_[^_]|[^_])+)__([^\\.]*)$");
  var PatN = $m_jl_StackTrace$StringRE$().re$extension0__T__O("^new (?:Object\\.|\\[object Object\\]\\.|Module\\.)?\\$c_([^\\.]+)$");
  var PatM = $m_jl_StackTrace$StringRE$().re$extension0__T__O("^(?:Object\\.|\\[object Object\\]\\.|Module\\.)?\\$m_([^\\.]+)$");
  var matchBC = PatBC.exec(functionName);
  var matchBCOrS = ((matchBC !== null) ? matchBC : PatS.exec(functionName));
  if ((matchBCOrS !== null)) {
    var x = matchBCOrS[1];
    var $x_1 = $p_jl_StackTrace$__decodeClassName__T__T($thiz, $as_T(x));
    var x$1 = matchBCOrS[2];
    return [$x_1, $p_jl_StackTrace$__decodeMethodName__T__T($thiz, $as_T(x$1))];
  } else {
    var matchCT = PatCT.exec(functionName);
    var matchCTOrN = ((matchCT !== null) ? matchCT : PatN.exec(functionName));
    if ((matchCTOrN !== null)) {
      var x$2 = matchCTOrN[1];
      return [$p_jl_StackTrace$__decodeClassName__T__T($thiz, $as_T(x$2)), "<init>"];
    } else {
      var matchM = PatM.exec(functionName);
      if ((matchM !== null)) {
        var x$3 = matchM[1];
        return [$p_jl_StackTrace$__decodeClassName__T__T($thiz, $as_T(x$3)), "<clinit>"];
      } else {
        return ["<jscode>", functionName];
      }
    }
  }
}
function $p_jl_StackTrace$__decodeClassName__T__T($thiz, encodedName) {
  var dict = $p_jl_StackTrace$__decompressedClasses__O($thiz);
  if ($uZ($m_jl_Utils$Cache$().jl_Utils$Cache$__f_safeHasOwnProperty.call(dict, encodedName))) {
    var dict$1 = $p_jl_StackTrace$__decompressedClasses__O($thiz);
    var base = $as_T(dict$1[encodedName]);
  } else {
    var base = $p_jl_StackTrace$__loop$1__I__T__T($thiz, 0, encodedName);
  }
  var this$3 = $n(base);
  var this$4 = $n($as_T(this$3.split("_").join(".")));
  return $as_T(this$4.split("\uff3f").join("_"));
}
function $p_jl_StackTrace$__decompressedClasses$lzycompute__O($thiz) {
  if (((((1 & $thiz.jl_StackTrace$__f_bitmap$0) << 24) >> 24) === 0)) {
    var dict = ({});
    dict.O = "java_lang_Object";
    dict.T = "java_lang_String";
    var index = 0;
    while ((index <= 22)) {
      if ((index >= 2)) {
        var key = ("T" + index);
        var value = ("scala_Tuple" + index);
        dict[key] = value;
      }
      var key$1 = ("F" + index);
      var value$1 = ("scala_Function" + index);
      dict[key$1] = value$1;
      index = ((1 + index) | 0);
    }
    $thiz.jl_StackTrace$__f_decompressedClasses = dict;
    $thiz.jl_StackTrace$__f_bitmap$0 = (((1 | $thiz.jl_StackTrace$__f_bitmap$0) << 24) >> 24);
  }
  return $thiz.jl_StackTrace$__f_decompressedClasses;
}
function $p_jl_StackTrace$__decompressedClasses__O($thiz) {
  return (((((1 & $thiz.jl_StackTrace$__f_bitmap$0) << 24) >> 24) === 0) ? $p_jl_StackTrace$__decompressedClasses$lzycompute__O($thiz) : $thiz.jl_StackTrace$__f_decompressedClasses);
}
function $p_jl_StackTrace$__decompressedPrefixes$lzycompute__O($thiz) {
  if (((((2 & $thiz.jl_StackTrace$__f_bitmap$0) << 24) >> 24) === 0)) {
    var dict = ({});
    dict.sjsr_ = "scala_scalajs_runtime_";
    dict.sjs_ = "scala_scalajs_";
    dict.sci_ = "scala_collection_immutable_";
    dict.scm_ = "scala_collection_mutable_";
    dict.scg_ = "scala_collection_generic_";
    dict.sc_ = "scala_collection_";
    dict.sr_ = "scala_runtime_";
    dict.s_ = "scala_";
    dict.jl_ = "java_lang_";
    dict.ju_ = "java_util_";
    $thiz.jl_StackTrace$__f_decompressedPrefixes = dict;
    $thiz.jl_StackTrace$__f_bitmap$0 = (((2 | $thiz.jl_StackTrace$__f_bitmap$0) << 24) >> 24);
  }
  return $thiz.jl_StackTrace$__f_decompressedPrefixes;
}
function $p_jl_StackTrace$__decompressedPrefixes__O($thiz) {
  return (((((2 & $thiz.jl_StackTrace$__f_bitmap$0) << 24) >> 24) === 0) ? $p_jl_StackTrace$__decompressedPrefixes$lzycompute__O($thiz) : $thiz.jl_StackTrace$__f_decompressedPrefixes);
}
function $p_jl_StackTrace$__compressedPrefixes$lzycompute__O($thiz) {
  if (((((4 & $thiz.jl_StackTrace$__f_bitmap$0) << 24) >> 24) === 0)) {
    $thiz.jl_StackTrace$__f_compressedPrefixes = Object.keys($p_jl_StackTrace$__decompressedPrefixes__O($thiz));
    $thiz.jl_StackTrace$__f_bitmap$0 = (((4 | $thiz.jl_StackTrace$__f_bitmap$0) << 24) >> 24);
  }
  return $thiz.jl_StackTrace$__f_compressedPrefixes;
}
function $p_jl_StackTrace$__compressedPrefixes__O($thiz) {
  return (((((4 & $thiz.jl_StackTrace$__f_bitmap$0) << 24) >> 24) === 0) ? $p_jl_StackTrace$__compressedPrefixes$lzycompute__O($thiz) : $thiz.jl_StackTrace$__f_compressedPrefixes);
}
function $p_jl_StackTrace$__decodeMethodName__T__T($thiz, encodedName) {
  var this$1 = $n(encodedName);
  if ($uZ(this$1.startsWith("init___"))) {
    return "<init>";
  } else {
    var this$2 = $n(encodedName);
    var methodNameLen = $uI(this$2.indexOf("__"));
    return ((methodNameLen < 0) ? encodedName : $as_T(encodedName.substring(0, methodNameLen)));
  }
}
function $p_jl_StackTrace$__normalizeStackTraceLines__O__O($thiz, e) {
  return ($uZ((!(!(!e)))) ? [] : ($uZ((!(!(e.arguments && e.stack)))) ? $p_jl_StackTrace$__extractChrome__O__O($thiz, e) : ($uZ((!(!(e.stack && e.sourceURL)))) ? $p_jl_StackTrace$__extractSafari__O__O($thiz, e) : ($uZ((!(!(e.stack && e.number)))) ? $p_jl_StackTrace$__extractIE__O__O($thiz, e) : ($uZ((!(!(e.stack && e.fileName)))) ? $p_jl_StackTrace$__extractFirefox__O__O($thiz, e) : ($uZ((!(!(e.message && e["opera#sourceloc"])))) ? ($uZ((!(!(!e.stacktrace)))) ? $p_jl_StackTrace$__extractOpera9__O__O($thiz, e) : ($uZ((!(!((e.message.indexOf("\n") > (-1.0)) && (e.message.split("\n").length > e.stacktrace.split("\n").length))))) ? $p_jl_StackTrace$__extractOpera9__O__O($thiz, e) : $p_jl_StackTrace$__extractOpera10a__O__O($thiz, e))) : ($uZ((!(!((e.message && e.stack) && e.stacktrace)))) ? ($uZ((!(!(e.stacktrace.indexOf("called from line") < 0.0)))) ? $p_jl_StackTrace$__extractOpera10b__O__O($thiz, e) : $p_jl_StackTrace$__extractOpera11__O__O($thiz, e)) : ($uZ((!(!(e.stack && (!e.fileName))))) ? $p_jl_StackTrace$__extractChrome__O__O($thiz, e) : $p_jl_StackTrace$__extractOther__O__O($thiz, e)))))))));
}
function $p_jl_StackTrace$__extractChrome__O__O($thiz, e) {
  return $as_T($as_T($as_T($as_T($as_T(($as_T(e.stack) + "\n").replace($m_jl_StackTrace$StringRE$().re$extension0__T__O("^[\\s\\S]+?\\s+at\\s+"), " at ")).replace($m_jl_StackTrace$StringRE$().re$extension1__T__T__O("^\\s+(at eval )?at\\s+", "gm"), "")).replace($m_jl_StackTrace$StringRE$().re$extension1__T__T__O("^([^\\(]+?)([\\n])", "gm"), "{anonymous}() ($1)$2")).replace($m_jl_StackTrace$StringRE$().re$extension1__T__T__O("^Object.<anonymous>\\s*\\(([^\\)]+)\\)", "gm"), "{anonymous}() ($1)")).replace($m_jl_StackTrace$StringRE$().re$extension1__T__T__O("^([^\\(]+|\\{anonymous\\}\\(\\)) \\((.+)\\)$", "gm"), "$1@$2")).split("\n").slice(0, (-1));
}
function $p_jl_StackTrace$__extractFirefox__O__O($thiz, e) {
  return $as_T($as_T($as_T(e.stack).replace($m_jl_StackTrace$StringRE$().re$extension1__T__T__O("(?:\\n@:0)?\\s+$", "m"), "")).replace($m_jl_StackTrace$StringRE$().re$extension1__T__T__O("^(?:\\((\\S*)\\))?@", "gm"), "{anonymous}($1)@")).split("\n");
}
function $p_jl_StackTrace$__extractIE__O__O($thiz, e) {
  var qual$1 = $as_T($as_T($as_T($as_T(e.stack).replace($m_jl_StackTrace$StringRE$().re$extension1__T__T__O("^\\s*at\\s+(.*)$", "gm"), "$1")).replace($m_jl_StackTrace$StringRE$().re$extension1__T__T__O("^Anonymous function\\s+", "gm"), "{anonymous}() ")).replace($m_jl_StackTrace$StringRE$().re$extension1__T__T__O("^([^\\(]+|\\{anonymous\\}\\(\\))\\s+\\((.+)\\)$", "gm"), "$1@$2")).split("\n");
  return qual$1.slice(1);
}
function $p_jl_StackTrace$__extractSafari__O__O($thiz, e) {
  return $as_T($as_T($as_T($as_T(e.stack).replace($m_jl_StackTrace$StringRE$().re$extension1__T__T__O("\\[native code\\]\\n", "m"), "")).replace($m_jl_StackTrace$StringRE$().re$extension1__T__T__O("^(?=\\w+Error\\:).*$\\n", "m"), "")).replace($m_jl_StackTrace$StringRE$().re$extension1__T__T__O("^@", "gm"), "{anonymous}()@")).split("\n");
}
function $p_jl_StackTrace$__extractOpera9__O__O($thiz, e) {
  var lineRE = $m_jl_StackTrace$StringRE$().re$extension1__T__T__O("Line (\\d+).*script (?:in )?(\\S+)", "i");
  var lines = $as_T(e.message).split("\n");
  var result = [];
  var i = 2;
  var len = $uI(lines.length);
  while ((i < len)) {
    var mtch = lineRE.exec($as_T(lines[i]));
    if ((mtch !== null)) {
      var x = mtch[2];
      var x$1 = mtch[1];
      var $x_1 = result.push(((("{anonymous}()@" + x) + ":") + x$1));
      $uI($x_1);
    }
    i = ((2 + i) | 0);
  }
  return result;
}
function $p_jl_StackTrace$__extractOpera10a__O__O($thiz, e) {
  var lineRE = $m_jl_StackTrace$StringRE$().re$extension1__T__T__O("Line (\\d+).*script (?:in )?(\\S+)(?:: In function (\\S+))?$", "i");
  var lines = $as_T(e.stacktrace).split("\n");
  var result = [];
  var i = 0;
  var len = $uI(lines.length);
  while ((i < len)) {
    var mtch = lineRE.exec($as_T(lines[i]));
    if ((mtch !== null)) {
      var x = mtch[3];
      var fnName = $as_T(((x !== (void 0)) ? x : "{anonymous}"));
      var x$1 = mtch[2];
      var x$2 = mtch[1];
      var $x_1 = result.push(((((fnName + "()@") + x$1) + ":") + x$2));
      $uI($x_1);
    }
    i = ((2 + i) | 0);
  }
  return result;
}
function $p_jl_StackTrace$__extractOpera10b__O__O($thiz, e) {
  var lineRE = $m_jl_StackTrace$StringRE$().re$extension0__T__O("^(.*)@(.+):(\\d+)$");
  var lines = $as_T(e.stacktrace).split("\n");
  var result = [];
  var i = 0;
  var len = $uI(lines.length);
  while ((i < len)) {
    var mtch = lineRE.exec($as_T(lines[i]));
    if ((mtch !== null)) {
      var x = mtch[1];
      if ((x !== (void 0))) {
        var t = $as_T(x);
        var fnName = (t + "()");
      } else {
        var fnName = "global code";
      }
      var x$1 = mtch[2];
      var x$2 = mtch[3];
      var $x_1 = result.push(((((fnName + "@") + x$1) + ":") + x$2));
      $uI($x_1);
    }
    i = ((1 + i) | 0);
  }
  return result;
}
function $p_jl_StackTrace$__extractOpera11__O__O($thiz, e) {
  var lineRE = $m_jl_StackTrace$StringRE$().re$extension0__T__O("^.*line (\\d+), column (\\d+)(?: in (.+))? in (\\S+):$");
  var lines = $as_T(e.stacktrace).split("\n");
  var result = [];
  var i = 0;
  var len = $uI(lines.length);
  while ((i < len)) {
    var mtch = lineRE.exec($as_T(lines[i]));
    if ((mtch !== null)) {
      var x = mtch[4];
      var $x_1 = $as_T(x);
      var x$1 = mtch[1];
      var x$2 = mtch[2];
      var location = (((($x_1 + ":") + x$1) + ":") + x$2);
      var x$3 = mtch[2];
      var fnName0 = $as_T(((x$3 !== (void 0)) ? x$3 : "global code"));
      var fnName = $as_T($as_T(fnName0.replace($m_jl_StackTrace$StringRE$().re$extension0__T__O("<anonymous function: (\\S+)>"), "$1")).replace($m_jl_StackTrace$StringRE$().re$extension0__T__O("<anonymous function>"), "{anonymous}"));
      $uI(result.push(((fnName + "@") + location)));
    }
    i = ((2 + i) | 0);
  }
  return result;
}
function $p_jl_StackTrace$__extractOther__O__O($thiz, e) {
  return [];
}
function $p_jl_StackTrace$__loop$1__I__T__T($thiz, i, encodedName$1) {
  while (true) {
    if ((i < $uI($p_jl_StackTrace$__compressedPrefixes__O($thiz).length))) {
      var prefix = $as_T($p_jl_StackTrace$__compressedPrefixes__O($thiz)[i]);
      var this$1 = $n(encodedName$1);
      $n(prefix);
      if ($uZ(this$1.startsWith(prefix))) {
        var dict = $p_jl_StackTrace$__decompressedPrefixes__O($thiz);
        var $x_2 = $as_T(dict[prefix]);
        var this$4 = $n(prefix);
        var $x_1 = encodedName$1.substring(this$4.length);
        return (("" + $x_2) + $as_T($x_1));
      } else {
        i = ((1 + i) | 0);
      }
    } else {
      var this$5 = $n(encodedName$1);
      if ($uZ(this$5.startsWith("L"))) {
        return $as_T(encodedName$1.substring(1));
      } else {
        return encodedName$1;
      }
    }
  }
}
/** @constructor */
function $c_jl_StackTrace$() {
  this.jl_StackTrace$__f_decompressedClasses = null;
  this.jl_StackTrace$__f_decompressedPrefixes = null;
  this.jl_StackTrace$__f_compressedPrefixes = null;
  this.jl_StackTrace$__f_bitmap$0 = 0;
}
$c_jl_StackTrace$.prototype = new $h_O();
$c_jl_StackTrace$.prototype.constructor = $c_jl_StackTrace$;
/** @constructor */
function $h_jl_StackTrace$() {
}
$h_jl_StackTrace$.prototype = $c_jl_StackTrace$.prototype;
$c_jl_StackTrace$.prototype.extract__O__Ajl_StackTraceElement = (function(jsError) {
  var lines = $p_jl_StackTrace$__normalizeStackTraceLines__O__O(this, jsError);
  return $p_jl_StackTrace$__normalizedLinesToStackTrace__O__Ajl_StackTraceElement(this, lines);
});
var $d_jl_StackTrace$ = new $TypeData().initClass($c_jl_StackTrace$, "java.lang.StackTrace$", ({
  jl_StackTrace$: 1
}));
var $n_jl_StackTrace$;
function $m_jl_StackTrace$() {
  if ((!$n_jl_StackTrace$)) {
    $n_jl_StackTrace$ = new $c_jl_StackTrace$();
  }
  return $n_jl_StackTrace$;
}
/** @constructor */
function $c_jl_StackTrace$StringRE$() {
}
$c_jl_StackTrace$StringRE$.prototype = new $h_O();
$c_jl_StackTrace$StringRE$.prototype.constructor = $c_jl_StackTrace$StringRE$;
/** @constructor */
function $h_jl_StackTrace$StringRE$() {
}
$h_jl_StackTrace$StringRE$.prototype = $c_jl_StackTrace$StringRE$.prototype;
$c_jl_StackTrace$StringRE$.prototype.re$extension0__T__O = (function(this$) {
  return new RegExp(this$);
});
$c_jl_StackTrace$StringRE$.prototype.re$extension1__T__T__O = (function(this$, mods) {
  return new RegExp(this$, mods);
});
var $d_jl_StackTrace$StringRE$ = new $TypeData().initClass($c_jl_StackTrace$StringRE$, "java.lang.StackTrace$StringRE$", ({
  jl_StackTrace$StringRE$: 1
}));
var $n_jl_StackTrace$StringRE$;
function $m_jl_StackTrace$StringRE$() {
  if ((!$n_jl_StackTrace$StringRE$)) {
    $n_jl_StackTrace$StringRE$ = new $c_jl_StackTrace$StringRE$();
  }
  return $n_jl_StackTrace$StringRE$;
}
/** @constructor */
function $c_jl_Utils$Cache$() {
  this.jl_Utils$Cache$__f_safeHasOwnProperty = null;
  $n_jl_Utils$Cache$ = this;
  this.jl_Utils$Cache$__f_safeHasOwnProperty = Object.prototype.hasOwnProperty;
}
$c_jl_Utils$Cache$.prototype = new $h_O();
$c_jl_Utils$Cache$.prototype.constructor = $c_jl_Utils$Cache$;
/** @constructor */
function $h_jl_Utils$Cache$() {
}
$h_jl_Utils$Cache$.prototype = $c_jl_Utils$Cache$.prototype;
var $d_jl_Utils$Cache$ = new $TypeData().initClass($c_jl_Utils$Cache$, "java.lang.Utils$Cache$", ({
  jl_Utils$Cache$: 1
}));
var $n_jl_Utils$Cache$;
function $m_jl_Utils$Cache$() {
  if ((!$n_jl_Utils$Cache$)) {
    $n_jl_Utils$Cache$ = new $c_jl_Utils$Cache$();
  }
  return $n_jl_Utils$Cache$;
}
function $f_jl_Void__equals__O__Z($thiz, that) {
  return ($thiz === that);
}
function $f_jl_Void__hashCode__I($thiz) {
  return 0;
}
function $f_jl_Void__toString__T($thiz) {
  return "undefined";
}
function $as_jl_Void(obj) {
  return (((obj === (void 0)) || (obj === null)) ? obj : $throwClassCastException(obj, "java.lang.Void"));
}
function $isArrayOf_jl_Void(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Void)));
}
function $asArrayOf_jl_Void(obj, depth) {
  return (($isArrayOf_jl_Void(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Void;", depth));
}
var $d_jl_Void = new $TypeData().initClass(0, "java.lang.Void", ({
  jl_Void: 1
}), ((x) => (x === (void 0))));
function $p_jl_reflect_Array$__mismatch__O__E($thiz, array) {
  $n(array);
  throw new $c_jl_IllegalArgumentException("argument type mismatch");
}
/** @constructor */
function $c_jl_reflect_Array$() {
}
$c_jl_reflect_Array$.prototype = new $h_O();
$c_jl_reflect_Array$.prototype.constructor = $c_jl_reflect_Array$;
/** @constructor */
function $h_jl_reflect_Array$() {
}
$h_jl_reflect_Array$.prototype = $c_jl_reflect_Array$.prototype;
$c_jl_reflect_Array$.prototype.getLength__O__I = (function(array) {
  if ((array instanceof $ac_O)) {
    var x2 = $asArrayOf_O(array, 1);
    return $n(x2).u.length;
  } else if ((array instanceof $ac_Z)) {
    var x3 = $asArrayOf_Z(array, 1);
    return $n(x3).u.length;
  } else if ((array instanceof $ac_C)) {
    var x4 = $asArrayOf_C(array, 1);
    return $n(x4).u.length;
  } else if ((array instanceof $ac_B)) {
    var x5 = $asArrayOf_B(array, 1);
    return $n(x5).u.length;
  } else if ((array instanceof $ac_S)) {
    var x6 = $asArrayOf_S(array, 1);
    return $n(x6).u.length;
  } else if ((array instanceof $ac_I)) {
    var x7 = $asArrayOf_I(array, 1);
    return $n(x7).u.length;
  } else if ((array instanceof $ac_J)) {
    var x8 = $asArrayOf_J(array, 1);
    return $n(x8).u.length;
  } else if ((array instanceof $ac_F)) {
    var x9 = $asArrayOf_F(array, 1);
    return $n(x9).u.length;
  } else if ((array instanceof $ac_D)) {
    var x10 = $asArrayOf_D(array, 1);
    return $n(x10).u.length;
  } else {
    $p_jl_reflect_Array$__mismatch__O__E(this, array);
  }
});
var $d_jl_reflect_Array$ = new $TypeData().initClass($c_jl_reflect_Array$, "java.lang.reflect.Array$", ({
  jl_reflect_Array$: 1
}));
var $n_jl_reflect_Array$;
function $m_jl_reflect_Array$() {
  if ((!$n_jl_reflect_Array$)) {
    $n_jl_reflect_Array$ = new $c_jl_reflect_Array$();
  }
  return $n_jl_reflect_Array$;
}
/** @constructor */
function $c_ju_Arrays$() {
}
$c_ju_Arrays$.prototype = new $h_O();
$c_ju_Arrays$.prototype.constructor = $c_ju_Arrays$;
/** @constructor */
function $h_ju_Arrays$() {
}
$h_ju_Arrays$.prototype = $c_ju_Arrays$.prototype;
$c_ju_Arrays$.prototype.equals__AJ__AJ__Z = (function(a, b) {
  if ((a === b)) {
    return true;
  }
  if (((a === null) || (b === null))) {
    return false;
  }
  var len = $n(a).u.length;
  if (($n(b).u.length !== len)) {
    return false;
  }
  var i = 0;
  while ((i !== len)) {
    var i$1 = i;
    var t = $n(a).get(i$1);
    var lo = t.RTLong__f_lo;
    var hi = t.RTLong__f_hi;
    var i$2 = i;
    var t$1 = $n(b).get(i$2);
    var lo$1 = t$1.RTLong__f_lo;
    var hi$1 = t$1.RTLong__f_hi;
    if ((!((lo === lo$1) && (hi === hi$1)))) {
      return false;
    }
    i = ((1 + i) | 0);
  }
  return true;
});
$c_ju_Arrays$.prototype.equals__AI__AI__Z = (function(a, b) {
  if ((a === b)) {
    return true;
  }
  if (((a === null) || (b === null))) {
    return false;
  }
  var len = $n(a).u.length;
  if (($n(b).u.length !== len)) {
    return false;
  }
  var i = 0;
  while ((i !== len)) {
    var i$1 = i;
    var a$1 = $n(a).get(i$1);
    var i$2 = i;
    var b$1 = $n(b).get(i$2);
    if ((!(a$1 === b$1))) {
      return false;
    }
    i = ((1 + i) | 0);
  }
  return true;
});
$c_ju_Arrays$.prototype.equals__AS__AS__Z = (function(a, b) {
  if ((a === b)) {
    return true;
  }
  if (((a === null) || (b === null))) {
    return false;
  }
  var len = $n(a).u.length;
  if (($n(b).u.length !== len)) {
    return false;
  }
  var i = 0;
  while ((i !== len)) {
    var i$1 = i;
    var a$1 = $n(a).get(i$1);
    var i$2 = i;
    var b$1 = $n(b).get(i$2);
    if ((!(a$1 === b$1))) {
      return false;
    }
    i = ((1 + i) | 0);
  }
  return true;
});
$c_ju_Arrays$.prototype.equals__AC__AC__Z = (function(a, b) {
  if ((a === b)) {
    return true;
  }
  if (((a === null) || (b === null))) {
    return false;
  }
  var len = $n(a).u.length;
  if (($n(b).u.length !== len)) {
    return false;
  }
  var i = 0;
  while ((i !== len)) {
    var i$1 = i;
    var a$1 = $n(a).get(i$1);
    var i$2 = i;
    var b$1 = $n(b).get(i$2);
    if ((!(a$1 === b$1))) {
      return false;
    }
    i = ((1 + i) | 0);
  }
  return true;
});
$c_ju_Arrays$.prototype.equals__AB__AB__Z = (function(a, b) {
  if ((a === b)) {
    return true;
  }
  if (((a === null) || (b === null))) {
    return false;
  }
  var len = $n(a).u.length;
  if (($n(b).u.length !== len)) {
    return false;
  }
  var i = 0;
  while ((i !== len)) {
    var i$1 = i;
    var a$1 = $n(a).get(i$1);
    var i$2 = i;
    var b$1 = $n(b).get(i$2);
    if ((!(a$1 === b$1))) {
      return false;
    }
    i = ((1 + i) | 0);
  }
  return true;
});
$c_ju_Arrays$.prototype.equals__AZ__AZ__Z = (function(a, b) {
  if ((a === b)) {
    return true;
  }
  if (((a === null) || (b === null))) {
    return false;
  }
  var len = $n(a).u.length;
  if (($n(b).u.length !== len)) {
    return false;
  }
  var i = 0;
  while ((i !== len)) {
    var i$1 = i;
    var a$1 = $n(a).get(i$1);
    var i$2 = i;
    var b$1 = $n(b).get(i$2);
    if ((!(a$1 === b$1))) {
      return false;
    }
    i = ((1 + i) | 0);
  }
  return true;
});
$c_ju_Arrays$.prototype.equals__AD__AD__Z = (function(a, b) {
  if ((a === b)) {
    return true;
  }
  if (((a === null) || (b === null))) {
    return false;
  }
  var len = $n(a).u.length;
  if (($n(b).u.length !== len)) {
    return false;
  }
  var i = 0;
  while ((i !== len)) {
    var i$1 = i;
    var a$1 = $n(a).get(i$1);
    var i$2 = i;
    var b$1 = $n(b).get(i$2);
    if ((!Object.is(a$1, b$1))) {
      return false;
    }
    i = ((1 + i) | 0);
  }
  return true;
});
$c_ju_Arrays$.prototype.equals__AF__AF__Z = (function(a, b) {
  if ((a === b)) {
    return true;
  }
  if (((a === null) || (b === null))) {
    return false;
  }
  var len = $n(a).u.length;
  if (($n(b).u.length !== len)) {
    return false;
  }
  var i = 0;
  while ((i !== len)) {
    var i$1 = i;
    var a$1 = $n(a).get(i$1);
    var i$2 = i;
    var b$1 = $n(b).get(i$2);
    if ((!Object.is(a$1, b$1))) {
      return false;
    }
    i = ((1 + i) | 0);
  }
  return true;
});
var $d_ju_Arrays$ = new $TypeData().initClass($c_ju_Arrays$, "java.util.Arrays$", ({
  ju_Arrays$: 1
}));
var $n_ju_Arrays$;
function $m_ju_Arrays$() {
  if ((!$n_ju_Arrays$)) {
    $n_ju_Arrays$ = new $c_ju_Arrays$();
  }
  return $n_ju_Arrays$;
}
/** @constructor */
function $c_RTLong(lo, hi) {
  this.RTLong__f_lo = 0;
  this.RTLong__f_hi = 0;
  this.RTLong__f_lo = lo;
  this.RTLong__f_hi = hi;
}
$c_RTLong.prototype = new $h_O();
$c_RTLong.prototype.constructor = $c_RTLong;
/** @constructor */
function $h_RTLong() {
}
$h_RTLong.prototype = $c_RTLong.prototype;
$c_RTLong.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_RTLong)) {
    var x2 = $as_RTLong(that);
    return ((this.RTLong__f_lo === $n(x2).RTLong__f_lo) && (this.RTLong__f_hi === $n(x2).RTLong__f_hi));
  } else {
    return false;
  }
});
$c_RTLong.prototype.hashCode__I = (function() {
  return (this.RTLong__f_lo ^ this.RTLong__f_hi);
});
$c_RTLong.prototype.toString__T = (function() {
  return $m_RTLong$().org$scalajs$linker$runtime$RuntimeLong$$toString__I__I__T(this.RTLong__f_lo, this.RTLong__f_hi);
});
$c_RTLong.prototype.toInt__I = (function() {
  return this.RTLong__f_lo;
});
$c_RTLong.prototype.toFloat__F = (function() {
  return $m_RTLong$().org$scalajs$linker$runtime$RuntimeLong$$toFloat__I__I__F(this.RTLong__f_lo, this.RTLong__f_hi);
});
$c_RTLong.prototype.toDouble__D = (function() {
  return $m_RTLong$().org$scalajs$linker$runtime$RuntimeLong$$toDouble__I__I__D(this.RTLong__f_lo, this.RTLong__f_hi);
});
$c_RTLong.prototype.byteValue__B = (function() {
  return ((this.RTLong__f_lo << 24) >> 24);
});
$c_RTLong.prototype.shortValue__S = (function() {
  return ((this.RTLong__f_lo << 16) >> 16);
});
$c_RTLong.prototype.intValue__I = (function() {
  return this.RTLong__f_lo;
});
$c_RTLong.prototype.longValue__J = (function() {
  return $uJ(this);
});
$c_RTLong.prototype.floatValue__F = (function() {
  return $m_RTLong$().org$scalajs$linker$runtime$RuntimeLong$$toFloat__I__I__F(this.RTLong__f_lo, this.RTLong__f_hi);
});
$c_RTLong.prototype.doubleValue__D = (function() {
  return $m_RTLong$().org$scalajs$linker$runtime$RuntimeLong$$toDouble__I__I__D(this.RTLong__f_lo, this.RTLong__f_hi);
});
$c_RTLong.prototype.compareTo__O__I = (function(that) {
  var b = $as_RTLong(that);
  return $m_RTLong$().org$scalajs$linker$runtime$RuntimeLong$$compare__I__I__I__I__I(this.RTLong__f_lo, this.RTLong__f_hi, $n(b).RTLong__f_lo, $n(b).RTLong__f_hi);
});
$c_RTLong.prototype.compareTo__jl_Long__I = (function(that) {
  return $m_RTLong$().org$scalajs$linker$runtime$RuntimeLong$$compare__I__I__I__I__I(this.RTLong__f_lo, this.RTLong__f_hi, $n(that).RTLong__f_lo, $n(that).RTLong__f_hi);
});
$c_RTLong.prototype.equals__RTLong__Z = (function(b) {
  return ((this.RTLong__f_lo === $n(b).RTLong__f_lo) && (this.RTLong__f_hi === $n(b).RTLong__f_hi));
});
$c_RTLong.prototype.notEquals__RTLong__Z = (function(b) {
  return (!((this.RTLong__f_lo === $n(b).RTLong__f_lo) && (this.RTLong__f_hi === $n(b).RTLong__f_hi)));
});
$c_RTLong.prototype.$less__RTLong__Z = (function(b) {
  var ahi = this.RTLong__f_hi;
  var bhi = $n(b).RTLong__f_hi;
  return ((ahi === bhi) ? (((-2147483648) ^ this.RTLong__f_lo) < ((-2147483648) ^ $n(b).RTLong__f_lo)) : (ahi < bhi));
});
$c_RTLong.prototype.$less$eq__RTLong__Z = (function(b) {
  var ahi = this.RTLong__f_hi;
  var bhi = $n(b).RTLong__f_hi;
  return ((ahi === bhi) ? (((-2147483648) ^ this.RTLong__f_lo) <= ((-2147483648) ^ $n(b).RTLong__f_lo)) : (ahi < bhi));
});
$c_RTLong.prototype.$greater__RTLong__Z = (function(b) {
  var ahi = this.RTLong__f_hi;
  var bhi = $n(b).RTLong__f_hi;
  return ((ahi === bhi) ? (((-2147483648) ^ this.RTLong__f_lo) > ((-2147483648) ^ $n(b).RTLong__f_lo)) : (ahi > bhi));
});
$c_RTLong.prototype.$greater$eq__RTLong__Z = (function(b) {
  var ahi = this.RTLong__f_hi;
  var bhi = $n(b).RTLong__f_hi;
  return ((ahi === bhi) ? (((-2147483648) ^ this.RTLong__f_lo) >= ((-2147483648) ^ $n(b).RTLong__f_lo)) : (ahi > bhi));
});
$c_RTLong.prototype.unary_$tilde__RTLong = (function() {
  return new $c_RTLong((~this.RTLong__f_lo), (~this.RTLong__f_hi));
});
$c_RTLong.prototype.$bar__RTLong__RTLong = (function(b) {
  return new $c_RTLong((this.RTLong__f_lo | $n(b).RTLong__f_lo), (this.RTLong__f_hi | $n(b).RTLong__f_hi));
});
$c_RTLong.prototype.$amp__RTLong__RTLong = (function(b) {
  return new $c_RTLong((this.RTLong__f_lo & $n(b).RTLong__f_lo), (this.RTLong__f_hi & $n(b).RTLong__f_hi));
});
$c_RTLong.prototype.$up__RTLong__RTLong = (function(b) {
  return new $c_RTLong((this.RTLong__f_lo ^ $n(b).RTLong__f_lo), (this.RTLong__f_hi ^ $n(b).RTLong__f_hi));
});
$c_RTLong.prototype.$less$less__I__RTLong = (function(n) {
  var lo = this.RTLong__f_lo;
  return new $c_RTLong((((32 & n) === 0) ? (lo << n) : 0), (((32 & n) === 0) ? (((((lo >>> 1) | 0) >>> ((31 - n) | 0)) | 0) | (this.RTLong__f_hi << n)) : (lo << n)));
});
$c_RTLong.prototype.$greater$greater$greater__I__RTLong = (function(n) {
  var hi = this.RTLong__f_hi;
  return new $c_RTLong((((32 & n) === 0) ? (((this.RTLong__f_lo >>> n) | 0) | ((hi << 1) << ((31 - n) | 0))) : ((hi >>> n) | 0)), (((32 & n) === 0) ? ((hi >>> n) | 0) : 0));
});
$c_RTLong.prototype.$greater$greater__I__RTLong = (function(n) {
  var hi = this.RTLong__f_hi;
  return new $c_RTLong((((32 & n) === 0) ? (((this.RTLong__f_lo >>> n) | 0) | ((hi << 1) << ((31 - n) | 0))) : (hi >> n)), (((32 & n) === 0) ? (hi >> n) : (hi >> 31)));
});
$c_RTLong.prototype.unary_$minus__RTLong = (function() {
  var lo = this.RTLong__f_lo;
  var hi = this.RTLong__f_hi;
  return new $c_RTLong(((-lo) | 0), ((lo !== 0) ? (~hi) : ((-hi) | 0)));
});
$c_RTLong.prototype.$plus__RTLong__RTLong = (function(b) {
  var alo = this.RTLong__f_lo;
  var ahi = this.RTLong__f_hi;
  var bhi = $n(b).RTLong__f_hi;
  var lo = ((alo + $n(b).RTLong__f_lo) | 0);
  return new $c_RTLong(lo, ((((-2147483648) ^ lo) < ((-2147483648) ^ alo)) ? ((1 + ((ahi + bhi) | 0)) | 0) : ((ahi + bhi) | 0)));
});
$c_RTLong.prototype.$minus__RTLong__RTLong = (function(b) {
  var alo = this.RTLong__f_lo;
  var ahi = this.RTLong__f_hi;
  var bhi = $n(b).RTLong__f_hi;
  var lo = ((alo - $n(b).RTLong__f_lo) | 0);
  return new $c_RTLong(lo, ((((-2147483648) ^ lo) > ((-2147483648) ^ alo)) ? (((-1) + ((ahi - bhi) | 0)) | 0) : ((ahi - bhi) | 0)));
});
$c_RTLong.prototype.$times__RTLong__RTLong = (function(b) {
  var alo = this.RTLong__f_lo;
  var blo = $n(b).RTLong__f_lo;
  var a0 = (65535 & alo);
  var a1 = ((alo >>> 16) | 0);
  var b0 = (65535 & blo);
  var b1 = ((blo >>> 16) | 0);
  var a0b0 = Math.imul(a0, b0);
  var a1b0 = Math.imul(a1, b0);
  var a0b1 = Math.imul(a0, b1);
  var lo = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
  var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
  var hi = ((((((((Math.imul(alo, $n(b).RTLong__f_hi) + Math.imul(this.RTLong__f_hi, blo)) | 0) + Math.imul(a1, b1)) | 0) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
  return new $c_RTLong(lo, hi);
});
$c_RTLong.prototype.$div__RTLong__RTLong = (function(b) {
  var this$1 = $m_RTLong$();
  var lo = this$1.divideImpl__I__I__I__I__I(this.RTLong__f_lo, this.RTLong__f_hi, $n(b).RTLong__f_lo, $n(b).RTLong__f_hi);
  return new $c_RTLong(lo, this$1.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn);
});
$c_RTLong.prototype.$percent__RTLong__RTLong = (function(b) {
  var this$1 = $m_RTLong$();
  var lo = this$1.remainderImpl__I__I__I__I__I(this.RTLong__f_lo, this.RTLong__f_hi, $n(b).RTLong__f_lo, $n(b).RTLong__f_hi);
  return new $c_RTLong(lo, this$1.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn);
});
function $as_RTLong(obj) {
  return (((obj instanceof $c_RTLong) || (obj === null)) ? obj : $throwClassCastException(obj, "org.scalajs.linker.runtime.RuntimeLong"));
}
function $isArrayOf_RTLong(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.RTLong)));
}
function $asArrayOf_RTLong(obj, depth) {
  return (($isArrayOf_RTLong(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lorg.scalajs.linker.runtime.RuntimeLong;", depth));
}
var $d_RTLong = new $TypeData().initClass($c_RTLong, "org.scalajs.linker.runtime.RuntimeLong", ({
  RTLong: 1
}));
function $p_RTLong$__toUnsignedString__I__I__T($thiz, lo, hi) {
  if ((((-2097152) & hi) === 0)) {
    var this$1 = ((4.294967296E9 * hi) + $uD((lo >>> 0.0)));
    return ("" + this$1);
  } else {
    return $as_T($p_RTLong$__unsignedDivModHelper__I__I__I__I__I__O($thiz, lo, hi, 1000000000, 0, 2));
  }
}
function $p_RTLong$__unsigned_$div__I__I__I__I__I($thiz, alo, ahi, blo, bhi) {
  if ((((-2097152) & ahi) === 0)) {
    if ((((-2097152) & bhi) === 0)) {
      var aDouble = ((4.294967296E9 * ahi) + $uD((alo >>> 0.0)));
      var bDouble = ((4.294967296E9 * bhi) + $uD((blo >>> 0.0)));
      var rDouble = (aDouble / bDouble);
      var x = (rDouble / 4.294967296E9);
      $thiz.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = $uI((x | 0.0));
      return $uI((rDouble | 0.0));
    } else {
      $thiz.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = 0;
      return 0;
    }
  } else if (((bhi === 0) && ((blo & (((-1) + blo) | 0)) === 0))) {
    var pow = ((31 - $uI(Math.clz32(blo))) | 0);
    $thiz.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = ((ahi >>> pow) | 0);
    return (((alo >>> pow) | 0) | ((ahi << 1) << ((31 - pow) | 0)));
  } else if (((blo === 0) && ((bhi & (((-1) + bhi) | 0)) === 0))) {
    var pow$2 = ((31 - $uI(Math.clz32(bhi))) | 0);
    $thiz.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = 0;
    return ((ahi >>> pow$2) | 0);
  } else {
    return $uI($p_RTLong$__unsignedDivModHelper__I__I__I__I__I__O($thiz, alo, ahi, blo, bhi, 0));
  }
}
function $p_RTLong$__unsigned_$percent__I__I__I__I__I($thiz, alo, ahi, blo, bhi) {
  if ((((-2097152) & ahi) === 0)) {
    if ((((-2097152) & bhi) === 0)) {
      var aDouble = ((4.294967296E9 * ahi) + $uD((alo >>> 0.0)));
      var bDouble = ((4.294967296E9 * bhi) + $uD((blo >>> 0.0)));
      var rDouble = (aDouble % bDouble);
      var x = (rDouble / 4.294967296E9);
      $thiz.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = $uI((x | 0.0));
      return $uI((rDouble | 0.0));
    } else {
      $thiz.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = ahi;
      return alo;
    }
  } else if (((bhi === 0) && ((blo & (((-1) + blo) | 0)) === 0))) {
    $thiz.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = 0;
    return (alo & (((-1) + blo) | 0));
  } else if (((blo === 0) && ((bhi & (((-1) + bhi) | 0)) === 0))) {
    $thiz.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = (ahi & (((-1) + bhi) | 0));
    return alo;
  } else {
    return $uI($p_RTLong$__unsignedDivModHelper__I__I__I__I__I__O($thiz, alo, ahi, blo, bhi, 1));
  }
}
function $p_RTLong$__unsignedDivModHelper__I__I__I__I__I__O($thiz, alo, ahi, blo, bhi, ask) {
  var shift = ((((bhi !== 0) ? $uI(Math.clz32(bhi)) : ((32 + $uI(Math.clz32(blo))) | 0)) - ((ahi !== 0) ? $uI(Math.clz32(ahi)) : ((32 + $uI(Math.clz32(alo))) | 0))) | 0);
  var n = shift;
  var lo = (((32 & n) === 0) ? (blo << n) : 0);
  var hi = (((32 & n) === 0) ? (((((blo >>> 1) | 0) >>> ((31 - n) | 0)) | 0) | (bhi << n)) : (blo << n));
  var bShiftLo = lo;
  var bShiftHi = hi;
  var remLo = alo;
  var remHi = ahi;
  var quotLo = 0;
  var quotHi = 0;
  while (((shift >= 0) && (((-2097152) & remHi) !== 0))) {
    var alo$1 = remLo;
    var ahi$1 = remHi;
    var blo$1 = bShiftLo;
    var bhi$1 = bShiftHi;
    if (((ahi$1 === bhi$1) ? (((-2147483648) ^ alo$1) >= ((-2147483648) ^ blo$1)) : (((-2147483648) ^ ahi$1) >= ((-2147483648) ^ bhi$1)))) {
      var lo$1 = remLo;
      var hi$1 = remHi;
      var lo$2 = bShiftLo;
      var hi$2 = bShiftHi;
      var lo$3 = ((lo$1 - lo$2) | 0);
      var hi$3 = ((((-2147483648) ^ lo$3) > ((-2147483648) ^ lo$1)) ? (((-1) + ((hi$1 - hi$2) | 0)) | 0) : ((hi$1 - hi$2) | 0));
      remLo = lo$3;
      remHi = hi$3;
      if ((shift < 32)) {
        quotLo = (quotLo | (1 << shift));
      } else {
        quotHi = (quotHi | (1 << shift));
      }
    }
    shift = (((-1) + shift) | 0);
    var lo$4 = bShiftLo;
    var hi$4 = bShiftHi;
    var lo$5 = (((lo$4 >>> 1) | 0) | (hi$4 << 31));
    var hi$5 = ((hi$4 >>> 1) | 0);
    bShiftLo = lo$5;
    bShiftHi = hi$5;
  }
  var alo$2 = remLo;
  var ahi$2 = remHi;
  if (((ahi$2 === bhi) ? (((-2147483648) ^ alo$2) >= ((-2147483648) ^ blo)) : (((-2147483648) ^ ahi$2) >= ((-2147483648) ^ bhi)))) {
    var lo$6 = remLo;
    var hi$6 = remHi;
    var remDouble = ((4.294967296E9 * hi$6) + $uD((lo$6 >>> 0.0)));
    var bDouble = ((4.294967296E9 * bhi) + $uD((blo >>> 0.0)));
    if ((ask !== 1)) {
      var x = (remDouble / bDouble);
      var lo$7 = $uI((x | 0.0));
      var x$1 = (x / 4.294967296E9);
      var hi$7 = $uI((x$1 | 0.0));
      var lo$8 = quotLo;
      var hi$8 = quotHi;
      var lo$9 = ((lo$8 + lo$7) | 0);
      var hi$9 = ((((-2147483648) ^ lo$9) < ((-2147483648) ^ lo$8)) ? ((1 + ((hi$8 + hi$7) | 0)) | 0) : ((hi$8 + hi$7) | 0));
      quotLo = lo$9;
      quotHi = hi$9;
    }
    if ((ask !== 0)) {
      var rem_mod_bDouble = (remDouble % bDouble);
      remLo = $uI((rem_mod_bDouble | 0.0));
      var x$2 = (rem_mod_bDouble / 4.294967296E9);
      remHi = $uI((x$2 | 0.0));
    }
  }
  if ((ask === 0)) {
    $thiz.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = quotHi;
    return quotLo;
  } else if ((ask === 1)) {
    $thiz.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = remHi;
    return remLo;
  } else {
    var lo$10 = quotLo;
    var hi$10 = quotHi;
    var quot = ((4.294967296E9 * hi$10) + $uD((lo$10 >>> 0.0)));
    var this$7 = remLo;
    var remStr = ("" + this$7);
    var start = remStr.length;
    return ((("" + quot) + $as_T("000000000".substring(start))) + remStr);
  }
}
/** @constructor */
function $c_RTLong$() {
  this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = 0;
}
$c_RTLong$.prototype = new $h_O();
$c_RTLong$.prototype.constructor = $c_RTLong$;
/** @constructor */
function $h_RTLong$() {
}
$h_RTLong$.prototype = $c_RTLong$.prototype;
$c_RTLong$.prototype.org$scalajs$linker$runtime$RuntimeLong$$toString__I__I__T = (function(lo, hi) {
  return ((hi === (lo >> 31)) ? ("" + lo) : ((hi < 0) ? ("-" + $p_RTLong$__toUnsignedString__I__I__T(this, ((-lo) | 0), ((lo !== 0) ? (~hi) : ((-hi) | 0)))) : $p_RTLong$__toUnsignedString__I__I__T(this, lo, hi)));
});
$c_RTLong$.prototype.org$scalajs$linker$runtime$RuntimeLong$$toDouble__I__I__D = (function(lo, hi) {
  if ((hi < 0)) {
    var x = ((lo !== 0) ? (~hi) : ((-hi) | 0));
    var $x_1 = $uD((x >>> 0.0));
    var x$1 = ((-lo) | 0);
    return (-((4.294967296E9 * $x_1) + $uD((x$1 >>> 0.0))));
  } else {
    return ((4.294967296E9 * hi) + $uD((lo >>> 0.0)));
  }
});
$c_RTLong$.prototype.org$scalajs$linker$runtime$RuntimeLong$$toFloat__I__I__F = (function(lo, hi) {
  if ((hi < 0)) {
    var lo$1 = ((-lo) | 0);
    var hi$1 = ((lo !== 0) ? (~hi) : ((-hi) | 0));
    var abs__lo = lo$1;
    var abs__hi = hi$1;
  } else {
    var abs__lo = lo;
    var abs__hi = hi;
  }
  var hi$2 = abs__hi;
  if (((((-2097152) & hi$2) === 0) || ((65535 & abs__lo) === 0))) {
    var compressedAbsLo = abs__lo;
  } else {
    var compressedAbsLo = (32768 | ((-65536) & abs__lo));
  }
  var x = abs__hi;
  var absRes = ((4.294967296E9 * $uD((x >>> 0.0))) + $uD((compressedAbsLo >>> 0.0)));
  return Math.fround(((hi < 0) ? (-absRes) : absRes));
});
$c_RTLong$.prototype.fromInt__I__RTLong = (function(value) {
  return new $c_RTLong(value, (value >> 31));
});
$c_RTLong$.prototype.fromDouble__D__RTLong = (function(value) {
  var lo = this.org$scalajs$linker$runtime$RuntimeLong$$fromDoubleImpl__D__I(value);
  return new $c_RTLong(lo, this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn);
});
$c_RTLong$.prototype.org$scalajs$linker$runtime$RuntimeLong$$fromDoubleImpl__D__I = (function(value) {
  if ((value < (-9.223372036854776E18))) {
    this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = (-2147483648);
    return 0;
  } else if ((value >= 9.223372036854776E18)) {
    this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = 2147483647;
    return (-1);
  } else {
    var rawLo = $uI((value | 0.0));
    var x = (value / 4.294967296E9);
    var rawHi = $uI((x | 0.0));
    this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = (((value < 0.0) && (rawLo !== 0)) ? (((-1) + rawHi) | 0) : rawHi);
    return rawLo;
  }
});
$c_RTLong$.prototype.org$scalajs$linker$runtime$RuntimeLong$$compare__I__I__I__I__I = (function(alo, ahi, blo, bhi) {
  return ((ahi === bhi) ? ((alo === blo) ? 0 : ((((-2147483648) ^ alo) < ((-2147483648) ^ blo)) ? (-1) : 1)) : ((ahi < bhi) ? (-1) : 1));
});
$c_RTLong$.prototype.divideImpl__I__I__I__I__I = (function(alo, ahi, blo, bhi) {
  if (((blo | bhi) === 0)) {
    throw new $c_jl_ArithmeticException("/ by zero");
  }
  if ((ahi === (alo >> 31))) {
    if ((bhi === (blo >> 31))) {
      if (((alo === (-2147483648)) && (blo === (-1)))) {
        this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = 0;
        return (-2147483648);
      } else {
        var lo = $intDiv(alo, blo);
        this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = (lo >> 31);
        return lo;
      }
    } else if (((alo === (-2147483648)) && ((blo === (-2147483648)) && (bhi === 0)))) {
      this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = (-1);
      return (-1);
    } else {
      this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = 0;
      return 0;
    }
  } else {
    if ((ahi < 0)) {
      var lo$1 = ((-alo) | 0);
      var hi = ((alo !== 0) ? (~ahi) : ((-ahi) | 0));
      var aAbs__lo = lo$1;
      var aAbs__hi = hi;
    } else {
      var aAbs__lo = alo;
      var aAbs__hi = ahi;
    }
    if ((bhi < 0)) {
      var lo$2 = ((-blo) | 0);
      var hi$1 = ((blo !== 0) ? (~bhi) : ((-bhi) | 0));
      var bAbs__lo = lo$2;
      var bAbs__hi = hi$1;
    } else {
      var bAbs__lo = blo;
      var bAbs__hi = bhi;
    }
    var absRLo = $p_RTLong$__unsigned_$div__I__I__I__I__I(this, aAbs__lo, aAbs__hi, bAbs__lo, bAbs__hi);
    if (((ahi ^ bhi) >= 0)) {
      return absRLo;
    } else {
      var hi$2 = this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn;
      this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = ((absRLo !== 0) ? (~hi$2) : ((-hi$2) | 0));
      return ((-absRLo) | 0);
    }
  }
});
$c_RTLong$.prototype.remainderImpl__I__I__I__I__I = (function(alo, ahi, blo, bhi) {
  if (((blo | bhi) === 0)) {
    throw new $c_jl_ArithmeticException("/ by zero");
  }
  if ((ahi === (alo >> 31))) {
    if ((bhi === (blo >> 31))) {
      if ((blo !== (-1))) {
        var lo = $intMod(alo, blo);
        this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = (lo >> 31);
        return lo;
      } else {
        this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = 0;
        return 0;
      }
    } else if (((alo === (-2147483648)) && ((blo === (-2147483648)) && (bhi === 0)))) {
      this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = 0;
      return 0;
    } else {
      this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = ahi;
      return alo;
    }
  } else {
    if ((ahi < 0)) {
      var lo$1 = ((-alo) | 0);
      var hi = ((alo !== 0) ? (~ahi) : ((-ahi) | 0));
      var aAbs__lo = lo$1;
      var aAbs__hi = hi;
    } else {
      var aAbs__lo = alo;
      var aAbs__hi = ahi;
    }
    if ((bhi < 0)) {
      var lo$2 = ((-blo) | 0);
      var hi$1 = ((blo !== 0) ? (~bhi) : ((-bhi) | 0));
      var bAbs__lo = lo$2;
      var bAbs__hi = hi$1;
    } else {
      var bAbs__lo = blo;
      var bAbs__hi = bhi;
    }
    var absRLo = $p_RTLong$__unsigned_$percent__I__I__I__I__I(this, aAbs__lo, aAbs__hi, bAbs__lo, bAbs__hi);
    if ((ahi < 0)) {
      var hi$2 = this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn;
      this.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn = ((absRLo !== 0) ? (~hi$2) : ((-hi$2) | 0));
      return ((-absRLo) | 0);
    } else {
      return absRLo;
    }
  }
});
var $d_RTLong$ = new $TypeData().initClass($c_RTLong$, "org.scalajs.linker.runtime.RuntimeLong$", ({
  RTLong$: 1
}));
var $n_RTLong$;
function $m_RTLong$() {
  if ((!$n_RTLong$)) {
    $n_RTLong$ = new $c_RTLong$();
  }
  return $n_RTLong$;
}
function $is_F0(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.F0)));
}
function $as_F0(obj) {
  return (($is_F0(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.Function0"));
}
function $isArrayOf_F0(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.F0)));
}
function $asArrayOf_F0(obj, depth) {
  return (($isArrayOf_F0(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.Function0;", depth));
}
function $is_F1(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.F1)));
}
function $as_F1(obj) {
  return (($is_F1(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.Function1"));
}
function $isArrayOf_F1(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.F1)));
}
function $asArrayOf_F1(obj, depth) {
  return (($isArrayOf_F1(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.Function1;", depth));
}
var $d_F1 = new $TypeData().initClass(1, "scala.Function1", ({
  F1: 1
}));
/** @constructor */
function $c_s_LowPriorityImplicits2() {
}
$c_s_LowPriorityImplicits2.prototype = new $h_O();
$c_s_LowPriorityImplicits2.prototype.constructor = $c_s_LowPriorityImplicits2;
/** @constructor */
function $h_s_LowPriorityImplicits2() {
}
$h_s_LowPriorityImplicits2.prototype = $c_s_LowPriorityImplicits2.prototype;
/** @constructor */
function $c_s_PartialFunction$() {
  this.s_PartialFunction$__f_fallback_fn = null;
  this.s_PartialFunction$__f_empty_pf = null;
  $n_s_PartialFunction$ = this;
  this.s_PartialFunction$__f_fallback_fn = new $c_sjsr_AnonFunction1(((x$2$2$2) => $m_s_PartialFunction$().s_PartialFunction$__f_fallback_fn));
  this.s_PartialFunction$__f_empty_pf = new $c_s_PartialFunction$$anon$1();
}
$c_s_PartialFunction$.prototype = new $h_O();
$c_s_PartialFunction$.prototype.constructor = $c_s_PartialFunction$;
/** @constructor */
function $h_s_PartialFunction$() {
}
$h_s_PartialFunction$.prototype = $c_s_PartialFunction$.prototype;
var $d_s_PartialFunction$ = new $TypeData().initClass($c_s_PartialFunction$, "scala.PartialFunction$", ({
  s_PartialFunction$: 1
}));
var $n_s_PartialFunction$;
function $m_s_PartialFunction$() {
  if ((!$n_s_PartialFunction$)) {
    $n_s_PartialFunction$ = new $c_s_PartialFunction$();
  }
  return $n_s_PartialFunction$;
}
function $is_sc_IterableOnce(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_IterableOnce)));
}
function $as_sc_IterableOnce(obj) {
  return (($is_sc_IterableOnce(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.IterableOnce"));
}
function $isArrayOf_sc_IterableOnce(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_IterableOnce)));
}
function $asArrayOf_sc_IterableOnce(obj, depth) {
  return (($isArrayOf_sc_IterableOnce(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.IterableOnce;", depth));
}
function $f_sc_IterableOnceOps__foreach__F1__V($thiz, f) {
  var it = $n($as_sc_IterableOnce($thiz)).iterator__sc_Iterator();
  while ($n(it).hasNext__Z()) {
    $n(f).apply__O__O($n(it).next__O());
  }
}
function $f_sc_IterableOnceOps__mkString__T__T__T__T($thiz, start, sep, end) {
  if (($n($as_sc_IterableOnce($thiz)).knownSize__I() === 0)) {
    return (("" + start) + end);
  } else {
    var this$1 = $n($thiz.addString__scm_StringBuilder__T__T__T__scm_StringBuilder($ct_scm_StringBuilder__(new $c_scm_StringBuilder()), start, sep, end));
    return $n(this$1.scm_StringBuilder__f_underlying).jl_StringBuilder__f_java$lang$StringBuilder$$content;
  }
}
function $f_sc_IterableOnceOps__addString__scm_StringBuilder__T__T__T__scm_StringBuilder($thiz, b, start, sep, end) {
  var jsb = $n(b).scm_StringBuilder__f_underlying;
  var this$1 = $n(start);
  if ((this$1.length !== 0)) {
    var this$2 = $n(jsb);
    this$2.jl_StringBuilder__f_java$lang$StringBuilder$$content = (("" + this$2.jl_StringBuilder__f_java$lang$StringBuilder$$content) + start);
  }
  var it = $n($as_sc_IterableOnce($thiz)).iterator__sc_Iterator();
  if ($n(it).hasNext__Z()) {
    var this$3 = $n(jsb);
    var obj = $n(it).next__O();
    this$3.jl_StringBuilder__f_java$lang$StringBuilder$$content = (("" + this$3.jl_StringBuilder__f_java$lang$StringBuilder$$content) + obj);
    while ($n(it).hasNext__Z()) {
      var this$4 = $n(jsb);
      this$4.jl_StringBuilder__f_java$lang$StringBuilder$$content = (("" + this$4.jl_StringBuilder__f_java$lang$StringBuilder$$content) + sep);
      var this$5 = $n(jsb);
      var obj$1 = $n(it).next__O();
      this$5.jl_StringBuilder__f_java$lang$StringBuilder$$content = (("" + this$5.jl_StringBuilder__f_java$lang$StringBuilder$$content) + obj$1);
    }
  }
  var this$6 = $n(end);
  if ((this$6.length !== 0)) {
    var this$7 = $n(jsb);
    this$7.jl_StringBuilder__f_java$lang$StringBuilder$$content = (("" + this$7.jl_StringBuilder__f_java$lang$StringBuilder$$content) + end);
  }
  return b;
}
/** @constructor */
function $c_sc_Iterator$ConcatIteratorCell(head, tail) {
  this.sc_Iterator$ConcatIteratorCell__f_head = null;
  this.sc_Iterator$ConcatIteratorCell__f_tail = null;
  this.sc_Iterator$ConcatIteratorCell__f_head = head;
  this.sc_Iterator$ConcatIteratorCell__f_tail = tail;
}
$c_sc_Iterator$ConcatIteratorCell.prototype = new $h_O();
$c_sc_Iterator$ConcatIteratorCell.prototype.constructor = $c_sc_Iterator$ConcatIteratorCell;
/** @constructor */
function $h_sc_Iterator$ConcatIteratorCell() {
}
$h_sc_Iterator$ConcatIteratorCell.prototype = $c_sc_Iterator$ConcatIteratorCell.prototype;
$c_sc_Iterator$ConcatIteratorCell.prototype.headIterator__sc_Iterator = (function() {
  return $n($as_sc_IterableOnce($n(this.sc_Iterator$ConcatIteratorCell__f_head).apply__O())).iterator__sc_Iterator();
});
var $d_sc_Iterator$ConcatIteratorCell = new $TypeData().initClass($c_sc_Iterator$ConcatIteratorCell, "scala.collection.Iterator$ConcatIteratorCell", ({
  sc_Iterator$ConcatIteratorCell: 1
}));
/** @constructor */
function $c_scm_MutationTracker$() {
}
$c_scm_MutationTracker$.prototype = new $h_O();
$c_scm_MutationTracker$.prototype.constructor = $c_scm_MutationTracker$;
/** @constructor */
function $h_scm_MutationTracker$() {
}
$h_scm_MutationTracker$.prototype = $c_scm_MutationTracker$.prototype;
$c_scm_MutationTracker$.prototype.checkMutations__I__I__T__V = (function(expectedCount, actualCount, message) {
  if ((actualCount !== expectedCount)) {
    throw new $c_ju_ConcurrentModificationException(message);
  }
});
var $d_scm_MutationTracker$ = new $TypeData().initClass($c_scm_MutationTracker$, "scala.collection.mutable.MutationTracker$", ({
  scm_MutationTracker$: 1
}));
var $n_scm_MutationTracker$;
function $m_scm_MutationTracker$() {
  if ((!$n_scm_MutationTracker$)) {
    $n_scm_MutationTracker$ = new $c_scm_MutationTracker$();
  }
  return $n_scm_MutationTracker$;
}
/** @constructor */
function $c_sr_BoxesRunTime$() {
}
$c_sr_BoxesRunTime$.prototype = new $h_O();
$c_sr_BoxesRunTime$.prototype.constructor = $c_sr_BoxesRunTime$;
/** @constructor */
function $h_sr_BoxesRunTime$() {
}
$h_sr_BoxesRunTime$.prototype = $c_sr_BoxesRunTime$.prototype;
$c_sr_BoxesRunTime$.prototype.equals__O__O__Z = (function(x, y) {
  if ((x === y)) {
    return true;
  } else if ($is_jl_Number(x)) {
    var x2 = $as_jl_Number(x);
    return this.equalsNumObject__jl_Number__O__Z(x2, y);
  } else if ((x instanceof $Char)) {
    var x3 = $as_jl_Character(x);
    return this.equalsCharObject__jl_Character__O__Z(x3, y);
  } else {
    return ((x === null) ? (y === null) : $dp_equals__O__Z($n(x), y));
  }
});
$c_sr_BoxesRunTime$.prototype.equalsNumObject__jl_Number__O__Z = (function(xn, y) {
  if ($is_jl_Number(y)) {
    var x2 = $as_jl_Number(y);
    return this.equalsNumNum__jl_Number__jl_Number__Z(xn, x2);
  } else if ((y instanceof $Char)) {
    var x3 = $as_jl_Character(y);
    if (((typeof xn) === "number")) {
      var x2$1 = $uD(xn);
      var this$1 = $n(x3).c;
      return (x2$1 === this$1);
    } else if ((xn instanceof $c_RTLong)) {
      var t = $uJ(xn);
      var lo = t.RTLong__f_lo;
      var hi = t.RTLong__f_hi;
      var this$2 = $n(x3).c;
      var value = this$2;
      var hi$1 = (value >> 31);
      return ((lo === value) && (hi === hi$1));
    } else {
      return ((xn === null) ? (x3 === null) : $dp_equals__O__Z($n(xn), x3));
    }
  } else {
    return ((xn === null) ? (y === null) : $dp_equals__O__Z($n(xn), y));
  }
});
$c_sr_BoxesRunTime$.prototype.equalsNumNum__jl_Number__jl_Number__Z = (function(xn, yn) {
  if (((typeof xn) === "number")) {
    var x2 = $uD(xn);
    if (((typeof yn) === "number")) {
      var x2$2 = $uD(yn);
      return (x2 === x2$2);
    } else if ((yn instanceof $c_RTLong)) {
      var t = $uJ(yn);
      var lo = t.RTLong__f_lo;
      var hi = t.RTLong__f_hi;
      return (x2 === $m_RTLong$().org$scalajs$linker$runtime$RuntimeLong$$toDouble__I__I__D(lo, hi));
    } else if (false) {
      var x4 = $as_s_math_ScalaNumber(yn);
      return $n(x4).equals__O__Z(x2);
    } else {
      return false;
    }
  } else if ((xn instanceof $c_RTLong)) {
    var t$1 = $uJ(xn);
    var lo$1 = t$1.RTLong__f_lo;
    var hi$1 = t$1.RTLong__f_hi;
    if ((yn instanceof $c_RTLong)) {
      var t$2 = $uJ(yn);
      var lo$2 = t$2.RTLong__f_lo;
      var hi$2 = t$2.RTLong__f_hi;
      return ((lo$1 === lo$2) && (hi$1 === hi$2));
    } else if (((typeof yn) === "number")) {
      var x3$3 = $uD(yn);
      return ($m_RTLong$().org$scalajs$linker$runtime$RuntimeLong$$toDouble__I__I__D(lo$1, hi$1) === x3$3);
    } else if (false) {
      var x4$2 = $as_s_math_ScalaNumber(yn);
      return $n(x4$2).equals__O__Z(new $c_RTLong(lo$1, hi$1));
    } else {
      return false;
    }
  } else {
    return ((xn === null) ? (yn === null) : $dp_equals__O__Z($n(xn), yn));
  }
});
$c_sr_BoxesRunTime$.prototype.equalsCharObject__jl_Character__O__Z = (function(xc, y) {
  if ((y instanceof $Char)) {
    var x2 = $as_jl_Character(y);
    var this$1 = $n(xc).c;
    var this$2 = $n(x2).c;
    return (this$1 === this$2);
  } else if ($is_jl_Number(y)) {
    var x3 = $as_jl_Number(y);
    if (((typeof x3) === "number")) {
      var x2$1 = $uD(x3);
      var this$3 = $n(xc).c;
      return (x2$1 === this$3);
    } else if ((x3 instanceof $c_RTLong)) {
      var t = $uJ(x3);
      var lo = t.RTLong__f_lo;
      var hi = t.RTLong__f_hi;
      var this$4 = $n(xc).c;
      var value = this$4;
      var hi$1 = (value >> 31);
      return ((lo === value) && (hi === hi$1));
    } else {
      return ((x3 === null) ? (xc === null) : $dp_equals__O__Z($n(x3), xc));
    }
  } else {
    return ((xc === null) && (y === null));
  }
});
var $d_sr_BoxesRunTime$ = new $TypeData().initClass($c_sr_BoxesRunTime$, "scala.runtime.BoxesRunTime$", ({
  sr_BoxesRunTime$: 1
}));
var $n_sr_BoxesRunTime$;
function $m_sr_BoxesRunTime$() {
  if ((!$n_sr_BoxesRunTime$)) {
    $n_sr_BoxesRunTime$ = new $c_sr_BoxesRunTime$();
  }
  return $n_sr_BoxesRunTime$;
}
var $d_sr_Null$ = new $TypeData().initClass(0, "scala.runtime.Null$", ({
  sr_Null$: 1
}));
/** @constructor */
function $c_sr_ScalaRunTime$() {
}
$c_sr_ScalaRunTime$.prototype = new $h_O();
$c_sr_ScalaRunTime$.prototype.constructor = $c_sr_ScalaRunTime$;
/** @constructor */
function $h_sr_ScalaRunTime$() {
}
$h_sr_ScalaRunTime$.prototype = $c_sr_ScalaRunTime$.prototype;
$c_sr_ScalaRunTime$.prototype.array_apply__O__I__O = (function(xs, idx) {
  if ((xs instanceof $ac_O)) {
    var x2 = $asArrayOf_O(xs, 1);
    return $n(x2).get(idx);
  } else if ((xs instanceof $ac_I)) {
    var x3 = $asArrayOf_I(xs, 1);
    return $n(x3).get(idx);
  } else if ((xs instanceof $ac_D)) {
    var x4 = $asArrayOf_D(xs, 1);
    return $n(x4).get(idx);
  } else if ((xs instanceof $ac_J)) {
    var x5 = $asArrayOf_J(xs, 1);
    return $n(x5).get(idx);
  } else if ((xs instanceof $ac_F)) {
    var x6 = $asArrayOf_F(xs, 1);
    return $n(x6).get(idx);
  } else if ((xs instanceof $ac_C)) {
    var x7 = $asArrayOf_C(xs, 1);
    return $bC($n(x7).get(idx));
  } else if ((xs instanceof $ac_B)) {
    var x8 = $asArrayOf_B(xs, 1);
    return $n(x8).get(idx);
  } else if ((xs instanceof $ac_S)) {
    var x9 = $asArrayOf_S(xs, 1);
    return $n(x9).get(idx);
  } else if ((xs instanceof $ac_Z)) {
    var x10 = $asArrayOf_Z(xs, 1);
    return $n(x10).get(idx);
  } else if ((xs === null)) {
    throw new $c_jl_NullPointerException();
  } else {
    throw new $c_s_MatchError(xs);
  }
});
$c_sr_ScalaRunTime$.prototype.array_update__O__I__O__V = (function(xs, idx, value) {
  if ((xs instanceof $ac_O)) {
    var x2 = $asArrayOf_O(xs, 1);
    $n(x2).set(idx, value);
  } else if ((xs instanceof $ac_I)) {
    var x3 = $asArrayOf_I(xs, 1);
    $n(x3).set(idx, $uI(value));
  } else if ((xs instanceof $ac_D)) {
    var x4 = $asArrayOf_D(xs, 1);
    $n(x4).set(idx, $uD(value));
  } else if ((xs instanceof $ac_J)) {
    var x5 = $asArrayOf_J(xs, 1);
    $n(x5).set(idx, $uJ(value));
  } else if ((xs instanceof $ac_F)) {
    var x6 = $asArrayOf_F(xs, 1);
    $n(x6).set(idx, $uF(value));
  } else if ((xs instanceof $ac_C)) {
    var x7 = $asArrayOf_C(xs, 1);
    $n(x7).set(idx, $uC(value));
  } else if ((xs instanceof $ac_B)) {
    var x8 = $asArrayOf_B(xs, 1);
    $n(x8).set(idx, $uB(value));
  } else if ((xs instanceof $ac_S)) {
    var x9 = $asArrayOf_S(xs, 1);
    $n(x9).set(idx, $uS(value));
  } else if ((xs instanceof $ac_Z)) {
    var x10 = $asArrayOf_Z(xs, 1);
    $n(x10).set(idx, $uZ(value));
  } else if ((xs === null)) {
    throw new $c_jl_NullPointerException();
  } else {
    throw new $c_s_MatchError(xs);
  }
});
$c_sr_ScalaRunTime$.prototype._toString__s_Product__T = (function(x) {
  var this$1 = $n($n(x).productIterator__sc_Iterator());
  var start = ($n(x).productPrefix__T() + "(");
  return $f_sc_IterableOnceOps__mkString__T__T__T__T(this$1, start, ",", ")");
});
$c_sr_ScalaRunTime$.prototype.wrapRefArray__AO__sci_ArraySeq = (function(xs) {
  if ((xs === null)) {
    return null;
  } else if (($n(xs).u.length === 0)) {
    var this$1 = $m_sci_ArraySeq$();
    return $p_sci_ArraySeq$__emptyImpl__sci_ArraySeq$ofRef(this$1);
  } else {
    return new $c_sci_ArraySeq$ofRef(xs);
  }
});
var $d_sr_ScalaRunTime$ = new $TypeData().initClass($c_sr_ScalaRunTime$, "scala.runtime.ScalaRunTime$", ({
  sr_ScalaRunTime$: 1
}));
var $n_sr_ScalaRunTime$;
function $m_sr_ScalaRunTime$() {
  if ((!$n_sr_ScalaRunTime$)) {
    $n_sr_ScalaRunTime$ = new $c_sr_ScalaRunTime$();
  }
  return $n_sr_ScalaRunTime$;
}
/** @constructor */
function $c_sr_Statics$() {
}
$c_sr_Statics$.prototype = new $h_O();
$c_sr_Statics$.prototype.constructor = $c_sr_Statics$;
/** @constructor */
function $h_sr_Statics$() {
}
$h_sr_Statics$.prototype = $c_sr_Statics$.prototype;
$c_sr_Statics$.prototype.mix__I__I__I = (function(hash, data) {
  var h = this.mixLast__I__I__I(hash, data);
  var i = h;
  h = ((i << 13) | ((i >>> 19) | 0));
  return (((-430675100) + Math.imul(5, h)) | 0);
});
$c_sr_Statics$.prototype.mixLast__I__I__I = (function(hash, data) {
  var k = data;
  k = Math.imul((-862048943), k);
  var i = k;
  k = ((i << 15) | ((i >>> 17) | 0));
  k = Math.imul(461845907, k);
  return (hash ^ k);
});
$c_sr_Statics$.prototype.finalizeHash__I__I__I = (function(hash, length) {
  return this.avalanche__I__I((hash ^ length));
});
$c_sr_Statics$.prototype.avalanche__I__I = (function(h0) {
  var h = h0;
  h = (h ^ ((h >>> 16) | 0));
  h = Math.imul((-2048144789), h);
  h = (h ^ ((h >>> 13) | 0));
  h = Math.imul((-1028477387), h);
  h = (h ^ ((h >>> 16) | 0));
  return h;
});
$c_sr_Statics$.prototype.longHash__J__I = (function(lv) {
  var lo = lv.RTLong__f_lo;
  var hi = lv.RTLong__f_hi;
  return ((hi === (lo >> 31)) ? lo : (lo ^ hi));
});
$c_sr_Statics$.prototype.doubleHash__D__I = (function(dv) {
  var iv = $doubleToInt(dv);
  if ((iv === dv)) {
    return iv;
  } else {
    var this$1 = $m_RTLong$();
    var lo = this$1.org$scalajs$linker$runtime$RuntimeLong$$fromDoubleImpl__D__I(dv);
    var hi = this$1.RTLong$__f_org$scalajs$linker$runtime$RuntimeLong$$hiReturn;
    return (($m_RTLong$().org$scalajs$linker$runtime$RuntimeLong$$toDouble__I__I__D(lo, hi) === dv) ? (lo ^ hi) : $m_jl_FloatingPointBits$().numberHashCode__D__I(dv));
  }
});
$c_sr_Statics$.prototype.anyHash__O__I = (function(x) {
  if ((x === null)) {
    return 0;
  } else if (((typeof x) === "number")) {
    var x3 = $uD(x);
    return this.doubleHash__D__I(x3);
  } else if ((x instanceof $c_RTLong)) {
    var t = $uJ(x);
    var lo = t.RTLong__f_lo;
    var hi = t.RTLong__f_hi;
    return this.longHash__J__I(new $c_RTLong(lo, hi));
  } else {
    return $dp_hashCode__I($n(x));
  }
});
$c_sr_Statics$.prototype.ioobe__I__O = (function(n) {
  throw $ct_jl_IndexOutOfBoundsException__T__(new $c_jl_IndexOutOfBoundsException(), ("" + n));
});
var $d_sr_Statics$ = new $TypeData().initClass($c_sr_Statics$, "scala.runtime.Statics$", ({
  sr_Statics$: 1
}));
var $n_sr_Statics$;
function $m_sr_Statics$() {
  if ((!$n_sr_Statics$)) {
    $n_sr_Statics$ = new $c_sr_Statics$();
  }
  return $n_sr_Statics$;
}
/** @constructor */
function $c_sjs_js_timers_package$() {
}
$c_sjs_js_timers_package$.prototype = new $h_O();
$c_sjs_js_timers_package$.prototype.constructor = $c_sjs_js_timers_package$;
/** @constructor */
function $h_sjs_js_timers_package$() {
}
$h_sjs_js_timers_package$.prototype = $c_sjs_js_timers_package$.prototype;
$c_sjs_js_timers_package$.prototype.setTimeout__D__F0__sjs_js_timers_SetTimeoutHandle = (function(interval, body) {
  return setTimeout((() => {
    var this$1 = $n(body);
    this$1.apply__O();
  }), interval);
});
var $d_sjs_js_timers_package$ = new $TypeData().initClass($c_sjs_js_timers_package$, "scala.scalajs.js.timers.package$", ({
  sjs_js_timers_package$: 1
}));
var $n_sjs_js_timers_package$;
function $m_sjs_js_timers_package$() {
  if ((!$n_sjs_js_timers_package$)) {
    $n_sjs_js_timers_package$ = new $c_sjs_js_timers_package$();
  }
  return $n_sjs_js_timers_package$;
}
/** @constructor */
function $c_sjsr_Compat$() {
}
$c_sjsr_Compat$.prototype = new $h_O();
$c_sjsr_Compat$.prototype.constructor = $c_sjsr_Compat$;
/** @constructor */
function $h_sjsr_Compat$() {
}
$h_sjsr_Compat$.prototype = $c_sjsr_Compat$.prototype;
$c_sjsr_Compat$.prototype.toJSVarArgsImpl__sci_Seq__sjs_js_Array = (function(seq) {
  if (false) {
    var x2 = $as_sjsr_WrappedVarArgs(seq);
    return $n(x2).sjsr_WrappedVarArgs__f_scala$scalajs$runtime$WrappedVarArgs$$array;
  } else {
    var result = [];
    $n(seq).foreach__F1__V(new $c_sjsr_AnonFunction1(((x$2$2) => $uI(result.push(x$2$2)))));
    return result;
  }
});
var $d_sjsr_Compat$ = new $TypeData().initClass($c_sjsr_Compat$, "scala.scalajs.runtime.Compat$", ({
  sjsr_Compat$: 1
}));
var $n_sjsr_Compat$;
function $m_sjsr_Compat$() {
  if ((!$n_sjsr_Compat$)) {
    $n_sjsr_Compat$ = new $c_sjsr_Compat$();
  }
  return $n_sjsr_Compat$;
}
/** @constructor */
function $c_s_util_hashing_MurmurHash3() {
}
$c_s_util_hashing_MurmurHash3.prototype = new $h_O();
$c_s_util_hashing_MurmurHash3.prototype.constructor = $c_s_util_hashing_MurmurHash3;
/** @constructor */
function $h_s_util_hashing_MurmurHash3() {
}
$h_s_util_hashing_MurmurHash3.prototype = $c_s_util_hashing_MurmurHash3.prototype;
$c_s_util_hashing_MurmurHash3.prototype.mix__I__I__I = (function(hash, data) {
  var h = this.mixLast__I__I__I(hash, data);
  var i = h;
  h = ((i << 13) | ((i >>> 19) | 0));
  return (((-430675100) + Math.imul(5, h)) | 0);
});
$c_s_util_hashing_MurmurHash3.prototype.mixLast__I__I__I = (function(hash, data) {
  var k = data;
  k = Math.imul((-862048943), k);
  var i = k;
  k = ((i << 15) | ((i >>> 17) | 0));
  k = Math.imul(461845907, k);
  return (hash ^ k);
});
$c_s_util_hashing_MurmurHash3.prototype.finalizeHash__I__I__I = (function(hash, length) {
  return this.scala$util$hashing$MurmurHash3$$avalanche__I__I((hash ^ length));
});
$c_s_util_hashing_MurmurHash3.prototype.scala$util$hashing$MurmurHash3$$avalanche__I__I = (function(hash) {
  var h = hash;
  h = (h ^ ((h >>> 16) | 0));
  h = Math.imul((-2048144789), h);
  h = (h ^ ((h >>> 13) | 0));
  h = Math.imul((-1028477387), h);
  h = (h ^ ((h >>> 16) | 0));
  return h;
});
$c_s_util_hashing_MurmurHash3.prototype.productHash__s_Product__I__Z__I = (function(x, seed, ignorePrefix) {
  var arr = $n(x).productArity__I();
  if ((arr === 0)) {
    return $f_T__hashCode__I($n($n(x).productPrefix__T()));
  } else {
    var h = seed;
    if ((!ignorePrefix)) {
      h = this.mix__I__I__I(h, $f_T__hashCode__I($n($n(x).productPrefix__T())));
    }
    var i = 0;
    while ((i < arr)) {
      var $x_1 = h;
      var x$1 = $n(x).productElement__I__O(i);
      h = this.mix__I__I__I($x_1, $m_sr_Statics$().anyHash__O__I(x$1));
      i = ((1 + i) | 0);
    }
    return this.finalizeHash__I__I__I(h, arr);
  }
});
$c_s_util_hashing_MurmurHash3.prototype.unorderedHash__sc_IterableOnce__I__I = (function(xs, seed) {
  var a = 0;
  var b = 0;
  var n = 0;
  var c = 1;
  var iterator = $n(xs).iterator__sc_Iterator();
  while ($n(iterator).hasNext__Z()) {
    var x = $n(iterator).next__O();
    var h = $m_sr_Statics$().anyHash__O__I(x);
    a = ((a + h) | 0);
    b = (b ^ h);
    c = Math.imul(c, (1 | h));
    n = ((1 + n) | 0);
  }
  var h$2 = seed;
  h$2 = this.mix__I__I__I(h$2, a);
  h$2 = this.mix__I__I__I(h$2, b);
  h$2 = this.mixLast__I__I__I(h$2, c);
  return this.finalizeHash__I__I__I(h$2, n);
});
$c_s_util_hashing_MurmurHash3.prototype.orderedHash__sc_IterableOnce__I__I = (function(xs, seed) {
  var it = $n(xs).iterator__sc_Iterator();
  var h = seed;
  if ((!$n(it).hasNext__Z())) {
    return this.finalizeHash__I__I__I(h, 0);
  }
  var x0 = $n(it).next__O();
  if ((!$n(it).hasNext__Z())) {
    return this.finalizeHash__I__I__I(this.mix__I__I__I(h, $m_sr_Statics$().anyHash__O__I(x0)), 1);
  }
  var x1 = $n(it).next__O();
  var initial = $m_sr_Statics$().anyHash__O__I(x0);
  h = this.mix__I__I__I(h, initial);
  var h0 = h;
  var prev = $m_sr_Statics$().anyHash__O__I(x1);
  var rangeDiff = ((prev - initial) | 0);
  var i = 2;
  while ($n(it).hasNext__Z()) {
    h = this.mix__I__I__I(h, prev);
    var x = $n(it).next__O();
    var hash = $m_sr_Statics$().anyHash__O__I(x);
    if (((rangeDiff !== ((hash - prev) | 0)) || (rangeDiff === 0))) {
      h = this.mix__I__I__I(h, hash);
      i = ((1 + i) | 0);
      while ($n(it).hasNext__Z()) {
        var $x_1 = h;
        var x$1 = $n(it).next__O();
        h = this.mix__I__I__I($x_1, $m_sr_Statics$().anyHash__O__I(x$1));
        i = ((1 + i) | 0);
      }
      return this.finalizeHash__I__I__I(h, i);
    }
    prev = hash;
    i = ((1 + i) | 0);
  }
  return this.scala$util$hashing$MurmurHash3$$avalanche__I__I(this.mix__I__I__I(this.mix__I__I__I(h0, rangeDiff), prev));
});
$c_s_util_hashing_MurmurHash3.prototype.arrayHash__O__I__I = (function(a, seed) {
  var h = seed;
  var l = $m_jl_reflect_Array$().getLength__O__I(a);
  switch (l) {
    case 0: {
      return this.finalizeHash__I__I__I(h, 0);
      break;
    }
    case 1: {
      var $x_1 = h;
      var x = $m_sr_ScalaRunTime$().array_apply__O__I__O(a, 0);
      return this.finalizeHash__I__I__I(this.mix__I__I__I($x_1, $m_sr_Statics$().anyHash__O__I(x)), 1);
      break;
    }
    default: {
      var x$1 = $m_sr_ScalaRunTime$().array_apply__O__I__O(a, 0);
      var initial = $m_sr_Statics$().anyHash__O__I(x$1);
      h = this.mix__I__I__I(h, initial);
      var h0 = h;
      var x$2 = $m_sr_ScalaRunTime$().array_apply__O__I__O(a, 1);
      var prev = $m_sr_Statics$().anyHash__O__I(x$2);
      var rangeDiff = ((prev - initial) | 0);
      var i = 2;
      while ((i < l)) {
        h = this.mix__I__I__I(h, prev);
        var x$3 = $m_sr_ScalaRunTime$().array_apply__O__I__O(a, i);
        var hash = $m_sr_Statics$().anyHash__O__I(x$3);
        if (((rangeDiff !== ((hash - prev) | 0)) || (rangeDiff === 0))) {
          h = this.mix__I__I__I(h, hash);
          i = ((1 + i) | 0);
          while ((i < l)) {
            var $x_2 = h;
            var x$4 = $m_sr_ScalaRunTime$().array_apply__O__I__O(a, i);
            h = this.mix__I__I__I($x_2, $m_sr_Statics$().anyHash__O__I(x$4));
            i = ((1 + i) | 0);
          }
          return this.finalizeHash__I__I__I(h, l);
        }
        prev = hash;
        i = ((1 + i) | 0);
      }
      return this.scala$util$hashing$MurmurHash3$$avalanche__I__I(this.mix__I__I__I(this.mix__I__I__I(h0, rangeDiff), prev));
    }
  }
});
$c_s_util_hashing_MurmurHash3.prototype.rangeHash__I__I__I__I__I = (function(start, step, last, seed) {
  return this.scala$util$hashing$MurmurHash3$$avalanche__I__I(this.mix__I__I__I(this.mix__I__I__I(this.mix__I__I__I(seed, start), step), last));
});
$c_s_util_hashing_MurmurHash3.prototype.indexedSeqHash__sc_IndexedSeq__I__I = (function(a, seed) {
  var h = seed;
  var l = $n(a).length__I();
  switch (l) {
    case 0: {
      return this.finalizeHash__I__I__I(h, 0);
      break;
    }
    case 1: {
      var $x_1 = h;
      var x = $n(a).apply__I__O(0);
      return this.finalizeHash__I__I__I(this.mix__I__I__I($x_1, $m_sr_Statics$().anyHash__O__I(x)), 1);
      break;
    }
    default: {
      var x$1 = $n(a).apply__I__O(0);
      var initial = $m_sr_Statics$().anyHash__O__I(x$1);
      h = this.mix__I__I__I(h, initial);
      var h0 = h;
      var x$2 = $n(a).apply__I__O(1);
      var prev = $m_sr_Statics$().anyHash__O__I(x$2);
      var rangeDiff = ((prev - initial) | 0);
      var i = 2;
      while ((i < l)) {
        h = this.mix__I__I__I(h, prev);
        var x$3 = $n(a).apply__I__O(i);
        var hash = $m_sr_Statics$().anyHash__O__I(x$3);
        if (((rangeDiff !== ((hash - prev) | 0)) || (rangeDiff === 0))) {
          h = this.mix__I__I__I(h, hash);
          i = ((1 + i) | 0);
          while ((i < l)) {
            var $x_2 = h;
            var x$4 = $n(a).apply__I__O(i);
            h = this.mix__I__I__I($x_2, $m_sr_Statics$().anyHash__O__I(x$4));
            i = ((1 + i) | 0);
          }
          return this.finalizeHash__I__I__I(h, l);
        }
        prev = hash;
        i = ((1 + i) | 0);
      }
      return this.scala$util$hashing$MurmurHash3$$avalanche__I__I(this.mix__I__I__I(this.mix__I__I__I(h0, rangeDiff), prev));
    }
  }
});
$c_s_util_hashing_MurmurHash3.prototype.listHash__sci_List__I__I = (function(xs, seed) {
  var n = 0;
  var h = seed;
  var rangeState = 0;
  var rangeDiff = 0;
  var prev = 0;
  var initial = 0;
  var elems = xs;
  while ((!$n(elems).isEmpty__Z())) {
    var head = $n(elems).head__O();
    var tail = $as_sci_List($n(elems).tail__O());
    var hash = $m_sr_Statics$().anyHash__O__I(head);
    h = this.mix__I__I__I(h, hash);
    var x1 = rangeState;
    switch (x1) {
      case 0: {
        initial = hash;
        rangeState = 1;
        break;
      }
      case 1: {
        rangeDiff = ((hash - prev) | 0);
        rangeState = 2;
        break;
      }
      case 2: {
        if (((rangeDiff !== ((hash - prev) | 0)) || (rangeDiff === 0))) {
          rangeState = 3;
        }
        break;
      }
    }
    prev = hash;
    n = ((1 + n) | 0);
    elems = tail;
  }
  return ((rangeState === 2) ? this.rangeHash__I__I__I__I__I(initial, rangeDiff, prev, seed) : this.finalizeHash__I__I__I(h, n));
});
$c_s_util_hashing_MurmurHash3.prototype.arrayHash$mZc$sp__AZ__I__I = (function(a, seed) {
  var h = seed;
  var l = $n(a).u.length;
  switch (l) {
    case 0: {
      return this.finalizeHash__I__I__I(h, 0);
      break;
    }
    case 1: {
      return this.finalizeHash__I__I__I(this.mix__I__I__I(h, ($n(a).get(0) ? 1231 : 1237)), 1);
      break;
    }
    default: {
      var initial = ($n(a).get(0) ? 1231 : 1237);
      h = this.mix__I__I__I(h, initial);
      var h0 = h;
      var prev = ($n(a).get(1) ? 1231 : 1237);
      var rangeDiff = ((prev - initial) | 0);
      var i = 2;
      while ((i < l)) {
        h = this.mix__I__I__I(h, prev);
        var hash = ($n(a).get(i) ? 1231 : 1237);
        if (((rangeDiff !== ((hash - prev) | 0)) || (rangeDiff === 0))) {
          h = this.mix__I__I__I(h, hash);
          i = ((1 + i) | 0);
          while ((i < l)) {
            h = this.mix__I__I__I(h, ($n(a).get(i) ? 1231 : 1237));
            i = ((1 + i) | 0);
          }
          return this.finalizeHash__I__I__I(h, l);
        }
        prev = hash;
        i = ((1 + i) | 0);
      }
      return this.scala$util$hashing$MurmurHash3$$avalanche__I__I(this.mix__I__I__I(this.mix__I__I__I(h0, rangeDiff), prev));
    }
  }
});
$c_s_util_hashing_MurmurHash3.prototype.arrayHash$mBc$sp__AB__I__I = (function(a, seed) {
  var h = seed;
  var l = $n(a).u.length;
  switch (l) {
    case 0: {
      return this.finalizeHash__I__I__I(h, 0);
      break;
    }
    case 1: {
      return this.finalizeHash__I__I__I(this.mix__I__I__I(h, $n(a).get(0)), 1);
      break;
    }
    default: {
      var initial = $n(a).get(0);
      h = this.mix__I__I__I(h, initial);
      var h0 = h;
      var prev = $n(a).get(1);
      var rangeDiff = ((prev - initial) | 0);
      var i = 2;
      while ((i < l)) {
        h = this.mix__I__I__I(h, prev);
        var hash = $n(a).get(i);
        if (((rangeDiff !== ((hash - prev) | 0)) || (rangeDiff === 0))) {
          h = this.mix__I__I__I(h, hash);
          i = ((1 + i) | 0);
          while ((i < l)) {
            h = this.mix__I__I__I(h, $n(a).get(i));
            i = ((1 + i) | 0);
          }
          return this.finalizeHash__I__I__I(h, l);
        }
        prev = hash;
        i = ((1 + i) | 0);
      }
      return this.scala$util$hashing$MurmurHash3$$avalanche__I__I(this.mix__I__I__I(this.mix__I__I__I(h0, rangeDiff), prev));
    }
  }
});
$c_s_util_hashing_MurmurHash3.prototype.arrayHash$mCc$sp__AC__I__I = (function(a, seed) {
  var h = seed;
  var l = $n(a).u.length;
  switch (l) {
    case 0: {
      return this.finalizeHash__I__I__I(h, 0);
      break;
    }
    case 1: {
      return this.finalizeHash__I__I__I(this.mix__I__I__I(h, $n(a).get(0)), 1);
      break;
    }
    default: {
      var initial = $n(a).get(0);
      h = this.mix__I__I__I(h, initial);
      var h0 = h;
      var prev = $n(a).get(1);
      var rangeDiff = ((prev - initial) | 0);
      var i = 2;
      while ((i < l)) {
        h = this.mix__I__I__I(h, prev);
        var hash = $n(a).get(i);
        if (((rangeDiff !== ((hash - prev) | 0)) || (rangeDiff === 0))) {
          h = this.mix__I__I__I(h, hash);
          i = ((1 + i) | 0);
          while ((i < l)) {
            h = this.mix__I__I__I(h, $n(a).get(i));
            i = ((1 + i) | 0);
          }
          return this.finalizeHash__I__I__I(h, l);
        }
        prev = hash;
        i = ((1 + i) | 0);
      }
      return this.scala$util$hashing$MurmurHash3$$avalanche__I__I(this.mix__I__I__I(this.mix__I__I__I(h0, rangeDiff), prev));
    }
  }
});
$c_s_util_hashing_MurmurHash3.prototype.arrayHash$mDc$sp__AD__I__I = (function(a, seed) {
  var h = seed;
  var l = $n(a).u.length;
  switch (l) {
    case 0: {
      return this.finalizeHash__I__I__I(h, 0);
      break;
    }
    case 1: {
      var $x_1 = h;
      var dv = $n(a).get(0);
      return this.finalizeHash__I__I__I(this.mix__I__I__I($x_1, $m_sr_Statics$().doubleHash__D__I(dv)), 1);
      break;
    }
    default: {
      var dv$1 = $n(a).get(0);
      var initial = $m_sr_Statics$().doubleHash__D__I(dv$1);
      h = this.mix__I__I__I(h, initial);
      var h0 = h;
      var dv$2 = $n(a).get(1);
      var prev = $m_sr_Statics$().doubleHash__D__I(dv$2);
      var rangeDiff = ((prev - initial) | 0);
      var i = 2;
      while ((i < l)) {
        h = this.mix__I__I__I(h, prev);
        var dv$3 = $n(a).get(i);
        var hash = $m_sr_Statics$().doubleHash__D__I(dv$3);
        if (((rangeDiff !== ((hash - prev) | 0)) || (rangeDiff === 0))) {
          h = this.mix__I__I__I(h, hash);
          i = ((1 + i) | 0);
          while ((i < l)) {
            var $x_2 = h;
            var dv$4 = $n(a).get(i);
            h = this.mix__I__I__I($x_2, $m_sr_Statics$().doubleHash__D__I(dv$4));
            i = ((1 + i) | 0);
          }
          return this.finalizeHash__I__I__I(h, l);
        }
        prev = hash;
        i = ((1 + i) | 0);
      }
      return this.scala$util$hashing$MurmurHash3$$avalanche__I__I(this.mix__I__I__I(this.mix__I__I__I(h0, rangeDiff), prev));
    }
  }
});
$c_s_util_hashing_MurmurHash3.prototype.arrayHash$mFc$sp__AF__I__I = (function(a, seed) {
  var h = seed;
  var l = $n(a).u.length;
  switch (l) {
    case 0: {
      return this.finalizeHash__I__I__I(h, 0);
      break;
    }
    case 1: {
      var $x_1 = h;
      var fv = $n(a).get(0);
      var this$1 = $m_sr_Statics$();
      return this.finalizeHash__I__I__I(this.mix__I__I__I($x_1, this$1.doubleHash__D__I(fv)), 1);
      break;
    }
    default: {
      var fv$1 = $n(a).get(0);
      var this$2 = $m_sr_Statics$();
      var initial = this$2.doubleHash__D__I(fv$1);
      h = this.mix__I__I__I(h, initial);
      var h0 = h;
      var fv$2 = $n(a).get(1);
      var this$3 = $m_sr_Statics$();
      var prev = this$3.doubleHash__D__I(fv$2);
      var rangeDiff = ((prev - initial) | 0);
      var i = 2;
      while ((i < l)) {
        h = this.mix__I__I__I(h, prev);
        var fv$3 = $n(a).get(i);
        var this$4 = $m_sr_Statics$();
        var hash = this$4.doubleHash__D__I(fv$3);
        if (((rangeDiff !== ((hash - prev) | 0)) || (rangeDiff === 0))) {
          h = this.mix__I__I__I(h, hash);
          i = ((1 + i) | 0);
          while ((i < l)) {
            var $x_2 = h;
            var fv$4 = $n(a).get(i);
            var this$5 = $m_sr_Statics$();
            h = this.mix__I__I__I($x_2, this$5.doubleHash__D__I(fv$4));
            i = ((1 + i) | 0);
          }
          return this.finalizeHash__I__I__I(h, l);
        }
        prev = hash;
        i = ((1 + i) | 0);
      }
      return this.scala$util$hashing$MurmurHash3$$avalanche__I__I(this.mix__I__I__I(this.mix__I__I__I(h0, rangeDiff), prev));
    }
  }
});
$c_s_util_hashing_MurmurHash3.prototype.arrayHash$mIc$sp__AI__I__I = (function(a, seed) {
  var h = seed;
  var l = $n(a).u.length;
  switch (l) {
    case 0: {
      return this.finalizeHash__I__I__I(h, 0);
      break;
    }
    case 1: {
      return this.finalizeHash__I__I__I(this.mix__I__I__I(h, $n(a).get(0)), 1);
      break;
    }
    default: {
      var initial = $n(a).get(0);
      h = this.mix__I__I__I(h, initial);
      var h0 = h;
      var prev = $n(a).get(1);
      var rangeDiff = ((prev - initial) | 0);
      var i = 2;
      while ((i < l)) {
        h = this.mix__I__I__I(h, prev);
        var hash = $n(a).get(i);
        if (((rangeDiff !== ((hash - prev) | 0)) || (rangeDiff === 0))) {
          h = this.mix__I__I__I(h, hash);
          i = ((1 + i) | 0);
          while ((i < l)) {
            h = this.mix__I__I__I(h, $n(a).get(i));
            i = ((1 + i) | 0);
          }
          return this.finalizeHash__I__I__I(h, l);
        }
        prev = hash;
        i = ((1 + i) | 0);
      }
      return this.scala$util$hashing$MurmurHash3$$avalanche__I__I(this.mix__I__I__I(this.mix__I__I__I(h0, rangeDiff), prev));
    }
  }
});
$c_s_util_hashing_MurmurHash3.prototype.arrayHash$mJc$sp__AJ__I__I = (function(a, seed) {
  var h = seed;
  var l = $n(a).u.length;
  switch (l) {
    case 0: {
      return this.finalizeHash__I__I__I(h, 0);
      break;
    }
    case 1: {
      var $x_1 = h;
      var t = $n(a).get(0);
      var lo = t.RTLong__f_lo;
      var hi = t.RTLong__f_hi;
      return this.finalizeHash__I__I__I(this.mix__I__I__I($x_1, $m_sr_Statics$().longHash__J__I(new $c_RTLong(lo, hi))), 1);
      break;
    }
    default: {
      var t$1 = $n(a).get(0);
      var lo$1 = t$1.RTLong__f_lo;
      var hi$1 = t$1.RTLong__f_hi;
      var initial = $m_sr_Statics$().longHash__J__I(new $c_RTLong(lo$1, hi$1));
      h = this.mix__I__I__I(h, initial);
      var h0 = h;
      var t$2 = $n(a).get(1);
      var lo$2 = t$2.RTLong__f_lo;
      var hi$2 = t$2.RTLong__f_hi;
      var prev = $m_sr_Statics$().longHash__J__I(new $c_RTLong(lo$2, hi$2));
      var rangeDiff = ((prev - initial) | 0);
      var i = 2;
      while ((i < l)) {
        h = this.mix__I__I__I(h, prev);
        var t$3 = $n(a).get(i);
        var lo$3 = t$3.RTLong__f_lo;
        var hi$3 = t$3.RTLong__f_hi;
        var hash = $m_sr_Statics$().longHash__J__I(new $c_RTLong(lo$3, hi$3));
        if (((rangeDiff !== ((hash - prev) | 0)) || (rangeDiff === 0))) {
          h = this.mix__I__I__I(h, hash);
          i = ((1 + i) | 0);
          while ((i < l)) {
            var $x_2 = h;
            var t$4 = $n(a).get(i);
            var lo$4 = t$4.RTLong__f_lo;
            var hi$4 = t$4.RTLong__f_hi;
            h = this.mix__I__I__I($x_2, $m_sr_Statics$().longHash__J__I(new $c_RTLong(lo$4, hi$4)));
            i = ((1 + i) | 0);
          }
          return this.finalizeHash__I__I__I(h, l);
        }
        prev = hash;
        i = ((1 + i) | 0);
      }
      return this.scala$util$hashing$MurmurHash3$$avalanche__I__I(this.mix__I__I__I(this.mix__I__I__I(h0, rangeDiff), prev));
    }
  }
});
$c_s_util_hashing_MurmurHash3.prototype.arrayHash$mSc$sp__AS__I__I = (function(a, seed) {
  var h = seed;
  var l = $n(a).u.length;
  switch (l) {
    case 0: {
      return this.finalizeHash__I__I__I(h, 0);
      break;
    }
    case 1: {
      return this.finalizeHash__I__I__I(this.mix__I__I__I(h, $n(a).get(0)), 1);
      break;
    }
    default: {
      var initial = $n(a).get(0);
      h = this.mix__I__I__I(h, initial);
      var h0 = h;
      var prev = $n(a).get(1);
      var rangeDiff = ((prev - initial) | 0);
      var i = 2;
      while ((i < l)) {
        h = this.mix__I__I__I(h, prev);
        var hash = $n(a).get(i);
        if (((rangeDiff !== ((hash - prev) | 0)) || (rangeDiff === 0))) {
          h = this.mix__I__I__I(h, hash);
          i = ((1 + i) | 0);
          while ((i < l)) {
            h = this.mix__I__I__I(h, $n(a).get(i));
            i = ((1 + i) | 0);
          }
          return this.finalizeHash__I__I__I(h, l);
        }
        prev = hash;
        i = ((1 + i) | 0);
      }
      return this.scala$util$hashing$MurmurHash3$$avalanche__I__I(this.mix__I__I__I(this.mix__I__I__I(h0, rangeDiff), prev));
    }
  }
});
$c_s_util_hashing_MurmurHash3.prototype.arrayHash$mVc$sp__Ajl_Void__I__I = (function(a, seed) {
  var h = seed;
  var l = $n(a).u.length;
  switch (l) {
    case 0: {
      return this.finalizeHash__I__I__I(h, 0);
      break;
    }
    case 1: {
      return this.finalizeHash__I__I__I(this.mix__I__I__I(h, 0), 1);
      break;
    }
    default: {
      h = this.mix__I__I__I(h, 0);
      var h0 = h;
      var prev = 0;
      var rangeDiff = prev;
      var i = 2;
      while ((i < l)) {
        h = this.mix__I__I__I(h, prev);
        if (((rangeDiff !== ((-prev) | 0)) || (rangeDiff === 0))) {
          h = this.mix__I__I__I(h, 0);
          i = ((1 + i) | 0);
          while ((i < l)) {
            h = this.mix__I__I__I(h, 0);
            i = ((1 + i) | 0);
          }
          return this.finalizeHash__I__I__I(h, l);
        }
        prev = 0;
        i = ((1 + i) | 0);
      }
      return this.scala$util$hashing$MurmurHash3$$avalanche__I__I(this.mix__I__I__I(this.mix__I__I__I(h0, rangeDiff), prev));
    }
  }
});
/** @constructor */
function $c_Lcom_raquo_airstream_ownership_OneTimeOwner(onAccessAfterKilled) {
  this.Lcom_raquo_airstream_ownership_OneTimeOwner__f_subscriptions = null;
  this.Lcom_raquo_airstream_ownership_OneTimeOwner__f_onAccessAfterKilled = null;
  this.Lcom_raquo_airstream_ownership_OneTimeOwner__f__isKilledForever = false;
  this.Lcom_raquo_airstream_ownership_OneTimeOwner__f_onAccessAfterKilled = onAccessAfterKilled;
  $f_Lcom_raquo_airstream_ownership_Owner__$init$__V(this);
  this.Lcom_raquo_airstream_ownership_OneTimeOwner__f__isKilledForever = false;
}
$c_Lcom_raquo_airstream_ownership_OneTimeOwner.prototype = new $h_O();
$c_Lcom_raquo_airstream_ownership_OneTimeOwner.prototype.constructor = $c_Lcom_raquo_airstream_ownership_OneTimeOwner;
/** @constructor */
function $h_Lcom_raquo_airstream_ownership_OneTimeOwner() {
}
$h_Lcom_raquo_airstream_ownership_OneTimeOwner.prototype = $c_Lcom_raquo_airstream_ownership_OneTimeOwner.prototype;
$c_Lcom_raquo_airstream_ownership_OneTimeOwner.prototype.own__Lcom_raquo_airstream_ownership_Subscription__V = (function(subscription) {
  if (this.Lcom_raquo_airstream_ownership_OneTimeOwner__f__isKilledForever) {
    var this$1 = $n(subscription);
    $p_Lcom_raquo_airstream_ownership_Subscription__safeCleanup__V(this$1);
    $n(this.Lcom_raquo_airstream_ownership_OneTimeOwner__f_onAccessAfterKilled).apply__O();
  } else {
    $f_Lcom_raquo_airstream_ownership_Owner__own__Lcom_raquo_airstream_ownership_Subscription__V(this, subscription);
  }
});
$c_Lcom_raquo_airstream_ownership_OneTimeOwner.prototype.killSubscriptions__V = (function() {
  $f_Lcom_raquo_airstream_ownership_Owner__killSubscriptions__V(this);
  this.Lcom_raquo_airstream_ownership_OneTimeOwner__f__isKilledForever = true;
});
var $d_Lcom_raquo_airstream_ownership_OneTimeOwner = new $TypeData().initClass($c_Lcom_raquo_airstream_ownership_OneTimeOwner, "com.raquo.airstream.ownership.OneTimeOwner", ({
  Lcom_raquo_airstream_ownership_OneTimeOwner: 1,
  Lcom_raquo_airstream_ownership_Owner: 1
}));
/** @constructor */
function $c_Lcom_raquo_laminar_codecs_package$$anon$2(outer) {
  if ((outer === null)) {
    throw new $c_jl_NullPointerException();
  }
}
$c_Lcom_raquo_laminar_codecs_package$$anon$2.prototype = new $h_O();
$c_Lcom_raquo_laminar_codecs_package$$anon$2.prototype.constructor = $c_Lcom_raquo_laminar_codecs_package$$anon$2;
/** @constructor */
function $h_Lcom_raquo_laminar_codecs_package$$anon$2() {
}
$h_Lcom_raquo_laminar_codecs_package$$anon$2.prototype = $c_Lcom_raquo_laminar_codecs_package$$anon$2.prototype;
$c_Lcom_raquo_laminar_codecs_package$$anon$2.prototype.encode__O__O = (function(scalaValue) {
  return scalaValue;
});
$c_Lcom_raquo_laminar_codecs_package$$anon$2.prototype.decode__O__O = (function(domValue) {
  return domValue;
});
var $d_Lcom_raquo_laminar_codecs_package$$anon$2 = new $TypeData().initClass($c_Lcom_raquo_laminar_codecs_package$$anon$2, "com.raquo.laminar.codecs.package$$anon$2", ({
  Lcom_raquo_laminar_codecs_package$$anon$2: 1,
  Lcom_raquo_laminar_codecs_Codec: 1
}));
/** @constructor */
function $c_Lcom_raquo_laminar_keys_CompositeKey(name, getRawDomValue, setRawDomValue, separator) {
}
$c_Lcom_raquo_laminar_keys_CompositeKey.prototype = new $h_Lcom_raquo_laminar_keys_Key();
$c_Lcom_raquo_laminar_keys_CompositeKey.prototype.constructor = $c_Lcom_raquo_laminar_keys_CompositeKey;
/** @constructor */
function $h_Lcom_raquo_laminar_keys_CompositeKey() {
}
$h_Lcom_raquo_laminar_keys_CompositeKey.prototype = $c_Lcom_raquo_laminar_keys_CompositeKey.prototype;
var $d_Lcom_raquo_laminar_keys_CompositeKey = new $TypeData().initClass($c_Lcom_raquo_laminar_keys_CompositeKey, "com.raquo.laminar.keys.CompositeKey", ({
  Lcom_raquo_laminar_keys_CompositeKey: 1,
  Lcom_raquo_laminar_keys_Key: 1
}));
/** @constructor */
function $c_Lcom_raquo_laminar_keys_HtmlAttr(name, codec) {
  this.Lcom_raquo_laminar_keys_HtmlAttr__f_name = null;
  this.Lcom_raquo_laminar_keys_HtmlAttr__f_codec = null;
  this.Lcom_raquo_laminar_keys_HtmlAttr__f_name = name;
  this.Lcom_raquo_laminar_keys_HtmlAttr__f_codec = codec;
}
$c_Lcom_raquo_laminar_keys_HtmlAttr.prototype = new $h_Lcom_raquo_laminar_keys_Key();
$c_Lcom_raquo_laminar_keys_HtmlAttr.prototype.constructor = $c_Lcom_raquo_laminar_keys_HtmlAttr;
/** @constructor */
function $h_Lcom_raquo_laminar_keys_HtmlAttr() {
}
$h_Lcom_raquo_laminar_keys_HtmlAttr.prototype = $c_Lcom_raquo_laminar_keys_HtmlAttr.prototype;
var $d_Lcom_raquo_laminar_keys_HtmlAttr = new $TypeData().initClass($c_Lcom_raquo_laminar_keys_HtmlAttr, "com.raquo.laminar.keys.HtmlAttr", ({
  Lcom_raquo_laminar_keys_HtmlAttr: 1,
  Lcom_raquo_laminar_keys_Key: 1
}));
/** @constructor */
function $c_Lcom_raquo_laminar_keys_SvgAttr(localName, codec, namespacePrefix) {
  this.Lcom_raquo_laminar_keys_SvgAttr__f_localName = null;
  this.Lcom_raquo_laminar_keys_SvgAttr__f_codec = null;
  this.Lcom_raquo_laminar_keys_SvgAttr__f_name = null;
  this.Lcom_raquo_laminar_keys_SvgAttr__f_namespaceUri = null;
  this.Lcom_raquo_laminar_keys_SvgAttr__f_localName = localName;
  this.Lcom_raquo_laminar_keys_SvgAttr__f_codec = codec;
  var this$1 = $n(namespacePrefix);
  if (this$1.isEmpty__Z()) {
    var this$2 = $m_s_None$();
  } else {
    var arg1 = this$1.get__O();
    var _$1 = $as_T(arg1);
    var this$2 = new $c_s_Some(((_$1 + ":") + localName));
  }
  this.Lcom_raquo_laminar_keys_SvgAttr__f_name = $as_T((this$2.isEmpty__Z() ? localName : this$2.get__O()));
  var this$3 = $n(namespacePrefix);
  if (this$3.isEmpty__Z()) {
    var $x_1 = $m_s_None$();
  } else {
    var arg1$1 = this$3.get__O();
    var namespace = $as_T(arg1$1);
    var $x_1 = new $c_s_Some($m_Lcom_raquo_laminar_keys_SvgAttr$().namespaceUri__T__T(namespace));
  }
  this.Lcom_raquo_laminar_keys_SvgAttr__f_namespaceUri = $x_1;
}
$c_Lcom_raquo_laminar_keys_SvgAttr.prototype = new $h_Lcom_raquo_laminar_keys_Key();
$c_Lcom_raquo_laminar_keys_SvgAttr.prototype.constructor = $c_Lcom_raquo_laminar_keys_SvgAttr;
/** @constructor */
function $h_Lcom_raquo_laminar_keys_SvgAttr() {
}
$h_Lcom_raquo_laminar_keys_SvgAttr.prototype = $c_Lcom_raquo_laminar_keys_SvgAttr.prototype;
var $d_Lcom_raquo_laminar_keys_SvgAttr = new $TypeData().initClass($c_Lcom_raquo_laminar_keys_SvgAttr, "com.raquo.laminar.keys.SvgAttr", ({
  Lcom_raquo_laminar_keys_SvgAttr: 1,
  Lcom_raquo_laminar_keys_Key: 1
}));
/** @constructor */
function $c_Lcom_raquo_laminar_modifiers_Modifier$$anon$1() {
}
$c_Lcom_raquo_laminar_modifiers_Modifier$$anon$1.prototype = new $h_O();
$c_Lcom_raquo_laminar_modifiers_Modifier$$anon$1.prototype.constructor = $c_Lcom_raquo_laminar_modifiers_Modifier$$anon$1;
/** @constructor */
function $h_Lcom_raquo_laminar_modifiers_Modifier$$anon$1() {
}
$h_Lcom_raquo_laminar_modifiers_Modifier$$anon$1.prototype = $c_Lcom_raquo_laminar_modifiers_Modifier$$anon$1.prototype;
$c_Lcom_raquo_laminar_modifiers_Modifier$$anon$1.prototype.apply__Lcom_raquo_laminar_nodes_ReactiveElement__V = (function(element) {
});
var $d_Lcom_raquo_laminar_modifiers_Modifier$$anon$1 = new $TypeData().initClass($c_Lcom_raquo_laminar_modifiers_Modifier$$anon$1, "com.raquo.laminar.modifiers.Modifier$$anon$1", ({
  Lcom_raquo_laminar_modifiers_Modifier$$anon$1: 1,
  Lcom_raquo_laminar_modifiers_Modifier: 1
}));
/** @constructor */
function $c_Lcom_raquo_laminar_modifiers_Modifier$$anon$2(f$2, outer) {
  this.Lcom_raquo_laminar_modifiers_Modifier$$anon$2__f_f$1 = null;
  this.Lcom_raquo_laminar_modifiers_Modifier$$anon$2__f_f$1 = f$2;
  if ((outer === null)) {
    throw new $c_jl_NullPointerException();
  }
}
$c_Lcom_raquo_laminar_modifiers_Modifier$$anon$2.prototype = new $h_O();
$c_Lcom_raquo_laminar_modifiers_Modifier$$anon$2.prototype.constructor = $c_Lcom_raquo_laminar_modifiers_Modifier$$anon$2;
/** @constructor */
function $h_Lcom_raquo_laminar_modifiers_Modifier$$anon$2() {
}
$h_Lcom_raquo_laminar_modifiers_Modifier$$anon$2.prototype = $c_Lcom_raquo_laminar_modifiers_Modifier$$anon$2.prototype;
$c_Lcom_raquo_laminar_modifiers_Modifier$$anon$2.prototype.apply__Lcom_raquo_laminar_nodes_ReactiveElement__V = (function(element) {
  var this$2 = $m_Lcom_raquo_airstream_core_Transaction$onStart$();
  var f = (() => {
    $n(this.Lcom_raquo_laminar_modifiers_Modifier$$anon$2__f_f$1).apply__O__O(element);
  });
  $m_Lcom_raquo_airstream_core_Transaction$onStart$();
  var when = true;
  if ((this$2.Lcom_raquo_airstream_core_Transaction$onStart$__f_isSharedStart || (!when))) {
    f();
  } else {
    this$2.Lcom_raquo_airstream_core_Transaction$onStart$__f_isSharedStart = true;
    try {
      f();
    } finally {
      this$2.Lcom_raquo_airstream_core_Transaction$onStart$__f_isSharedStart = false;
      $p_Lcom_raquo_airstream_core_Transaction$onStart$__resolve__V(this$2);
    }
  }
});
var $d_Lcom_raquo_laminar_modifiers_Modifier$$anon$2 = new $TypeData().initClass($c_Lcom_raquo_laminar_modifiers_Modifier$$anon$2, "com.raquo.laminar.modifiers.Modifier$$anon$2", ({
  Lcom_raquo_laminar_modifiers_Modifier$$anon$2: 1,
  Lcom_raquo_laminar_modifiers_Modifier: 1
}));
/** @constructor */
function $c_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1(render$2, outer) {
  this.Lcom_raquo_laminar_modifiers_RenderableText$$anon$1__f_render$1 = null;
  this.Lcom_raquo_laminar_modifiers_RenderableText$$anon$1__f_render$1 = render$2;
  if ((outer === null)) {
    throw new $c_jl_NullPointerException();
  }
}
$c_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1.prototype = new $h_O();
$c_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1.prototype.constructor = $c_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1;
/** @constructor */
function $h_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1() {
}
$h_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1.prototype = $c_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1.prototype;
$c_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1.prototype.asString__O__T = (function(value) {
  return $as_T($n(this.Lcom_raquo_laminar_modifiers_RenderableText$$anon$1__f_render$1).apply__O__O(value));
});
var $d_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1 = new $TypeData().initClass($c_Lcom_raquo_laminar_modifiers_RenderableText$$anon$1, "com.raquo.laminar.modifiers.RenderableText$$anon$1", ({
  Lcom_raquo_laminar_modifiers_RenderableText$$anon$1: 1,
  Lcom_raquo_laminar_modifiers_RenderableText: 1
}));
function $f_Lcom_raquo_laminar_nodes_ParentNode__$init$__V($thiz) {
  $thiz.com$raquo$laminar$nodes$ParentNode$_setter_$dynamicOwner_$eq__Lcom_raquo_airstream_ownership_DynamicOwner__V(new $c_Lcom_raquo_airstream_ownership_DynamicOwner(new $c_sjsr_AnonFunction0((() => {
    var this$3 = $n($m_Lcom_raquo_laminar_DomApi$().debugPath__Lorg_scalajs_dom_Node__sci_List__sci_List($thiz.ref__Lorg_scalajs_dom_Node(), ($m_Lcom_raquo_laminar_DomApi$(), $m_sci_Nil$())));
    var path = $f_sc_IterableOnceOps__mkString__T__T__T__T(this$3, "", " > ", "");
    throw $ct_jl_Exception__T__(new $c_jl_Exception(), ("Attempting to use owner of unmounted element: " + path));
  }))));
}
function $is_Lcom_raquo_laminar_nodes_ParentNode(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lcom_raquo_laminar_nodes_ParentNode)));
}
function $as_Lcom_raquo_laminar_nodes_ParentNode(obj) {
  return (($is_Lcom_raquo_laminar_nodes_ParentNode(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.laminar.nodes.ParentNode"));
}
function $isArrayOf_Lcom_raquo_laminar_nodes_ParentNode(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_laminar_nodes_ParentNode)));
}
function $asArrayOf_Lcom_raquo_laminar_nodes_ParentNode(obj, depth) {
  return (($isArrayOf_Lcom_raquo_laminar_nodes_ParentNode(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.laminar.nodes.ParentNode;", depth));
}
/** @constructor */
function $c_Lcom_raquo_laminar_tags_HtmlTag(name, void$1) {
  this.Lcom_raquo_laminar_tags_HtmlTag__f_name = null;
  this.Lcom_raquo_laminar_tags_HtmlTag__f_name = name;
}
$c_Lcom_raquo_laminar_tags_HtmlTag.prototype = new $h_O();
$c_Lcom_raquo_laminar_tags_HtmlTag.prototype.constructor = $c_Lcom_raquo_laminar_tags_HtmlTag;
/** @constructor */
function $h_Lcom_raquo_laminar_tags_HtmlTag() {
}
$h_Lcom_raquo_laminar_tags_HtmlTag.prototype = $c_Lcom_raquo_laminar_tags_HtmlTag.prototype;
$c_Lcom_raquo_laminar_tags_HtmlTag.prototype.apply__sci_Seq__Lcom_raquo_laminar_nodes_ReactiveHtmlElement = (function(modifiers) {
  var element = this.build__Lcom_raquo_laminar_nodes_ReactiveHtmlElement();
  $n(modifiers).foreach__F1__V(new $c_sjsr_AnonFunction1(((modifier) => {
    var modifier$1 = $as_Lcom_raquo_laminar_modifiers_Modifier(modifier);
    $n(modifier$1).apply__Lcom_raquo_laminar_nodes_ReactiveElement__V(element);
  })));
  return element;
});
$c_Lcom_raquo_laminar_tags_HtmlTag.prototype.build__Lcom_raquo_laminar_nodes_ReactiveHtmlElement = (function() {
  return new $c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement(this, $m_Lcom_raquo_laminar_DomApi$().createHtmlElement__Lcom_raquo_laminar_tags_HtmlTag__Lorg_scalajs_dom_HTMLElement(this));
});
var $d_Lcom_raquo_laminar_tags_HtmlTag = new $TypeData().initClass($c_Lcom_raquo_laminar_tags_HtmlTag, "com.raquo.laminar.tags.HtmlTag", ({
  Lcom_raquo_laminar_tags_HtmlTag: 1,
  Lcom_raquo_laminar_tags_Tag: 1
}));
/** @constructor */
function $c_Lcom_raquo_laminar_tags_SvgTag(name, void$1) {
  this.Lcom_raquo_laminar_tags_SvgTag__f_name = null;
  this.Lcom_raquo_laminar_tags_SvgTag__f_name = name;
}
$c_Lcom_raquo_laminar_tags_SvgTag.prototype = new $h_O();
$c_Lcom_raquo_laminar_tags_SvgTag.prototype.constructor = $c_Lcom_raquo_laminar_tags_SvgTag;
/** @constructor */
function $h_Lcom_raquo_laminar_tags_SvgTag() {
}
$h_Lcom_raquo_laminar_tags_SvgTag.prototype = $c_Lcom_raquo_laminar_tags_SvgTag.prototype;
var $d_Lcom_raquo_laminar_tags_SvgTag = new $TypeData().initClass($c_Lcom_raquo_laminar_tags_SvgTag, "com.raquo.laminar.tags.SvgTag", ({
  Lcom_raquo_laminar_tags_SvgTag: 1,
  Lcom_raquo_laminar_tags_Tag: 1
}));
/** @constructor */
function $c_jl_Number() {
}
$c_jl_Number.prototype = new $h_O();
$c_jl_Number.prototype.constructor = $c_jl_Number;
/** @constructor */
function $h_jl_Number() {
}
$h_jl_Number.prototype = $c_jl_Number.prototype;
function $is_jl_Number(obj) {
  return (((obj instanceof $c_jl_Number) || ((typeof obj) === "number")) || (obj instanceof $c_RTLong));
}
function $as_jl_Number(obj) {
  return (($is_jl_Number(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "java.lang.Number"));
}
function $isArrayOf_jl_Number(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Number)));
}
function $asArrayOf_jl_Number(obj, depth) {
  return (($isArrayOf_jl_Number(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Number;", depth));
}
/** @constructor */
function $c_jl_StackTraceElement(declaringClass, methodName, fileName, lineNumber, columnNumber) {
  this.jl_StackTraceElement__f_declaringClass = null;
  this.jl_StackTraceElement__f_methodName = null;
  this.jl_StackTraceElement__f_fileName = null;
  this.jl_StackTraceElement__f_lineNumber = 0;
  this.jl_StackTraceElement__f_columnNumber = 0;
  this.jl_StackTraceElement__f_declaringClass = declaringClass;
  this.jl_StackTraceElement__f_methodName = methodName;
  this.jl_StackTraceElement__f_fileName = fileName;
  this.jl_StackTraceElement__f_lineNumber = lineNumber;
  this.jl_StackTraceElement__f_columnNumber = columnNumber;
}
$c_jl_StackTraceElement.prototype = new $h_O();
$c_jl_StackTraceElement.prototype.constructor = $c_jl_StackTraceElement;
/** @constructor */
function $h_jl_StackTraceElement() {
}
$h_jl_StackTraceElement.prototype = $c_jl_StackTraceElement.prototype;
$c_jl_StackTraceElement.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_jl_StackTraceElement)) {
    var x2 = $as_jl_StackTraceElement(that);
    return (((((this.jl_StackTraceElement__f_fileName === $n(x2).jl_StackTraceElement__f_fileName) && (this.jl_StackTraceElement__f_lineNumber === $n(x2).jl_StackTraceElement__f_lineNumber)) && (this.jl_StackTraceElement__f_columnNumber === $n(x2).jl_StackTraceElement__f_columnNumber)) && (this.jl_StackTraceElement__f_declaringClass === $n(x2).jl_StackTraceElement__f_declaringClass)) && (this.jl_StackTraceElement__f_methodName === $n(x2).jl_StackTraceElement__f_methodName));
  } else {
    return false;
  }
});
$c_jl_StackTraceElement.prototype.toString__T = (function() {
  var result = "";
  if ((this.jl_StackTraceElement__f_declaringClass !== "<jscode>")) {
    result = ((("" + result) + this.jl_StackTraceElement__f_declaringClass) + ".");
  }
  result = (("" + result) + this.jl_StackTraceElement__f_methodName);
  if ((this.jl_StackTraceElement__f_fileName === null)) {
    result = (result + "(Unknown Source)");
  } else {
    result = ((result + "(") + this.jl_StackTraceElement__f_fileName);
    if ((this.jl_StackTraceElement__f_lineNumber >= 0)) {
      result = ((result + ":") + this.jl_StackTraceElement__f_lineNumber);
      if ((this.jl_StackTraceElement__f_columnNumber >= 0)) {
        result = ((result + ":") + this.jl_StackTraceElement__f_columnNumber);
      }
    }
    result = (result + ")");
  }
  return result;
});
$c_jl_StackTraceElement.prototype.hashCode__I = (function() {
  return (((($f_T__hashCode__I($n(this.jl_StackTraceElement__f_declaringClass)) ^ $f_T__hashCode__I($n(this.jl_StackTraceElement__f_methodName))) ^ $f_T__hashCode__I($n(this.jl_StackTraceElement__f_fileName))) ^ this.jl_StackTraceElement__f_lineNumber) ^ this.jl_StackTraceElement__f_columnNumber);
});
function $as_jl_StackTraceElement(obj) {
  return (((obj instanceof $c_jl_StackTraceElement) || (obj === null)) ? obj : $throwClassCastException(obj, "java.lang.StackTraceElement"));
}
function $isArrayOf_jl_StackTraceElement(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_StackTraceElement)));
}
function $asArrayOf_jl_StackTraceElement(obj, depth) {
  return (($isArrayOf_jl_StackTraceElement(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.StackTraceElement;", depth));
}
var $d_jl_StackTraceElement = new $TypeData().initClass($c_jl_StackTraceElement, "java.lang.StackTraceElement", ({
  jl_StackTraceElement: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_jl_String$() {
}
$c_jl_String$.prototype = new $h_O();
$c_jl_String$.prototype.constructor = $c_jl_String$;
/** @constructor */
function $h_jl_String$() {
}
$h_jl_String$.prototype = $c_jl_String$.prototype;
$c_jl_String$.prototype.new__AC__I__I__T = (function(value, offset, count) {
  var end = ((offset + count) | 0);
  if ((((offset < 0) || (end < offset)) || (end > $n(value).u.length))) {
    throw $ct_jl_StringIndexOutOfBoundsException__(new $c_jl_StringIndexOutOfBoundsException());
  }
  var result = "";
  var i = offset;
  while ((i !== end)) {
    var $x_1 = result;
    var this$1 = $n(value).get(i);
    result = ($x_1 + ("" + $cToS(this$1)));
    i = ((1 + i) | 0);
  }
  return result;
});
var $d_jl_String$ = new $TypeData().initClass($c_jl_String$, "java.lang.String$", ({
  jl_String$: 1,
  Ljava_io_Serializable: 1
}));
var $n_jl_String$;
function $m_jl_String$() {
  if ((!$n_jl_String$)) {
    $n_jl_String$ = new $c_jl_String$();
  }
  return $n_jl_String$;
}
function $ct_jl_Throwable__T__jl_Throwable__Z__Z__($thiz, s, e, enableSuppression, writableStackTrace) {
  $thiz.jl_Throwable__f_s = s;
  $thiz.jl_Throwable__f_writableStackTrace = writableStackTrace;
  if (writableStackTrace) {
    $thiz.fillInStackTrace__jl_Throwable();
  }
  return $thiz;
}
class $c_jl_Throwable extends Error {
  constructor() {
    super();
    this.jl_Throwable__f_s = null;
    this.jl_Throwable__f_writableStackTrace = false;
    this.jl_Throwable__f_jsErrorForStackTrace = null;
    this.jl_Throwable__f_stackTrace = null;
  }
  getMessage__T() {
    return this.jl_Throwable__f_s;
  }
  fillInStackTrace__jl_Throwable() {
    var reference = ((this instanceof $c_sjs_js_JavaScriptException) ? this.sjs_js_JavaScriptException__f_exception : this);
    var identifyingString = Object.prototype.toString.call(reference);
    this.jl_Throwable__f_jsErrorForStackTrace = ((identifyingString === "[object Error]") ? reference : (((Error.captureStackTrace === (void 0)) || $uZ(Object.isSealed(this))) ? new Error() : (Error.captureStackTrace(this), this)));
    return this;
  }
  getStackTrace__Ajl_StackTraceElement() {
    if ((this.jl_Throwable__f_stackTrace === null)) {
      if (this.jl_Throwable__f_writableStackTrace) {
        this.jl_Throwable__f_stackTrace = $m_jl_StackTrace$().extract__O__Ajl_StackTraceElement(this.jl_Throwable__f_jsErrorForStackTrace);
      } else {
        this.jl_Throwable__f_stackTrace = new ($d_jl_StackTraceElement.getArrayOf().constr)(0);
      }
    }
    return this.jl_Throwable__f_stackTrace;
  }
  toString__T() {
    var className = $objectClassName(this);
    var message = this.getMessage__T();
    return ((message === null) ? className : ((className + ": ") + message));
  }
  hashCode__I() {
    return $c_O.prototype.hashCode__I.call(this);
  }
  equals__O__Z(that) {
    return $c_O.prototype.equals__O__Z.call(this, that);
  }
  get "message"() {
    var m = this.getMessage__T();
    return ((m === null) ? "" : m);
  }
  get "name"() {
    return $objectClassName(this);
  }
  "toString"() {
    return this.toString__T();
  }
}
function $as_jl_Throwable(obj) {
  return (((obj instanceof $c_jl_Throwable) || (obj === null)) ? obj : $throwClassCastException(obj, "java.lang.Throwable"));
}
function $isArrayOf_jl_Throwable(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Throwable)));
}
function $asArrayOf_jl_Throwable(obj, depth) {
  return (($isArrayOf_jl_Throwable(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Throwable;", depth));
}
/** @constructor */
function $c_s_$less$colon$less$() {
  this.s_$less$colon$less$__f_singleton = null;
  $n_s_$less$colon$less$ = this;
  this.s_$less$colon$less$__f_singleton = new $c_s_$less$colon$less$$anon$1();
}
$c_s_$less$colon$less$.prototype = new $h_O();
$c_s_$less$colon$less$.prototype.constructor = $c_s_$less$colon$less$;
/** @constructor */
function $h_s_$less$colon$less$() {
}
$h_s_$less$colon$less$.prototype = $c_s_$less$colon$less$.prototype;
var $d_s_$less$colon$less$ = new $TypeData().initClass($c_s_$less$colon$less$, "scala.$less$colon$less$", ({
  s_$less$colon$less$: 1,
  Ljava_io_Serializable: 1
}));
var $n_s_$less$colon$less$;
function $m_s_$less$colon$less$() {
  if ((!$n_s_$less$colon$less$)) {
    $n_s_$less$colon$less$ = new $c_s_$less$colon$less$();
  }
  return $n_s_$less$colon$less$;
}
function $f_s_App__main__AT__V($thiz, args) {
  var this$1 = $n($thiz.LMain$__f_scala$App$$initCode);
  var it = this$1.iterator__sc_Iterator();
  while ($n(it).hasNext__Z()) {
    var arg1 = $n(it).next__O();
    var proc$2 = $as_F0(arg1);
    var this$2 = $n(proc$2);
    this$2.apply__O();
  }
}
function $f_s_App__$init$__V($thiz) {
  var this$2 = $m_RTLong$();
  var value = $uD(new Date().getTime());
  this$2.org$scalajs$linker$runtime$RuntimeLong$$fromDoubleImpl__D__I(value);
  $thiz.LMain$__f_scala$App$$initCode = new $c_scm_ListBuffer();
}
/** @constructor */
function $c_s_Array$() {
}
$c_s_Array$.prototype = new $h_O();
$c_s_Array$.prototype.constructor = $c_s_Array$;
/** @constructor */
function $h_s_Array$() {
}
$h_s_Array$.prototype = $c_s_Array$.prototype;
$c_s_Array$.prototype.from__sc_IterableOnce__s_reflect_ClassTag__O = (function(it, evidence$3) {
  var n = $n(it).knownSize__I();
  if ((n > (-1))) {
    var elements = $n(evidence$3).newArray__I__O(n);
    var iterator = $n(it).iterator__sc_Iterator();
    var i = 0;
    while ((i < n)) {
      $m_sr_ScalaRunTime$().array_update__O__I__O__V(elements, i, $n(iterator).next__O());
      i = ((1 + i) | 0);
    }
    return elements;
  } else {
    var capacity = 0;
    var size = 0;
    var jsElems = null;
    var elementClass = $n(evidence$3).runtimeClass__jl_Class();
    capacity = 0;
    size = 0;
    var isCharArrayBuilder = (elementClass === $d_C.getClassOf());
    jsElems = [];
    var iterator$2 = $n(it).iterator__sc_Iterator();
    while ($n(iterator$2).hasNext__Z()) {
      var elem = $n(iterator$2).next__O();
      var unboxedElem = (isCharArrayBuilder ? $uC(elem) : ((elem === null) ? $n(elementClass).data.zero : elem));
      jsElems.push(unboxedElem);
    }
    var elemRuntimeClass = ((elementClass === $d_V.getClassOf()) ? $d_jl_Void.getClassOf() : (((elementClass === $d_sr_Null$.getClassOf()) || (elementClass === $d_sr_Nothing$.getClassOf())) ? $d_O.getClassOf() : elementClass));
    return $n(elemRuntimeClass).data.getArrayOf().wrapArray(jsElems);
  }
});
$c_s_Array$.prototype.equals__AO__AO__Z = (function(xs, ys) {
  if ((xs === ys)) {
    return true;
  }
  if (($n(xs).u.length !== $n(ys).u.length)) {
    return false;
  }
  var len = $n(xs).u.length;
  var i = 0;
  while ((i < len)) {
    if ((!$m_sr_BoxesRunTime$().equals__O__O__Z($n(xs).get(i), $n(ys).get(i)))) {
      return false;
    }
    i = ((1 + i) | 0);
  }
  return true;
});
var $d_s_Array$ = new $TypeData().initClass($c_s_Array$, "scala.Array$", ({
  s_Array$: 1,
  Ljava_io_Serializable: 1
}));
var $n_s_Array$;
function $m_s_Array$() {
  if ((!$n_s_Array$)) {
    $n_s_Array$ = new $c_s_Array$();
  }
  return $n_s_Array$;
}
/** @constructor */
function $c_s_LowPriorityImplicits() {
}
$c_s_LowPriorityImplicits.prototype = new $h_s_LowPriorityImplicits2();
$c_s_LowPriorityImplicits.prototype.constructor = $c_s_LowPriorityImplicits;
/** @constructor */
function $h_s_LowPriorityImplicits() {
}
$h_s_LowPriorityImplicits.prototype = $c_s_LowPriorityImplicits.prototype;
$c_s_LowPriorityImplicits.prototype.wrapRefArray__AO__scm_ArraySeq$ofRef = (function(xs) {
  if ((xs === null)) {
    return null;
  } else if (($n(xs).u.length === 0)) {
    var this$1 = $m_scm_ArraySeq$();
    return this$1.scm_ArraySeq$__f_EmptyArraySeq;
  } else {
    return new $c_scm_ArraySeq$ofRef(xs);
  }
});
function $f_scm_Growable__addAll__sc_IterableOnce__scm_Growable($thiz, elems) {
  if ((elems === $thiz)) {
    $thiz.addAll__sc_IterableOnce__scm_Growable($m_scm_Buffer$().from__sc_IterableOnce__sc_SeqOps(elems));
  } else {
    var it = $n(elems).iterator__sc_Iterator();
    while ($n(it).hasNext__Z()) {
      $thiz.addOne__O__scm_Growable($n(it).next__O());
    }
  }
  return $thiz;
}
/** @constructor */
function $c_sr_AbstractFunction0() {
}
$c_sr_AbstractFunction0.prototype = new $h_O();
$c_sr_AbstractFunction0.prototype.constructor = $c_sr_AbstractFunction0;
/** @constructor */
function $h_sr_AbstractFunction0() {
}
$h_sr_AbstractFunction0.prototype = $c_sr_AbstractFunction0.prototype;
$c_sr_AbstractFunction0.prototype.toString__T = (function() {
  return "<function0>";
});
/** @constructor */
function $c_sr_AbstractFunction1() {
}
$c_sr_AbstractFunction1.prototype = new $h_O();
$c_sr_AbstractFunction1.prototype.constructor = $c_sr_AbstractFunction1;
/** @constructor */
function $h_sr_AbstractFunction1() {
}
$h_sr_AbstractFunction1.prototype = $c_sr_AbstractFunction1.prototype;
$c_sr_AbstractFunction1.prototype.toString__T = (function() {
  return "<function1>";
});
/** @constructor */
function $c_sr_AbstractFunction2() {
}
$c_sr_AbstractFunction2.prototype = new $h_O();
$c_sr_AbstractFunction2.prototype.constructor = $c_sr_AbstractFunction2;
/** @constructor */
function $h_sr_AbstractFunction2() {
}
$h_sr_AbstractFunction2.prototype = $c_sr_AbstractFunction2.prototype;
$c_sr_AbstractFunction2.prototype.toString__T = (function() {
  return "<function2>";
});
/** @constructor */
function $c_sr_BooleanRef(elem) {
  this.sr_BooleanRef__f_elem = false;
  this.sr_BooleanRef__f_elem = elem;
}
$c_sr_BooleanRef.prototype = new $h_O();
$c_sr_BooleanRef.prototype.constructor = $c_sr_BooleanRef;
/** @constructor */
function $h_sr_BooleanRef() {
}
$h_sr_BooleanRef.prototype = $c_sr_BooleanRef.prototype;
$c_sr_BooleanRef.prototype.toString__T = (function() {
  var b = this.sr_BooleanRef__f_elem;
  return ("" + b);
});
var $d_sr_BooleanRef = new $TypeData().initClass($c_sr_BooleanRef, "scala.runtime.BooleanRef", ({
  sr_BooleanRef: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sr_IntRef(elem) {
  this.sr_IntRef__f_elem = 0;
  this.sr_IntRef__f_elem = elem;
}
$c_sr_IntRef.prototype = new $h_O();
$c_sr_IntRef.prototype.constructor = $c_sr_IntRef;
/** @constructor */
function $h_sr_IntRef() {
}
$h_sr_IntRef.prototype = $c_sr_IntRef.prototype;
$c_sr_IntRef.prototype.toString__T = (function() {
  var i = this.sr_IntRef__f_elem;
  return ("" + i);
});
var $d_sr_IntRef = new $TypeData().initClass($c_sr_IntRef, "scala.runtime.IntRef", ({
  sr_IntRef: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_s_util_hashing_MurmurHash3$() {
  this.s_util_hashing_MurmurHash3$__f_seqSeed = 0;
  this.s_util_hashing_MurmurHash3$__f_mapSeed = 0;
  this.s_util_hashing_MurmurHash3$__f_setSeed = 0;
  this.s_util_hashing_MurmurHash3$__f_emptyMapHash = 0;
  $n_s_util_hashing_MurmurHash3$ = this;
  this.s_util_hashing_MurmurHash3$__f_seqSeed = $f_T__hashCode__I("Seq");
  this.s_util_hashing_MurmurHash3$__f_mapSeed = $f_T__hashCode__I("Map");
  this.s_util_hashing_MurmurHash3$__f_setSeed = $f_T__hashCode__I("Set");
  this.s_util_hashing_MurmurHash3$__f_emptyMapHash = this.unorderedHash__sc_IterableOnce__I__I($m_sci_Nil$(), this.s_util_hashing_MurmurHash3$__f_mapSeed);
}
$c_s_util_hashing_MurmurHash3$.prototype = new $h_s_util_hashing_MurmurHash3();
$c_s_util_hashing_MurmurHash3$.prototype.constructor = $c_s_util_hashing_MurmurHash3$;
/** @constructor */
function $h_s_util_hashing_MurmurHash3$() {
}
$h_s_util_hashing_MurmurHash3$.prototype = $c_s_util_hashing_MurmurHash3$.prototype;
$c_s_util_hashing_MurmurHash3$.prototype.seqHash__sc_Seq__I = (function(xs) {
  if ($is_sc_IndexedSeq(xs)) {
    var x2 = $as_sc_IndexedSeq(xs);
    return this.indexedSeqHash__sc_IndexedSeq__I__I(x2, this.s_util_hashing_MurmurHash3$__f_seqSeed);
  } else if ((xs instanceof $c_sci_List)) {
    var x3 = $as_sci_List(xs);
    return this.listHash__sci_List__I__I(x3, this.s_util_hashing_MurmurHash3$__f_seqSeed);
  } else {
    return this.orderedHash__sc_IterableOnce__I__I(xs, this.s_util_hashing_MurmurHash3$__f_seqSeed);
  }
});
var $d_s_util_hashing_MurmurHash3$ = new $TypeData().initClass($c_s_util_hashing_MurmurHash3$, "scala.util.hashing.MurmurHash3$", ({
  s_util_hashing_MurmurHash3$: 1,
  s_util_hashing_MurmurHash3: 1
}));
var $n_s_util_hashing_MurmurHash3$;
function $m_s_util_hashing_MurmurHash3$() {
  if ((!$n_s_util_hashing_MurmurHash3$)) {
    $n_s_util_hashing_MurmurHash3$ = new $c_s_util_hashing_MurmurHash3$();
  }
  return $n_s_util_hashing_MurmurHash3$;
}
/** @constructor */
function $c_LMain$() {
  this.LMain$__f_scala$App$$initCode = null;
  this.LMain$__f_container$lzy1 = null;
  this.LMain$__f_containerbitmap$1 = false;
  this.LMain$__f_app = null;
  $n_LMain$ = this;
  $f_s_App__$init$__V(this);
  var $x_2 = $n($m_Lcom_raquo_laminar_api_package$().Lcom_raquo_laminar_api_package$__f_L).div__Lcom_raquo_laminar_tags_HtmlTag();
  var $x_1 = $m_sr_ScalaRunTime$();
  var this$1 = $n($m_Lcom_raquo_laminar_api_package$().Lcom_raquo_laminar_api_package$__f_L);
  var r = $m_Lcom_raquo_laminar_modifiers_RenderableText$().Lcom_raquo_laminar_modifiers_RenderableText$__f_stringRenderable;
  this.LMain$__f_app = $n($x_2).apply__sci_Seq__Lcom_raquo_laminar_nodes_ReactiveHtmlElement($x_1.wrapRefArray__AO__sci_ArraySeq(new ($d_Lcom_raquo_laminar_modifiers_Modifier.getArrayOf().constr)([$f_Lcom_raquo_laminar_api_Implicits__textToTextNode__O__Lcom_raquo_laminar_modifiers_RenderableText__Lcom_raquo_laminar_nodes_TextNode(this$1, "Replaced", r)])));
  $n($m_Lcom_raquo_laminar_api_package$().Lcom_raquo_laminar_api_package$__f_L);
  var container = this.container__Lorg_scalajs_dom_Element();
  var rootNode = this.LMain$__f_app;
  new $c_Lcom_raquo_laminar_nodes_RootNode(container, rootNode);
}
$c_LMain$.prototype = new $h_O();
$c_LMain$.prototype.constructor = $c_LMain$;
/** @constructor */
function $h_LMain$() {
}
$h_LMain$.prototype = $c_LMain$.prototype;
$c_LMain$.prototype.container__Lorg_scalajs_dom_Element = (function() {
  if ((!this.LMain$__f_containerbitmap$1)) {
    this.LMain$__f_container$lzy1 = document.getElementById("app");
    this.LMain$__f_containerbitmap$1 = true;
  }
  return this.LMain$__f_container$lzy1;
});
var $d_LMain$ = new $TypeData().initClass($c_LMain$, "Main$", ({
  LMain$: 1,
  s_DelayedInit: 1,
  s_App: 1
}));
var $n_LMain$;
function $m_LMain$() {
  if ((!$n_LMain$)) {
    $n_LMain$ = new $c_LMain$();
  }
  return $n_LMain$;
}
class $c_Lcom_raquo_airstream_core_AirstreamError extends $c_jl_Throwable {
}
/** @constructor */
function $c_Lcom_raquo_airstream_core_AirstreamError$() {
  this.Lcom_raquo_airstream_core_AirstreamError$__f_unhandledErrorCallbacks = null;
  this.Lcom_raquo_airstream_core_AirstreamError$__f_consoleErrorCallback = null;
  this.Lcom_raquo_airstream_core_AirstreamError$__f_unsafeRethrowErrorCallback = null;
  $n_Lcom_raquo_airstream_core_AirstreamError$ = this;
  this.Lcom_raquo_airstream_core_AirstreamError$__f_unhandledErrorCallbacks = $as_scm_Buffer($m_scm_Buffer$().apply__sci_Seq__sc_SeqOps($m_sr_ScalaRunTime$().wrapRefArray__AO__sci_ArraySeq(new ($d_F1.getArrayOf().constr)([]))));
  this.Lcom_raquo_airstream_core_AirstreamError$__f_consoleErrorCallback = new $c_sjsr_AnonFunction1(((err) => {
    var err$1 = $as_jl_Throwable(err);
    try {
      console.error(((this.getFullMessage__jl_Throwable__T(err$1) + "\n") + this.getStackTrace__jl_Throwable__T__T(err$1, "\n")));
    } catch (e) {
      var e$2 = ((e instanceof $c_jl_Throwable) ? e : new $c_sjs_js_JavaScriptException(e));
      console.error("Error in AirstreamError.consoleErrorCallback:");
      console.error(e$2);
    }
  }));
  this.Lcom_raquo_airstream_core_AirstreamError$__f_unsafeRethrowErrorCallback = new $c_sjsr_AnonFunction1(((err$2) => {
    var err$3 = $as_jl_Throwable(err$2);
    console.warn("Using unsafe rethrow error callback. Note: other registered error callbacks might not run. Use with caution.");
    var $x_1 = $n(err$3);
    throw (($x_1 instanceof $c_sjs_js_JavaScriptException) ? $x_1.sjs_js_JavaScriptException__f_exception : $x_1);
  }));
  this.registerUnhandledErrorCallback__F1__V(this.Lcom_raquo_airstream_core_AirstreamError$__f_consoleErrorCallback);
}
$c_Lcom_raquo_airstream_core_AirstreamError$.prototype = new $h_O();
$c_Lcom_raquo_airstream_core_AirstreamError$.prototype.constructor = $c_Lcom_raquo_airstream_core_AirstreamError$;
/** @constructor */
function $h_Lcom_raquo_airstream_core_AirstreamError$() {
}
$h_Lcom_raquo_airstream_core_AirstreamError$.prototype = $c_Lcom_raquo_airstream_core_AirstreamError$.prototype;
$c_Lcom_raquo_airstream_core_AirstreamError$.prototype.getFullMessage__jl_Throwable__T = (function(e) {
  try {
    var errorMessage = $n(e).getMessage__T();
  } catch (e$2) {
    var errorMessage = "(Unable to get the message for this error - exception occurred in its getMessage)";
  }
  var this$1 = $n(e);
  return (($objectGetClass(this$1).getSimpleName__T() + ": ") + errorMessage);
});
$c_Lcom_raquo_airstream_core_AirstreamError$.prototype.getStackTrace__jl_Throwable__T__T = (function(err, newline) {
  try {
    var this$1 = $n($m_s_Predef$().wrapRefArray__AO__scm_ArraySeq$ofRef($n(err).getStackTrace__Ajl_StackTraceElement()));
    return $f_sc_IterableOnceOps__mkString__T__T__T__T(this$1, "", newline, "");
  } catch (e) {
    return "(Unable to get the stacktrace for this error - exception occurred in its getStackTrace)";
  }
});
$c_Lcom_raquo_airstream_core_AirstreamError$.prototype.registerUnhandledErrorCallback__F1__V = (function(fn) {
  var this$1 = $n(this.Lcom_raquo_airstream_core_AirstreamError$__f_unhandledErrorCallbacks);
  $as_scm_Buffer(this$1.addOne__O__scm_Growable(fn));
});
$c_Lcom_raquo_airstream_core_AirstreamError$.prototype.sendUnhandledError__jl_Throwable__V = (function(err) {
  var this$1 = $n(this.Lcom_raquo_airstream_core_AirstreamError$__f_unhandledErrorCallbacks);
  var it = this$1.iterator__sc_Iterator();
  while ($n(it).hasNext__Z()) {
    var arg1 = $n(it).next__O();
    var fn = $as_F1(arg1);
    try {
      $n(fn).apply__O__O(err);
    } catch (e) {
      var e$2 = ((e instanceof $c_jl_Throwable) ? e : new $c_sjs_js_JavaScriptException(e));
      var x$2 = this.Lcom_raquo_airstream_core_AirstreamError$__f_unsafeRethrowErrorCallback;
      if (((fn === null) ? (x$2 === null) : $n(fn).equals__O__Z(x$2))) {
        throw ((e$2 instanceof $c_sjs_js_JavaScriptException) ? e$2.sjs_js_JavaScriptException__f_exception : e$2);
      }
      console.warn("Error processing an unhandled error callback:");
      $m_sjs_js_timers_package$().setTimeout__D__F0__sjs_js_timers_SetTimeoutHandle(0.0, new $c_sjsr_AnonFunction0(((e$2) => (() => {
        throw ((e$2 instanceof $c_sjs_js_JavaScriptException) ? e$2.sjs_js_JavaScriptException__f_exception : e$2);
      }))(e$2)));
    }
  }
});
var $d_Lcom_raquo_airstream_core_AirstreamError$ = new $TypeData().initClass($c_Lcom_raquo_airstream_core_AirstreamError$, "com.raquo.airstream.core.AirstreamError$", ({
  Lcom_raquo_airstream_core_AirstreamError$: 1,
  s_deriving_Mirror: 1,
  s_deriving_Mirror$Sum: 1
}));
var $n_Lcom_raquo_airstream_core_AirstreamError$;
function $m_Lcom_raquo_airstream_core_AirstreamError$() {
  if ((!$n_Lcom_raquo_airstream_core_AirstreamError$)) {
    $n_Lcom_raquo_airstream_core_AirstreamError$ = new $c_Lcom_raquo_airstream_core_AirstreamError$();
  }
  return $n_Lcom_raquo_airstream_core_AirstreamError$;
}
function $f_Lcom_raquo_laminar_api_Implicits__textToTextNode__O__Lcom_raquo_laminar_modifiers_RenderableText__Lcom_raquo_laminar_nodes_TextNode($thiz, value, r) {
  return new $c_Lcom_raquo_laminar_nodes_TextNode($n(r).asString__O__T(value));
}
/** @constructor */
function $c_Lcom_raquo_laminar_nodes_RootNode(container, child) {
  this.Lcom_raquo_laminar_nodes_RootNode__f_dynamicOwner = null;
  this.Lcom_raquo_laminar_nodes_RootNode__f_child = null;
  this.Lcom_raquo_laminar_nodes_RootNode__f_ref = null;
  this.Lcom_raquo_laminar_nodes_RootNode__f_child = child;
  $f_Lcom_raquo_laminar_nodes_ParentNode__$init$__V(this);
  if ((container === null)) {
    throw $ct_jl_Exception__T__(new $c_jl_Exception(), "Unable to mount Laminar RootNode into a null container. See https://laminar.dev/documentation#waiting-for-the-dom-to-load");
  }
  if ((!$m_Lcom_raquo_laminar_DomApi$().isDescendantOf__Lorg_scalajs_dom_Node__Lorg_scalajs_dom_Node__Z(container, document))) {
    throw $ct_jl_Exception__T__(new $c_jl_Exception(), "Unable to mount Laminar RootNode into an unmounted container. See https://laminar.dev/documentation#rendering");
  }
  this.Lcom_raquo_laminar_nodes_RootNode__f_ref = container;
  this.mount__Z();
}
$c_Lcom_raquo_laminar_nodes_RootNode.prototype = new $h_O();
$c_Lcom_raquo_laminar_nodes_RootNode.prototype.constructor = $c_Lcom_raquo_laminar_nodes_RootNode;
/** @constructor */
function $h_Lcom_raquo_laminar_nodes_RootNode() {
}
$h_Lcom_raquo_laminar_nodes_RootNode.prototype = $c_Lcom_raquo_laminar_nodes_RootNode.prototype;
$c_Lcom_raquo_laminar_nodes_RootNode.prototype.dynamicOwner__Lcom_raquo_airstream_ownership_DynamicOwner = (function() {
  return this.Lcom_raquo_laminar_nodes_RootNode__f_dynamicOwner;
});
$c_Lcom_raquo_laminar_nodes_RootNode.prototype.com$raquo$laminar$nodes$ParentNode$_setter_$dynamicOwner_$eq__Lcom_raquo_airstream_ownership_DynamicOwner__V = (function(x$0) {
  this.Lcom_raquo_laminar_nodes_RootNode__f_dynamicOwner = x$0;
});
$c_Lcom_raquo_laminar_nodes_RootNode.prototype.mount__Z = (function() {
  $n(this.Lcom_raquo_laminar_nodes_RootNode__f_dynamicOwner).activate__V();
  return $m_Lcom_raquo_laminar_nodes_ParentNode$().appendChild__Lcom_raquo_laminar_nodes_ParentNode__Lcom_raquo_laminar_nodes_ChildNode__O__Z(this, this.Lcom_raquo_laminar_nodes_RootNode__f_child, (void 0));
});
$c_Lcom_raquo_laminar_nodes_RootNode.prototype.ref__Lorg_scalajs_dom_Node = (function() {
  return this.Lcom_raquo_laminar_nodes_RootNode__f_ref;
});
var $d_Lcom_raquo_laminar_nodes_RootNode = new $TypeData().initClass($c_Lcom_raquo_laminar_nodes_RootNode, "com.raquo.laminar.nodes.RootNode", ({
  Lcom_raquo_laminar_nodes_RootNode: 1,
  Lcom_raquo_laminar_nodes_ReactiveNode: 1,
  Lcom_raquo_laminar_nodes_ParentNode: 1
}));
function $p_jl_Class__computeCachedSimpleNameBestEffort__T($thiz) {
  if ($thiz.data.isArrayClass) {
    return ($n($thiz.data.getComponentType()).getSimpleName__T() + "[]");
  } else {
    var name = $thiz.data.name;
    var idx = (((-1) + name.length) | 0);
    while (true) {
      if ((idx >= 0)) {
        var index = idx;
        var $x_1 = ($charAt(name, index) === 36);
      } else {
        var $x_1 = false;
      }
      if ($x_1) {
        idx = (((-1) + idx) | 0);
      } else {
        break;
      }
    }
    if ((idx >= 0)) {
      var index$1 = idx;
      var c = $charAt(name, index$1);
      var $x_2 = ((c >= 48) && (c <= 57));
    } else {
      var $x_2 = false;
    }
    if ($x_2) {
      idx = (((-1) + idx) | 0);
      while (true) {
        if ((idx >= 0)) {
          var index$2 = idx;
          var c$1 = $charAt(name, index$2);
          var $x_3 = ((c$1 >= 48) && (c$1 <= 57));
        } else {
          var $x_3 = false;
        }
        if ($x_3) {
          idx = (((-1) + idx) | 0);
        } else {
          break;
        }
      }
      while (true) {
        if ((idx >= 0)) {
          var index$3 = idx;
          var $x_4 = ($charAt(name, index$3) === 36);
        } else {
          var $x_4 = false;
        }
        if ($x_4) {
          idx = (((-1) + idx) | 0);
        } else {
          break;
        }
      }
    }
    while (true) {
      if ((idx >= 0)) {
        var index$4 = idx;
        var currChar = $charAt(name, index$4);
        var $x_5 = ((currChar !== 46) && (currChar !== 36));
      } else {
        var $x_5 = false;
      }
      if ($x_5) {
        idx = (((-1) + idx) | 0);
      } else {
        break;
      }
    }
    var beginIndex = ((1 + idx) | 0);
    if (((beginIndex < 0) || (beginIndex > name.length))) {
      $charAt(name, beginIndex);
    }
    return $as_T(name.substring(beginIndex));
  }
}
/** @constructor */
function $c_jl_Class($data) {
  this.jl_Class__f_cachedSimpleName = null;
  this.data = $data;
}
$c_jl_Class.prototype = new $h_O();
$c_jl_Class.prototype.constructor = $c_jl_Class;
/** @constructor */
function $h_jl_Class() {
}
$h_jl_Class.prototype = $c_jl_Class.prototype;
$c_jl_Class.prototype.toString__T = (function() {
  return ((this.data.isInterface ? "interface " : (this.data.isPrimitive ? "" : "class ")) + this.data.name);
});
$c_jl_Class.prototype.getSimpleName__T = (function() {
  if ((this.jl_Class__f_cachedSimpleName === null)) {
    this.jl_Class__f_cachedSimpleName = $p_jl_Class__computeCachedSimpleNameBestEffort__T(this);
  }
  return this.jl_Class__f_cachedSimpleName;
});
var $d_jl_Class = new $TypeData().initClass($c_jl_Class, "java.lang.Class", ({
  jl_Class: 1,
  Ljava_io_Serializable: 1,
  jl_constant_Constable: 1
}));
class $c_jl_Error extends $c_jl_Throwable {
}
function $ct_jl_Exception__T__($thiz, s) {
  $ct_jl_Throwable__T__jl_Throwable__Z__Z__($thiz, s, null, true, true);
  return $thiz;
}
class $c_jl_Exception extends $c_jl_Throwable {
}
var $d_jl_Exception = new $TypeData().initClass($c_jl_Exception, "java.lang.Exception", ({
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_s_$less$colon$less() {
}
$c_s_$less$colon$less.prototype = new $h_O();
$c_s_$less$colon$less.prototype.constructor = $c_s_$less$colon$less;
/** @constructor */
function $h_s_$less$colon$less() {
}
$h_s_$less$colon$less.prototype = $c_s_$less$colon$less.prototype;
/** @constructor */
function $c_s_Predef$() {
  this.s_Predef$__f_Map = null;
  $n_s_Predef$ = this;
  this.s_Predef$__f_Map = $m_sci_Map$();
}
$c_s_Predef$.prototype = new $h_s_LowPriorityImplicits();
$c_s_Predef$.prototype.constructor = $c_s_Predef$;
/** @constructor */
function $h_s_Predef$() {
}
$h_s_Predef$.prototype = $c_s_Predef$.prototype;
var $d_s_Predef$ = new $TypeData().initClass($c_s_Predef$, "scala.Predef$", ({
  s_Predef$: 1,
  s_LowPriorityImplicits: 1,
  s_LowPriorityImplicits2: 1
}));
var $n_s_Predef$;
function $m_s_Predef$() {
  if ((!$n_s_Predef$)) {
    $n_s_Predef$ = new $c_s_Predef$();
  }
  return $n_s_Predef$;
}
function $ct_sc_ClassTagIterableFactory$AnyIterableDelegate__sc_ClassTagIterableFactory__($thiz, delegate) {
  $thiz.sc_ClassTagIterableFactory$AnyIterableDelegate__f_delegate = delegate;
  return $thiz;
}
/** @constructor */
function $c_sc_ClassTagIterableFactory$AnyIterableDelegate() {
  this.sc_ClassTagIterableFactory$AnyIterableDelegate__f_delegate = null;
}
$c_sc_ClassTagIterableFactory$AnyIterableDelegate.prototype = new $h_O();
$c_sc_ClassTagIterableFactory$AnyIterableDelegate.prototype.constructor = $c_sc_ClassTagIterableFactory$AnyIterableDelegate;
/** @constructor */
function $h_sc_ClassTagIterableFactory$AnyIterableDelegate() {
}
$h_sc_ClassTagIterableFactory$AnyIterableDelegate.prototype = $c_sc_ClassTagIterableFactory$AnyIterableDelegate.prototype;
$c_sc_ClassTagIterableFactory$AnyIterableDelegate.prototype.from__sc_IterableOnce__O = (function(it) {
  return $n(this.sc_ClassTagIterableFactory$AnyIterableDelegate__f_delegate).from__sc_IterableOnce__O__O(it, $m_s_reflect_ManifestFactory$AnyManifest$());
});
$c_sc_ClassTagIterableFactory$AnyIterableDelegate.prototype.apply__sci_Seq__O = (function(elems) {
  var this$1 = $n(this.sc_ClassTagIterableFactory$AnyIterableDelegate__f_delegate);
  var evidence$7 = $m_s_reflect_ManifestFactory$AnyManifest$();
  return this$1.from__sc_IterableOnce__O__O(elems, evidence$7);
});
function $f_sc_IterableOps__sizeCompare__I__I($thiz, otherSize) {
  if ((otherSize < 0)) {
    return 1;
  } else {
    var known = $thiz.knownSize__I();
    if ((known >= 0)) {
      return ((known === otherSize) ? 0 : ((known < otherSize) ? (-1) : 1));
    } else {
      var i = 0;
      var it = $thiz.iterator__sc_Iterator();
      while ($n(it).hasNext__Z()) {
        if ((i === otherSize)) {
          return 1;
        }
        $n(it).next__O();
        i = ((1 + i) | 0);
      }
      return ((i - otherSize) | 0);
    }
  }
}
function $f_sc_Iterator__concat__F0__sc_Iterator($thiz, xs) {
  return new $c_sc_Iterator$ConcatIterator($thiz).concat__F0__sc_Iterator(xs);
}
function $f_sc_Iterator__sliceIterator__I__I__sc_Iterator($thiz, from, until) {
  var lo = ((from > 0) ? from : 0);
  var rest = ((until < 0) ? (-1) : ((until <= lo) ? 0 : ((until - lo) | 0)));
  return ((rest === 0) ? $m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty : new $c_sc_Iterator$SliceIterator($thiz, lo, rest));
}
function $f_sc_Iterator__sameElements__sc_IterableOnce__Z($thiz, that) {
  var those = $n(that).iterator__sc_Iterator();
  while (($thiz.hasNext__Z() && $n(those).hasNext__Z())) {
    if ((!$m_sr_BoxesRunTime$().equals__O__O__Z($thiz.next__O(), $n(those).next__O()))) {
      return false;
    }
  }
  return ($thiz.hasNext__Z() === $n(those).hasNext__Z());
}
/** @constructor */
function $c_sc_Iterator$() {
  this.sc_Iterator$__f_scala$collection$Iterator$$_empty = null;
  $n_sc_Iterator$ = this;
  this.sc_Iterator$__f_scala$collection$Iterator$$_empty = new $c_sc_Iterator$$anon$19();
}
$c_sc_Iterator$.prototype = new $h_O();
$c_sc_Iterator$.prototype.constructor = $c_sc_Iterator$;
/** @constructor */
function $h_sc_Iterator$() {
}
$h_sc_Iterator$.prototype = $c_sc_Iterator$.prototype;
var $d_sc_Iterator$ = new $TypeData().initClass($c_sc_Iterator$, "scala.collection.Iterator$", ({
  sc_Iterator$: 1,
  sc_IterableFactory: 1,
  Ljava_io_Serializable: 1
}));
var $n_sc_Iterator$;
function $m_sc_Iterator$() {
  if ((!$n_sc_Iterator$)) {
    $n_sc_Iterator$ = new $c_sc_Iterator$();
  }
  return $n_sc_Iterator$;
}
/** @constructor */
function $c_sci_Map$() {
}
$c_sci_Map$.prototype = new $h_O();
$c_sci_Map$.prototype.constructor = $c_sci_Map$;
/** @constructor */
function $h_sci_Map$() {
}
$h_sci_Map$.prototype = $c_sci_Map$.prototype;
var $d_sci_Map$ = new $TypeData().initClass($c_sci_Map$, "scala.collection.immutable.Map$", ({
  sci_Map$: 1,
  sc_MapFactory: 1,
  Ljava_io_Serializable: 1
}));
var $n_sci_Map$;
function $m_sci_Map$() {
  if ((!$n_sci_Map$)) {
    $n_sci_Map$ = new $c_sci_Map$();
  }
  return $n_sci_Map$;
}
function $is_scm_Builder(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scm_Builder)));
}
function $as_scm_Builder(obj) {
  return (($is_scm_Builder(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.mutable.Builder"));
}
function $isArrayOf_scm_Builder(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_Builder)));
}
function $asArrayOf_scm_Builder(obj, depth) {
  return (($isArrayOf_scm_Builder(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.mutable.Builder;", depth));
}
function $as_s_math_ScalaNumber(obj) {
  return ((false || (obj === null)) ? obj : $throwClassCastException(obj, "scala.math.ScalaNumber"));
}
function $isArrayOf_s_math_ScalaNumber(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.s_math_ScalaNumber)));
}
function $asArrayOf_s_math_ScalaNumber(obj, depth) {
  return (($isArrayOf_s_math_ScalaNumber(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.math.ScalaNumber;", depth));
}
var $d_sr_Nothing$ = new $TypeData().initClass(0, "scala.runtime.Nothing$", ({
  sr_Nothing$: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sjsr_AnonFunction0(f) {
  this.sjsr_AnonFunction0__f_f = null;
  this.sjsr_AnonFunction0__f_f = f;
}
$c_sjsr_AnonFunction0.prototype = new $h_sr_AbstractFunction0();
$c_sjsr_AnonFunction0.prototype.constructor = $c_sjsr_AnonFunction0;
/** @constructor */
function $h_sjsr_AnonFunction0() {
}
$h_sjsr_AnonFunction0.prototype = $c_sjsr_AnonFunction0.prototype;
$c_sjsr_AnonFunction0.prototype.apply__O = (function() {
  return (0, this.sjsr_AnonFunction0__f_f)();
});
var $d_sjsr_AnonFunction0 = new $TypeData().initClass($c_sjsr_AnonFunction0, "scala.scalajs.runtime.AnonFunction0", ({
  sjsr_AnonFunction0: 1,
  sr_AbstractFunction0: 1,
  F0: 1
}));
/** @constructor */
function $c_sjsr_AnonFunction1(f) {
  this.sjsr_AnonFunction1__f_f = null;
  this.sjsr_AnonFunction1__f_f = f;
}
$c_sjsr_AnonFunction1.prototype = new $h_sr_AbstractFunction1();
$c_sjsr_AnonFunction1.prototype.constructor = $c_sjsr_AnonFunction1;
/** @constructor */
function $h_sjsr_AnonFunction1() {
}
$h_sjsr_AnonFunction1.prototype = $c_sjsr_AnonFunction1.prototype;
$c_sjsr_AnonFunction1.prototype.apply__O__O = (function(arg1) {
  return (0, this.sjsr_AnonFunction1__f_f)(arg1);
});
var $d_sjsr_AnonFunction1 = new $TypeData().initClass($c_sjsr_AnonFunction1, "scala.scalajs.runtime.AnonFunction1", ({
  sjsr_AnonFunction1: 1,
  sr_AbstractFunction1: 1,
  F1: 1
}));
/** @constructor */
function $c_sjsr_AnonFunction2(f) {
  this.sjsr_AnonFunction2__f_f = null;
  this.sjsr_AnonFunction2__f_f = f;
}
$c_sjsr_AnonFunction2.prototype = new $h_sr_AbstractFunction2();
$c_sjsr_AnonFunction2.prototype.constructor = $c_sjsr_AnonFunction2;
/** @constructor */
function $h_sjsr_AnonFunction2() {
}
$h_sjsr_AnonFunction2.prototype = $c_sjsr_AnonFunction2.prototype;
var $d_sjsr_AnonFunction2 = new $TypeData().initClass($c_sjsr_AnonFunction2, "scala.scalajs.runtime.AnonFunction2", ({
  sjsr_AnonFunction2: 1,
  sr_AbstractFunction2: 1,
  F2: 1
}));
/** @constructor */
function $c_Lcom_raquo_airstream_core_Observer$$anon$8(onNextParam$2, handleObserverErrors$3, onErrorParam$2, outer) {
  this.Lcom_raquo_airstream_core_Observer$$anon$8__f_maybeDisplayName = null;
  if ((outer === null)) {
    throw new $c_jl_NullPointerException();
  }
  this.Lcom_raquo_airstream_core_Observer$$anon$8__f_maybeDisplayName = (void 0);
}
$c_Lcom_raquo_airstream_core_Observer$$anon$8.prototype = new $h_O();
$c_Lcom_raquo_airstream_core_Observer$$anon$8.prototype.constructor = $c_Lcom_raquo_airstream_core_Observer$$anon$8;
/** @constructor */
function $h_Lcom_raquo_airstream_core_Observer$$anon$8() {
}
$h_Lcom_raquo_airstream_core_Observer$$anon$8.prototype = $c_Lcom_raquo_airstream_core_Observer$$anon$8.prototype;
$c_Lcom_raquo_airstream_core_Observer$$anon$8.prototype.toString__T = (function() {
  return $f_Lcom_raquo_airstream_core_Named__displayName__T(this);
});
var $d_Lcom_raquo_airstream_core_Observer$$anon$8 = new $TypeData().initClass($c_Lcom_raquo_airstream_core_Observer$$anon$8, "com.raquo.airstream.core.Observer$$anon$8", ({
  Lcom_raquo_airstream_core_Observer$$anon$8: 1,
  Lcom_raquo_airstream_core_Sink: 1,
  Lcom_raquo_airstream_core_Named: 1,
  Lcom_raquo_airstream_core_Observer: 1
}));
/** @constructor */
function $c_Lcom_raquo_laminar_api_Laminar$svg$(outer) {
  this.Lcom_raquo_laminar_api_Laminar$svg$__f_svg$lzy1 = null;
  this.Lcom_raquo_laminar_api_Laminar$svg$__f_svgbitmap$1 = false;
  this.Lcom_raquo_laminar_api_Laminar$svg$__f_className = null;
  if ((outer === null)) {
    throw new $c_jl_NullPointerException();
  }
  $f_Lcom_raquo_laminar_defs_complex_ComplexSvgKeys__$init$__V(this);
}
$c_Lcom_raquo_laminar_api_Laminar$svg$.prototype = new $h_O();
$c_Lcom_raquo_laminar_api_Laminar$svg$.prototype.constructor = $c_Lcom_raquo_laminar_api_Laminar$svg$;
/** @constructor */
function $h_Lcom_raquo_laminar_api_Laminar$svg$() {
}
$h_Lcom_raquo_laminar_api_Laminar$svg$.prototype = $c_Lcom_raquo_laminar_api_Laminar$svg$.prototype;
$c_Lcom_raquo_laminar_api_Laminar$svg$.prototype.svg__Lcom_raquo_laminar_tags_SvgTag = (function() {
  if ((!this.Lcom_raquo_laminar_api_Laminar$svg$__f_svgbitmap$1)) {
    this.Lcom_raquo_laminar_api_Laminar$svg$__f_svg$lzy1 = new $c_Lcom_raquo_laminar_tags_SvgTag("svg", false);
    this.Lcom_raquo_laminar_api_Laminar$svg$__f_svgbitmap$1 = true;
  }
  return this.Lcom_raquo_laminar_api_Laminar$svg$__f_svg$lzy1;
});
var $d_Lcom_raquo_laminar_api_Laminar$svg$ = new $TypeData().initClass($c_Lcom_raquo_laminar_api_Laminar$svg$, "com.raquo.laminar.api.Laminar$svg$", ({
  Lcom_raquo_laminar_api_Laminar$svg$: 1,
  Lcom_raquo_laminar_defs_tags_SvgTags: 1,
  Lcom_raquo_laminar_defs_attrs_SvgAttrs: 1,
  Lcom_raquo_laminar_defs_complex_ComplexSvgKeys: 1
}));
/** @constructor */
function $c_Lcom_raquo_laminar_api_package$() {
  this.Lcom_raquo_laminar_api_package$__f_L = null;
  $n_Lcom_raquo_laminar_api_package$ = this;
  this.Lcom_raquo_laminar_api_package$__f_L = new $c_Lcom_raquo_laminar_api_package$$anon$1();
}
$c_Lcom_raquo_laminar_api_package$.prototype = new $h_O();
$c_Lcom_raquo_laminar_api_package$.prototype.constructor = $c_Lcom_raquo_laminar_api_package$;
/** @constructor */
function $h_Lcom_raquo_laminar_api_package$() {
}
$h_Lcom_raquo_laminar_api_package$.prototype = $c_Lcom_raquo_laminar_api_package$.prototype;
var $d_Lcom_raquo_laminar_api_package$ = new $TypeData().initClass($c_Lcom_raquo_laminar_api_package$, "com.raquo.laminar.api.package$", ({
  Lcom_raquo_laminar_api_package$: 1,
  Lcom_raquo_laminar_api_Implicits$LowPriorityImplicits: 1,
  Lcom_raquo_laminar_keys_CompositeKey$CompositeValueMappers: 1,
  Lcom_raquo_laminar_api_Implicits: 1
}));
var $n_Lcom_raquo_laminar_api_package$;
function $m_Lcom_raquo_laminar_api_package$() {
  if ((!$n_Lcom_raquo_laminar_api_package$)) {
    $n_Lcom_raquo_laminar_api_package$ = new $c_Lcom_raquo_laminar_api_package$();
  }
  return $n_Lcom_raquo_laminar_api_package$;
}
/** @constructor */
function $c_Lcom_raquo_laminar_nodes_TextNode(initialText) {
  this.Lcom_raquo_laminar_nodes_TextNode__f_com$raquo$laminar$nodes$ChildNode$$_maybeParent = null;
  this.Lcom_raquo_laminar_nodes_TextNode__f_ref = null;
  this.Lcom_raquo_laminar_nodes_TextNode__f_com$raquo$laminar$nodes$ChildNode$$_maybeParent = $m_s_None$();
  this.Lcom_raquo_laminar_nodes_TextNode__f_ref = $m_Lcom_raquo_laminar_DomApi$().createTextNode__T__Lorg_scalajs_dom_Text(initialText);
}
$c_Lcom_raquo_laminar_nodes_TextNode.prototype = new $h_O();
$c_Lcom_raquo_laminar_nodes_TextNode.prototype.constructor = $c_Lcom_raquo_laminar_nodes_TextNode;
/** @constructor */
function $h_Lcom_raquo_laminar_nodes_TextNode() {
}
$h_Lcom_raquo_laminar_nodes_TextNode.prototype = $c_Lcom_raquo_laminar_nodes_TextNode.prototype;
$c_Lcom_raquo_laminar_nodes_TextNode.prototype.setParent__s_Option__V = (function(maybeNextParent) {
  this.Lcom_raquo_laminar_nodes_TextNode__f_com$raquo$laminar$nodes$ChildNode$$_maybeParent = maybeNextParent;
});
$c_Lcom_raquo_laminar_nodes_TextNode.prototype.willSetParent__s_Option__V = (function(maybeNextParent) {
});
$c_Lcom_raquo_laminar_nodes_TextNode.prototype.apply__Lcom_raquo_laminar_nodes_ReactiveElement__V = (function(parentNode) {
  $m_Lcom_raquo_laminar_nodes_ParentNode$().appendChild__Lcom_raquo_laminar_nodes_ParentNode__Lcom_raquo_laminar_nodes_ChildNode__O__Z(parentNode, this, (void 0));
});
$c_Lcom_raquo_laminar_nodes_TextNode.prototype.text__T = (function() {
  return $as_T(this.Lcom_raquo_laminar_nodes_TextNode__f_ref.data);
});
$c_Lcom_raquo_laminar_nodes_TextNode.prototype.ref__Lorg_scalajs_dom_Node = (function() {
  return this.Lcom_raquo_laminar_nodes_TextNode__f_ref;
});
function $as_Lcom_raquo_laminar_nodes_TextNode(obj) {
  return (((obj instanceof $c_Lcom_raquo_laminar_nodes_TextNode) || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.laminar.nodes.TextNode"));
}
function $isArrayOf_Lcom_raquo_laminar_nodes_TextNode(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_laminar_nodes_TextNode)));
}
function $asArrayOf_Lcom_raquo_laminar_nodes_TextNode(obj, depth) {
  return (($isArrayOf_Lcom_raquo_laminar_nodes_TextNode(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.laminar.nodes.TextNode;", depth));
}
var $d_Lcom_raquo_laminar_nodes_TextNode = new $TypeData().initClass($c_Lcom_raquo_laminar_nodes_TextNode, "com.raquo.laminar.nodes.TextNode", ({
  Lcom_raquo_laminar_nodes_TextNode: 1,
  Lcom_raquo_laminar_nodes_ReactiveNode: 1,
  Lcom_raquo_laminar_modifiers_Modifier: 1,
  Lcom_raquo_laminar_nodes_ChildNode: 1
}));
function $f_jl_Boolean__equals__O__Z($thiz, that) {
  return ($thiz === that);
}
function $f_jl_Boolean__hashCode__I($thiz) {
  return ($thiz ? 1231 : 1237);
}
function $f_jl_Boolean__toString__T($thiz) {
  return ("" + $thiz);
}
var $d_jl_Boolean = new $TypeData().initClass(0, "java.lang.Boolean", ({
  jl_Boolean: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1,
  jl_constant_Constable: 1
}), ((x) => ((typeof x) === "boolean")));
function $f_jl_Character__hashCode__I($thiz) {
  return $thiz;
}
function $f_jl_Character__equals__O__Z($thiz, that) {
  if ((that instanceof $Char)) {
    var this$1 = $n($as_jl_Character(that)).c;
    return ($thiz === this$1);
  } else {
    return false;
  }
}
function $f_jl_Character__toString__T($thiz) {
  return ("" + $cToS($thiz));
}
function $as_jl_Character(obj) {
  return (((obj instanceof $Char) || (obj === null)) ? obj : $throwClassCastException(obj, "java.lang.Character"));
}
function $isArrayOf_jl_Character(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Character)));
}
function $asArrayOf_jl_Character(obj, depth) {
  return (($isArrayOf_jl_Character(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Character;", depth));
}
var $d_jl_Character = new $TypeData().initClass(0, "java.lang.Character", ({
  jl_Character: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1,
  jl_constant_Constable: 1
}), ((x) => (x instanceof $Char)));
class $c_jl_RuntimeException extends $c_jl_Exception {
}
/** @constructor */
function $c_jl_StringBuilder() {
  this.jl_StringBuilder__f_java$lang$StringBuilder$$content = null;
  this.jl_StringBuilder__f_java$lang$StringBuilder$$content = "";
}
$c_jl_StringBuilder.prototype = new $h_O();
$c_jl_StringBuilder.prototype.constructor = $c_jl_StringBuilder;
/** @constructor */
function $h_jl_StringBuilder() {
}
$h_jl_StringBuilder.prototype = $c_jl_StringBuilder.prototype;
$c_jl_StringBuilder.prototype.append__AC__jl_StringBuilder = (function(str) {
  var this$1 = $m_jl_String$();
  var count = $n(str).u.length;
  var str$1 = this$1.new__AC__I__I__T(str, 0, count);
  this.jl_StringBuilder__f_java$lang$StringBuilder$$content = (("" + this.jl_StringBuilder__f_java$lang$StringBuilder$$content) + str$1);
  return this;
});
$c_jl_StringBuilder.prototype.toString__T = (function() {
  return this.jl_StringBuilder__f_java$lang$StringBuilder$$content;
});
$c_jl_StringBuilder.prototype.length__I = (function() {
  var this$1 = $n(this.jl_StringBuilder__f_java$lang$StringBuilder$$content);
  return this$1.length;
});
$c_jl_StringBuilder.prototype.charAt__I__C = (function(index) {
  var this$1 = $n(this.jl_StringBuilder__f_java$lang$StringBuilder$$content);
  return $charAt(this$1, index);
});
var $d_jl_StringBuilder = new $TypeData().initClass($c_jl_StringBuilder, "java.lang.StringBuilder", ({
  jl_StringBuilder: 1,
  jl_CharSequence: 1,
  jl_Appendable: 1,
  Ljava_io_Serializable: 1
}));
class $c_jl_VirtualMachineError extends $c_jl_Error {
}
/** @constructor */
function $c_s_$eq$colon$eq() {
}
$c_s_$eq$colon$eq.prototype = new $h_s_$less$colon$less();
$c_s_$eq$colon$eq.prototype.constructor = $c_s_$eq$colon$eq;
/** @constructor */
function $h_s_$eq$colon$eq() {
}
$h_s_$eq$colon$eq.prototype = $c_s_$eq$colon$eq.prototype;
/** @constructor */
function $c_s_PartialFunction$$anon$1() {
}
$c_s_PartialFunction$$anon$1.prototype = new $h_O();
$c_s_PartialFunction$$anon$1.prototype.constructor = $c_s_PartialFunction$$anon$1;
/** @constructor */
function $h_s_PartialFunction$$anon$1() {
}
$h_s_PartialFunction$$anon$1.prototype = $c_s_PartialFunction$$anon$1.prototype;
$c_s_PartialFunction$$anon$1.prototype.toString__T = (function() {
  return "<function1>";
});
$c_s_PartialFunction$$anon$1.prototype.apply__O__E = (function(x) {
  throw new $c_s_MatchError(x);
});
$c_s_PartialFunction$$anon$1.prototype.apply__O__O = (function(v1) {
  this.apply__O__E(v1);
});
var $d_s_PartialFunction$$anon$1 = new $TypeData().initClass($c_s_PartialFunction$$anon$1, "scala.PartialFunction$$anon$1", ({
  s_PartialFunction$$anon$1: 1,
  s_PartialFunction: 1,
  F1: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sc_AbstractIterator() {
}
$c_sc_AbstractIterator.prototype = new $h_O();
$c_sc_AbstractIterator.prototype.constructor = $c_sc_AbstractIterator;
/** @constructor */
function $h_sc_AbstractIterator() {
}
$h_sc_AbstractIterator.prototype = $c_sc_AbstractIterator.prototype;
$c_sc_AbstractIterator.prototype.iterator__sc_Iterator = (function() {
  return this;
});
$c_sc_AbstractIterator.prototype.drop__I__sc_Iterator = (function(n) {
  return this.sliceIterator__I__I__sc_Iterator(n, (-1));
});
$c_sc_AbstractIterator.prototype.sliceIterator__I__I__sc_Iterator = (function(from, until) {
  return $f_sc_Iterator__sliceIterator__I__I__sc_Iterator(this, from, until);
});
$c_sc_AbstractIterator.prototype.toString__T = (function() {
  return "<iterator>";
});
$c_sc_AbstractIterator.prototype.addString__scm_StringBuilder__T__T__T__scm_StringBuilder = (function(b, start, sep, end) {
  return $f_sc_IterableOnceOps__addString__scm_StringBuilder__T__T__T__scm_StringBuilder(this, b, start, sep, end);
});
$c_sc_AbstractIterator.prototype.knownSize__I = (function() {
  return (-1);
});
function $ct_sc_SeqFactory$Delegate__sc_SeqFactory__($thiz, delegate) {
  $thiz.sc_SeqFactory$Delegate__f_delegate = delegate;
  return $thiz;
}
/** @constructor */
function $c_sc_SeqFactory$Delegate() {
  this.sc_SeqFactory$Delegate__f_delegate = null;
}
$c_sc_SeqFactory$Delegate.prototype = new $h_O();
$c_sc_SeqFactory$Delegate.prototype.constructor = $c_sc_SeqFactory$Delegate;
/** @constructor */
function $h_sc_SeqFactory$Delegate() {
}
$h_sc_SeqFactory$Delegate.prototype = $c_sc_SeqFactory$Delegate.prototype;
$c_sc_SeqFactory$Delegate.prototype.apply__sci_Seq__sc_SeqOps = (function(elems) {
  return $as_sc_SeqOps($n(this.sc_SeqFactory$Delegate__f_delegate).apply__sci_Seq__O(elems));
});
$c_sc_SeqFactory$Delegate.prototype.from__sc_IterableOnce__sc_SeqOps = (function(it) {
  return $as_sc_SeqOps($n(this.sc_SeqFactory$Delegate__f_delegate).from__sc_IterableOnce__O(it));
});
$c_sc_SeqFactory$Delegate.prototype.from__sc_IterableOnce__O = (function(source) {
  return this.from__sc_IterableOnce__sc_SeqOps(source);
});
$c_sc_SeqFactory$Delegate.prototype.apply__sci_Seq__O = (function(elems) {
  return this.apply__sci_Seq__sc_SeqOps(elems);
});
function $f_sc_SeqOps__isEmpty__Z($thiz) {
  return ($thiz.lengthCompare__I__I(0) === 0);
}
function $f_sc_SeqOps__sameElements__sc_IterableOnce__Z($thiz, that) {
  var thisKnownSize = $thiz.knownSize__I();
  if ((thisKnownSize !== (-1))) {
    var thatKnownSize = $n(that).knownSize__I();
    var knownSizeDifference = ((thatKnownSize !== (-1)) && (thisKnownSize !== thatKnownSize));
  } else {
    var knownSizeDifference = false;
  }
  if ((!knownSizeDifference)) {
    var this$1 = $n($thiz.iterator__sc_Iterator());
    return $f_sc_Iterator__sameElements__sc_IterableOnce__Z(this$1, that);
  } else {
    return false;
  }
}
function $is_sc_SeqOps(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_SeqOps)));
}
function $as_sc_SeqOps(obj) {
  return (($is_sc_SeqOps(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.SeqOps"));
}
function $isArrayOf_sc_SeqOps(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_SeqOps)));
}
function $asArrayOf_sc_SeqOps(obj, depth) {
  return (($isArrayOf_sc_SeqOps(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.SeqOps;", depth));
}
function $is_Lcom_raquo_airstream_core_SyncObservable(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lcom_raquo_airstream_core_SyncObservable)));
}
function $as_Lcom_raquo_airstream_core_SyncObservable(obj) {
  return (($is_Lcom_raquo_airstream_core_SyncObservable(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.airstream.core.SyncObservable"));
}
function $isArrayOf_Lcom_raquo_airstream_core_SyncObservable(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_airstream_core_SyncObservable)));
}
function $asArrayOf_Lcom_raquo_airstream_core_SyncObservable(obj, depth) {
  return (($isArrayOf_Lcom_raquo_airstream_core_SyncObservable(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.airstream.core.SyncObservable;", depth));
}
function $f_Lcom_raquo_laminar_nodes_ReactiveElement__$init$__V($thiz) {
  $thiz.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_com$raquo$laminar$nodes$ReactiveElement$$pilotSubscription = new $c_Lcom_raquo_airstream_ownership_TransferableSubscription(new $c_sjsr_AnonFunction0((() => {
    $n($thiz.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_dynamicOwner).activate__V();
  })), new $c_sjsr_AnonFunction0((() => {
    $n($thiz.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_dynamicOwner).deactivate__V();
  })));
}
function $f_Lcom_raquo_laminar_nodes_ReactiveElement__willSetParent__s_Option__V($thiz, maybeNextParent) {
  if ($p_Lcom_raquo_laminar_nodes_ReactiveElement__isUnmounting__s_Option__s_Option__Z($thiz, $thiz.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_com$raquo$laminar$nodes$ChildNode$$_maybeParent, maybeNextParent)) {
    $p_Lcom_raquo_laminar_nodes_ReactiveElement__setPilotSubscriptionOwner__s_Option__V($thiz, maybeNextParent);
  }
}
function $f_Lcom_raquo_laminar_nodes_ReactiveElement__setParent__s_Option__V($thiz, maybeNextParent) {
  var maybePrevParent = $thiz.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_com$raquo$laminar$nodes$ChildNode$$_maybeParent;
  $thiz.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_com$raquo$laminar$nodes$ChildNode$$_maybeParent = maybeNextParent;
  if ((!$p_Lcom_raquo_laminar_nodes_ReactiveElement__isUnmounting__s_Option__s_Option__Z($thiz, maybePrevParent, maybeNextParent))) {
    $p_Lcom_raquo_laminar_nodes_ReactiveElement__setPilotSubscriptionOwner__s_Option__V($thiz, maybeNextParent);
  }
}
function $p_Lcom_raquo_laminar_nodes_ReactiveElement__isUnmounting__s_Option__s_Option__Z($thiz, maybePrevParent, maybeNextParent) {
  var this$1 = $n(maybePrevParent);
  if ((!this$1.isEmpty__Z())) {
    var arg1 = this$1.get__O();
    var _$4 = $as_Lcom_raquo_laminar_nodes_ParentNode(arg1);
    var this$2 = $n($n(_$4).dynamicOwner__Lcom_raquo_airstream_ownership_DynamicOwner());
    var this$3 = $n(this$2.Lcom_raquo_airstream_ownership_DynamicOwner__f__maybeCurrentOwner);
    var isPrevParentActive = (!this$3.isEmpty__Z());
  } else {
    var isPrevParentActive = false;
  }
  var this$4 = $n(maybeNextParent);
  if ((!this$4.isEmpty__Z())) {
    var arg1$1 = this$4.get__O();
    var _$5 = $as_Lcom_raquo_laminar_nodes_ParentNode(arg1$1);
    var this$5 = $n($n(_$5).dynamicOwner__Lcom_raquo_airstream_ownership_DynamicOwner());
    var this$6 = $n(this$5.Lcom_raquo_airstream_ownership_DynamicOwner__f__maybeCurrentOwner);
    var isNextParentActive = (!this$6.isEmpty__Z());
  } else {
    var isNextParentActive = false;
  }
  return (isPrevParentActive && (!isNextParentActive));
}
function $p_Lcom_raquo_laminar_nodes_ReactiveElement__setPilotSubscriptionOwner__s_Option__V($thiz, maybeNextParent) {
  var this$1 = $n(maybeNextParent);
  if (this$1.isEmpty__Z()) {
    var maybeNextOwner = $m_s_None$();
  } else {
    var arg1 = this$1.get__O();
    var _$6 = $as_Lcom_raquo_laminar_nodes_ParentNode(arg1);
    var maybeNextOwner = new $c_s_Some($n(_$6).dynamicOwner__Lcom_raquo_airstream_ownership_DynamicOwner());
  }
  $f_Lcom_raquo_laminar_nodes_ReactiveElement__unsafeSetPilotSubscriptionOwner__s_Option__V($thiz, maybeNextOwner);
}
function $f_Lcom_raquo_laminar_nodes_ReactiveElement__unsafeSetPilotSubscriptionOwner__s_Option__V($thiz, maybeNextOwner) {
  var this$1 = $n(maybeNextOwner);
  if (this$1.isEmpty__Z()) {
    $n($thiz.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_com$raquo$laminar$nodes$ReactiveElement$$pilotSubscription).clearOwner__V();
  } else {
    var arg1 = this$1.get__O();
    var nextOwner = $as_Lcom_raquo_airstream_ownership_DynamicOwner(arg1);
    $n($thiz.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_com$raquo$laminar$nodes$ReactiveElement$$pilotSubscription).setOwner__Lcom_raquo_airstream_ownership_DynamicOwner__V(nextOwner);
  }
}
function $is_Lcom_raquo_laminar_nodes_ReactiveElement(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Lcom_raquo_laminar_nodes_ReactiveElement)));
}
function $as_Lcom_raquo_laminar_nodes_ReactiveElement(obj) {
  return (($is_Lcom_raquo_laminar_nodes_ReactiveElement(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.laminar.nodes.ReactiveElement"));
}
function $isArrayOf_Lcom_raquo_laminar_nodes_ReactiveElement(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_laminar_nodes_ReactiveElement)));
}
function $asArrayOf_Lcom_raquo_laminar_nodes_ReactiveElement(obj, depth) {
  return (($isArrayOf_Lcom_raquo_laminar_nodes_ReactiveElement(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.laminar.nodes.ReactiveElement;", depth));
}
class $c_jl_ArithmeticException extends $c_jl_RuntimeException {
  constructor(s) {
    super();
    $ct_jl_Throwable__T__jl_Throwable__Z__Z__(this, s, null, true, true);
  }
}
var $d_jl_ArithmeticException = new $TypeData().initClass($c_jl_ArithmeticException, "java.lang.ArithmeticException", ({
  jl_ArithmeticException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
class $c_jl_ArrayStoreException extends $c_jl_RuntimeException {
  constructor(s) {
    super();
    $ct_jl_Throwable__T__jl_Throwable__Z__Z__(this, s, null, true, true);
  }
}
var $d_jl_ArrayStoreException = new $TypeData().initClass($c_jl_ArrayStoreException, "java.lang.ArrayStoreException", ({
  jl_ArrayStoreException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
function $f_jl_Byte__equals__O__Z($thiz, that) {
  return Object.is($thiz, that);
}
function $f_jl_Byte__hashCode__I($thiz) {
  return $thiz;
}
function $f_jl_Byte__toString__T($thiz) {
  return ("" + $thiz);
}
var $d_jl_Byte = new $TypeData().initClass(0, "java.lang.Byte", ({
  jl_Byte: 1,
  jl_Number: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1,
  jl_constant_Constable: 1
}), ((x) => $isByte(x)));
class $c_jl_ClassCastException extends $c_jl_RuntimeException {
  constructor(s) {
    super();
    $ct_jl_Throwable__T__jl_Throwable__Z__Z__(this, s, null, true, true);
  }
}
var $d_jl_ClassCastException = new $TypeData().initClass($c_jl_ClassCastException, "java.lang.ClassCastException", ({
  jl_ClassCastException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
class $c_jl_IllegalArgumentException extends $c_jl_RuntimeException {
  constructor(s) {
    super();
    $ct_jl_Throwable__T__jl_Throwable__Z__Z__(this, s, null, true, true);
  }
}
var $d_jl_IllegalArgumentException = new $TypeData().initClass($c_jl_IllegalArgumentException, "java.lang.IllegalArgumentException", ({
  jl_IllegalArgumentException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
function $ct_jl_IndexOutOfBoundsException__T__($thiz, s) {
  $ct_jl_Throwable__T__jl_Throwable__Z__Z__($thiz, s, null, true, true);
  return $thiz;
}
class $c_jl_IndexOutOfBoundsException extends $c_jl_RuntimeException {
}
var $d_jl_IndexOutOfBoundsException = new $TypeData().initClass($c_jl_IndexOutOfBoundsException, "java.lang.IndexOutOfBoundsException", ({
  jl_IndexOutOfBoundsException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
class $c_jl_NegativeArraySizeException extends $c_jl_RuntimeException {
  constructor() {
    super();
    $ct_jl_Throwable__T__jl_Throwable__Z__Z__(this, null, null, true, true);
  }
}
var $d_jl_NegativeArraySizeException = new $TypeData().initClass($c_jl_NegativeArraySizeException, "java.lang.NegativeArraySizeException", ({
  jl_NegativeArraySizeException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
class $c_jl_NullPointerException extends $c_jl_RuntimeException {
  constructor() {
    super();
    $ct_jl_Throwable__T__jl_Throwable__Z__Z__(this, null, null, true, true);
  }
}
var $d_jl_NullPointerException = new $TypeData().initClass($c_jl_NullPointerException, "java.lang.NullPointerException", ({
  jl_NullPointerException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
function $f_jl_Short__equals__O__Z($thiz, that) {
  return Object.is($thiz, that);
}
function $f_jl_Short__hashCode__I($thiz) {
  return $thiz;
}
function $f_jl_Short__toString__T($thiz) {
  return ("" + $thiz);
}
var $d_jl_Short = new $TypeData().initClass(0, "java.lang.Short", ({
  jl_Short: 1,
  jl_Number: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1,
  jl_constant_Constable: 1
}), ((x) => $isShort(x)));
class $c_jl_UnsupportedOperationException extends $c_jl_RuntimeException {
  constructor(s) {
    super();
    $ct_jl_Throwable__T__jl_Throwable__Z__Z__(this, s, null, true, true);
  }
}
var $d_jl_UnsupportedOperationException = new $TypeData().initClass($c_jl_UnsupportedOperationException, "java.lang.UnsupportedOperationException", ({
  jl_UnsupportedOperationException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
class $c_ju_ConcurrentModificationException extends $c_jl_RuntimeException {
  constructor(s) {
    super();
    $ct_jl_Throwable__T__jl_Throwable__Z__Z__(this, s, null, true, true);
  }
}
var $d_ju_ConcurrentModificationException = new $TypeData().initClass($c_ju_ConcurrentModificationException, "java.util.ConcurrentModificationException", ({
  ju_ConcurrentModificationException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
class $c_ju_NoSuchElementException extends $c_jl_RuntimeException {
  constructor(s) {
    super();
    $ct_jl_Throwable__T__jl_Throwable__Z__Z__(this, s, null, true, true);
  }
}
var $d_ju_NoSuchElementException = new $TypeData().initClass($c_ju_NoSuchElementException, "java.util.NoSuchElementException", ({
  ju_NoSuchElementException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
class $c_Lorg_scalajs_linker_runtime_UndefinedBehaviorError extends $c_jl_VirtualMachineError {
  constructor(cause) {
    super();
    var message = ((cause === null) ? null : $n(cause).toString__T());
    $ct_jl_Throwable__T__jl_Throwable__Z__Z__(this, message, cause, true, true);
  }
}
var $d_Lorg_scalajs_linker_runtime_UndefinedBehaviorError = new $TypeData().initClass($c_Lorg_scalajs_linker_runtime_UndefinedBehaviorError, "org.scalajs.linker.runtime.UndefinedBehaviorError", ({
  Lorg_scalajs_linker_runtime_UndefinedBehaviorError: 1,
  jl_VirtualMachineError: 1,
  jl_Error: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_s_$less$colon$less$$anon$1() {
}
$c_s_$less$colon$less$$anon$1.prototype = new $h_s_$eq$colon$eq();
$c_s_$less$colon$less$$anon$1.prototype.constructor = $c_s_$less$colon$less$$anon$1;
/** @constructor */
function $h_s_$less$colon$less$$anon$1() {
}
$h_s_$less$colon$less$$anon$1.prototype = $c_s_$less$colon$less$$anon$1.prototype;
$c_s_$less$colon$less$$anon$1.prototype.apply__O__O = (function(x) {
  return x;
});
$c_s_$less$colon$less$$anon$1.prototype.toString__T = (function() {
  return "generalized constraint";
});
var $d_s_$less$colon$less$$anon$1 = new $TypeData().initClass($c_s_$less$colon$less$$anon$1, "scala.$less$colon$less$$anon$1", ({
  s_$less$colon$less$$anon$1: 1,
  s_$eq$colon$eq: 1,
  s_$less$colon$less: 1,
  F1: 1,
  Ljava_io_Serializable: 1
}));
function $p_s_MatchError__objString$lzycompute__T($thiz) {
  if ((!$thiz.s_MatchError__f_bitmap$0)) {
    $thiz.s_MatchError__f_objString = (($thiz.s_MatchError__f_obj === null) ? "null" : $p_s_MatchError__liftedTree1$1__T($thiz));
    $thiz.s_MatchError__f_bitmap$0 = true;
  }
  return $thiz.s_MatchError__f_objString;
}
function $p_s_MatchError__objString__T($thiz) {
  return ((!$thiz.s_MatchError__f_bitmap$0) ? $p_s_MatchError__objString$lzycompute__T($thiz) : $thiz.s_MatchError__f_objString);
}
function $p_s_MatchError__ofClass$1__T($thiz) {
  var this$1 = $n($thiz.s_MatchError__f_obj);
  return ("of class " + $objectClassName(this$1));
}
function $p_s_MatchError__liftedTree1$1__T($thiz) {
  try {
    return ((($thiz.s_MatchError__f_obj + " (") + $p_s_MatchError__ofClass$1__T($thiz)) + ")");
  } catch (e) {
    return ("an instance " + $p_s_MatchError__ofClass$1__T($thiz));
  }
}
class $c_s_MatchError extends $c_jl_RuntimeException {
  constructor(obj) {
    super();
    this.s_MatchError__f_objString = null;
    this.s_MatchError__f_obj = null;
    this.s_MatchError__f_bitmap$0 = false;
    this.s_MatchError__f_obj = obj;
    $ct_jl_Throwable__T__jl_Throwable__Z__Z__(this, null, null, true, true);
  }
  getMessage__T() {
    return $p_s_MatchError__objString__T(this);
  }
}
var $d_s_MatchError = new $TypeData().initClass($c_s_MatchError, "scala.MatchError", ({
  s_MatchError: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_s_Option() {
}
$c_s_Option.prototype = new $h_O();
$c_s_Option.prototype.constructor = $c_s_Option;
/** @constructor */
function $h_s_Option() {
}
$h_s_Option.prototype = $c_s_Option.prototype;
$c_s_Option.prototype.isEmpty__Z = (function() {
  return (this === $m_s_None$());
});
$c_s_Option.prototype.knownSize__I = (function() {
  return (this.isEmpty__Z() ? 0 : 1);
});
$c_s_Option.prototype.iterator__sc_Iterator = (function() {
  if (this.isEmpty__Z()) {
    return $m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty;
  } else {
    var a = this.get__O();
    return new $c_sc_Iterator$$anon$20(a);
  }
});
function $as_s_Option(obj) {
  return (((obj instanceof $c_s_Option) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.Option"));
}
function $isArrayOf_s_Option(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.s_Option)));
}
function $asArrayOf_s_Option(obj, depth) {
  return (($isArrayOf_s_Option(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.Option;", depth));
}
/** @constructor */
function $c_s_Product$$anon$1(outer) {
  this.s_Product$$anon$1__f_c = 0;
  this.s_Product$$anon$1__f_cmax = 0;
  this.s_Product$$anon$1__f_$outer = null;
  $n(outer);
  this.s_Product$$anon$1__f_$outer = outer;
  this.s_Product$$anon$1__f_c = 0;
  this.s_Product$$anon$1__f_cmax = $n(outer).productArity__I();
}
$c_s_Product$$anon$1.prototype = new $h_sc_AbstractIterator();
$c_s_Product$$anon$1.prototype.constructor = $c_s_Product$$anon$1;
/** @constructor */
function $h_s_Product$$anon$1() {
}
$h_s_Product$$anon$1.prototype = $c_s_Product$$anon$1.prototype;
$c_s_Product$$anon$1.prototype.hasNext__Z = (function() {
  return (this.s_Product$$anon$1__f_c < this.s_Product$$anon$1__f_cmax);
});
$c_s_Product$$anon$1.prototype.next__O = (function() {
  var result = $n(this.s_Product$$anon$1__f_$outer).productElement__I__O(this.s_Product$$anon$1__f_c);
  this.s_Product$$anon$1__f_c = ((1 + this.s_Product$$anon$1__f_c) | 0);
  return result;
});
var $d_s_Product$$anon$1 = new $TypeData().initClass($c_s_Product$$anon$1, "scala.Product$$anon$1", ({
  s_Product$$anon$1: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1
}));
/** @constructor */
function $c_sc_ClassTagSeqFactory$AnySeqDelegate(delegate) {
  this.sc_ClassTagIterableFactory$AnyIterableDelegate__f_delegate = null;
  $ct_sc_ClassTagIterableFactory$AnyIterableDelegate__sc_ClassTagIterableFactory__(this, delegate);
}
$c_sc_ClassTagSeqFactory$AnySeqDelegate.prototype = new $h_sc_ClassTagIterableFactory$AnyIterableDelegate();
$c_sc_ClassTagSeqFactory$AnySeqDelegate.prototype.constructor = $c_sc_ClassTagSeqFactory$AnySeqDelegate;
/** @constructor */
function $h_sc_ClassTagSeqFactory$AnySeqDelegate() {
}
$h_sc_ClassTagSeqFactory$AnySeqDelegate.prototype = $c_sc_ClassTagSeqFactory$AnySeqDelegate.prototype;
var $d_sc_ClassTagSeqFactory$AnySeqDelegate = new $TypeData().initClass($c_sc_ClassTagSeqFactory$AnySeqDelegate, "scala.collection.ClassTagSeqFactory$AnySeqDelegate", ({
  sc_ClassTagSeqFactory$AnySeqDelegate: 1,
  sc_ClassTagIterableFactory$AnyIterableDelegate: 1,
  sc_IterableFactory: 1,
  Ljava_io_Serializable: 1,
  sc_SeqFactory: 1
}));
function $f_sc_Iterable__toString__T($thiz) {
  return $f_sc_IterableOnceOps__mkString__T__T__T__T($thiz, ($thiz.className__T() + "("), ", ", ")");
}
/** @constructor */
function $c_sc_Iterator$$anon$19() {
}
$c_sc_Iterator$$anon$19.prototype = new $h_sc_AbstractIterator();
$c_sc_Iterator$$anon$19.prototype.constructor = $c_sc_Iterator$$anon$19;
/** @constructor */
function $h_sc_Iterator$$anon$19() {
}
$h_sc_Iterator$$anon$19.prototype = $c_sc_Iterator$$anon$19.prototype;
$c_sc_Iterator$$anon$19.prototype.hasNext__Z = (function() {
  return false;
});
$c_sc_Iterator$$anon$19.prototype.next__E = (function() {
  throw new $c_ju_NoSuchElementException("next on empty iterator");
});
$c_sc_Iterator$$anon$19.prototype.knownSize__I = (function() {
  return 0;
});
$c_sc_Iterator$$anon$19.prototype.sliceIterator__I__I__sc_Iterator = (function(from, until) {
  return this;
});
$c_sc_Iterator$$anon$19.prototype.next__O = (function() {
  this.next__E();
});
var $d_sc_Iterator$$anon$19 = new $TypeData().initClass($c_sc_Iterator$$anon$19, "scala.collection.Iterator$$anon$19", ({
  sc_Iterator$$anon$19: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1
}));
/** @constructor */
function $c_sc_Iterator$$anon$20(a$1) {
  this.sc_Iterator$$anon$20__f_consumed = false;
  this.sc_Iterator$$anon$20__f_a$1 = null;
  this.sc_Iterator$$anon$20__f_a$1 = a$1;
  this.sc_Iterator$$anon$20__f_consumed = false;
}
$c_sc_Iterator$$anon$20.prototype = new $h_sc_AbstractIterator();
$c_sc_Iterator$$anon$20.prototype.constructor = $c_sc_Iterator$$anon$20;
/** @constructor */
function $h_sc_Iterator$$anon$20() {
}
$h_sc_Iterator$$anon$20.prototype = $c_sc_Iterator$$anon$20.prototype;
$c_sc_Iterator$$anon$20.prototype.hasNext__Z = (function() {
  return (!this.sc_Iterator$$anon$20__f_consumed);
});
$c_sc_Iterator$$anon$20.prototype.next__O = (function() {
  if (this.sc_Iterator$$anon$20__f_consumed) {
    return $n($m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty).next__O();
  } else {
    this.sc_Iterator$$anon$20__f_consumed = true;
    return this.sc_Iterator$$anon$20__f_a$1;
  }
});
$c_sc_Iterator$$anon$20.prototype.sliceIterator__I__I__sc_Iterator = (function(from, until) {
  return (((this.sc_Iterator$$anon$20__f_consumed || (from > 0)) || (until === 0)) ? $m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty : this);
});
var $d_sc_Iterator$$anon$20 = new $TypeData().initClass($c_sc_Iterator$$anon$20, "scala.collection.Iterator$$anon$20", ({
  sc_Iterator$$anon$20: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1
}));
function $p_sc_Iterator$ConcatIterator__merge$1__V($thiz) {
  while (true) {
    if (($thiz.sc_Iterator$ConcatIterator__f_current instanceof $c_sc_Iterator$ConcatIterator)) {
      var c = $as_sc_Iterator$ConcatIterator($thiz.sc_Iterator$ConcatIterator__f_current);
      $thiz.sc_Iterator$ConcatIterator__f_current = $n(c).sc_Iterator$ConcatIterator__f_current;
      $thiz.sc_Iterator$ConcatIterator__f_currentHasNextChecked = $n(c).sc_Iterator$ConcatIterator__f_currentHasNextChecked;
      if (($n(c).sc_Iterator$ConcatIterator__f_tail !== null)) {
        if (($thiz.sc_Iterator$ConcatIterator__f_last === null)) {
          $thiz.sc_Iterator$ConcatIterator__f_last = $n(c).sc_Iterator$ConcatIterator__f_last;
        }
        $n($n(c).sc_Iterator$ConcatIterator__f_last).sc_Iterator$ConcatIteratorCell__f_tail = $thiz.sc_Iterator$ConcatIterator__f_tail;
        $thiz.sc_Iterator$ConcatIterator__f_tail = $n(c).sc_Iterator$ConcatIterator__f_tail;
      }
      continue;
    }
    return (void 0);
  }
}
function $p_sc_Iterator$ConcatIterator__advance$1__Z($thiz) {
  while (true) {
    if (($thiz.sc_Iterator$ConcatIterator__f_tail === null)) {
      $thiz.sc_Iterator$ConcatIterator__f_current = null;
      $thiz.sc_Iterator$ConcatIterator__f_last = null;
      return false;
    } else {
      $thiz.sc_Iterator$ConcatIterator__f_current = $n($thiz.sc_Iterator$ConcatIterator__f_tail).headIterator__sc_Iterator();
      if (($thiz.sc_Iterator$ConcatIterator__f_last === $thiz.sc_Iterator$ConcatIterator__f_tail)) {
        $thiz.sc_Iterator$ConcatIterator__f_last = $n($thiz.sc_Iterator$ConcatIterator__f_last).sc_Iterator$ConcatIteratorCell__f_tail;
      }
      $thiz.sc_Iterator$ConcatIterator__f_tail = $n($thiz.sc_Iterator$ConcatIterator__f_tail).sc_Iterator$ConcatIteratorCell__f_tail;
      $p_sc_Iterator$ConcatIterator__merge$1__V($thiz);
      if ($thiz.sc_Iterator$ConcatIterator__f_currentHasNextChecked) {
        return true;
      } else if ((($thiz.sc_Iterator$ConcatIterator__f_current !== null) && $n($thiz.sc_Iterator$ConcatIterator__f_current).hasNext__Z())) {
        $thiz.sc_Iterator$ConcatIterator__f_currentHasNextChecked = true;
        return true;
      }
    }
  }
}
/** @constructor */
function $c_sc_Iterator$ConcatIterator(current) {
  this.sc_Iterator$ConcatIterator__f_current = null;
  this.sc_Iterator$ConcatIterator__f_tail = null;
  this.sc_Iterator$ConcatIterator__f_last = null;
  this.sc_Iterator$ConcatIterator__f_currentHasNextChecked = false;
  this.sc_Iterator$ConcatIterator__f_current = current;
  this.sc_Iterator$ConcatIterator__f_tail = null;
  this.sc_Iterator$ConcatIterator__f_last = null;
  this.sc_Iterator$ConcatIterator__f_currentHasNextChecked = false;
}
$c_sc_Iterator$ConcatIterator.prototype = new $h_sc_AbstractIterator();
$c_sc_Iterator$ConcatIterator.prototype.constructor = $c_sc_Iterator$ConcatIterator;
/** @constructor */
function $h_sc_Iterator$ConcatIterator() {
}
$h_sc_Iterator$ConcatIterator.prototype = $c_sc_Iterator$ConcatIterator.prototype;
$c_sc_Iterator$ConcatIterator.prototype.hasNext__Z = (function() {
  if (this.sc_Iterator$ConcatIterator__f_currentHasNextChecked) {
    return true;
  } else if ((this.sc_Iterator$ConcatIterator__f_current !== null)) {
    if ($n(this.sc_Iterator$ConcatIterator__f_current).hasNext__Z()) {
      this.sc_Iterator$ConcatIterator__f_currentHasNextChecked = true;
      return true;
    } else {
      return $p_sc_Iterator$ConcatIterator__advance$1__Z(this);
    }
  } else {
    return false;
  }
});
$c_sc_Iterator$ConcatIterator.prototype.next__O = (function() {
  if (this.hasNext__Z()) {
    this.sc_Iterator$ConcatIterator__f_currentHasNextChecked = false;
    return $n(this.sc_Iterator$ConcatIterator__f_current).next__O();
  } else {
    return $n($m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty).next__O();
  }
});
$c_sc_Iterator$ConcatIterator.prototype.concat__F0__sc_Iterator = (function(that) {
  var c = new $c_sc_Iterator$ConcatIteratorCell(that, null);
  if ((this.sc_Iterator$ConcatIterator__f_tail === null)) {
    this.sc_Iterator$ConcatIterator__f_tail = c;
    this.sc_Iterator$ConcatIterator__f_last = c;
  } else {
    $n(this.sc_Iterator$ConcatIterator__f_last).sc_Iterator$ConcatIteratorCell__f_tail = c;
    this.sc_Iterator$ConcatIterator__f_last = c;
  }
  if ((this.sc_Iterator$ConcatIterator__f_current === null)) {
    this.sc_Iterator$ConcatIterator__f_current = $m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty;
  }
  return this;
});
function $as_sc_Iterator$ConcatIterator(obj) {
  return (((obj instanceof $c_sc_Iterator$ConcatIterator) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.Iterator$ConcatIterator"));
}
function $isArrayOf_sc_Iterator$ConcatIterator(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_Iterator$ConcatIterator)));
}
function $asArrayOf_sc_Iterator$ConcatIterator(obj, depth) {
  return (($isArrayOf_sc_Iterator$ConcatIterator(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.Iterator$ConcatIterator;", depth));
}
var $d_sc_Iterator$ConcatIterator = new $TypeData().initClass($c_sc_Iterator$ConcatIterator, "scala.collection.Iterator$ConcatIterator", ({
  sc_Iterator$ConcatIterator: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1
}));
function $p_sc_Iterator$SliceIterator__skip__V($thiz) {
  while (($thiz.sc_Iterator$SliceIterator__f_dropping > 0)) {
    if ($n($thiz.sc_Iterator$SliceIterator__f_underlying).hasNext__Z()) {
      $n($thiz.sc_Iterator$SliceIterator__f_underlying).next__O();
      $thiz.sc_Iterator$SliceIterator__f_dropping = (((-1) + $thiz.sc_Iterator$SliceIterator__f_dropping) | 0);
    } else {
      $thiz.sc_Iterator$SliceIterator__f_dropping = 0;
    }
  }
}
function $p_sc_Iterator$SliceIterator__adjustedBound$1__I__I($thiz, lo$1) {
  if (($thiz.sc_Iterator$SliceIterator__f_scala$collection$Iterator$SliceIterator$$remaining < 0)) {
    return (-1);
  } else {
    var that = (($thiz.sc_Iterator$SliceIterator__f_scala$collection$Iterator$SliceIterator$$remaining - lo$1) | 0);
    return ((that < 0) ? 0 : that);
  }
}
/** @constructor */
function $c_sc_Iterator$SliceIterator(underlying, start, limit) {
  this.sc_Iterator$SliceIterator__f_underlying = null;
  this.sc_Iterator$SliceIterator__f_scala$collection$Iterator$SliceIterator$$remaining = 0;
  this.sc_Iterator$SliceIterator__f_dropping = 0;
  this.sc_Iterator$SliceIterator__f_underlying = underlying;
  this.sc_Iterator$SliceIterator__f_scala$collection$Iterator$SliceIterator$$remaining = limit;
  this.sc_Iterator$SliceIterator__f_dropping = start;
}
$c_sc_Iterator$SliceIterator.prototype = new $h_sc_AbstractIterator();
$c_sc_Iterator$SliceIterator.prototype.constructor = $c_sc_Iterator$SliceIterator;
/** @constructor */
function $h_sc_Iterator$SliceIterator() {
}
$h_sc_Iterator$SliceIterator.prototype = $c_sc_Iterator$SliceIterator.prototype;
$c_sc_Iterator$SliceIterator.prototype.knownSize__I = (function() {
  var size = $n(this.sc_Iterator$SliceIterator__f_underlying).knownSize__I();
  if ((size < 0)) {
    return (-1);
  } else {
    var that = ((size - this.sc_Iterator$SliceIterator__f_dropping) | 0);
    var dropSize = ((that < 0) ? 0 : that);
    if ((this.sc_Iterator$SliceIterator__f_scala$collection$Iterator$SliceIterator$$remaining < 0)) {
      return dropSize;
    } else {
      var x = this.sc_Iterator$SliceIterator__f_scala$collection$Iterator$SliceIterator$$remaining;
      return ((x < dropSize) ? x : dropSize);
    }
  }
});
$c_sc_Iterator$SliceIterator.prototype.hasNext__Z = (function() {
  $p_sc_Iterator$SliceIterator__skip__V(this);
  return ((this.sc_Iterator$SliceIterator__f_scala$collection$Iterator$SliceIterator$$remaining !== 0) && $n(this.sc_Iterator$SliceIterator__f_underlying).hasNext__Z());
});
$c_sc_Iterator$SliceIterator.prototype.next__O = (function() {
  $p_sc_Iterator$SliceIterator__skip__V(this);
  if ((this.sc_Iterator$SliceIterator__f_scala$collection$Iterator$SliceIterator$$remaining > 0)) {
    this.sc_Iterator$SliceIterator__f_scala$collection$Iterator$SliceIterator$$remaining = (((-1) + this.sc_Iterator$SliceIterator__f_scala$collection$Iterator$SliceIterator$$remaining) | 0);
    return $n(this.sc_Iterator$SliceIterator__f_underlying).next__O();
  } else {
    return ((this.sc_Iterator$SliceIterator__f_scala$collection$Iterator$SliceIterator$$remaining < 0) ? $n(this.sc_Iterator$SliceIterator__f_underlying).next__O() : $n($m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty).next__O());
  }
});
$c_sc_Iterator$SliceIterator.prototype.sliceIterator__I__I__sc_Iterator = (function(from, until) {
  var lo = ((from > 0) ? from : 0);
  if ((until < 0)) {
    var rest = $p_sc_Iterator$SliceIterator__adjustedBound$1__I__I(this, lo);
  } else if ((until <= lo)) {
    var rest = 0;
  } else if ((this.sc_Iterator$SliceIterator__f_scala$collection$Iterator$SliceIterator$$remaining < 0)) {
    var rest = ((until - lo) | 0);
  } else {
    var x = $p_sc_Iterator$SliceIterator__adjustedBound$1__I__I(this, lo);
    var that = ((until - lo) | 0);
    var rest = ((x < that) ? x : that);
  }
  var sum = ((this.sc_Iterator$SliceIterator__f_dropping + lo) | 0);
  if ((rest === 0)) {
    return $m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty;
  } else if ((sum < 0)) {
    this.sc_Iterator$SliceIterator__f_dropping = 2147483647;
    this.sc_Iterator$SliceIterator__f_scala$collection$Iterator$SliceIterator$$remaining = 0;
    var xs = new $c_sjsr_AnonFunction0((() => new $c_sc_Iterator$SliceIterator(this.sc_Iterator$SliceIterator__f_underlying, (((-2147483647) + sum) | 0), rest)));
    return $f_sc_Iterator__concat__F0__sc_Iterator(this, xs);
  } else {
    this.sc_Iterator$SliceIterator__f_dropping = sum;
    this.sc_Iterator$SliceIterator__f_scala$collection$Iterator$SliceIterator$$remaining = rest;
    return this;
  }
});
var $d_sc_Iterator$SliceIterator = new $TypeData().initClass($c_sc_Iterator$SliceIterator, "scala.collection.Iterator$SliceIterator", ({
  sc_Iterator$SliceIterator: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1
}));
function $f_sc_LinearSeqOps__apply__I__O($thiz, n) {
  if ((n < 0)) {
    throw $ct_jl_IndexOutOfBoundsException__T__(new $c_jl_IndexOutOfBoundsException(), ("" + n));
  }
  var skipped = $as_sc_LinearSeq($thiz.drop__I__O(n));
  if ($n(skipped).isEmpty__Z()) {
    throw $ct_jl_IndexOutOfBoundsException__T__(new $c_jl_IndexOutOfBoundsException(), ("" + n));
  }
  return $n(skipped).head__O();
}
function $f_sc_LinearSeqOps__sameElements__sc_IterableOnce__Z($thiz, that) {
  if ($is_sc_LinearSeq(that)) {
    var x2 = $as_sc_LinearSeq(that);
    return $p_sc_LinearSeqOps__linearSeqEq$1__sc_LinearSeq__sc_LinearSeq__Z($thiz, $as_sc_LinearSeq($thiz), x2);
  } else {
    return $f_sc_SeqOps__sameElements__sc_IterableOnce__Z($thiz, that);
  }
}
function $p_sc_LinearSeqOps__linearSeqEq$1__sc_LinearSeq__sc_LinearSeq__Z($thiz, a, b) {
  while (true) {
    if ((a === b)) {
      return true;
    } else {
      var this$1 = $n(a);
      if ((!this$1.isEmpty__Z())) {
        var this$2 = $n(b);
        var $x_1 = (!this$2.isEmpty__Z());
      } else {
        var $x_1 = false;
      }
      if (($x_1 && $m_sr_BoxesRunTime$().equals__O__O__Z($n(a).head__O(), $n(b).head__O()))) {
        var temp$a = $as_sc_LinearSeq($n(a).tail__O());
        var temp$b = $as_sc_LinearSeq($n(b).tail__O());
        a = temp$a;
        b = temp$b;
      } else {
        return ($n(a).isEmpty__Z() && $n(b).isEmpty__Z());
      }
    }
  }
}
/** @constructor */
function $c_sc_StrictOptimizedLinearSeqOps$$anon$1(outer) {
  this.sc_StrictOptimizedLinearSeqOps$$anon$1__f_current = null;
  this.sc_StrictOptimizedLinearSeqOps$$anon$1__f_current = outer;
}
$c_sc_StrictOptimizedLinearSeqOps$$anon$1.prototype = new $h_sc_AbstractIterator();
$c_sc_StrictOptimizedLinearSeqOps$$anon$1.prototype.constructor = $c_sc_StrictOptimizedLinearSeqOps$$anon$1;
/** @constructor */
function $h_sc_StrictOptimizedLinearSeqOps$$anon$1() {
}
$h_sc_StrictOptimizedLinearSeqOps$$anon$1.prototype = $c_sc_StrictOptimizedLinearSeqOps$$anon$1.prototype;
$c_sc_StrictOptimizedLinearSeqOps$$anon$1.prototype.hasNext__Z = (function() {
  return (!$n(this.sc_StrictOptimizedLinearSeqOps$$anon$1__f_current).isEmpty__Z());
});
$c_sc_StrictOptimizedLinearSeqOps$$anon$1.prototype.next__O = (function() {
  var r = $n(this.sc_StrictOptimizedLinearSeqOps$$anon$1__f_current).head__O();
  this.sc_StrictOptimizedLinearSeqOps$$anon$1__f_current = $as_sc_StrictOptimizedLinearSeqOps($n(this.sc_StrictOptimizedLinearSeqOps$$anon$1__f_current).tail__O());
  return r;
});
var $d_sc_StrictOptimizedLinearSeqOps$$anon$1 = new $TypeData().initClass($c_sc_StrictOptimizedLinearSeqOps$$anon$1, "scala.collection.StrictOptimizedLinearSeqOps$$anon$1", ({
  sc_StrictOptimizedLinearSeqOps$$anon$1: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1
}));
/** @constructor */
function $c_scm_Buffer$() {
  this.sc_SeqFactory$Delegate__f_delegate = null;
  $ct_sc_SeqFactory$Delegate__sc_SeqFactory__(this, $m_sjs_js_WrappedArray$());
}
$c_scm_Buffer$.prototype = new $h_sc_SeqFactory$Delegate();
$c_scm_Buffer$.prototype.constructor = $c_scm_Buffer$;
/** @constructor */
function $h_scm_Buffer$() {
}
$h_scm_Buffer$.prototype = $c_scm_Buffer$.prototype;
var $d_scm_Buffer$ = new $TypeData().initClass($c_scm_Buffer$, "scala.collection.mutable.Buffer$", ({
  scm_Buffer$: 1,
  sc_SeqFactory$Delegate: 1,
  sc_SeqFactory: 1,
  sc_IterableFactory: 1,
  Ljava_io_Serializable: 1
}));
var $n_scm_Buffer$;
function $m_scm_Buffer$() {
  if ((!$n_scm_Buffer$)) {
    $n_scm_Buffer$ = new $c_scm_Buffer$();
  }
  return $n_scm_Buffer$;
}
/** @constructor */
function $c_scm_MutationTracker$CheckedIterator(underlying, mutationCount) {
  this.scm_MutationTracker$CheckedIterator__f_underlying = null;
  this.scm_MutationTracker$CheckedIterator__f_mutationCount = null;
  this.scm_MutationTracker$CheckedIterator__f_expectedCount = 0;
  this.scm_MutationTracker$CheckedIterator__f_underlying = underlying;
  this.scm_MutationTracker$CheckedIterator__f_mutationCount = mutationCount;
  var this$1 = $n(mutationCount);
  this.scm_MutationTracker$CheckedIterator__f_expectedCount = $uI(this$1.apply__O());
}
$c_scm_MutationTracker$CheckedIterator.prototype = new $h_sc_AbstractIterator();
$c_scm_MutationTracker$CheckedIterator.prototype.constructor = $c_scm_MutationTracker$CheckedIterator;
/** @constructor */
function $h_scm_MutationTracker$CheckedIterator() {
}
$h_scm_MutationTracker$CheckedIterator.prototype = $c_scm_MutationTracker$CheckedIterator.prototype;
$c_scm_MutationTracker$CheckedIterator.prototype.hasNext__Z = (function() {
  var this$2 = $m_scm_MutationTracker$();
  var expectedCount = this.scm_MutationTracker$CheckedIterator__f_expectedCount;
  var this$1 = $n(this.scm_MutationTracker$CheckedIterator__f_mutationCount);
  var actualCount = $uI(this$1.apply__O());
  this$2.checkMutations__I__I__T__V(expectedCount, actualCount, "mutation occurred during iteration");
  return $n(this.scm_MutationTracker$CheckedIterator__f_underlying).hasNext__Z();
});
$c_scm_MutationTracker$CheckedIterator.prototype.next__O = (function() {
  return $n(this.scm_MutationTracker$CheckedIterator__f_underlying).next__O();
});
var $d_scm_MutationTracker$CheckedIterator = new $TypeData().initClass($c_scm_MutationTracker$CheckedIterator, "scala.collection.mutable.MutationTracker$CheckedIterator", ({
  scm_MutationTracker$CheckedIterator: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1
}));
function $is_s_reflect_ClassTag(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.s_reflect_ClassTag)));
}
function $as_s_reflect_ClassTag(obj) {
  return (($is_s_reflect_ClassTag(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.reflect.ClassTag"));
}
function $isArrayOf_s_reflect_ClassTag(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.s_reflect_ClassTag)));
}
function $asArrayOf_s_reflect_ClassTag(obj, depth) {
  return (($isArrayOf_s_reflect_ClassTag(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.reflect.ClassTag;", depth));
}
/** @constructor */
function $c_sr_ScalaRunTime$$anon$1(x$2) {
  this.sr_ScalaRunTime$$anon$1__f_c = 0;
  this.sr_ScalaRunTime$$anon$1__f_cmax = 0;
  this.sr_ScalaRunTime$$anon$1__f_x$2 = null;
  this.sr_ScalaRunTime$$anon$1__f_x$2 = x$2;
  this.sr_ScalaRunTime$$anon$1__f_c = 0;
  this.sr_ScalaRunTime$$anon$1__f_cmax = $n(x$2).productArity__I();
}
$c_sr_ScalaRunTime$$anon$1.prototype = new $h_sc_AbstractIterator();
$c_sr_ScalaRunTime$$anon$1.prototype.constructor = $c_sr_ScalaRunTime$$anon$1;
/** @constructor */
function $h_sr_ScalaRunTime$$anon$1() {
}
$h_sr_ScalaRunTime$$anon$1.prototype = $c_sr_ScalaRunTime$$anon$1.prototype;
$c_sr_ScalaRunTime$$anon$1.prototype.hasNext__Z = (function() {
  return (this.sr_ScalaRunTime$$anon$1__f_c < this.sr_ScalaRunTime$$anon$1__f_cmax);
});
$c_sr_ScalaRunTime$$anon$1.prototype.next__O = (function() {
  var result = $n(this.sr_ScalaRunTime$$anon$1__f_x$2).productElement__I__O(this.sr_ScalaRunTime$$anon$1__f_c);
  this.sr_ScalaRunTime$$anon$1__f_c = ((1 + this.sr_ScalaRunTime$$anon$1__f_c) | 0);
  return result;
});
var $d_sr_ScalaRunTime$$anon$1 = new $TypeData().initClass($c_sr_ScalaRunTime$$anon$1, "scala.runtime.ScalaRunTime$$anon$1", ({
  sr_ScalaRunTime$$anon$1: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1
}));
/** @constructor */
function $c_sjs_js_WrappedArray$() {
}
$c_sjs_js_WrappedArray$.prototype = new $h_O();
$c_sjs_js_WrappedArray$.prototype.constructor = $c_sjs_js_WrappedArray$;
/** @constructor */
function $h_sjs_js_WrappedArray$() {
}
$h_sjs_js_WrappedArray$.prototype = $c_sjs_js_WrappedArray$.prototype;
$c_sjs_js_WrappedArray$.prototype.apply__sci_Seq__O = (function(elems) {
  return this.from__sc_IterableOnce__sjs_js_WrappedArray(elems);
});
$c_sjs_js_WrappedArray$.prototype.from__sc_IterableOnce__sjs_js_WrappedArray = (function(source) {
  var this$1 = $ct_sjs_js_WrappedArray__(new $c_sjs_js_WrappedArray());
  return $as_sjs_js_WrappedArray($n($as_scm_Builder($f_scm_Growable__addAll__sc_IterableOnce__scm_Growable(this$1, source))).result__O());
});
$c_sjs_js_WrappedArray$.prototype.from__sc_IterableOnce__O = (function(source) {
  return this.from__sc_IterableOnce__sjs_js_WrappedArray(source);
});
var $d_sjs_js_WrappedArray$ = new $TypeData().initClass($c_sjs_js_WrappedArray$, "scala.scalajs.js.WrappedArray$", ({
  sjs_js_WrappedArray$: 1,
  sc_StrictOptimizedSeqFactory: 1,
  sc_SeqFactory: 1,
  sc_IterableFactory: 1,
  Ljava_io_Serializable: 1
}));
var $n_sjs_js_WrappedArray$;
function $m_sjs_js_WrappedArray$() {
  if ((!$n_sjs_js_WrappedArray$)) {
    $n_sjs_js_WrappedArray$ = new $c_sjs_js_WrappedArray$();
  }
  return $n_sjs_js_WrappedArray$;
}
class $c_Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded extends $c_Lcom_raquo_airstream_core_AirstreamError {
  constructor(trx, depth) {
    super();
    this.Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded__f_trx = null;
    this.Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded__f_depth = 0;
    this.Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded__f_trx = trx;
    this.Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded__f_depth = depth;
    var message = (((("Transaction depth exceeded maxDepth = " + depth) + ": Execution of ") + trx) + " aborted. See `Transaction.maxDepth`.");
    $ct_jl_Throwable__T__jl_Throwable__Z__Z__(this, message, null, true, true);
  }
  productIterator__sc_Iterator() {
    return new $c_s_Product$$anon$1(this);
  }
  hashCode__I() {
    var acc = (-889275714);
    var hash = acc;
    var data = $f_T__hashCode__I("TransactionDepthExceeded");
    acc = $m_sr_Statics$().mix__I__I__I(hash, data);
    var hash$1 = acc;
    var x = this.Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded__f_trx;
    var data$1 = $m_sr_Statics$().anyHash__O__I(x);
    acc = $m_sr_Statics$().mix__I__I__I(hash$1, data$1);
    var hash$2 = acc;
    var data$2 = this.Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded__f_depth;
    acc = $m_sr_Statics$().mix__I__I__I(hash$2, data$2);
    var hash$3 = acc;
    return $m_sr_Statics$().finalizeHash__I__I__I(hash$3, 2);
  }
  equals__O__Z(x$0) {
    if ((this === x$0)) {
      return true;
    } else if ((x$0 instanceof $c_Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded)) {
      var x$0$2 = $as_Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded(x$0);
      if ((this.Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded__f_depth === $n(x$0$2).Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded__f_depth)) {
        var x = this.Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded__f_trx;
        var x$2 = $n(x$0$2).Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded__f_trx;
        if ((x === null)) {
          var $x_1 = (x$2 === null);
        } else {
          var this$1 = $n(x);
          var $x_1 = (this$1 === x$2);
        }
      } else {
        var $x_1 = false;
      }
      if ($x_1) {
        $n(x$0$2);
        return true;
      } else {
        return false;
      }
    } else {
      return false;
    }
  }
  productArity__I() {
    return 2;
  }
  productPrefix__T() {
    return "TransactionDepthExceeded";
  }
  productElement__I__O(n) {
    if ((n === 0)) {
      return this.Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded__f_trx;
    }
    if ((n === 1)) {
      return this.Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded__f_depth;
    }
    throw $ct_jl_IndexOutOfBoundsException__T__(new $c_jl_IndexOutOfBoundsException(), ("" + n));
  }
  toString__T() {
    return ((("TransactionDepthExceeded: " + this.Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded__f_trx) + "; maxDepth: ") + this.Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded__f_depth);
  }
}
function $as_Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded(obj) {
  return (((obj instanceof $c_Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded) || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.airstream.core.AirstreamError$TransactionDepthExceeded"));
}
function $isArrayOf_Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded)));
}
function $asArrayOf_Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded(obj, depth) {
  return (($isArrayOf_Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.airstream.core.AirstreamError$TransactionDepthExceeded;", depth));
}
var $d_Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded = new $TypeData().initClass($c_Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded, "com.raquo.airstream.core.AirstreamError$TransactionDepthExceeded", ({
  Lcom_raquo_airstream_core_AirstreamError$TransactionDepthExceeded: 1,
  Lcom_raquo_airstream_core_AirstreamError: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1,
  s_Product: 1
}));
/** @constructor */
function $c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement(tag, ref) {
  this.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_com$raquo$laminar$nodes$ChildNode$$_maybeParent = null;
  this.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_dynamicOwner = null;
  this.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_com$raquo$laminar$nodes$ReactiveElement$$pilotSubscription = null;
  this.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_tag = null;
  this.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_ref = null;
  this.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_tag = tag;
  this.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_ref = ref;
  this.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_com$raquo$laminar$nodes$ChildNode$$_maybeParent = $m_s_None$();
  $f_Lcom_raquo_laminar_nodes_ParentNode__$init$__V(this);
  $f_Lcom_raquo_laminar_nodes_ReactiveElement__$init$__V(this);
}
$c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement.prototype = new $h_O();
$c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement.prototype.constructor = $c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement;
/** @constructor */
function $h_Lcom_raquo_laminar_nodes_ReactiveHtmlElement() {
}
$h_Lcom_raquo_laminar_nodes_ReactiveHtmlElement.prototype = $c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement.prototype;
$c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement.prototype.apply__Lcom_raquo_laminar_nodes_ReactiveElement__V = (function(parentNode) {
  $m_Lcom_raquo_laminar_nodes_ParentNode$().appendChild__Lcom_raquo_laminar_nodes_ParentNode__Lcom_raquo_laminar_nodes_ChildNode__O__Z(parentNode, this, (void 0));
});
$c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement.prototype.dynamicOwner__Lcom_raquo_airstream_ownership_DynamicOwner = (function() {
  return this.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_dynamicOwner;
});
$c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement.prototype.com$raquo$laminar$nodes$ParentNode$_setter_$dynamicOwner_$eq__Lcom_raquo_airstream_ownership_DynamicOwner__V = (function(x$0) {
  this.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_dynamicOwner = x$0;
});
$c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement.prototype.willSetParent__s_Option__V = (function(maybeNextParent) {
  $f_Lcom_raquo_laminar_nodes_ReactiveElement__willSetParent__s_Option__V(this, maybeNextParent);
});
$c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement.prototype.setParent__s_Option__V = (function(maybeNextParent) {
  $f_Lcom_raquo_laminar_nodes_ReactiveElement__setParent__s_Option__V(this, maybeNextParent);
});
$c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement.prototype.toString__T = (function() {
  return (("ReactiveHtmlElement(" + ((this.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_ref !== null) ? $as_T(this.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_ref.outerHTML) : ("tag=" + $n(this.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_tag).Lcom_raquo_laminar_tags_HtmlTag__f_name))) + ")");
});
$c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement.prototype.ref__Lorg_scalajs_dom_Node = (function() {
  return this.Lcom_raquo_laminar_nodes_ReactiveHtmlElement__f_ref;
});
function $as_Lcom_raquo_laminar_nodes_ReactiveHtmlElement(obj) {
  return (((obj instanceof $c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement) || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.laminar.nodes.ReactiveHtmlElement"));
}
function $isArrayOf_Lcom_raquo_laminar_nodes_ReactiveHtmlElement(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_laminar_nodes_ReactiveHtmlElement)));
}
function $asArrayOf_Lcom_raquo_laminar_nodes_ReactiveHtmlElement(obj, depth) {
  return (($isArrayOf_Lcom_raquo_laminar_nodes_ReactiveHtmlElement(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.laminar.nodes.ReactiveHtmlElement;", depth));
}
var $d_Lcom_raquo_laminar_nodes_ReactiveHtmlElement = new $TypeData().initClass($c_Lcom_raquo_laminar_nodes_ReactiveHtmlElement, "com.raquo.laminar.nodes.ReactiveHtmlElement", ({
  Lcom_raquo_laminar_nodes_ReactiveHtmlElement: 1,
  Lcom_raquo_laminar_nodes_ReactiveNode: 1,
  Lcom_raquo_laminar_modifiers_Modifier: 1,
  Lcom_raquo_laminar_nodes_ChildNode: 1,
  Lcom_raquo_laminar_nodes_ParentNode: 1,
  Lcom_raquo_laminar_nodes_ReactiveElement: 1
}));
function $as_Lcom_raquo_laminar_nodes_ReactiveSvgElement(obj) {
  return ((false || (obj === null)) ? obj : $throwClassCastException(obj, "com.raquo.laminar.nodes.ReactiveSvgElement"));
}
function $isArrayOf_Lcom_raquo_laminar_nodes_ReactiveSvgElement(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Lcom_raquo_laminar_nodes_ReactiveSvgElement)));
}
function $asArrayOf_Lcom_raquo_laminar_nodes_ReactiveSvgElement(obj, depth) {
  return (($isArrayOf_Lcom_raquo_laminar_nodes_ReactiveSvgElement(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lcom.raquo.laminar.nodes.ReactiveSvgElement;", depth));
}
class $c_jl_ArrayIndexOutOfBoundsException extends $c_jl_IndexOutOfBoundsException {
  constructor(s) {
    super();
    $ct_jl_Throwable__T__jl_Throwable__Z__Z__(this, s, null, true, true);
  }
}
var $d_jl_ArrayIndexOutOfBoundsException = new $TypeData().initClass($c_jl_ArrayIndexOutOfBoundsException, "java.lang.ArrayIndexOutOfBoundsException", ({
  jl_ArrayIndexOutOfBoundsException: 1,
  jl_IndexOutOfBoundsException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
function $f_jl_Double__equals__O__Z($thiz, that) {
  return Object.is($thiz, that);
}
function $f_jl_Double__hashCode__I($thiz) {
  return $m_jl_FloatingPointBits$().numberHashCode__D__I($thiz);
}
function $f_jl_Double__toString__T($thiz) {
  return ("" + $thiz);
}
function $as_jl_Double(obj) {
  return ((((typeof obj) === "number") || (obj === null)) ? obj : $throwClassCastException(obj, "java.lang.Double"));
}
function $isArrayOf_jl_Double(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Double)));
}
function $asArrayOf_jl_Double(obj, depth) {
  return (($isArrayOf_jl_Double(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Double;", depth));
}
var $d_jl_Double = new $TypeData().initClass(0, "java.lang.Double", ({
  jl_Double: 1,
  jl_Number: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1,
  jl_constant_Constable: 1,
  jl_constant_ConstantDesc: 1
}), ((x) => ((typeof x) === "number")));
function $f_jl_Float__equals__O__Z($thiz, that) {
  return Object.is($thiz, that);
}
function $f_jl_Float__hashCode__I($thiz) {
  var value = $thiz;
  return $m_jl_FloatingPointBits$().numberHashCode__D__I(value);
}
function $f_jl_Float__toString__T($thiz) {
  return ("" + $thiz);
}
var $d_jl_Float = new $TypeData().initClass(0, "java.lang.Float", ({
  jl_Float: 1,
  jl_Number: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1,
  jl_constant_Constable: 1,
  jl_constant_ConstantDesc: 1
}), ((x) => $isFloat(x)));
function $f_jl_Integer__equals__O__Z($thiz, that) {
  return Object.is($thiz, that);
}
function $f_jl_Integer__hashCode__I($thiz) {
  return $thiz;
}
function $f_jl_Integer__toString__T($thiz) {
  return ("" + $thiz);
}
var $d_jl_Integer = new $TypeData().initClass(0, "java.lang.Integer", ({
  jl_Integer: 1,
  jl_Number: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1,
  jl_constant_Constable: 1,
  jl_constant_ConstantDesc: 1
}), ((x) => $isInt(x)));
function $f_jl_Long__equals__O__Z($thiz, that) {
  if ((that instanceof $c_RTLong)) {
    var x2 = $as_jl_Long(that);
    var this$1 = $n(x2);
    var b = $uJ(this$1);
    return (($thiz.RTLong__f_lo === b.RTLong__f_lo) && ($thiz.RTLong__f_hi === b.RTLong__f_hi));
  } else {
    return false;
  }
}
function $f_jl_Long__hashCode__I($thiz) {
  var $x_1 = $thiz.RTLong__f_lo;
  var hi = $thiz.RTLong__f_hi;
  return ($x_1 ^ hi);
}
function $f_jl_Long__toString__T($thiz) {
  return $m_RTLong$().org$scalajs$linker$runtime$RuntimeLong$$toString__I__I__T($thiz.RTLong__f_lo, $thiz.RTLong__f_hi);
}
function $as_jl_Long(obj) {
  return (((obj instanceof $c_RTLong) || (obj === null)) ? obj : $throwClassCastException(obj, "java.lang.Long"));
}
function $isArrayOf_jl_Long(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Long)));
}
function $asArrayOf_jl_Long(obj, depth) {
  return (($isArrayOf_jl_Long(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Long;", depth));
}
var $d_jl_Long = new $TypeData().initClass(0, "java.lang.Long", ({
  jl_Long: 1,
  jl_Number: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1,
  jl_constant_Constable: 1,
  jl_constant_ConstantDesc: 1
}), ((x) => (x instanceof $c_RTLong)));
function $f_T__hashCode__I($thiz) {
  var res = 0;
  var mul = 1;
  var i = (((-1) + $thiz.length) | 0);
  while ((i >= 0)) {
    var $x_1 = res;
    var index = i;
    res = (($x_1 + Math.imul($charAt($thiz, index), mul)) | 0);
    mul = Math.imul(31, mul);
    i = (((-1) + i) | 0);
  }
  return res;
}
function $f_T__equals__O__Z($thiz, that) {
  return ($thiz === that);
}
function $f_T__toString__T($thiz) {
  return $thiz;
}
function $as_T(obj) {
  return ((((typeof obj) === "string") || (obj === null)) ? obj : $throwClassCastException(obj, "java.lang.String"));
}
function $isArrayOf_T(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.T)));
}
function $asArrayOf_T(obj, depth) {
  return (($isArrayOf_T(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.String;", depth));
}
var $d_T = new $TypeData().initClass(0, "java.lang.String", ({
  T: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1,
  jl_CharSequence: 1,
  jl_constant_Constable: 1,
  jl_constant_ConstantDesc: 1
}), ((x) => ((typeof x) === "string")));
function $ct_jl_StringIndexOutOfBoundsException__I__($thiz, index) {
  var s = ("String index out of range: " + index);
  $ct_jl_Throwable__T__jl_Throwable__Z__Z__($thiz, s, null, true, true);
  return $thiz;
}
function $ct_jl_StringIndexOutOfBoundsException__($thiz) {
  $ct_jl_Throwable__T__jl_Throwable__Z__Z__($thiz, null, null, true, true);
  return $thiz;
}
class $c_jl_StringIndexOutOfBoundsException extends $c_jl_IndexOutOfBoundsException {
}
var $d_jl_StringIndexOutOfBoundsException = new $TypeData().initClass($c_jl_StringIndexOutOfBoundsException, "java.lang.StringIndexOutOfBoundsException", ({
  jl_StringIndexOutOfBoundsException: 1,
  jl_IndexOutOfBoundsException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_s_None$() {
}
$c_s_None$.prototype = new $h_s_Option();
$c_s_None$.prototype.constructor = $c_s_None$;
/** @constructor */
function $h_s_None$() {
}
$h_s_None$.prototype = $c_s_None$.prototype;
$c_s_None$.prototype.get__E = (function() {
  throw new $c_ju_NoSuchElementException("None.get");
});
$c_s_None$.prototype.productPrefix__T = (function() {
  return "None";
});
$c_s_None$.prototype.productArity__I = (function() {
  return 0;
});
$c_s_None$.prototype.productElement__I__O = (function(x$1) {
  return $m_sr_Statics$().ioobe__I__O(x$1);
});
$c_s_None$.prototype.productIterator__sc_Iterator = (function() {
  return new $c_sr_ScalaRunTime$$anon$1(this);
});
$c_s_None$.prototype.hashCode__I = (function() {
  return 2433880;
});
$c_s_None$.prototype.toString__T = (function() {
  return "None";
});
$c_s_None$.prototype.get__O = (function() {
  this.get__E();
});
var $d_s_None$ = new $TypeData().initClass($c_s_None$, "scala.None$", ({
  s_None$: 1,
  s_Option: 1,
  sc_IterableOnce: 1,
  s_Product: 1,
  s_Equals: 1,
  Ljava_io_Serializable: 1
}));
var $n_s_None$;
function $m_s_None$() {
  if ((!$n_s_None$)) {
    $n_s_None$ = new $c_s_None$();
  }
  return $n_s_None$;
}
/** @constructor */
function $c_s_Some(value) {
  this.s_Some__f_value = null;
  this.s_Some__f_value = value;
}
$c_s_Some.prototype = new $h_s_Option();
$c_s_Some.prototype.constructor = $c_s_Some;
/** @constructor */
function $h_s_Some() {
}
$h_s_Some.prototype = $c_s_Some.prototype;
$c_s_Some.prototype.get__O = (function() {
  return this.s_Some__f_value;
});
$c_s_Some.prototype.productPrefix__T = (function() {
  return "Some";
});
$c_s_Some.prototype.productArity__I = (function() {
  return 1;
});
$c_s_Some.prototype.productElement__I__O = (function(x$1) {
  return ((x$1 === 0) ? this.s_Some__f_value : $m_sr_Statics$().ioobe__I__O(x$1));
});
$c_s_Some.prototype.productIterator__sc_Iterator = (function() {
  return new $c_sr_ScalaRunTime$$anon$1(this);
});
$c_s_Some.prototype.hashCode__I = (function() {
  var this$2 = $m_s_util_hashing_MurmurHash3$();
  return this$2.productHash__s_Product__I__Z__I(this, (-889275714), false);
});
$c_s_Some.prototype.toString__T = (function() {
  return $m_sr_ScalaRunTime$()._toString__s_Product__T(this);
});
$c_s_Some.prototype.equals__O__Z = (function(x$1) {
  if ((this === x$1)) {
    return true;
  } else if ((x$1 instanceof $c_s_Some)) {
    var Some$1 = $as_s_Some(x$1);
    return $m_sr_BoxesRunTime$().equals__O__O__Z(this.s_Some__f_value, $n(Some$1).s_Some__f_value);
  } else {
    return false;
  }
});
function $as_s_Some(obj) {
  return (((obj instanceof $c_s_Some) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.Some"));
}
function $isArrayOf_s_Some(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.s_Some)));
}
function $asArrayOf_s_Some(obj, depth) {
  return (($isArrayOf_s_Some(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.Some;", depth));
}
var $d_s_Some = new $TypeData().initClass($c_s_Some, "scala.Some", ({
  s_Some: 1,
  s_Option: 1,
  sc_IterableOnce: 1,
  s_Product: 1,
  s_Equals: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sc_AbstractIterable() {
}
$c_sc_AbstractIterable.prototype = new $h_O();
$c_sc_AbstractIterable.prototype.constructor = $c_sc_AbstractIterable;
/** @constructor */
function $h_sc_AbstractIterable() {
}
$h_sc_AbstractIterable.prototype = $c_sc_AbstractIterable.prototype;
$c_sc_AbstractIterable.prototype.className__T = (function() {
  return this.stringPrefix__T();
});
$c_sc_AbstractIterable.prototype.foreach__F1__V = (function(f) {
  $f_sc_IterableOnceOps__foreach__F1__V(this, f);
});
$c_sc_AbstractIterable.prototype.addString__scm_StringBuilder__T__T__T__scm_StringBuilder = (function(b, start, sep, end) {
  return $f_sc_IterableOnceOps__addString__scm_StringBuilder__T__T__T__scm_StringBuilder(this, b, start, sep, end);
});
$c_sc_AbstractIterable.prototype.knownSize__I = (function() {
  return (-1);
});
function $ct_sc_ArrayOps$ArrayIterator__O__($thiz, xs) {
  $thiz.sc_ArrayOps$ArrayIterator__f_xs = xs;
  $thiz.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = 0;
  var xs$1 = $thiz.sc_ArrayOps$ArrayIterator__f_xs;
  $thiz.sc_ArrayOps$ArrayIterator__f_len = $m_jl_reflect_Array$().getLength__O__I(xs$1);
  return $thiz;
}
/** @constructor */
function $c_sc_ArrayOps$ArrayIterator() {
  this.sc_ArrayOps$ArrayIterator__f_xs = null;
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = 0;
  this.sc_ArrayOps$ArrayIterator__f_len = 0;
}
$c_sc_ArrayOps$ArrayIterator.prototype = new $h_sc_AbstractIterator();
$c_sc_ArrayOps$ArrayIterator.prototype.constructor = $c_sc_ArrayOps$ArrayIterator;
/** @constructor */
function $h_sc_ArrayOps$ArrayIterator() {
}
$h_sc_ArrayOps$ArrayIterator.prototype = $c_sc_ArrayOps$ArrayIterator.prototype;
$c_sc_ArrayOps$ArrayIterator.prototype.knownSize__I = (function() {
  return ((this.sc_ArrayOps$ArrayIterator__f_len - this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos) | 0);
});
$c_sc_ArrayOps$ArrayIterator.prototype.hasNext__Z = (function() {
  return (this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos < this.sc_ArrayOps$ArrayIterator__f_len);
});
$c_sc_ArrayOps$ArrayIterator.prototype.next__O = (function() {
  var $x_1 = this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos;
  var xs = this.sc_ArrayOps$ArrayIterator__f_xs;
  if (($x_1 >= $m_jl_reflect_Array$().getLength__O__I(xs))) {
    $n($m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty).next__O();
  }
  var r = $m_sr_ScalaRunTime$().array_apply__O__I__O(this.sc_ArrayOps$ArrayIterator__f_xs, this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos);
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = ((1 + this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos) | 0);
  return r;
});
$c_sc_ArrayOps$ArrayIterator.prototype.drop__I__sc_Iterator = (function(n) {
  if ((n > 0)) {
    var newPos = ((this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos + n) | 0);
    if ((newPos < 0)) {
      var $x_1 = this.sc_ArrayOps$ArrayIterator__f_len;
    } else {
      var a = this.sc_ArrayOps$ArrayIterator__f_len;
      var $x_1 = ((a < newPos) ? a : newPos);
    }
    this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = $x_1;
  }
  return this;
});
var $d_sc_ArrayOps$ArrayIterator = new $TypeData().initClass($c_sc_ArrayOps$ArrayIterator, "scala.collection.ArrayOps$ArrayIterator", ({
  sc_ArrayOps$ArrayIterator: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1,
  Ljava_io_Serializable: 1
}));
function $p_sc_IndexedSeqView$IndexedSeqViewIterator__formatRange$1__I__I($thiz, value) {
  return ((value < 0) ? 0 : ((value > $thiz.sc_IndexedSeqView$IndexedSeqViewIterator__f_scala$collection$IndexedSeqView$IndexedSeqViewIterator$$remainder) ? $thiz.sc_IndexedSeqView$IndexedSeqViewIterator__f_scala$collection$IndexedSeqView$IndexedSeqViewIterator$$remainder : value));
}
/** @constructor */
function $c_sc_IndexedSeqView$IndexedSeqViewIterator(self) {
  this.sc_IndexedSeqView$IndexedSeqViewIterator__f_self = null;
  this.sc_IndexedSeqView$IndexedSeqViewIterator__f_current = 0;
  this.sc_IndexedSeqView$IndexedSeqViewIterator__f_scala$collection$IndexedSeqView$IndexedSeqViewIterator$$remainder = 0;
  this.sc_IndexedSeqView$IndexedSeqViewIterator__f_self = self;
  this.sc_IndexedSeqView$IndexedSeqViewIterator__f_current = 0;
  this.sc_IndexedSeqView$IndexedSeqViewIterator__f_scala$collection$IndexedSeqView$IndexedSeqViewIterator$$remainder = $n(self).length__I();
}
$c_sc_IndexedSeqView$IndexedSeqViewIterator.prototype = new $h_sc_AbstractIterator();
$c_sc_IndexedSeqView$IndexedSeqViewIterator.prototype.constructor = $c_sc_IndexedSeqView$IndexedSeqViewIterator;
/** @constructor */
function $h_sc_IndexedSeqView$IndexedSeqViewIterator() {
}
$h_sc_IndexedSeqView$IndexedSeqViewIterator.prototype = $c_sc_IndexedSeqView$IndexedSeqViewIterator.prototype;
$c_sc_IndexedSeqView$IndexedSeqViewIterator.prototype.knownSize__I = (function() {
  return this.sc_IndexedSeqView$IndexedSeqViewIterator__f_scala$collection$IndexedSeqView$IndexedSeqViewIterator$$remainder;
});
$c_sc_IndexedSeqView$IndexedSeqViewIterator.prototype.hasNext__Z = (function() {
  return (this.sc_IndexedSeqView$IndexedSeqViewIterator__f_scala$collection$IndexedSeqView$IndexedSeqViewIterator$$remainder > 0);
});
$c_sc_IndexedSeqView$IndexedSeqViewIterator.prototype.next__O = (function() {
  if ((this.sc_IndexedSeqView$IndexedSeqViewIterator__f_scala$collection$IndexedSeqView$IndexedSeqViewIterator$$remainder > 0)) {
    var r = $n(this.sc_IndexedSeqView$IndexedSeqViewIterator__f_self).apply__I__O(this.sc_IndexedSeqView$IndexedSeqViewIterator__f_current);
    this.sc_IndexedSeqView$IndexedSeqViewIterator__f_current = ((1 + this.sc_IndexedSeqView$IndexedSeqViewIterator__f_current) | 0);
    this.sc_IndexedSeqView$IndexedSeqViewIterator__f_scala$collection$IndexedSeqView$IndexedSeqViewIterator$$remainder = (((-1) + this.sc_IndexedSeqView$IndexedSeqViewIterator__f_scala$collection$IndexedSeqView$IndexedSeqViewIterator$$remainder) | 0);
    return r;
  } else {
    return $n($m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty).next__O();
  }
});
$c_sc_IndexedSeqView$IndexedSeqViewIterator.prototype.drop__I__sc_Iterator = (function(n) {
  if ((n > 0)) {
    this.sc_IndexedSeqView$IndexedSeqViewIterator__f_current = ((this.sc_IndexedSeqView$IndexedSeqViewIterator__f_current + n) | 0);
    var b = ((this.sc_IndexedSeqView$IndexedSeqViewIterator__f_scala$collection$IndexedSeqView$IndexedSeqViewIterator$$remainder - n) | 0);
    this.sc_IndexedSeqView$IndexedSeqViewIterator__f_scala$collection$IndexedSeqView$IndexedSeqViewIterator$$remainder = ((b < 0) ? 0 : b);
  }
  return this;
});
$c_sc_IndexedSeqView$IndexedSeqViewIterator.prototype.sliceIterator__I__I__sc_Iterator = (function(from, until) {
  var formatFrom = $p_sc_IndexedSeqView$IndexedSeqViewIterator__formatRange$1__I__I(this, from);
  var formatUntil = $p_sc_IndexedSeqView$IndexedSeqViewIterator__formatRange$1__I__I(this, until);
  var b = ((formatUntil - formatFrom) | 0);
  this.sc_IndexedSeqView$IndexedSeqViewIterator__f_scala$collection$IndexedSeqView$IndexedSeqViewIterator$$remainder = ((b < 0) ? 0 : b);
  this.sc_IndexedSeqView$IndexedSeqViewIterator__f_current = ((this.sc_IndexedSeqView$IndexedSeqViewIterator__f_current + formatFrom) | 0);
  return this;
});
var $d_sc_IndexedSeqView$IndexedSeqViewIterator = new $TypeData().initClass($c_sc_IndexedSeqView$IndexedSeqViewIterator, "scala.collection.IndexedSeqView$IndexedSeqViewIterator", ({
  sc_IndexedSeqView$IndexedSeqViewIterator: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1,
  Ljava_io_Serializable: 1
}));
function $p_sci_ArraySeq$__emptyImpl$lzycompute__sci_ArraySeq$ofRef($thiz) {
  if ((!$thiz.sci_ArraySeq$__f_bitmap$0)) {
    $thiz.sci_ArraySeq$__f_emptyImpl = new $c_sci_ArraySeq$ofRef(new $ac_O(0));
    $thiz.sci_ArraySeq$__f_bitmap$0 = true;
  }
  return $thiz.sci_ArraySeq$__f_emptyImpl;
}
function $p_sci_ArraySeq$__emptyImpl__sci_ArraySeq$ofRef($thiz) {
  return ((!$thiz.sci_ArraySeq$__f_bitmap$0) ? $p_sci_ArraySeq$__emptyImpl$lzycompute__sci_ArraySeq$ofRef($thiz) : $thiz.sci_ArraySeq$__f_emptyImpl);
}
/** @constructor */
function $c_sci_ArraySeq$() {
  this.sci_ArraySeq$__f_emptyImpl = null;
  this.sci_ArraySeq$__f_untagged = null;
  this.sci_ArraySeq$__f_bitmap$0 = false;
  $n_sci_ArraySeq$ = this;
  this.sci_ArraySeq$__f_untagged = new $c_sc_ClassTagSeqFactory$AnySeqDelegate(this);
}
$c_sci_ArraySeq$.prototype = new $h_O();
$c_sci_ArraySeq$.prototype.constructor = $c_sci_ArraySeq$;
/** @constructor */
function $h_sci_ArraySeq$() {
}
$h_sci_ArraySeq$.prototype = $c_sci_ArraySeq$.prototype;
$c_sci_ArraySeq$.prototype.from__sc_IterableOnce__s_reflect_ClassTag__sci_ArraySeq = (function(it, tag) {
  if ((it instanceof $c_sci_ArraySeq)) {
    var x2 = $as_sci_ArraySeq(it);
    return x2;
  } else {
    return this.unsafeWrapArray__O__sci_ArraySeq($m_s_Array$().from__sc_IterableOnce__s_reflect_ClassTag__O(it, tag));
  }
});
$c_sci_ArraySeq$.prototype.unsafeWrapArray__O__sci_ArraySeq = (function(x) {
  if ((x === null)) {
    return null;
  } else if ((x instanceof $ac_O)) {
    var x3 = $asArrayOf_O(x, 1);
    return new $c_sci_ArraySeq$ofRef(x3);
  } else if ((x instanceof $ac_I)) {
    var x4 = $asArrayOf_I(x, 1);
    return new $c_sci_ArraySeq$ofInt(x4);
  } else if ((x instanceof $ac_D)) {
    var x5 = $asArrayOf_D(x, 1);
    return new $c_sci_ArraySeq$ofDouble(x5);
  } else if ((x instanceof $ac_J)) {
    var x6 = $asArrayOf_J(x, 1);
    return new $c_sci_ArraySeq$ofLong(x6);
  } else if ((x instanceof $ac_F)) {
    var x7 = $asArrayOf_F(x, 1);
    return new $c_sci_ArraySeq$ofFloat(x7);
  } else if ((x instanceof $ac_C)) {
    var x8 = $asArrayOf_C(x, 1);
    return new $c_sci_ArraySeq$ofChar(x8);
  } else if ((x instanceof $ac_B)) {
    var x9 = $asArrayOf_B(x, 1);
    return new $c_sci_ArraySeq$ofByte(x9);
  } else if ((x instanceof $ac_S)) {
    var x10 = $asArrayOf_S(x, 1);
    return new $c_sci_ArraySeq$ofShort(x10);
  } else if ((x instanceof $ac_Z)) {
    var x11 = $asArrayOf_Z(x, 1);
    return new $c_sci_ArraySeq$ofBoolean(x11);
  } else if ($isArrayOf_jl_Void(x, 1)) {
    var x12 = $asArrayOf_jl_Void(x, 1);
    return new $c_sci_ArraySeq$ofUnit(x12);
  } else {
    throw new $c_s_MatchError(x);
  }
});
$c_sci_ArraySeq$.prototype.from__sc_IterableOnce__O__O = (function(it, evidence$5) {
  return this.from__sc_IterableOnce__s_reflect_ClassTag__sci_ArraySeq(it, $as_s_reflect_ClassTag(evidence$5));
});
var $d_sci_ArraySeq$ = new $TypeData().initClass($c_sci_ArraySeq$, "scala.collection.immutable.ArraySeq$", ({
  sci_ArraySeq$: 1,
  sc_StrictOptimizedClassTagSeqFactory: 1,
  sc_ClassTagSeqFactory: 1,
  sc_ClassTagIterableFactory: 1,
  sc_EvidenceIterableFactory: 1,
  Ljava_io_Serializable: 1
}));
var $n_sci_ArraySeq$;
function $m_sci_ArraySeq$() {
  if ((!$n_sci_ArraySeq$)) {
    $n_sci_ArraySeq$ = new $c_sci_ArraySeq$();
  }
  return $n_sci_ArraySeq$;
}
/** @constructor */
function $c_scm_ArraySeq$() {
  this.scm_ArraySeq$__f_untagged = null;
  this.scm_ArraySeq$__f_EmptyArraySeq = null;
  $n_scm_ArraySeq$ = this;
  this.scm_ArraySeq$__f_untagged = new $c_sc_ClassTagSeqFactory$AnySeqDelegate(this);
  this.scm_ArraySeq$__f_EmptyArraySeq = new $c_scm_ArraySeq$ofRef(new $ac_O(0));
}
$c_scm_ArraySeq$.prototype = new $h_O();
$c_scm_ArraySeq$.prototype.constructor = $c_scm_ArraySeq$;
/** @constructor */
function $h_scm_ArraySeq$() {
}
$h_scm_ArraySeq$.prototype = $c_scm_ArraySeq$.prototype;
$c_scm_ArraySeq$.prototype.from__sc_IterableOnce__s_reflect_ClassTag__scm_ArraySeq = (function(it, evidence$2) {
  return this.make__O__scm_ArraySeq($m_s_Array$().from__sc_IterableOnce__s_reflect_ClassTag__O(it, evidence$2));
});
$c_scm_ArraySeq$.prototype.make__O__scm_ArraySeq = (function(x) {
  if ((x === null)) {
    return null;
  } else if ((x instanceof $ac_O)) {
    var x3 = $asArrayOf_O(x, 1);
    return new $c_scm_ArraySeq$ofRef(x3);
  } else if ((x instanceof $ac_I)) {
    var x4 = $asArrayOf_I(x, 1);
    return new $c_scm_ArraySeq$ofInt(x4);
  } else if ((x instanceof $ac_D)) {
    var x5 = $asArrayOf_D(x, 1);
    return new $c_scm_ArraySeq$ofDouble(x5);
  } else if ((x instanceof $ac_J)) {
    var x6 = $asArrayOf_J(x, 1);
    return new $c_scm_ArraySeq$ofLong(x6);
  } else if ((x instanceof $ac_F)) {
    var x7 = $asArrayOf_F(x, 1);
    return new $c_scm_ArraySeq$ofFloat(x7);
  } else if ((x instanceof $ac_C)) {
    var x8 = $asArrayOf_C(x, 1);
    return new $c_scm_ArraySeq$ofChar(x8);
  } else if ((x instanceof $ac_B)) {
    var x9 = $asArrayOf_B(x, 1);
    return new $c_scm_ArraySeq$ofByte(x9);
  } else if ((x instanceof $ac_S)) {
    var x10 = $asArrayOf_S(x, 1);
    return new $c_scm_ArraySeq$ofShort(x10);
  } else if ((x instanceof $ac_Z)) {
    var x11 = $asArrayOf_Z(x, 1);
    return new $c_scm_ArraySeq$ofBoolean(x11);
  } else if ($isArrayOf_jl_Void(x, 1)) {
    var x12 = $asArrayOf_jl_Void(x, 1);
    return new $c_scm_ArraySeq$ofUnit(x12);
  } else {
    throw new $c_s_MatchError(x);
  }
});
$c_scm_ArraySeq$.prototype.from__sc_IterableOnce__O__O = (function(it, evidence$5) {
  return this.from__sc_IterableOnce__s_reflect_ClassTag__scm_ArraySeq(it, $as_s_reflect_ClassTag(evidence$5));
});
var $d_scm_ArraySeq$ = new $TypeData().initClass($c_scm_ArraySeq$, "scala.collection.mutable.ArraySeq$", ({
  scm_ArraySeq$: 1,
  sc_StrictOptimizedClassTagSeqFactory: 1,
  sc_ClassTagSeqFactory: 1,
  sc_ClassTagIterableFactory: 1,
  sc_EvidenceIterableFactory: 1,
  Ljava_io_Serializable: 1
}));
var $n_scm_ArraySeq$;
function $m_scm_ArraySeq$() {
  if ((!$n_scm_ArraySeq$)) {
    $n_scm_ArraySeq$ = new $c_scm_ArraySeq$();
  }
  return $n_scm_ArraySeq$;
}
/** @constructor */
function $c_sc_ArrayOps$ArrayIterator$mcB$sp(xs$mcB$sp) {
  this.sc_ArrayOps$ArrayIterator__f_xs = null;
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = 0;
  this.sc_ArrayOps$ArrayIterator__f_len = 0;
  this.sc_ArrayOps$ArrayIterator$mcB$sp__f_xs$mcB$sp = null;
  this.sc_ArrayOps$ArrayIterator$mcB$sp__f_xs$mcB$sp = xs$mcB$sp;
  $ct_sc_ArrayOps$ArrayIterator__O__(this, xs$mcB$sp);
}
$c_sc_ArrayOps$ArrayIterator$mcB$sp.prototype = new $h_sc_ArrayOps$ArrayIterator();
$c_sc_ArrayOps$ArrayIterator$mcB$sp.prototype.constructor = $c_sc_ArrayOps$ArrayIterator$mcB$sp;
/** @constructor */
function $h_sc_ArrayOps$ArrayIterator$mcB$sp() {
}
$h_sc_ArrayOps$ArrayIterator$mcB$sp.prototype = $c_sc_ArrayOps$ArrayIterator$mcB$sp.prototype;
$c_sc_ArrayOps$ArrayIterator$mcB$sp.prototype.next$mcB$sp__B = (function() {
  if ((this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos >= $n(this.sc_ArrayOps$ArrayIterator$mcB$sp__f_xs$mcB$sp).u.length)) {
    $n($m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty).next__O();
  }
  var r = $n(this.sc_ArrayOps$ArrayIterator$mcB$sp__f_xs$mcB$sp).get(this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos);
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = ((1 + this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos) | 0);
  return r;
});
$c_sc_ArrayOps$ArrayIterator$mcB$sp.prototype.next__O = (function() {
  return this.next$mcB$sp__B();
});
var $d_sc_ArrayOps$ArrayIterator$mcB$sp = new $TypeData().initClass($c_sc_ArrayOps$ArrayIterator$mcB$sp, "scala.collection.ArrayOps$ArrayIterator$mcB$sp", ({
  sc_ArrayOps$ArrayIterator$mcB$sp: 1,
  sc_ArrayOps$ArrayIterator: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sc_ArrayOps$ArrayIterator$mcC$sp(xs$mcC$sp) {
  this.sc_ArrayOps$ArrayIterator__f_xs = null;
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = 0;
  this.sc_ArrayOps$ArrayIterator__f_len = 0;
  this.sc_ArrayOps$ArrayIterator$mcC$sp__f_xs$mcC$sp = null;
  this.sc_ArrayOps$ArrayIterator$mcC$sp__f_xs$mcC$sp = xs$mcC$sp;
  $ct_sc_ArrayOps$ArrayIterator__O__(this, xs$mcC$sp);
}
$c_sc_ArrayOps$ArrayIterator$mcC$sp.prototype = new $h_sc_ArrayOps$ArrayIterator();
$c_sc_ArrayOps$ArrayIterator$mcC$sp.prototype.constructor = $c_sc_ArrayOps$ArrayIterator$mcC$sp;
/** @constructor */
function $h_sc_ArrayOps$ArrayIterator$mcC$sp() {
}
$h_sc_ArrayOps$ArrayIterator$mcC$sp.prototype = $c_sc_ArrayOps$ArrayIterator$mcC$sp.prototype;
$c_sc_ArrayOps$ArrayIterator$mcC$sp.prototype.next$mcC$sp__C = (function() {
  if ((this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos >= $n(this.sc_ArrayOps$ArrayIterator$mcC$sp__f_xs$mcC$sp).u.length)) {
    $n($m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty).next__O();
  }
  var r = $n(this.sc_ArrayOps$ArrayIterator$mcC$sp__f_xs$mcC$sp).get(this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos);
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = ((1 + this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos) | 0);
  return r;
});
$c_sc_ArrayOps$ArrayIterator$mcC$sp.prototype.next__O = (function() {
  return $bC(this.next$mcC$sp__C());
});
var $d_sc_ArrayOps$ArrayIterator$mcC$sp = new $TypeData().initClass($c_sc_ArrayOps$ArrayIterator$mcC$sp, "scala.collection.ArrayOps$ArrayIterator$mcC$sp", ({
  sc_ArrayOps$ArrayIterator$mcC$sp: 1,
  sc_ArrayOps$ArrayIterator: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sc_ArrayOps$ArrayIterator$mcD$sp(xs$mcD$sp) {
  this.sc_ArrayOps$ArrayIterator__f_xs = null;
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = 0;
  this.sc_ArrayOps$ArrayIterator__f_len = 0;
  this.sc_ArrayOps$ArrayIterator$mcD$sp__f_xs$mcD$sp = null;
  this.sc_ArrayOps$ArrayIterator$mcD$sp__f_xs$mcD$sp = xs$mcD$sp;
  $ct_sc_ArrayOps$ArrayIterator__O__(this, xs$mcD$sp);
}
$c_sc_ArrayOps$ArrayIterator$mcD$sp.prototype = new $h_sc_ArrayOps$ArrayIterator();
$c_sc_ArrayOps$ArrayIterator$mcD$sp.prototype.constructor = $c_sc_ArrayOps$ArrayIterator$mcD$sp;
/** @constructor */
function $h_sc_ArrayOps$ArrayIterator$mcD$sp() {
}
$h_sc_ArrayOps$ArrayIterator$mcD$sp.prototype = $c_sc_ArrayOps$ArrayIterator$mcD$sp.prototype;
$c_sc_ArrayOps$ArrayIterator$mcD$sp.prototype.next$mcD$sp__D = (function() {
  if ((this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos >= $n(this.sc_ArrayOps$ArrayIterator$mcD$sp__f_xs$mcD$sp).u.length)) {
    $n($m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty).next__O();
  }
  var r = $n(this.sc_ArrayOps$ArrayIterator$mcD$sp__f_xs$mcD$sp).get(this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos);
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = ((1 + this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos) | 0);
  return r;
});
$c_sc_ArrayOps$ArrayIterator$mcD$sp.prototype.next__O = (function() {
  return this.next$mcD$sp__D();
});
var $d_sc_ArrayOps$ArrayIterator$mcD$sp = new $TypeData().initClass($c_sc_ArrayOps$ArrayIterator$mcD$sp, "scala.collection.ArrayOps$ArrayIterator$mcD$sp", ({
  sc_ArrayOps$ArrayIterator$mcD$sp: 1,
  sc_ArrayOps$ArrayIterator: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sc_ArrayOps$ArrayIterator$mcF$sp(xs$mcF$sp) {
  this.sc_ArrayOps$ArrayIterator__f_xs = null;
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = 0;
  this.sc_ArrayOps$ArrayIterator__f_len = 0;
  this.sc_ArrayOps$ArrayIterator$mcF$sp__f_xs$mcF$sp = null;
  this.sc_ArrayOps$ArrayIterator$mcF$sp__f_xs$mcF$sp = xs$mcF$sp;
  $ct_sc_ArrayOps$ArrayIterator__O__(this, xs$mcF$sp);
}
$c_sc_ArrayOps$ArrayIterator$mcF$sp.prototype = new $h_sc_ArrayOps$ArrayIterator();
$c_sc_ArrayOps$ArrayIterator$mcF$sp.prototype.constructor = $c_sc_ArrayOps$ArrayIterator$mcF$sp;
/** @constructor */
function $h_sc_ArrayOps$ArrayIterator$mcF$sp() {
}
$h_sc_ArrayOps$ArrayIterator$mcF$sp.prototype = $c_sc_ArrayOps$ArrayIterator$mcF$sp.prototype;
$c_sc_ArrayOps$ArrayIterator$mcF$sp.prototype.next$mcF$sp__F = (function() {
  if ((this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos >= $n(this.sc_ArrayOps$ArrayIterator$mcF$sp__f_xs$mcF$sp).u.length)) {
    $n($m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty).next__O();
  }
  var r = $n(this.sc_ArrayOps$ArrayIterator$mcF$sp__f_xs$mcF$sp).get(this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos);
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = ((1 + this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos) | 0);
  return r;
});
$c_sc_ArrayOps$ArrayIterator$mcF$sp.prototype.next__O = (function() {
  return this.next$mcF$sp__F();
});
var $d_sc_ArrayOps$ArrayIterator$mcF$sp = new $TypeData().initClass($c_sc_ArrayOps$ArrayIterator$mcF$sp, "scala.collection.ArrayOps$ArrayIterator$mcF$sp", ({
  sc_ArrayOps$ArrayIterator$mcF$sp: 1,
  sc_ArrayOps$ArrayIterator: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sc_ArrayOps$ArrayIterator$mcI$sp(xs$mcI$sp) {
  this.sc_ArrayOps$ArrayIterator__f_xs = null;
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = 0;
  this.sc_ArrayOps$ArrayIterator__f_len = 0;
  this.sc_ArrayOps$ArrayIterator$mcI$sp__f_xs$mcI$sp = null;
  this.sc_ArrayOps$ArrayIterator$mcI$sp__f_xs$mcI$sp = xs$mcI$sp;
  $ct_sc_ArrayOps$ArrayIterator__O__(this, xs$mcI$sp);
}
$c_sc_ArrayOps$ArrayIterator$mcI$sp.prototype = new $h_sc_ArrayOps$ArrayIterator();
$c_sc_ArrayOps$ArrayIterator$mcI$sp.prototype.constructor = $c_sc_ArrayOps$ArrayIterator$mcI$sp;
/** @constructor */
function $h_sc_ArrayOps$ArrayIterator$mcI$sp() {
}
$h_sc_ArrayOps$ArrayIterator$mcI$sp.prototype = $c_sc_ArrayOps$ArrayIterator$mcI$sp.prototype;
$c_sc_ArrayOps$ArrayIterator$mcI$sp.prototype.next$mcI$sp__I = (function() {
  if ((this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos >= $n(this.sc_ArrayOps$ArrayIterator$mcI$sp__f_xs$mcI$sp).u.length)) {
    $n($m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty).next__O();
  }
  var r = $n(this.sc_ArrayOps$ArrayIterator$mcI$sp__f_xs$mcI$sp).get(this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos);
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = ((1 + this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos) | 0);
  return r;
});
$c_sc_ArrayOps$ArrayIterator$mcI$sp.prototype.next__O = (function() {
  return this.next$mcI$sp__I();
});
var $d_sc_ArrayOps$ArrayIterator$mcI$sp = new $TypeData().initClass($c_sc_ArrayOps$ArrayIterator$mcI$sp, "scala.collection.ArrayOps$ArrayIterator$mcI$sp", ({
  sc_ArrayOps$ArrayIterator$mcI$sp: 1,
  sc_ArrayOps$ArrayIterator: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sc_ArrayOps$ArrayIterator$mcJ$sp(xs$mcJ$sp) {
  this.sc_ArrayOps$ArrayIterator__f_xs = null;
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = 0;
  this.sc_ArrayOps$ArrayIterator__f_len = 0;
  this.sc_ArrayOps$ArrayIterator$mcJ$sp__f_xs$mcJ$sp = null;
  this.sc_ArrayOps$ArrayIterator$mcJ$sp__f_xs$mcJ$sp = xs$mcJ$sp;
  $ct_sc_ArrayOps$ArrayIterator__O__(this, xs$mcJ$sp);
}
$c_sc_ArrayOps$ArrayIterator$mcJ$sp.prototype = new $h_sc_ArrayOps$ArrayIterator();
$c_sc_ArrayOps$ArrayIterator$mcJ$sp.prototype.constructor = $c_sc_ArrayOps$ArrayIterator$mcJ$sp;
/** @constructor */
function $h_sc_ArrayOps$ArrayIterator$mcJ$sp() {
}
$h_sc_ArrayOps$ArrayIterator$mcJ$sp.prototype = $c_sc_ArrayOps$ArrayIterator$mcJ$sp.prototype;
$c_sc_ArrayOps$ArrayIterator$mcJ$sp.prototype.next$mcJ$sp__J = (function() {
  if ((this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos >= $n(this.sc_ArrayOps$ArrayIterator$mcJ$sp__f_xs$mcJ$sp).u.length)) {
    $n($m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty).next__O();
  }
  var t = $n(this.sc_ArrayOps$ArrayIterator$mcJ$sp__f_xs$mcJ$sp).get(this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos);
  var lo = t.RTLong__f_lo;
  var hi = t.RTLong__f_hi;
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = ((1 + this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos) | 0);
  return new $c_RTLong(lo, hi);
});
$c_sc_ArrayOps$ArrayIterator$mcJ$sp.prototype.next__O = (function() {
  return this.next$mcJ$sp__J();
});
var $d_sc_ArrayOps$ArrayIterator$mcJ$sp = new $TypeData().initClass($c_sc_ArrayOps$ArrayIterator$mcJ$sp, "scala.collection.ArrayOps$ArrayIterator$mcJ$sp", ({
  sc_ArrayOps$ArrayIterator$mcJ$sp: 1,
  sc_ArrayOps$ArrayIterator: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sc_ArrayOps$ArrayIterator$mcS$sp(xs$mcS$sp) {
  this.sc_ArrayOps$ArrayIterator__f_xs = null;
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = 0;
  this.sc_ArrayOps$ArrayIterator__f_len = 0;
  this.sc_ArrayOps$ArrayIterator$mcS$sp__f_xs$mcS$sp = null;
  this.sc_ArrayOps$ArrayIterator$mcS$sp__f_xs$mcS$sp = xs$mcS$sp;
  $ct_sc_ArrayOps$ArrayIterator__O__(this, xs$mcS$sp);
}
$c_sc_ArrayOps$ArrayIterator$mcS$sp.prototype = new $h_sc_ArrayOps$ArrayIterator();
$c_sc_ArrayOps$ArrayIterator$mcS$sp.prototype.constructor = $c_sc_ArrayOps$ArrayIterator$mcS$sp;
/** @constructor */
function $h_sc_ArrayOps$ArrayIterator$mcS$sp() {
}
$h_sc_ArrayOps$ArrayIterator$mcS$sp.prototype = $c_sc_ArrayOps$ArrayIterator$mcS$sp.prototype;
$c_sc_ArrayOps$ArrayIterator$mcS$sp.prototype.next$mcS$sp__S = (function() {
  if ((this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos >= $n(this.sc_ArrayOps$ArrayIterator$mcS$sp__f_xs$mcS$sp).u.length)) {
    $n($m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty).next__O();
  }
  var r = $n(this.sc_ArrayOps$ArrayIterator$mcS$sp__f_xs$mcS$sp).get(this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos);
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = ((1 + this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos) | 0);
  return r;
});
$c_sc_ArrayOps$ArrayIterator$mcS$sp.prototype.next__O = (function() {
  return this.next$mcS$sp__S();
});
var $d_sc_ArrayOps$ArrayIterator$mcS$sp = new $TypeData().initClass($c_sc_ArrayOps$ArrayIterator$mcS$sp, "scala.collection.ArrayOps$ArrayIterator$mcS$sp", ({
  sc_ArrayOps$ArrayIterator$mcS$sp: 1,
  sc_ArrayOps$ArrayIterator: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sc_ArrayOps$ArrayIterator$mcV$sp(xs$mcV$sp) {
  this.sc_ArrayOps$ArrayIterator__f_xs = null;
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = 0;
  this.sc_ArrayOps$ArrayIterator__f_len = 0;
  this.sc_ArrayOps$ArrayIterator$mcV$sp__f_xs$mcV$sp = null;
  this.sc_ArrayOps$ArrayIterator$mcV$sp__f_xs$mcV$sp = xs$mcV$sp;
  $ct_sc_ArrayOps$ArrayIterator__O__(this, xs$mcV$sp);
}
$c_sc_ArrayOps$ArrayIterator$mcV$sp.prototype = new $h_sc_ArrayOps$ArrayIterator();
$c_sc_ArrayOps$ArrayIterator$mcV$sp.prototype.constructor = $c_sc_ArrayOps$ArrayIterator$mcV$sp;
/** @constructor */
function $h_sc_ArrayOps$ArrayIterator$mcV$sp() {
}
$h_sc_ArrayOps$ArrayIterator$mcV$sp.prototype = $c_sc_ArrayOps$ArrayIterator$mcV$sp.prototype;
$c_sc_ArrayOps$ArrayIterator$mcV$sp.prototype.next$mcV$sp__V = (function() {
  if ((this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos >= $n(this.sc_ArrayOps$ArrayIterator$mcV$sp__f_xs$mcV$sp).u.length)) {
    $n($m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty).next__O();
  }
  $n(this.sc_ArrayOps$ArrayIterator$mcV$sp__f_xs$mcV$sp).get(this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos);
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = ((1 + this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos) | 0);
});
$c_sc_ArrayOps$ArrayIterator$mcV$sp.prototype.next__O = (function() {
  this.next$mcV$sp__V();
});
var $d_sc_ArrayOps$ArrayIterator$mcV$sp = new $TypeData().initClass($c_sc_ArrayOps$ArrayIterator$mcV$sp, "scala.collection.ArrayOps$ArrayIterator$mcV$sp", ({
  sc_ArrayOps$ArrayIterator$mcV$sp: 1,
  sc_ArrayOps$ArrayIterator: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sc_ArrayOps$ArrayIterator$mcZ$sp(xs$mcZ$sp) {
  this.sc_ArrayOps$ArrayIterator__f_xs = null;
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = 0;
  this.sc_ArrayOps$ArrayIterator__f_len = 0;
  this.sc_ArrayOps$ArrayIterator$mcZ$sp__f_xs$mcZ$sp = null;
  this.sc_ArrayOps$ArrayIterator$mcZ$sp__f_xs$mcZ$sp = xs$mcZ$sp;
  $ct_sc_ArrayOps$ArrayIterator__O__(this, xs$mcZ$sp);
}
$c_sc_ArrayOps$ArrayIterator$mcZ$sp.prototype = new $h_sc_ArrayOps$ArrayIterator();
$c_sc_ArrayOps$ArrayIterator$mcZ$sp.prototype.constructor = $c_sc_ArrayOps$ArrayIterator$mcZ$sp;
/** @constructor */
function $h_sc_ArrayOps$ArrayIterator$mcZ$sp() {
}
$h_sc_ArrayOps$ArrayIterator$mcZ$sp.prototype = $c_sc_ArrayOps$ArrayIterator$mcZ$sp.prototype;
$c_sc_ArrayOps$ArrayIterator$mcZ$sp.prototype.next$mcZ$sp__Z = (function() {
  if ((this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos >= $n(this.sc_ArrayOps$ArrayIterator$mcZ$sp__f_xs$mcZ$sp).u.length)) {
    $n($m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty).next__O();
  }
  var r = $n(this.sc_ArrayOps$ArrayIterator$mcZ$sp__f_xs$mcZ$sp).get(this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos);
  this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos = ((1 + this.sc_ArrayOps$ArrayIterator__f_scala$collection$ArrayOps$ArrayIterator$$pos) | 0);
  return r;
});
$c_sc_ArrayOps$ArrayIterator$mcZ$sp.prototype.next__O = (function() {
  return this.next$mcZ$sp__Z();
});
var $d_sc_ArrayOps$ArrayIterator$mcZ$sp = new $TypeData().initClass($c_sc_ArrayOps$ArrayIterator$mcZ$sp, "scala.collection.ArrayOps$ArrayIterator$mcZ$sp", ({
  sc_ArrayOps$ArrayIterator$mcZ$sp: 1,
  sc_ArrayOps$ArrayIterator: 1,
  sc_AbstractIterator: 1,
  sc_Iterator: 1,
  sc_IterableOnce: 1,
  sc_IterableOnceOps: 1,
  Ljava_io_Serializable: 1
}));
function $f_sc_View__toString__T($thiz) {
  return ($thiz.className__T() + "(<not computed>)");
}
/** @constructor */
function $c_s_reflect_ManifestFactory$ClassTypeManifest() {
}
$c_s_reflect_ManifestFactory$ClassTypeManifest.prototype = new $h_O();
$c_s_reflect_ManifestFactory$ClassTypeManifest.prototype.constructor = $c_s_reflect_ManifestFactory$ClassTypeManifest;
/** @constructor */
function $h_s_reflect_ManifestFactory$ClassTypeManifest() {
}
$h_s_reflect_ManifestFactory$ClassTypeManifest.prototype = $c_s_reflect_ManifestFactory$ClassTypeManifest.prototype;
class $c_sjs_js_JavaScriptException extends $c_jl_RuntimeException {
  constructor(exception) {
    super();
    this.sjs_js_JavaScriptException__f_exception = null;
    this.sjs_js_JavaScriptException__f_exception = exception;
    $ct_jl_Throwable__T__jl_Throwable__Z__Z__(this, null, null, true, true);
  }
  getMessage__T() {
    return $dp_toString__T($n(this.sjs_js_JavaScriptException__f_exception));
  }
  productPrefix__T() {
    return "JavaScriptException";
  }
  productArity__I() {
    return 1;
  }
  productElement__I__O(x$1) {
    return ((x$1 === 0) ? this.sjs_js_JavaScriptException__f_exception : $m_sr_Statics$().ioobe__I__O(x$1));
  }
  productIterator__sc_Iterator() {
    return new $c_sr_ScalaRunTime$$anon$1(this);
  }
  hashCode__I() {
    var this$2 = $m_s_util_hashing_MurmurHash3$();
    return this$2.productHash__s_Product__I__Z__I(this, (-889275714), false);
  }
  equals__O__Z(x$1) {
    if ((this === x$1)) {
      return true;
    } else if ((x$1 instanceof $c_sjs_js_JavaScriptException)) {
      var JavaScriptException$1 = $as_sjs_js_JavaScriptException(x$1);
      var x = this.sjs_js_JavaScriptException__f_exception;
      var y = $n(JavaScriptException$1).sjs_js_JavaScriptException__f_exception;
      return $m_sr_BoxesRunTime$().equals__O__O__Z(x, y);
    } else {
      return false;
    }
  }
}
function $as_sjs_js_JavaScriptException(obj) {
  return (((obj instanceof $c_sjs_js_JavaScriptException) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.scalajs.js.JavaScriptException"));
}
function $isArrayOf_sjs_js_JavaScriptException(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sjs_js_JavaScriptException)));
}
function $asArrayOf_sjs_js_JavaScriptException(obj, depth) {
  return (($isArrayOf_sjs_js_JavaScriptException(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.scalajs.js.JavaScriptException;", depth));
}
var $d_sjs_js_JavaScriptException = new $TypeData().initClass($c_sjs_js_JavaScriptException, "scala.scalajs.js.JavaScriptException", ({
  sjs_js_JavaScriptException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  Ljava_io_Serializable: 1,
  s_Product: 1,
  s_Equals: 1
}));
function $p_sc_StrictOptimizedLinearSeqOps__loop$2__I__sc_LinearSeq__sc_LinearSeq($thiz, n, s) {
  while (true) {
    if (((n <= 0) || $n(s).isEmpty__Z())) {
      return s;
    } else {
      var temp$n = (((-1) + n) | 0);
      var temp$s = $as_sc_LinearSeq($n(s).tail__O());
      n = temp$n;
      s = temp$s;
    }
  }
}
function $is_sc_StrictOptimizedLinearSeqOps(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_StrictOptimizedLinearSeqOps)));
}
function $as_sc_StrictOptimizedLinearSeqOps(obj) {
  return (($is_sc_StrictOptimizedLinearSeqOps(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.StrictOptimizedLinearSeqOps"));
}
function $isArrayOf_sc_StrictOptimizedLinearSeqOps(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_StrictOptimizedLinearSeqOps)));
}
function $asArrayOf_sc_StrictOptimizedLinearSeqOps(obj, depth) {
  return (($isArrayOf_sc_StrictOptimizedLinearSeqOps(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.StrictOptimizedLinearSeqOps;", depth));
}
/** @constructor */
function $c_s_reflect_ManifestFactory$PhantomManifest() {
  this.s_reflect_ManifestFactory$PhantomManifest__f_toString = null;
}
$c_s_reflect_ManifestFactory$PhantomManifest.prototype = new $h_s_reflect_ManifestFactory$ClassTypeManifest();
$c_s_reflect_ManifestFactory$PhantomManifest.prototype.constructor = $c_s_reflect_ManifestFactory$PhantomManifest;
/** @constructor */
function $h_s_reflect_ManifestFactory$PhantomManifest() {
}
$h_s_reflect_ManifestFactory$PhantomManifest.prototype = $c_s_reflect_ManifestFactory$PhantomManifest.prototype;
$c_s_reflect_ManifestFactory$PhantomManifest.prototype.toString__T = (function() {
  return this.s_reflect_ManifestFactory$PhantomManifest__f_toString;
});
$c_s_reflect_ManifestFactory$PhantomManifest.prototype.equals__O__Z = (function(that) {
  return (this === that);
});
$c_s_reflect_ManifestFactory$PhantomManifest.prototype.hashCode__I = (function() {
  return $systemIdentityHashCode(this);
});
/** @constructor */
function $c_sc_AbstractView() {
}
$c_sc_AbstractView.prototype = new $h_sc_AbstractIterable();
$c_sc_AbstractView.prototype.constructor = $c_sc_AbstractView;
/** @constructor */
function $h_sc_AbstractView() {
}
$h_sc_AbstractView.prototype = $c_sc_AbstractView.prototype;
$c_sc_AbstractView.prototype.toString__T = (function() {
  return $f_sc_View__toString__T(this);
});
/** @constructor */
function $c_s_reflect_ManifestFactory$AnyManifest$() {
  this.s_reflect_ManifestFactory$PhantomManifest__f_toString = null;
  this.s_reflect_ManifestFactory$PhantomManifest__f_toString = "Any";
}
$c_s_reflect_ManifestFactory$AnyManifest$.prototype = new $h_s_reflect_ManifestFactory$PhantomManifest();
$c_s_reflect_ManifestFactory$AnyManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$AnyManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$AnyManifest$() {
}
$h_s_reflect_ManifestFactory$AnyManifest$.prototype = $c_s_reflect_ManifestFactory$AnyManifest$.prototype;
$c_s_reflect_ManifestFactory$AnyManifest$.prototype.runtimeClass__jl_Class = (function() {
  return $d_O.getClassOf();
});
$c_s_reflect_ManifestFactory$AnyManifest$.prototype.newArray__I__O = (function(len) {
  return new $ac_O(len);
});
var $d_s_reflect_ManifestFactory$AnyManifest$ = new $TypeData().initClass($c_s_reflect_ManifestFactory$AnyManifest$, "scala.reflect.ManifestFactory$AnyManifest$", ({
  s_reflect_ManifestFactory$AnyManifest$: 1,
  s_reflect_ManifestFactory$PhantomManifest: 1,
  s_reflect_ManifestFactory$ClassTypeManifest: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
}));
var $n_s_reflect_ManifestFactory$AnyManifest$;
function $m_s_reflect_ManifestFactory$AnyManifest$() {
  if ((!$n_s_reflect_ManifestFactory$AnyManifest$)) {
    $n_s_reflect_ManifestFactory$AnyManifest$ = new $c_s_reflect_ManifestFactory$AnyManifest$();
  }
  return $n_s_reflect_ManifestFactory$AnyManifest$;
}
function $f_sc_Seq__equals__O__Z($thiz, o) {
  if (($thiz === o)) {
    return true;
  } else {
    if ($is_sc_Seq(o)) {
      var x2 = $as_sc_Seq(o);
      if ($n(x2).canEqual__O__Z($thiz)) {
        return $thiz.sameElements__sc_IterableOnce__Z(x2);
      }
    }
    return false;
  }
}
function $is_sc_Seq(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_Seq)));
}
function $as_sc_Seq(obj) {
  return (($is_sc_Seq(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.Seq"));
}
function $isArrayOf_sc_Seq(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_Seq)));
}
function $asArrayOf_sc_Seq(obj, depth) {
  return (($isArrayOf_sc_Seq(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.Seq;", depth));
}
/** @constructor */
function $c_sc_AbstractSeq() {
}
$c_sc_AbstractSeq.prototype = new $h_sc_AbstractIterable();
$c_sc_AbstractSeq.prototype.constructor = $c_sc_AbstractSeq;
/** @constructor */
function $h_sc_AbstractSeq() {
}
$h_sc_AbstractSeq.prototype = $c_sc_AbstractSeq.prototype;
$c_sc_AbstractSeq.prototype.canEqual__O__Z = (function(that) {
  return true;
});
$c_sc_AbstractSeq.prototype.equals__O__Z = (function(o) {
  return $f_sc_Seq__equals__O__Z(this, o);
});
$c_sc_AbstractSeq.prototype.hashCode__I = (function() {
  return $m_s_util_hashing_MurmurHash3$().seqHash__sc_Seq__I(this);
});
$c_sc_AbstractSeq.prototype.toString__T = (function() {
  return $f_sc_Iterable__toString__T(this);
});
$c_sc_AbstractSeq.prototype.lengthCompare__I__I = (function(len) {
  return $f_sc_IterableOps__sizeCompare__I__I(this, len);
});
$c_sc_AbstractSeq.prototype.isEmpty__Z = (function() {
  return $f_sc_SeqOps__isEmpty__Z(this);
});
$c_sc_AbstractSeq.prototype.sameElements__sc_IterableOnce__Z = (function(that) {
  return $f_sc_SeqOps__sameElements__sc_IterableOnce__Z(this, that);
});
/** @constructor */
function $c_sc_AbstractSeqView() {
}
$c_sc_AbstractSeqView.prototype = new $h_sc_AbstractView();
$c_sc_AbstractSeqView.prototype.constructor = $c_sc_AbstractSeqView;
/** @constructor */
function $h_sc_AbstractSeqView() {
}
$h_sc_AbstractSeqView.prototype = $c_sc_AbstractSeqView.prototype;
function $is_sc_IndexedSeq(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_IndexedSeq)));
}
function $as_sc_IndexedSeq(obj) {
  return (($is_sc_IndexedSeq(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.IndexedSeq"));
}
function $isArrayOf_sc_IndexedSeq(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_IndexedSeq)));
}
function $asArrayOf_sc_IndexedSeq(obj, depth) {
  return (($isArrayOf_sc_IndexedSeq(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.IndexedSeq;", depth));
}
function $is_sc_LinearSeq(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_LinearSeq)));
}
function $as_sc_LinearSeq(obj) {
  return (($is_sc_LinearSeq(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.LinearSeq"));
}
function $isArrayOf_sc_LinearSeq(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_LinearSeq)));
}
function $asArrayOf_sc_LinearSeq(obj, depth) {
  return (($isArrayOf_sc_LinearSeq(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.LinearSeq;", depth));
}
function $f_Lcom_raquo_laminar_api_Laminar__$init$__V($thiz) {
  $n($thiz.Lcom_raquo_laminar_api_package$$anon$1__f_Modifier);
}
function $ct_sc_SeqView$Id__sc_SeqOps__($thiz, underlying) {
  $thiz.sc_SeqView$Id__f_underlying = underlying;
  return $thiz;
}
/** @constructor */
function $c_sc_SeqView$Id() {
  this.sc_SeqView$Id__f_underlying = null;
}
$c_sc_SeqView$Id.prototype = new $h_sc_AbstractSeqView();
$c_sc_SeqView$Id.prototype.constructor = $c_sc_SeqView$Id;
/** @constructor */
function $h_sc_SeqView$Id() {
}
$h_sc_SeqView$Id.prototype = $c_sc_SeqView$Id.prototype;
$c_sc_SeqView$Id.prototype.apply__I__O = (function(idx) {
  return $n(this.sc_SeqView$Id__f_underlying).apply__I__O(idx);
});
$c_sc_SeqView$Id.prototype.length__I = (function() {
  return $n(this.sc_SeqView$Id__f_underlying).length__I();
});
$c_sc_SeqView$Id.prototype.isEmpty__Z = (function() {
  return $n(this.sc_SeqView$Id__f_underlying).isEmpty__Z();
});
/** @constructor */
function $c_Lcom_raquo_laminar_api_package$$anon$1() {
  this.Lcom_raquo_laminar_api_package$$anon$1__f_div$lzy1 = null;
  this.Lcom_raquo_laminar_api_package$$anon$1__f_divbitmap$1 = false;
  this.Lcom_raquo_laminar_api_package$$anon$1__f_className = null;
  this.Lcom_raquo_laminar_api_package$$anon$1__f_Modifier = null;
  this.Lcom_raquo_laminar_api_package$$anon$1__f_svg$lzy1 = null;
  this.Lcom_raquo_laminar_api_package$$anon$1__f_svgbitmap$1 = false;
  $f_Lcom_raquo_laminar_defs_complex_ComplexHtmlKeys__$init$__V(this);
  $f_Lcom_raquo_laminar_api_MountHooks__$init$__V(this);
  $f_Lcom_raquo_laminar_api_AirstreamAliases__$init$__V(this);
  $f_Lcom_raquo_laminar_api_LaminarAliases__$init$__V(this);
  $f_Lcom_raquo_laminar_api_Laminar__$init$__V(this);
}
$c_Lcom_raquo_laminar_api_package$$anon$1.prototype = new $h_O();
$c_Lcom_raquo_laminar_api_package$$anon$1.prototype.constructor = $c_Lcom_raquo_laminar_api_package$$anon$1;
/** @constructor */
function $h_Lcom_raquo_laminar_api_package$$anon$1() {
}
$h_Lcom_raquo_laminar_api_package$$anon$1.prototype = $c_Lcom_raquo_laminar_api_package$$anon$1.prototype;
$c_Lcom_raquo_laminar_api_package$$anon$1.prototype.div__Lcom_raquo_laminar_tags_HtmlTag = (function() {
  if ((!this.Lcom_raquo_laminar_api_package$$anon$1__f_divbitmap$1)) {
    this.Lcom_raquo_laminar_api_package$$anon$1__f_div$lzy1 = new $c_Lcom_raquo_laminar_tags_HtmlTag("div", false);
    this.Lcom_raquo_laminar_api_package$$anon$1__f_divbitmap$1 = true;
  }
  return this.Lcom_raquo_laminar_api_package$$anon$1__f_div$lzy1;
});
$c_Lcom_raquo_laminar_api_package$$anon$1.prototype.svg__Lcom_raquo_laminar_api_Laminar$svg$ = (function() {
  if ((!this.Lcom_raquo_laminar_api_package$$anon$1__f_svgbitmap$1)) {
    this.Lcom_raquo_laminar_api_package$$anon$1__f_svg$lzy1 = new $c_Lcom_raquo_laminar_api_Laminar$svg$(this);
    this.Lcom_raquo_laminar_api_package$$anon$1__f_svgbitmap$1 = true;
  }
  return this.Lcom_raquo_laminar_api_package$$anon$1__f_svg$lzy1;
});
var $d_Lcom_raquo_laminar_api_package$$anon$1 = new $TypeData().initClass($c_Lcom_raquo_laminar_api_package$$anon$1, "com.raquo.laminar.api.package$$anon$1", ({
  Lcom_raquo_laminar_api_package$$anon$1: 1,
  Lcom_raquo_laminar_defs_tags_HtmlTags: 1,
  Lcom_raquo_laminar_defs_attrs_HtmlAttrs: 1,
  Lcom_raquo_laminar_defs_props_HtmlProps: 1,
  Lcom_raquo_laminar_defs_eventProps_GlobalEventProps: 1,
  Lcom_raquo_laminar_defs_styles_StyleProps: 1,
  Lcom_raquo_laminar_defs_complex_ComplexHtmlKeys: 1,
  Lcom_raquo_laminar_api_MountHooks: 1,
  Lcom_raquo_laminar_api_AirstreamAliases: 1,
  Lcom_raquo_laminar_api_LaminarAliases: 1,
  Lcom_raquo_laminar_api_Implicits$LowPriorityImplicits: 1,
  Lcom_raquo_laminar_keys_CompositeKey$CompositeValueMappers: 1,
  Lcom_raquo_laminar_api_Implicits: 1,
  Lcom_raquo_laminar_api_Laminar: 1
}));
/** @constructor */
function $c_sc_IndexedSeqView$Id(underlying) {
  this.sc_SeqView$Id__f_underlying = null;
  $ct_sc_SeqView$Id__sc_SeqOps__(this, underlying);
}
$c_sc_IndexedSeqView$Id.prototype = new $h_sc_SeqView$Id();
$c_sc_IndexedSeqView$Id.prototype.constructor = $c_sc_IndexedSeqView$Id;
/** @constructor */
function $h_sc_IndexedSeqView$Id() {
}
$h_sc_IndexedSeqView$Id.prototype = $c_sc_IndexedSeqView$Id.prototype;
$c_sc_IndexedSeqView$Id.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_IndexedSeqView$IndexedSeqViewIterator(this);
});
$c_sc_IndexedSeqView$Id.prototype.stringPrefix__T = (function() {
  return "IndexedSeqView";
});
$c_sc_IndexedSeqView$Id.prototype.lengthCompare__I__I = (function(len) {
  var x = this.length__I();
  return ((x === len) ? 0 : ((x < len) ? (-1) : 1));
});
$c_sc_IndexedSeqView$Id.prototype.knownSize__I = (function() {
  return this.length__I();
});
var $d_sc_IndexedSeqView$Id = new $TypeData().initClass($c_sc_IndexedSeqView$Id, "scala.collection.IndexedSeqView$Id", ({
  sc_IndexedSeqView$Id: 1,
  sc_SeqView$Id: 1,
  sc_AbstractSeqView: 1,
  sc_AbstractView: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_View: 1,
  Ljava_io_Serializable: 1,
  sc_SeqView: 1,
  sc_SeqOps: 1,
  sc_IndexedSeqView: 1,
  sc_IndexedSeqOps: 1
}));
/** @constructor */
function $c_sci_AbstractSeq() {
}
$c_sci_AbstractSeq.prototype = new $h_sc_AbstractSeq();
$c_sci_AbstractSeq.prototype.constructor = $c_sci_AbstractSeq;
/** @constructor */
function $h_sci_AbstractSeq() {
}
$h_sci_AbstractSeq.prototype = $c_sci_AbstractSeq.prototype;
function $f_sci_IndexedSeq__canEqual__O__Z($thiz, that) {
  if ((!$is_sci_IndexedSeq(that))) {
    return true;
  } else {
    var x2 = $as_sci_IndexedSeq(that);
    return ($thiz.length__I() === $n(x2).length__I());
  }
}
function $f_sci_IndexedSeq__sameElements__sc_IterableOnce__Z($thiz, o) {
  if ($is_sci_IndexedSeq(o)) {
    var x2 = $as_sci_IndexedSeq(o);
    if (($thiz === x2)) {
      return true;
    } else {
      var length = $thiz.length__I();
      var equal = (length === $n(x2).length__I());
      if (equal) {
        var index = 0;
        var a = $thiz.applyPreferredMaxLength__I();
        var b = $n(x2).applyPreferredMaxLength__I();
        var preferredLength = ((a < b) ? a : b);
        var hi = (length >> 31);
        var hi$1 = (preferredLength >> 31);
        var lo = (preferredLength << 1);
        var hi$2 = (((preferredLength >>> 31) | 0) | (hi$1 << 1));
        if (((hi === hi$2) ? (((-2147483648) ^ length) > ((-2147483648) ^ lo)) : (hi > hi$2))) {
          var maxApplyCompare = preferredLength;
        } else {
          var maxApplyCompare = length;
        }
        while (((index < maxApplyCompare) && equal)) {
          equal = $m_sr_BoxesRunTime$().equals__O__O__Z($thiz.apply__I__O(index), $n(x2).apply__I__O(index));
          index = ((1 + index) | 0);
        }
        if (((index < length) && equal)) {
          var thisIt = $n($thiz.iterator__sc_Iterator()).drop__I__sc_Iterator(index);
          var thatIt = $n($n(x2).iterator__sc_Iterator()).drop__I__sc_Iterator(index);
          while ((equal && $n(thisIt).hasNext__Z())) {
            equal = $m_sr_BoxesRunTime$().equals__O__O__Z($n(thisIt).next__O(), $n(thatIt).next__O());
          }
        }
      }
      return equal;
    }
  } else {
    return $f_sc_SeqOps__sameElements__sc_IterableOnce__Z($thiz, o);
  }
}
function $is_sci_IndexedSeq(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_IndexedSeq)));
}
function $as_sci_IndexedSeq(obj) {
  return (($is_sci_IndexedSeq(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.IndexedSeq"));
}
function $isArrayOf_sci_IndexedSeq(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_IndexedSeq)));
}
function $asArrayOf_sci_IndexedSeq(obj, depth) {
  return (($isArrayOf_sci_IndexedSeq(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.IndexedSeq;", depth));
}
/** @constructor */
function $c_scm_AbstractSeq() {
}
$c_scm_AbstractSeq.prototype = new $h_sc_AbstractSeq();
$c_scm_AbstractSeq.prototype.constructor = $c_scm_AbstractSeq;
/** @constructor */
function $h_scm_AbstractSeq() {
}
$h_scm_AbstractSeq.prototype = $c_scm_AbstractSeq.prototype;
function $is_scm_Buffer(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.scm_Buffer)));
}
function $as_scm_Buffer(obj) {
  return (($is_scm_Buffer(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.mutable.Buffer"));
}
function $isArrayOf_scm_Buffer(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_Buffer)));
}
function $asArrayOf_scm_Buffer(obj, depth) {
  return (($isArrayOf_scm_Buffer(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.mutable.Buffer;", depth));
}
function $as_sjsr_WrappedVarArgs(obj) {
  return ((false || (obj === null)) ? obj : $throwClassCastException(obj, "scala.scalajs.runtime.WrappedVarArgs"));
}
function $isArrayOf_sjsr_WrappedVarArgs(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sjsr_WrappedVarArgs)));
}
function $asArrayOf_sjsr_WrappedVarArgs(obj, depth) {
  return (($isArrayOf_sjsr_WrappedVarArgs(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.scalajs.runtime.WrappedVarArgs;", depth));
}
/** @constructor */
function $c_scm_AbstractBuffer() {
}
$c_scm_AbstractBuffer.prototype = new $h_scm_AbstractSeq();
$c_scm_AbstractBuffer.prototype.constructor = $c_scm_AbstractBuffer;
/** @constructor */
function $h_scm_AbstractBuffer() {
}
$h_scm_AbstractBuffer.prototype = $c_scm_AbstractBuffer.prototype;
$c_scm_AbstractBuffer.prototype.addAll__sc_IterableOnce__scm_Growable = (function(elems) {
  return $f_scm_Growable__addAll__sc_IterableOnce__scm_Growable(this, elems);
});
/** @constructor */
function $c_sci_ArraySeq() {
}
$c_sci_ArraySeq.prototype = new $h_sci_AbstractSeq();
$c_sci_ArraySeq.prototype.constructor = $c_sci_ArraySeq;
/** @constructor */
function $h_sci_ArraySeq() {
}
$h_sci_ArraySeq.prototype = $c_sci_ArraySeq.prototype;
$c_sci_ArraySeq.prototype.canEqual__O__Z = (function(that) {
  return $f_sci_IndexedSeq__canEqual__O__Z(this, that);
});
$c_sci_ArraySeq.prototype.sameElements__sc_IterableOnce__Z = (function(o) {
  return $f_sci_IndexedSeq__sameElements__sc_IterableOnce__Z(this, o);
});
$c_sci_ArraySeq.prototype.stringPrefix__T = (function() {
  return "IndexedSeq";
});
$c_sci_ArraySeq.prototype.lengthCompare__I__I = (function(len) {
  var x = this.length__I();
  return ((x === len) ? 0 : ((x < len) ? (-1) : 1));
});
$c_sci_ArraySeq.prototype.knownSize__I = (function() {
  return this.length__I();
});
$c_sci_ArraySeq.prototype.className__T = (function() {
  return "ArraySeq";
});
$c_sci_ArraySeq.prototype.applyPreferredMaxLength__I = (function() {
  return 2147483647;
});
function $as_sci_ArraySeq(obj) {
  return (((obj instanceof $c_sci_ArraySeq) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.ArraySeq"));
}
function $isArrayOf_sci_ArraySeq(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_ArraySeq)));
}
function $asArrayOf_sci_ArraySeq(obj, depth) {
  return (($isArrayOf_sci_ArraySeq(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.ArraySeq;", depth));
}
/** @constructor */
function $c_scm_ArraySeq() {
}
$c_scm_ArraySeq.prototype = new $h_scm_AbstractSeq();
$c_scm_ArraySeq.prototype.constructor = $c_scm_ArraySeq;
/** @constructor */
function $h_scm_ArraySeq() {
}
$h_scm_ArraySeq.prototype = $c_scm_ArraySeq.prototype;
$c_scm_ArraySeq.prototype.stringPrefix__T = (function() {
  return "IndexedSeq";
});
$c_scm_ArraySeq.prototype.lengthCompare__I__I = (function(len) {
  var x = this.length__I();
  return ((x === len) ? 0 : ((x < len) ? (-1) : 1));
});
$c_scm_ArraySeq.prototype.knownSize__I = (function() {
  return this.length__I();
});
$c_scm_ArraySeq.prototype.className__T = (function() {
  return "ArraySeq";
});
$c_scm_ArraySeq.prototype.equals__O__Z = (function(other) {
  if ((other instanceof $c_scm_ArraySeq)) {
    var x2 = $as_scm_ArraySeq(other);
    var xs = this.array__O();
    var $x_1 = $m_jl_reflect_Array$().getLength__O__I(xs);
    var xs$1 = $n(x2).array__O();
    if (($x_1 !== $m_jl_reflect_Array$().getLength__O__I(xs$1))) {
      return false;
    }
  }
  return $f_sc_Seq__equals__O__Z(this, other);
});
function $as_scm_ArraySeq(obj) {
  return (((obj instanceof $c_scm_ArraySeq) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.mutable.ArraySeq"));
}
function $isArrayOf_scm_ArraySeq(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_ArraySeq)));
}
function $asArrayOf_scm_ArraySeq(obj, depth) {
  return (($isArrayOf_scm_ArraySeq(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.mutable.ArraySeq;", depth));
}
/** @constructor */
function $c_sci_ArraySeq$ofBoolean(unsafeArray) {
  this.sci_ArraySeq$ofBoolean__f_unsafeArray = null;
  this.sci_ArraySeq$ofBoolean__f_unsafeArray = unsafeArray;
}
$c_sci_ArraySeq$ofBoolean.prototype = new $h_sci_ArraySeq();
$c_sci_ArraySeq$ofBoolean.prototype.constructor = $c_sci_ArraySeq$ofBoolean;
/** @constructor */
function $h_sci_ArraySeq$ofBoolean() {
}
$h_sci_ArraySeq$ofBoolean.prototype = $c_sci_ArraySeq$ofBoolean.prototype;
$c_sci_ArraySeq$ofBoolean.prototype.length__I = (function() {
  return $n(this.sci_ArraySeq$ofBoolean__f_unsafeArray).u.length;
});
$c_sci_ArraySeq$ofBoolean.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.sci_ArraySeq$ofBoolean__f_unsafeArray;
  return this$1.arrayHash$mZc$sp__AZ__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_sci_ArraySeq$ofBoolean.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_sci_ArraySeq$ofBoolean)) {
    var x2 = $as_sci_ArraySeq$ofBoolean(that);
    var a = this.sci_ArraySeq$ofBoolean__f_unsafeArray;
    var b = $n(x2).sci_ArraySeq$ofBoolean__f_unsafeArray;
    return $m_ju_Arrays$().equals__AZ__AZ__Z(a, b);
  } else {
    return $f_sc_Seq__equals__O__Z(this, that);
  }
});
$c_sci_ArraySeq$ofBoolean.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcZ$sp(this.sci_ArraySeq$ofBoolean__f_unsafeArray);
});
$c_sci_ArraySeq$ofBoolean.prototype.apply$mcZI$sp__I__Z = (function(i) {
  return $n(this.sci_ArraySeq$ofBoolean__f_unsafeArray).get(i);
});
$c_sci_ArraySeq$ofBoolean.prototype.apply__O__O = (function(v1) {
  var i = $uI(v1);
  return this.apply$mcZI$sp__I__Z(i);
});
$c_sci_ArraySeq$ofBoolean.prototype.apply__I__O = (function(i) {
  return this.apply$mcZI$sp__I__Z(i);
});
function $as_sci_ArraySeq$ofBoolean(obj) {
  return (((obj instanceof $c_sci_ArraySeq$ofBoolean) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.ArraySeq$ofBoolean"));
}
function $isArrayOf_sci_ArraySeq$ofBoolean(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_ArraySeq$ofBoolean)));
}
function $asArrayOf_sci_ArraySeq$ofBoolean(obj, depth) {
  return (($isArrayOf_sci_ArraySeq$ofBoolean(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.ArraySeq$ofBoolean;", depth));
}
var $d_sci_ArraySeq$ofBoolean = new $TypeData().initClass($c_sci_ArraySeq$ofBoolean, "scala.collection.immutable.ArraySeq$ofBoolean", ({
  sci_ArraySeq$ofBoolean: 1,
  sci_ArraySeq: 1,
  sci_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_SeqOps: 1,
  sci_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  sci_IndexedSeqOps: 1,
  sci_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  sc_EvidenceIterableFactoryDefaults: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sci_ArraySeq$ofByte(unsafeArray) {
  this.sci_ArraySeq$ofByte__f_unsafeArray = null;
  this.sci_ArraySeq$ofByte__f_unsafeArray = unsafeArray;
}
$c_sci_ArraySeq$ofByte.prototype = new $h_sci_ArraySeq();
$c_sci_ArraySeq$ofByte.prototype.constructor = $c_sci_ArraySeq$ofByte;
/** @constructor */
function $h_sci_ArraySeq$ofByte() {
}
$h_sci_ArraySeq$ofByte.prototype = $c_sci_ArraySeq$ofByte.prototype;
$c_sci_ArraySeq$ofByte.prototype.length__I = (function() {
  return $n(this.sci_ArraySeq$ofByte__f_unsafeArray).u.length;
});
$c_sci_ArraySeq$ofByte.prototype.apply__I__B = (function(i) {
  return $n(this.sci_ArraySeq$ofByte__f_unsafeArray).get(i);
});
$c_sci_ArraySeq$ofByte.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.sci_ArraySeq$ofByte__f_unsafeArray;
  return this$1.arrayHash$mBc$sp__AB__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_sci_ArraySeq$ofByte.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_sci_ArraySeq$ofByte)) {
    var x2 = $as_sci_ArraySeq$ofByte(that);
    var a = this.sci_ArraySeq$ofByte__f_unsafeArray;
    var b = $n(x2).sci_ArraySeq$ofByte__f_unsafeArray;
    return $m_ju_Arrays$().equals__AB__AB__Z(a, b);
  } else {
    return $f_sc_Seq__equals__O__Z(this, that);
  }
});
$c_sci_ArraySeq$ofByte.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcB$sp(this.sci_ArraySeq$ofByte__f_unsafeArray);
});
$c_sci_ArraySeq$ofByte.prototype.apply__O__O = (function(v1) {
  return this.apply__I__B($uI(v1));
});
$c_sci_ArraySeq$ofByte.prototype.apply__I__O = (function(i) {
  return this.apply__I__B(i);
});
function $as_sci_ArraySeq$ofByte(obj) {
  return (((obj instanceof $c_sci_ArraySeq$ofByte) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.ArraySeq$ofByte"));
}
function $isArrayOf_sci_ArraySeq$ofByte(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_ArraySeq$ofByte)));
}
function $asArrayOf_sci_ArraySeq$ofByte(obj, depth) {
  return (($isArrayOf_sci_ArraySeq$ofByte(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.ArraySeq$ofByte;", depth));
}
var $d_sci_ArraySeq$ofByte = new $TypeData().initClass($c_sci_ArraySeq$ofByte, "scala.collection.immutable.ArraySeq$ofByte", ({
  sci_ArraySeq$ofByte: 1,
  sci_ArraySeq: 1,
  sci_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_SeqOps: 1,
  sci_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  sci_IndexedSeqOps: 1,
  sci_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  sc_EvidenceIterableFactoryDefaults: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sci_ArraySeq$ofChar(unsafeArray) {
  this.sci_ArraySeq$ofChar__f_unsafeArray = null;
  this.sci_ArraySeq$ofChar__f_unsafeArray = unsafeArray;
}
$c_sci_ArraySeq$ofChar.prototype = new $h_sci_ArraySeq();
$c_sci_ArraySeq$ofChar.prototype.constructor = $c_sci_ArraySeq$ofChar;
/** @constructor */
function $h_sci_ArraySeq$ofChar() {
}
$h_sci_ArraySeq$ofChar.prototype = $c_sci_ArraySeq$ofChar.prototype;
$c_sci_ArraySeq$ofChar.prototype.length__I = (function() {
  return $n(this.sci_ArraySeq$ofChar__f_unsafeArray).u.length;
});
$c_sci_ArraySeq$ofChar.prototype.apply__I__C = (function(i) {
  return $n(this.sci_ArraySeq$ofChar__f_unsafeArray).get(i);
});
$c_sci_ArraySeq$ofChar.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.sci_ArraySeq$ofChar__f_unsafeArray;
  return this$1.arrayHash$mCc$sp__AC__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_sci_ArraySeq$ofChar.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_sci_ArraySeq$ofChar)) {
    var x2 = $as_sci_ArraySeq$ofChar(that);
    var a = this.sci_ArraySeq$ofChar__f_unsafeArray;
    var b = $n(x2).sci_ArraySeq$ofChar__f_unsafeArray;
    return $m_ju_Arrays$().equals__AC__AC__Z(a, b);
  } else {
    return $f_sc_Seq__equals__O__Z(this, that);
  }
});
$c_sci_ArraySeq$ofChar.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcC$sp(this.sci_ArraySeq$ofChar__f_unsafeArray);
});
$c_sci_ArraySeq$ofChar.prototype.addString__scm_StringBuilder__T__T__T__scm_StringBuilder = (function(sb, start, sep, end) {
  return new $c_scm_ArraySeq$ofChar(this.sci_ArraySeq$ofChar__f_unsafeArray).addString__scm_StringBuilder__T__T__T__scm_StringBuilder(sb, start, sep, end);
});
$c_sci_ArraySeq$ofChar.prototype.apply__O__O = (function(v1) {
  return $bC(this.apply__I__C($uI(v1)));
});
$c_sci_ArraySeq$ofChar.prototype.apply__I__O = (function(i) {
  return $bC(this.apply__I__C(i));
});
function $as_sci_ArraySeq$ofChar(obj) {
  return (((obj instanceof $c_sci_ArraySeq$ofChar) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.ArraySeq$ofChar"));
}
function $isArrayOf_sci_ArraySeq$ofChar(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_ArraySeq$ofChar)));
}
function $asArrayOf_sci_ArraySeq$ofChar(obj, depth) {
  return (($isArrayOf_sci_ArraySeq$ofChar(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.ArraySeq$ofChar;", depth));
}
var $d_sci_ArraySeq$ofChar = new $TypeData().initClass($c_sci_ArraySeq$ofChar, "scala.collection.immutable.ArraySeq$ofChar", ({
  sci_ArraySeq$ofChar: 1,
  sci_ArraySeq: 1,
  sci_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_SeqOps: 1,
  sci_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  sci_IndexedSeqOps: 1,
  sci_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  sc_EvidenceIterableFactoryDefaults: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sci_ArraySeq$ofDouble(unsafeArray) {
  this.sci_ArraySeq$ofDouble__f_unsafeArray = null;
  this.sci_ArraySeq$ofDouble__f_unsafeArray = unsafeArray;
}
$c_sci_ArraySeq$ofDouble.prototype = new $h_sci_ArraySeq();
$c_sci_ArraySeq$ofDouble.prototype.constructor = $c_sci_ArraySeq$ofDouble;
/** @constructor */
function $h_sci_ArraySeq$ofDouble() {
}
$h_sci_ArraySeq$ofDouble.prototype = $c_sci_ArraySeq$ofDouble.prototype;
$c_sci_ArraySeq$ofDouble.prototype.length__I = (function() {
  return $n(this.sci_ArraySeq$ofDouble__f_unsafeArray).u.length;
});
$c_sci_ArraySeq$ofDouble.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.sci_ArraySeq$ofDouble__f_unsafeArray;
  return this$1.arrayHash$mDc$sp__AD__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_sci_ArraySeq$ofDouble.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_sci_ArraySeq$ofDouble)) {
    var x2 = $as_sci_ArraySeq$ofDouble(that);
    var a = this.sci_ArraySeq$ofDouble__f_unsafeArray;
    var b = $n(x2).sci_ArraySeq$ofDouble__f_unsafeArray;
    return $m_ju_Arrays$().equals__AD__AD__Z(a, b);
  } else {
    return $f_sc_Seq__equals__O__Z(this, that);
  }
});
$c_sci_ArraySeq$ofDouble.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcD$sp(this.sci_ArraySeq$ofDouble__f_unsafeArray);
});
$c_sci_ArraySeq$ofDouble.prototype.apply$mcDI$sp__I__D = (function(i) {
  return $n(this.sci_ArraySeq$ofDouble__f_unsafeArray).get(i);
});
$c_sci_ArraySeq$ofDouble.prototype.apply__O__O = (function(v1) {
  var i = $uI(v1);
  return this.apply$mcDI$sp__I__D(i);
});
$c_sci_ArraySeq$ofDouble.prototype.apply__I__O = (function(i) {
  return this.apply$mcDI$sp__I__D(i);
});
function $as_sci_ArraySeq$ofDouble(obj) {
  return (((obj instanceof $c_sci_ArraySeq$ofDouble) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.ArraySeq$ofDouble"));
}
function $isArrayOf_sci_ArraySeq$ofDouble(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_ArraySeq$ofDouble)));
}
function $asArrayOf_sci_ArraySeq$ofDouble(obj, depth) {
  return (($isArrayOf_sci_ArraySeq$ofDouble(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.ArraySeq$ofDouble;", depth));
}
var $d_sci_ArraySeq$ofDouble = new $TypeData().initClass($c_sci_ArraySeq$ofDouble, "scala.collection.immutable.ArraySeq$ofDouble", ({
  sci_ArraySeq$ofDouble: 1,
  sci_ArraySeq: 1,
  sci_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_SeqOps: 1,
  sci_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  sci_IndexedSeqOps: 1,
  sci_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  sc_EvidenceIterableFactoryDefaults: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sci_ArraySeq$ofFloat(unsafeArray) {
  this.sci_ArraySeq$ofFloat__f_unsafeArray = null;
  this.sci_ArraySeq$ofFloat__f_unsafeArray = unsafeArray;
}
$c_sci_ArraySeq$ofFloat.prototype = new $h_sci_ArraySeq();
$c_sci_ArraySeq$ofFloat.prototype.constructor = $c_sci_ArraySeq$ofFloat;
/** @constructor */
function $h_sci_ArraySeq$ofFloat() {
}
$h_sci_ArraySeq$ofFloat.prototype = $c_sci_ArraySeq$ofFloat.prototype;
$c_sci_ArraySeq$ofFloat.prototype.length__I = (function() {
  return $n(this.sci_ArraySeq$ofFloat__f_unsafeArray).u.length;
});
$c_sci_ArraySeq$ofFloat.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.sci_ArraySeq$ofFloat__f_unsafeArray;
  return this$1.arrayHash$mFc$sp__AF__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_sci_ArraySeq$ofFloat.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_sci_ArraySeq$ofFloat)) {
    var x2 = $as_sci_ArraySeq$ofFloat(that);
    var a = this.sci_ArraySeq$ofFloat__f_unsafeArray;
    var b = $n(x2).sci_ArraySeq$ofFloat__f_unsafeArray;
    return $m_ju_Arrays$().equals__AF__AF__Z(a, b);
  } else {
    return $f_sc_Seq__equals__O__Z(this, that);
  }
});
$c_sci_ArraySeq$ofFloat.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcF$sp(this.sci_ArraySeq$ofFloat__f_unsafeArray);
});
$c_sci_ArraySeq$ofFloat.prototype.apply$mcFI$sp__I__F = (function(i) {
  return $n(this.sci_ArraySeq$ofFloat__f_unsafeArray).get(i);
});
$c_sci_ArraySeq$ofFloat.prototype.apply__O__O = (function(v1) {
  var i = $uI(v1);
  return this.apply$mcFI$sp__I__F(i);
});
$c_sci_ArraySeq$ofFloat.prototype.apply__I__O = (function(i) {
  return this.apply$mcFI$sp__I__F(i);
});
function $as_sci_ArraySeq$ofFloat(obj) {
  return (((obj instanceof $c_sci_ArraySeq$ofFloat) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.ArraySeq$ofFloat"));
}
function $isArrayOf_sci_ArraySeq$ofFloat(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_ArraySeq$ofFloat)));
}
function $asArrayOf_sci_ArraySeq$ofFloat(obj, depth) {
  return (($isArrayOf_sci_ArraySeq$ofFloat(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.ArraySeq$ofFloat;", depth));
}
var $d_sci_ArraySeq$ofFloat = new $TypeData().initClass($c_sci_ArraySeq$ofFloat, "scala.collection.immutable.ArraySeq$ofFloat", ({
  sci_ArraySeq$ofFloat: 1,
  sci_ArraySeq: 1,
  sci_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_SeqOps: 1,
  sci_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  sci_IndexedSeqOps: 1,
  sci_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  sc_EvidenceIterableFactoryDefaults: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sci_ArraySeq$ofInt(unsafeArray) {
  this.sci_ArraySeq$ofInt__f_unsafeArray = null;
  this.sci_ArraySeq$ofInt__f_unsafeArray = unsafeArray;
}
$c_sci_ArraySeq$ofInt.prototype = new $h_sci_ArraySeq();
$c_sci_ArraySeq$ofInt.prototype.constructor = $c_sci_ArraySeq$ofInt;
/** @constructor */
function $h_sci_ArraySeq$ofInt() {
}
$h_sci_ArraySeq$ofInt.prototype = $c_sci_ArraySeq$ofInt.prototype;
$c_sci_ArraySeq$ofInt.prototype.length__I = (function() {
  return $n(this.sci_ArraySeq$ofInt__f_unsafeArray).u.length;
});
$c_sci_ArraySeq$ofInt.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.sci_ArraySeq$ofInt__f_unsafeArray;
  return this$1.arrayHash$mIc$sp__AI__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_sci_ArraySeq$ofInt.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_sci_ArraySeq$ofInt)) {
    var x2 = $as_sci_ArraySeq$ofInt(that);
    var a = this.sci_ArraySeq$ofInt__f_unsafeArray;
    var b = $n(x2).sci_ArraySeq$ofInt__f_unsafeArray;
    return $m_ju_Arrays$().equals__AI__AI__Z(a, b);
  } else {
    return $f_sc_Seq__equals__O__Z(this, that);
  }
});
$c_sci_ArraySeq$ofInt.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcI$sp(this.sci_ArraySeq$ofInt__f_unsafeArray);
});
$c_sci_ArraySeq$ofInt.prototype.apply$mcII$sp__I__I = (function(i) {
  return $n(this.sci_ArraySeq$ofInt__f_unsafeArray).get(i);
});
$c_sci_ArraySeq$ofInt.prototype.apply__O__O = (function(v1) {
  var i = $uI(v1);
  return this.apply$mcII$sp__I__I(i);
});
$c_sci_ArraySeq$ofInt.prototype.apply__I__O = (function(i) {
  return this.apply$mcII$sp__I__I(i);
});
function $as_sci_ArraySeq$ofInt(obj) {
  return (((obj instanceof $c_sci_ArraySeq$ofInt) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.ArraySeq$ofInt"));
}
function $isArrayOf_sci_ArraySeq$ofInt(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_ArraySeq$ofInt)));
}
function $asArrayOf_sci_ArraySeq$ofInt(obj, depth) {
  return (($isArrayOf_sci_ArraySeq$ofInt(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.ArraySeq$ofInt;", depth));
}
var $d_sci_ArraySeq$ofInt = new $TypeData().initClass($c_sci_ArraySeq$ofInt, "scala.collection.immutable.ArraySeq$ofInt", ({
  sci_ArraySeq$ofInt: 1,
  sci_ArraySeq: 1,
  sci_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_SeqOps: 1,
  sci_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  sci_IndexedSeqOps: 1,
  sci_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  sc_EvidenceIterableFactoryDefaults: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sci_ArraySeq$ofLong(unsafeArray) {
  this.sci_ArraySeq$ofLong__f_unsafeArray = null;
  this.sci_ArraySeq$ofLong__f_unsafeArray = unsafeArray;
}
$c_sci_ArraySeq$ofLong.prototype = new $h_sci_ArraySeq();
$c_sci_ArraySeq$ofLong.prototype.constructor = $c_sci_ArraySeq$ofLong;
/** @constructor */
function $h_sci_ArraySeq$ofLong() {
}
$h_sci_ArraySeq$ofLong.prototype = $c_sci_ArraySeq$ofLong.prototype;
$c_sci_ArraySeq$ofLong.prototype.length__I = (function() {
  return $n(this.sci_ArraySeq$ofLong__f_unsafeArray).u.length;
});
$c_sci_ArraySeq$ofLong.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.sci_ArraySeq$ofLong__f_unsafeArray;
  return this$1.arrayHash$mJc$sp__AJ__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_sci_ArraySeq$ofLong.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_sci_ArraySeq$ofLong)) {
    var x2 = $as_sci_ArraySeq$ofLong(that);
    var a = this.sci_ArraySeq$ofLong__f_unsafeArray;
    var b = $n(x2).sci_ArraySeq$ofLong__f_unsafeArray;
    return $m_ju_Arrays$().equals__AJ__AJ__Z(a, b);
  } else {
    return $f_sc_Seq__equals__O__Z(this, that);
  }
});
$c_sci_ArraySeq$ofLong.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcJ$sp(this.sci_ArraySeq$ofLong__f_unsafeArray);
});
$c_sci_ArraySeq$ofLong.prototype.apply$mcJI$sp__I__J = (function(i) {
  return $n(this.sci_ArraySeq$ofLong__f_unsafeArray).get(i);
});
$c_sci_ArraySeq$ofLong.prototype.apply__O__O = (function(v1) {
  var i = $uI(v1);
  return this.apply$mcJI$sp__I__J(i);
});
$c_sci_ArraySeq$ofLong.prototype.apply__I__O = (function(i) {
  return this.apply$mcJI$sp__I__J(i);
});
function $as_sci_ArraySeq$ofLong(obj) {
  return (((obj instanceof $c_sci_ArraySeq$ofLong) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.ArraySeq$ofLong"));
}
function $isArrayOf_sci_ArraySeq$ofLong(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_ArraySeq$ofLong)));
}
function $asArrayOf_sci_ArraySeq$ofLong(obj, depth) {
  return (($isArrayOf_sci_ArraySeq$ofLong(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.ArraySeq$ofLong;", depth));
}
var $d_sci_ArraySeq$ofLong = new $TypeData().initClass($c_sci_ArraySeq$ofLong, "scala.collection.immutable.ArraySeq$ofLong", ({
  sci_ArraySeq$ofLong: 1,
  sci_ArraySeq: 1,
  sci_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_SeqOps: 1,
  sci_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  sci_IndexedSeqOps: 1,
  sci_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  sc_EvidenceIterableFactoryDefaults: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sci_ArraySeq$ofRef(unsafeArray) {
  this.sci_ArraySeq$ofRef__f_unsafeArray = null;
  this.sci_ArraySeq$ofRef__f_unsafeArray = unsafeArray;
}
$c_sci_ArraySeq$ofRef.prototype = new $h_sci_ArraySeq();
$c_sci_ArraySeq$ofRef.prototype.constructor = $c_sci_ArraySeq$ofRef;
/** @constructor */
function $h_sci_ArraySeq$ofRef() {
}
$h_sci_ArraySeq$ofRef.prototype = $c_sci_ArraySeq$ofRef.prototype;
$c_sci_ArraySeq$ofRef.prototype.length__I = (function() {
  return $n(this.sci_ArraySeq$ofRef__f_unsafeArray).u.length;
});
$c_sci_ArraySeq$ofRef.prototype.apply__I__O = (function(i) {
  return $n(this.sci_ArraySeq$ofRef__f_unsafeArray).get(i);
});
$c_sci_ArraySeq$ofRef.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.sci_ArraySeq$ofRef__f_unsafeArray;
  return this$1.arrayHash__O__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_sci_ArraySeq$ofRef.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_sci_ArraySeq$ofRef)) {
    var x2 = $as_sci_ArraySeq$ofRef(that);
    return $m_s_Array$().equals__AO__AO__Z(this.sci_ArraySeq$ofRef__f_unsafeArray, $n(x2).sci_ArraySeq$ofRef__f_unsafeArray);
  } else {
    return $f_sc_Seq__equals__O__Z(this, that);
  }
});
$c_sci_ArraySeq$ofRef.prototype.iterator__sc_Iterator = (function() {
  return $ct_sc_ArrayOps$ArrayIterator__O__(new $c_sc_ArrayOps$ArrayIterator(), this.sci_ArraySeq$ofRef__f_unsafeArray);
});
$c_sci_ArraySeq$ofRef.prototype.apply__O__O = (function(v1) {
  return this.apply__I__O($uI(v1));
});
function $as_sci_ArraySeq$ofRef(obj) {
  return (((obj instanceof $c_sci_ArraySeq$ofRef) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.ArraySeq$ofRef"));
}
function $isArrayOf_sci_ArraySeq$ofRef(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_ArraySeq$ofRef)));
}
function $asArrayOf_sci_ArraySeq$ofRef(obj, depth) {
  return (($isArrayOf_sci_ArraySeq$ofRef(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.ArraySeq$ofRef;", depth));
}
var $d_sci_ArraySeq$ofRef = new $TypeData().initClass($c_sci_ArraySeq$ofRef, "scala.collection.immutable.ArraySeq$ofRef", ({
  sci_ArraySeq$ofRef: 1,
  sci_ArraySeq: 1,
  sci_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_SeqOps: 1,
  sci_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  sci_IndexedSeqOps: 1,
  sci_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  sc_EvidenceIterableFactoryDefaults: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sci_ArraySeq$ofShort(unsafeArray) {
  this.sci_ArraySeq$ofShort__f_unsafeArray = null;
  this.sci_ArraySeq$ofShort__f_unsafeArray = unsafeArray;
}
$c_sci_ArraySeq$ofShort.prototype = new $h_sci_ArraySeq();
$c_sci_ArraySeq$ofShort.prototype.constructor = $c_sci_ArraySeq$ofShort;
/** @constructor */
function $h_sci_ArraySeq$ofShort() {
}
$h_sci_ArraySeq$ofShort.prototype = $c_sci_ArraySeq$ofShort.prototype;
$c_sci_ArraySeq$ofShort.prototype.length__I = (function() {
  return $n(this.sci_ArraySeq$ofShort__f_unsafeArray).u.length;
});
$c_sci_ArraySeq$ofShort.prototype.apply__I__S = (function(i) {
  return $n(this.sci_ArraySeq$ofShort__f_unsafeArray).get(i);
});
$c_sci_ArraySeq$ofShort.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.sci_ArraySeq$ofShort__f_unsafeArray;
  return this$1.arrayHash$mSc$sp__AS__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_sci_ArraySeq$ofShort.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_sci_ArraySeq$ofShort)) {
    var x2 = $as_sci_ArraySeq$ofShort(that);
    var a = this.sci_ArraySeq$ofShort__f_unsafeArray;
    var b = $n(x2).sci_ArraySeq$ofShort__f_unsafeArray;
    return $m_ju_Arrays$().equals__AS__AS__Z(a, b);
  } else {
    return $f_sc_Seq__equals__O__Z(this, that);
  }
});
$c_sci_ArraySeq$ofShort.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcS$sp(this.sci_ArraySeq$ofShort__f_unsafeArray);
});
$c_sci_ArraySeq$ofShort.prototype.apply__O__O = (function(v1) {
  return this.apply__I__S($uI(v1));
});
$c_sci_ArraySeq$ofShort.prototype.apply__I__O = (function(i) {
  return this.apply__I__S(i);
});
function $as_sci_ArraySeq$ofShort(obj) {
  return (((obj instanceof $c_sci_ArraySeq$ofShort) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.ArraySeq$ofShort"));
}
function $isArrayOf_sci_ArraySeq$ofShort(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_ArraySeq$ofShort)));
}
function $asArrayOf_sci_ArraySeq$ofShort(obj, depth) {
  return (($isArrayOf_sci_ArraySeq$ofShort(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.ArraySeq$ofShort;", depth));
}
var $d_sci_ArraySeq$ofShort = new $TypeData().initClass($c_sci_ArraySeq$ofShort, "scala.collection.immutable.ArraySeq$ofShort", ({
  sci_ArraySeq$ofShort: 1,
  sci_ArraySeq: 1,
  sci_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_SeqOps: 1,
  sci_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  sci_IndexedSeqOps: 1,
  sci_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  sc_EvidenceIterableFactoryDefaults: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sci_ArraySeq$ofUnit(unsafeArray) {
  this.sci_ArraySeq$ofUnit__f_unsafeArray = null;
  this.sci_ArraySeq$ofUnit__f_unsafeArray = unsafeArray;
}
$c_sci_ArraySeq$ofUnit.prototype = new $h_sci_ArraySeq();
$c_sci_ArraySeq$ofUnit.prototype.constructor = $c_sci_ArraySeq$ofUnit;
/** @constructor */
function $h_sci_ArraySeq$ofUnit() {
}
$h_sci_ArraySeq$ofUnit.prototype = $c_sci_ArraySeq$ofUnit.prototype;
$c_sci_ArraySeq$ofUnit.prototype.length__I = (function() {
  return $n(this.sci_ArraySeq$ofUnit__f_unsafeArray).u.length;
});
$c_sci_ArraySeq$ofUnit.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.sci_ArraySeq$ofUnit__f_unsafeArray;
  return this$1.arrayHash$mVc$sp__Ajl_Void__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_sci_ArraySeq$ofUnit.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_sci_ArraySeq$ofUnit)) {
    var x2 = $as_sci_ArraySeq$ofUnit(that);
    return ($n(this.sci_ArraySeq$ofUnit__f_unsafeArray).u.length === $n($n(x2).sci_ArraySeq$ofUnit__f_unsafeArray).u.length);
  } else {
    return $f_sc_Seq__equals__O__Z(this, that);
  }
});
$c_sci_ArraySeq$ofUnit.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcV$sp(this.sci_ArraySeq$ofUnit__f_unsafeArray);
});
$c_sci_ArraySeq$ofUnit.prototype.apply$mcVI$sp__I__V = (function(i) {
  $n(this.sci_ArraySeq$ofUnit__f_unsafeArray).get(i);
});
$c_sci_ArraySeq$ofUnit.prototype.apply__O__O = (function(v1) {
  var i = $uI(v1);
  this.apply$mcVI$sp__I__V(i);
});
$c_sci_ArraySeq$ofUnit.prototype.apply__I__O = (function(i) {
  this.apply$mcVI$sp__I__V(i);
});
function $as_sci_ArraySeq$ofUnit(obj) {
  return (((obj instanceof $c_sci_ArraySeq$ofUnit) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.ArraySeq$ofUnit"));
}
function $isArrayOf_sci_ArraySeq$ofUnit(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_ArraySeq$ofUnit)));
}
function $asArrayOf_sci_ArraySeq$ofUnit(obj, depth) {
  return (($isArrayOf_sci_ArraySeq$ofUnit(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.ArraySeq$ofUnit;", depth));
}
var $d_sci_ArraySeq$ofUnit = new $TypeData().initClass($c_sci_ArraySeq$ofUnit, "scala.collection.immutable.ArraySeq$ofUnit", ({
  sci_ArraySeq$ofUnit: 1,
  sci_ArraySeq: 1,
  sci_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_SeqOps: 1,
  sci_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  sci_IndexedSeqOps: 1,
  sci_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  sc_EvidenceIterableFactoryDefaults: 1,
  Ljava_io_Serializable: 1
}));
function $p_sci_List__loop$2__I__sci_List__I__I($thiz, i, xs, len$1) {
  while (true) {
    if ((i === len$1)) {
      return ($n(xs).isEmpty__Z() ? 0 : 1);
    } else if ($n(xs).isEmpty__Z()) {
      return (-1);
    } else {
      var temp$i = ((1 + i) | 0);
      var temp$xs = $as_sci_List($n(xs).tail__O());
      i = temp$i;
      xs = temp$xs;
    }
  }
}
function $p_sci_List__listEq$1__sci_List__sci_List__Z($thiz, a, b) {
  while (true) {
    if ((a === b)) {
      return true;
    } else {
      var aEmpty = $n(a).isEmpty__Z();
      var bEmpty = $n(b).isEmpty__Z();
      if (((!(aEmpty || bEmpty)) && $m_sr_BoxesRunTime$().equals__O__O__Z($n(a).head__O(), $n(b).head__O()))) {
        var temp$a = $as_sci_List($n(a).tail__O());
        var temp$b = $as_sci_List($n(b).tail__O());
        a = temp$a;
        b = temp$b;
      } else {
        return (aEmpty && bEmpty);
      }
    }
  }
}
/** @constructor */
function $c_sci_List() {
}
$c_sci_List.prototype = new $h_sci_AbstractSeq();
$c_sci_List.prototype.constructor = $c_sci_List;
/** @constructor */
function $h_sci_List() {
}
$h_sci_List.prototype = $c_sci_List.prototype;
$c_sci_List.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_StrictOptimizedLinearSeqOps$$anon$1(this);
});
$c_sci_List.prototype.stringPrefix__T = (function() {
  return "LinearSeq";
});
$c_sci_List.prototype.apply__I__O = (function(n) {
  return $f_sc_LinearSeqOps__apply__I__O(this, n);
});
$c_sci_List.prototype.sameElements__sc_IterableOnce__Z = (function(that) {
  return $f_sc_LinearSeqOps__sameElements__sc_IterableOnce__Z(this, that);
});
$c_sci_List.prototype.isEmpty__Z = (function() {
  return (this === $m_sci_Nil$());
});
$c_sci_List.prototype.foreach__F1__V = (function(f) {
  var these = this;
  while ((!$n(these).isEmpty__Z())) {
    $n(f).apply__O__O($n(these).head__O());
    these = $as_sci_List($n(these).tail__O());
  }
});
$c_sci_List.prototype.length__I = (function() {
  var these = this;
  var len = 0;
  while ((!$n(these).isEmpty__Z())) {
    len = ((1 + len) | 0);
    these = $as_sci_List($n(these).tail__O());
  }
  return len;
});
$c_sci_List.prototype.lengthCompare__I__I = (function(len) {
  return ((len < 0) ? 1 : $p_sci_List__loop$2__I__sci_List__I__I(this, 0, this, len));
});
$c_sci_List.prototype.className__T = (function() {
  return "List";
});
$c_sci_List.prototype.equals__O__Z = (function(o) {
  if ((o instanceof $c_sci_List)) {
    var x2 = $as_sci_List(o);
    return $p_sci_List__listEq$1__sci_List__sci_List__Z(this, this, x2);
  } else {
    return $f_sc_Seq__equals__O__Z(this, o);
  }
});
$c_sci_List.prototype.apply__O__O = (function(v1) {
  var n = $uI(v1);
  return $f_sc_LinearSeqOps__apply__I__O(this, n);
});
$c_sci_List.prototype.drop__I__O = (function(n) {
  return $p_sc_StrictOptimizedLinearSeqOps__loop$2__I__sc_LinearSeq__sc_LinearSeq(this, n, this);
});
function $as_sci_List(obj) {
  return (((obj instanceof $c_sci_List) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.List"));
}
function $isArrayOf_sci_List(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_List)));
}
function $asArrayOf_sci_List(obj, depth) {
  return (($isArrayOf_sci_List(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.List;", depth));
}
/** @constructor */
function $c_scm_ArraySeq$ofBoolean(array) {
  this.scm_ArraySeq$ofBoolean__f_array = null;
  this.scm_ArraySeq$ofBoolean__f_array = array;
}
$c_scm_ArraySeq$ofBoolean.prototype = new $h_scm_ArraySeq();
$c_scm_ArraySeq$ofBoolean.prototype.constructor = $c_scm_ArraySeq$ofBoolean;
/** @constructor */
function $h_scm_ArraySeq$ofBoolean() {
}
$h_scm_ArraySeq$ofBoolean.prototype = $c_scm_ArraySeq$ofBoolean.prototype;
$c_scm_ArraySeq$ofBoolean.prototype.length__I = (function() {
  return $n(this.scm_ArraySeq$ofBoolean__f_array).u.length;
});
$c_scm_ArraySeq$ofBoolean.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.scm_ArraySeq$ofBoolean__f_array;
  return this$1.arrayHash$mZc$sp__AZ__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_scm_ArraySeq$ofBoolean.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_scm_ArraySeq$ofBoolean)) {
    var x2 = $as_scm_ArraySeq$ofBoolean(that);
    var a = this.scm_ArraySeq$ofBoolean__f_array;
    var b = $n(x2).scm_ArraySeq$ofBoolean__f_array;
    return $m_ju_Arrays$().equals__AZ__AZ__Z(a, b);
  } else {
    return $c_scm_ArraySeq.prototype.equals__O__Z.call(this, that);
  }
});
$c_scm_ArraySeq$ofBoolean.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcZ$sp(this.scm_ArraySeq$ofBoolean__f_array);
});
$c_scm_ArraySeq$ofBoolean.prototype.apply$mcZI$sp__I__Z = (function(index) {
  return $n(this.scm_ArraySeq$ofBoolean__f_array).get(index);
});
$c_scm_ArraySeq$ofBoolean.prototype.apply__O__O = (function(v1) {
  var index = $uI(v1);
  return this.apply$mcZI$sp__I__Z(index);
});
$c_scm_ArraySeq$ofBoolean.prototype.apply__I__O = (function(i) {
  return this.apply$mcZI$sp__I__Z(i);
});
$c_scm_ArraySeq$ofBoolean.prototype.array__O = (function() {
  return this.scm_ArraySeq$ofBoolean__f_array;
});
function $as_scm_ArraySeq$ofBoolean(obj) {
  return (((obj instanceof $c_scm_ArraySeq$ofBoolean) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.mutable.ArraySeq$ofBoolean"));
}
function $isArrayOf_scm_ArraySeq$ofBoolean(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_ArraySeq$ofBoolean)));
}
function $asArrayOf_scm_ArraySeq$ofBoolean(obj, depth) {
  return (($isArrayOf_scm_ArraySeq$ofBoolean(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.mutable.ArraySeq$ofBoolean;", depth));
}
var $d_scm_ArraySeq$ofBoolean = new $TypeData().initClass($c_scm_ArraySeq$ofBoolean, "scala.collection.mutable.ArraySeq$ofBoolean", ({
  scm_ArraySeq$ofBoolean: 1,
  scm_ArraySeq: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_SeqOps: 1,
  scm_Cloneable: 1,
  jl_Cloneable: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  scm_IndexedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_scm_ArraySeq$ofByte(array) {
  this.scm_ArraySeq$ofByte__f_array = null;
  this.scm_ArraySeq$ofByte__f_array = array;
}
$c_scm_ArraySeq$ofByte.prototype = new $h_scm_ArraySeq();
$c_scm_ArraySeq$ofByte.prototype.constructor = $c_scm_ArraySeq$ofByte;
/** @constructor */
function $h_scm_ArraySeq$ofByte() {
}
$h_scm_ArraySeq$ofByte.prototype = $c_scm_ArraySeq$ofByte.prototype;
$c_scm_ArraySeq$ofByte.prototype.length__I = (function() {
  return $n(this.scm_ArraySeq$ofByte__f_array).u.length;
});
$c_scm_ArraySeq$ofByte.prototype.apply__I__B = (function(index) {
  return $n(this.scm_ArraySeq$ofByte__f_array).get(index);
});
$c_scm_ArraySeq$ofByte.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.scm_ArraySeq$ofByte__f_array;
  return this$1.arrayHash$mBc$sp__AB__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_scm_ArraySeq$ofByte.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_scm_ArraySeq$ofByte)) {
    var x2 = $as_scm_ArraySeq$ofByte(that);
    var a = this.scm_ArraySeq$ofByte__f_array;
    var b = $n(x2).scm_ArraySeq$ofByte__f_array;
    return $m_ju_Arrays$().equals__AB__AB__Z(a, b);
  } else {
    return $c_scm_ArraySeq.prototype.equals__O__Z.call(this, that);
  }
});
$c_scm_ArraySeq$ofByte.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcB$sp(this.scm_ArraySeq$ofByte__f_array);
});
$c_scm_ArraySeq$ofByte.prototype.apply__O__O = (function(v1) {
  return this.apply__I__B($uI(v1));
});
$c_scm_ArraySeq$ofByte.prototype.apply__I__O = (function(i) {
  return this.apply__I__B(i);
});
$c_scm_ArraySeq$ofByte.prototype.array__O = (function() {
  return this.scm_ArraySeq$ofByte__f_array;
});
function $as_scm_ArraySeq$ofByte(obj) {
  return (((obj instanceof $c_scm_ArraySeq$ofByte) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.mutable.ArraySeq$ofByte"));
}
function $isArrayOf_scm_ArraySeq$ofByte(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_ArraySeq$ofByte)));
}
function $asArrayOf_scm_ArraySeq$ofByte(obj, depth) {
  return (($isArrayOf_scm_ArraySeq$ofByte(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.mutable.ArraySeq$ofByte;", depth));
}
var $d_scm_ArraySeq$ofByte = new $TypeData().initClass($c_scm_ArraySeq$ofByte, "scala.collection.mutable.ArraySeq$ofByte", ({
  scm_ArraySeq$ofByte: 1,
  scm_ArraySeq: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_SeqOps: 1,
  scm_Cloneable: 1,
  jl_Cloneable: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  scm_IndexedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_scm_ArraySeq$ofChar(array) {
  this.scm_ArraySeq$ofChar__f_array = null;
  this.scm_ArraySeq$ofChar__f_array = array;
}
$c_scm_ArraySeq$ofChar.prototype = new $h_scm_ArraySeq();
$c_scm_ArraySeq$ofChar.prototype.constructor = $c_scm_ArraySeq$ofChar;
/** @constructor */
function $h_scm_ArraySeq$ofChar() {
}
$h_scm_ArraySeq$ofChar.prototype = $c_scm_ArraySeq$ofChar.prototype;
$c_scm_ArraySeq$ofChar.prototype.length__I = (function() {
  return $n(this.scm_ArraySeq$ofChar__f_array).u.length;
});
$c_scm_ArraySeq$ofChar.prototype.apply__I__C = (function(index) {
  return $n(this.scm_ArraySeq$ofChar__f_array).get(index);
});
$c_scm_ArraySeq$ofChar.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.scm_ArraySeq$ofChar__f_array;
  return this$1.arrayHash$mCc$sp__AC__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_scm_ArraySeq$ofChar.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_scm_ArraySeq$ofChar)) {
    var x2 = $as_scm_ArraySeq$ofChar(that);
    var a = this.scm_ArraySeq$ofChar__f_array;
    var b = $n(x2).scm_ArraySeq$ofChar__f_array;
    return $m_ju_Arrays$().equals__AC__AC__Z(a, b);
  } else {
    return $c_scm_ArraySeq.prototype.equals__O__Z.call(this, that);
  }
});
$c_scm_ArraySeq$ofChar.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcC$sp(this.scm_ArraySeq$ofChar__f_array);
});
$c_scm_ArraySeq$ofChar.prototype.addString__scm_StringBuilder__T__T__T__scm_StringBuilder = (function(sb, start, sep, end) {
  var jsb = $n(sb).scm_StringBuilder__f_underlying;
  var this$1 = $n(start);
  if ((this$1.length !== 0)) {
    var this$2 = $n(jsb);
    this$2.jl_StringBuilder__f_java$lang$StringBuilder$$content = (("" + this$2.jl_StringBuilder__f_java$lang$StringBuilder$$content) + start);
  }
  var len = $n(this.scm_ArraySeq$ofChar__f_array).u.length;
  if ((len !== 0)) {
    var this$3 = $n(sep);
    if ((this$3 === "")) {
      $n(jsb).append__AC__jl_StringBuilder(this.scm_ArraySeq$ofChar__f_array);
    } else {
      $n(jsb);
      $n(jsb).length__I();
      $n(end);
      $n(sep);
      var this$6 = $n(jsb);
      var c = $n(this.scm_ArraySeq$ofChar__f_array).get(0);
      var str = ("" + $cToS(c));
      this$6.jl_StringBuilder__f_java$lang$StringBuilder$$content = (this$6.jl_StringBuilder__f_java$lang$StringBuilder$$content + str);
      var i = 1;
      while ((i < len)) {
        var this$8 = $n(jsb);
        this$8.jl_StringBuilder__f_java$lang$StringBuilder$$content = (("" + this$8.jl_StringBuilder__f_java$lang$StringBuilder$$content) + sep);
        var this$9 = $n(jsb);
        var c$1 = $n(this.scm_ArraySeq$ofChar__f_array).get(i);
        var str$1 = ("" + $cToS(c$1));
        this$9.jl_StringBuilder__f_java$lang$StringBuilder$$content = (this$9.jl_StringBuilder__f_java$lang$StringBuilder$$content + str$1);
        i = ((1 + i) | 0);
      }
    }
  }
  var this$11 = $n(end);
  if ((this$11.length !== 0)) {
    var this$12 = $n(jsb);
    this$12.jl_StringBuilder__f_java$lang$StringBuilder$$content = (("" + this$12.jl_StringBuilder__f_java$lang$StringBuilder$$content) + end);
  }
  return sb;
});
$c_scm_ArraySeq$ofChar.prototype.apply__O__O = (function(v1) {
  return $bC(this.apply__I__C($uI(v1)));
});
$c_scm_ArraySeq$ofChar.prototype.apply__I__O = (function(i) {
  return $bC(this.apply__I__C(i));
});
$c_scm_ArraySeq$ofChar.prototype.array__O = (function() {
  return this.scm_ArraySeq$ofChar__f_array;
});
function $as_scm_ArraySeq$ofChar(obj) {
  return (((obj instanceof $c_scm_ArraySeq$ofChar) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.mutable.ArraySeq$ofChar"));
}
function $isArrayOf_scm_ArraySeq$ofChar(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_ArraySeq$ofChar)));
}
function $asArrayOf_scm_ArraySeq$ofChar(obj, depth) {
  return (($isArrayOf_scm_ArraySeq$ofChar(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.mutable.ArraySeq$ofChar;", depth));
}
var $d_scm_ArraySeq$ofChar = new $TypeData().initClass($c_scm_ArraySeq$ofChar, "scala.collection.mutable.ArraySeq$ofChar", ({
  scm_ArraySeq$ofChar: 1,
  scm_ArraySeq: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_SeqOps: 1,
  scm_Cloneable: 1,
  jl_Cloneable: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  scm_IndexedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_scm_ArraySeq$ofDouble(array) {
  this.scm_ArraySeq$ofDouble__f_array = null;
  this.scm_ArraySeq$ofDouble__f_array = array;
}
$c_scm_ArraySeq$ofDouble.prototype = new $h_scm_ArraySeq();
$c_scm_ArraySeq$ofDouble.prototype.constructor = $c_scm_ArraySeq$ofDouble;
/** @constructor */
function $h_scm_ArraySeq$ofDouble() {
}
$h_scm_ArraySeq$ofDouble.prototype = $c_scm_ArraySeq$ofDouble.prototype;
$c_scm_ArraySeq$ofDouble.prototype.length__I = (function() {
  return $n(this.scm_ArraySeq$ofDouble__f_array).u.length;
});
$c_scm_ArraySeq$ofDouble.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.scm_ArraySeq$ofDouble__f_array;
  return this$1.arrayHash$mDc$sp__AD__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_scm_ArraySeq$ofDouble.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_scm_ArraySeq$ofDouble)) {
    var x2 = $as_scm_ArraySeq$ofDouble(that);
    var a = this.scm_ArraySeq$ofDouble__f_array;
    var b = $n(x2).scm_ArraySeq$ofDouble__f_array;
    return $m_ju_Arrays$().equals__AD__AD__Z(a, b);
  } else {
    return $c_scm_ArraySeq.prototype.equals__O__Z.call(this, that);
  }
});
$c_scm_ArraySeq$ofDouble.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcD$sp(this.scm_ArraySeq$ofDouble__f_array);
});
$c_scm_ArraySeq$ofDouble.prototype.apply$mcDI$sp__I__D = (function(index) {
  return $n(this.scm_ArraySeq$ofDouble__f_array).get(index);
});
$c_scm_ArraySeq$ofDouble.prototype.apply__O__O = (function(v1) {
  var index = $uI(v1);
  return this.apply$mcDI$sp__I__D(index);
});
$c_scm_ArraySeq$ofDouble.prototype.apply__I__O = (function(i) {
  return this.apply$mcDI$sp__I__D(i);
});
$c_scm_ArraySeq$ofDouble.prototype.array__O = (function() {
  return this.scm_ArraySeq$ofDouble__f_array;
});
function $as_scm_ArraySeq$ofDouble(obj) {
  return (((obj instanceof $c_scm_ArraySeq$ofDouble) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.mutable.ArraySeq$ofDouble"));
}
function $isArrayOf_scm_ArraySeq$ofDouble(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_ArraySeq$ofDouble)));
}
function $asArrayOf_scm_ArraySeq$ofDouble(obj, depth) {
  return (($isArrayOf_scm_ArraySeq$ofDouble(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.mutable.ArraySeq$ofDouble;", depth));
}
var $d_scm_ArraySeq$ofDouble = new $TypeData().initClass($c_scm_ArraySeq$ofDouble, "scala.collection.mutable.ArraySeq$ofDouble", ({
  scm_ArraySeq$ofDouble: 1,
  scm_ArraySeq: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_SeqOps: 1,
  scm_Cloneable: 1,
  jl_Cloneable: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  scm_IndexedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_scm_ArraySeq$ofFloat(array) {
  this.scm_ArraySeq$ofFloat__f_array = null;
  this.scm_ArraySeq$ofFloat__f_array = array;
}
$c_scm_ArraySeq$ofFloat.prototype = new $h_scm_ArraySeq();
$c_scm_ArraySeq$ofFloat.prototype.constructor = $c_scm_ArraySeq$ofFloat;
/** @constructor */
function $h_scm_ArraySeq$ofFloat() {
}
$h_scm_ArraySeq$ofFloat.prototype = $c_scm_ArraySeq$ofFloat.prototype;
$c_scm_ArraySeq$ofFloat.prototype.length__I = (function() {
  return $n(this.scm_ArraySeq$ofFloat__f_array).u.length;
});
$c_scm_ArraySeq$ofFloat.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.scm_ArraySeq$ofFloat__f_array;
  return this$1.arrayHash$mFc$sp__AF__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_scm_ArraySeq$ofFloat.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_scm_ArraySeq$ofFloat)) {
    var x2 = $as_scm_ArraySeq$ofFloat(that);
    var a = this.scm_ArraySeq$ofFloat__f_array;
    var b = $n(x2).scm_ArraySeq$ofFloat__f_array;
    return $m_ju_Arrays$().equals__AF__AF__Z(a, b);
  } else {
    return $c_scm_ArraySeq.prototype.equals__O__Z.call(this, that);
  }
});
$c_scm_ArraySeq$ofFloat.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcF$sp(this.scm_ArraySeq$ofFloat__f_array);
});
$c_scm_ArraySeq$ofFloat.prototype.apply$mcFI$sp__I__F = (function(index) {
  return $n(this.scm_ArraySeq$ofFloat__f_array).get(index);
});
$c_scm_ArraySeq$ofFloat.prototype.apply__O__O = (function(v1) {
  var index = $uI(v1);
  return this.apply$mcFI$sp__I__F(index);
});
$c_scm_ArraySeq$ofFloat.prototype.apply__I__O = (function(i) {
  return this.apply$mcFI$sp__I__F(i);
});
$c_scm_ArraySeq$ofFloat.prototype.array__O = (function() {
  return this.scm_ArraySeq$ofFloat__f_array;
});
function $as_scm_ArraySeq$ofFloat(obj) {
  return (((obj instanceof $c_scm_ArraySeq$ofFloat) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.mutable.ArraySeq$ofFloat"));
}
function $isArrayOf_scm_ArraySeq$ofFloat(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_ArraySeq$ofFloat)));
}
function $asArrayOf_scm_ArraySeq$ofFloat(obj, depth) {
  return (($isArrayOf_scm_ArraySeq$ofFloat(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.mutable.ArraySeq$ofFloat;", depth));
}
var $d_scm_ArraySeq$ofFloat = new $TypeData().initClass($c_scm_ArraySeq$ofFloat, "scala.collection.mutable.ArraySeq$ofFloat", ({
  scm_ArraySeq$ofFloat: 1,
  scm_ArraySeq: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_SeqOps: 1,
  scm_Cloneable: 1,
  jl_Cloneable: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  scm_IndexedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_scm_ArraySeq$ofInt(array) {
  this.scm_ArraySeq$ofInt__f_array = null;
  this.scm_ArraySeq$ofInt__f_array = array;
}
$c_scm_ArraySeq$ofInt.prototype = new $h_scm_ArraySeq();
$c_scm_ArraySeq$ofInt.prototype.constructor = $c_scm_ArraySeq$ofInt;
/** @constructor */
function $h_scm_ArraySeq$ofInt() {
}
$h_scm_ArraySeq$ofInt.prototype = $c_scm_ArraySeq$ofInt.prototype;
$c_scm_ArraySeq$ofInt.prototype.length__I = (function() {
  return $n(this.scm_ArraySeq$ofInt__f_array).u.length;
});
$c_scm_ArraySeq$ofInt.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.scm_ArraySeq$ofInt__f_array;
  return this$1.arrayHash$mIc$sp__AI__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_scm_ArraySeq$ofInt.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_scm_ArraySeq$ofInt)) {
    var x2 = $as_scm_ArraySeq$ofInt(that);
    var a = this.scm_ArraySeq$ofInt__f_array;
    var b = $n(x2).scm_ArraySeq$ofInt__f_array;
    return $m_ju_Arrays$().equals__AI__AI__Z(a, b);
  } else {
    return $c_scm_ArraySeq.prototype.equals__O__Z.call(this, that);
  }
});
$c_scm_ArraySeq$ofInt.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcI$sp(this.scm_ArraySeq$ofInt__f_array);
});
$c_scm_ArraySeq$ofInt.prototype.apply$mcII$sp__I__I = (function(index) {
  return $n(this.scm_ArraySeq$ofInt__f_array).get(index);
});
$c_scm_ArraySeq$ofInt.prototype.apply__O__O = (function(v1) {
  var index = $uI(v1);
  return this.apply$mcII$sp__I__I(index);
});
$c_scm_ArraySeq$ofInt.prototype.apply__I__O = (function(i) {
  return this.apply$mcII$sp__I__I(i);
});
$c_scm_ArraySeq$ofInt.prototype.array__O = (function() {
  return this.scm_ArraySeq$ofInt__f_array;
});
function $as_scm_ArraySeq$ofInt(obj) {
  return (((obj instanceof $c_scm_ArraySeq$ofInt) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.mutable.ArraySeq$ofInt"));
}
function $isArrayOf_scm_ArraySeq$ofInt(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_ArraySeq$ofInt)));
}
function $asArrayOf_scm_ArraySeq$ofInt(obj, depth) {
  return (($isArrayOf_scm_ArraySeq$ofInt(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.mutable.ArraySeq$ofInt;", depth));
}
var $d_scm_ArraySeq$ofInt = new $TypeData().initClass($c_scm_ArraySeq$ofInt, "scala.collection.mutable.ArraySeq$ofInt", ({
  scm_ArraySeq$ofInt: 1,
  scm_ArraySeq: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_SeqOps: 1,
  scm_Cloneable: 1,
  jl_Cloneable: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  scm_IndexedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_scm_ArraySeq$ofLong(array) {
  this.scm_ArraySeq$ofLong__f_array = null;
  this.scm_ArraySeq$ofLong__f_array = array;
}
$c_scm_ArraySeq$ofLong.prototype = new $h_scm_ArraySeq();
$c_scm_ArraySeq$ofLong.prototype.constructor = $c_scm_ArraySeq$ofLong;
/** @constructor */
function $h_scm_ArraySeq$ofLong() {
}
$h_scm_ArraySeq$ofLong.prototype = $c_scm_ArraySeq$ofLong.prototype;
$c_scm_ArraySeq$ofLong.prototype.length__I = (function() {
  return $n(this.scm_ArraySeq$ofLong__f_array).u.length;
});
$c_scm_ArraySeq$ofLong.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.scm_ArraySeq$ofLong__f_array;
  return this$1.arrayHash$mJc$sp__AJ__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_scm_ArraySeq$ofLong.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_scm_ArraySeq$ofLong)) {
    var x2 = $as_scm_ArraySeq$ofLong(that);
    var a = this.scm_ArraySeq$ofLong__f_array;
    var b = $n(x2).scm_ArraySeq$ofLong__f_array;
    return $m_ju_Arrays$().equals__AJ__AJ__Z(a, b);
  } else {
    return $c_scm_ArraySeq.prototype.equals__O__Z.call(this, that);
  }
});
$c_scm_ArraySeq$ofLong.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcJ$sp(this.scm_ArraySeq$ofLong__f_array);
});
$c_scm_ArraySeq$ofLong.prototype.apply$mcJI$sp__I__J = (function(index) {
  return $n(this.scm_ArraySeq$ofLong__f_array).get(index);
});
$c_scm_ArraySeq$ofLong.prototype.apply__O__O = (function(v1) {
  var index = $uI(v1);
  return this.apply$mcJI$sp__I__J(index);
});
$c_scm_ArraySeq$ofLong.prototype.apply__I__O = (function(i) {
  return this.apply$mcJI$sp__I__J(i);
});
$c_scm_ArraySeq$ofLong.prototype.array__O = (function() {
  return this.scm_ArraySeq$ofLong__f_array;
});
function $as_scm_ArraySeq$ofLong(obj) {
  return (((obj instanceof $c_scm_ArraySeq$ofLong) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.mutable.ArraySeq$ofLong"));
}
function $isArrayOf_scm_ArraySeq$ofLong(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_ArraySeq$ofLong)));
}
function $asArrayOf_scm_ArraySeq$ofLong(obj, depth) {
  return (($isArrayOf_scm_ArraySeq$ofLong(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.mutable.ArraySeq$ofLong;", depth));
}
var $d_scm_ArraySeq$ofLong = new $TypeData().initClass($c_scm_ArraySeq$ofLong, "scala.collection.mutable.ArraySeq$ofLong", ({
  scm_ArraySeq$ofLong: 1,
  scm_ArraySeq: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_SeqOps: 1,
  scm_Cloneable: 1,
  jl_Cloneable: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  scm_IndexedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_scm_ArraySeq$ofRef(array) {
  this.scm_ArraySeq$ofRef__f_array = null;
  this.scm_ArraySeq$ofRef__f_array = array;
}
$c_scm_ArraySeq$ofRef.prototype = new $h_scm_ArraySeq();
$c_scm_ArraySeq$ofRef.prototype.constructor = $c_scm_ArraySeq$ofRef;
/** @constructor */
function $h_scm_ArraySeq$ofRef() {
}
$h_scm_ArraySeq$ofRef.prototype = $c_scm_ArraySeq$ofRef.prototype;
$c_scm_ArraySeq$ofRef.prototype.length__I = (function() {
  return $n(this.scm_ArraySeq$ofRef__f_array).u.length;
});
$c_scm_ArraySeq$ofRef.prototype.apply__I__O = (function(index) {
  return $n(this.scm_ArraySeq$ofRef__f_array).get(index);
});
$c_scm_ArraySeq$ofRef.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.scm_ArraySeq$ofRef__f_array;
  return this$1.arrayHash__O__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_scm_ArraySeq$ofRef.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_scm_ArraySeq$ofRef)) {
    var x2 = $as_scm_ArraySeq$ofRef(that);
    return $m_s_Array$().equals__AO__AO__Z(this.scm_ArraySeq$ofRef__f_array, $n(x2).scm_ArraySeq$ofRef__f_array);
  } else {
    return $c_scm_ArraySeq.prototype.equals__O__Z.call(this, that);
  }
});
$c_scm_ArraySeq$ofRef.prototype.iterator__sc_Iterator = (function() {
  return $ct_sc_ArrayOps$ArrayIterator__O__(new $c_sc_ArrayOps$ArrayIterator(), this.scm_ArraySeq$ofRef__f_array);
});
$c_scm_ArraySeq$ofRef.prototype.apply__O__O = (function(v1) {
  return this.apply__I__O($uI(v1));
});
$c_scm_ArraySeq$ofRef.prototype.array__O = (function() {
  return this.scm_ArraySeq$ofRef__f_array;
});
function $as_scm_ArraySeq$ofRef(obj) {
  return (((obj instanceof $c_scm_ArraySeq$ofRef) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.mutable.ArraySeq$ofRef"));
}
function $isArrayOf_scm_ArraySeq$ofRef(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_ArraySeq$ofRef)));
}
function $asArrayOf_scm_ArraySeq$ofRef(obj, depth) {
  return (($isArrayOf_scm_ArraySeq$ofRef(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.mutable.ArraySeq$ofRef;", depth));
}
var $d_scm_ArraySeq$ofRef = new $TypeData().initClass($c_scm_ArraySeq$ofRef, "scala.collection.mutable.ArraySeq$ofRef", ({
  scm_ArraySeq$ofRef: 1,
  scm_ArraySeq: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_SeqOps: 1,
  scm_Cloneable: 1,
  jl_Cloneable: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  scm_IndexedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_scm_ArraySeq$ofShort(array) {
  this.scm_ArraySeq$ofShort__f_array = null;
  this.scm_ArraySeq$ofShort__f_array = array;
}
$c_scm_ArraySeq$ofShort.prototype = new $h_scm_ArraySeq();
$c_scm_ArraySeq$ofShort.prototype.constructor = $c_scm_ArraySeq$ofShort;
/** @constructor */
function $h_scm_ArraySeq$ofShort() {
}
$h_scm_ArraySeq$ofShort.prototype = $c_scm_ArraySeq$ofShort.prototype;
$c_scm_ArraySeq$ofShort.prototype.length__I = (function() {
  return $n(this.scm_ArraySeq$ofShort__f_array).u.length;
});
$c_scm_ArraySeq$ofShort.prototype.apply__I__S = (function(index) {
  return $n(this.scm_ArraySeq$ofShort__f_array).get(index);
});
$c_scm_ArraySeq$ofShort.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.scm_ArraySeq$ofShort__f_array;
  return this$1.arrayHash$mSc$sp__AS__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_scm_ArraySeq$ofShort.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_scm_ArraySeq$ofShort)) {
    var x2 = $as_scm_ArraySeq$ofShort(that);
    var a = this.scm_ArraySeq$ofShort__f_array;
    var b = $n(x2).scm_ArraySeq$ofShort__f_array;
    return $m_ju_Arrays$().equals__AS__AS__Z(a, b);
  } else {
    return $c_scm_ArraySeq.prototype.equals__O__Z.call(this, that);
  }
});
$c_scm_ArraySeq$ofShort.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcS$sp(this.scm_ArraySeq$ofShort__f_array);
});
$c_scm_ArraySeq$ofShort.prototype.apply__O__O = (function(v1) {
  return this.apply__I__S($uI(v1));
});
$c_scm_ArraySeq$ofShort.prototype.apply__I__O = (function(i) {
  return this.apply__I__S(i);
});
$c_scm_ArraySeq$ofShort.prototype.array__O = (function() {
  return this.scm_ArraySeq$ofShort__f_array;
});
function $as_scm_ArraySeq$ofShort(obj) {
  return (((obj instanceof $c_scm_ArraySeq$ofShort) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.mutable.ArraySeq$ofShort"));
}
function $isArrayOf_scm_ArraySeq$ofShort(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_ArraySeq$ofShort)));
}
function $asArrayOf_scm_ArraySeq$ofShort(obj, depth) {
  return (($isArrayOf_scm_ArraySeq$ofShort(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.mutable.ArraySeq$ofShort;", depth));
}
var $d_scm_ArraySeq$ofShort = new $TypeData().initClass($c_scm_ArraySeq$ofShort, "scala.collection.mutable.ArraySeq$ofShort", ({
  scm_ArraySeq$ofShort: 1,
  scm_ArraySeq: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_SeqOps: 1,
  scm_Cloneable: 1,
  jl_Cloneable: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  scm_IndexedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_scm_ArraySeq$ofUnit(array) {
  this.scm_ArraySeq$ofUnit__f_array = null;
  this.scm_ArraySeq$ofUnit__f_array = array;
}
$c_scm_ArraySeq$ofUnit.prototype = new $h_scm_ArraySeq();
$c_scm_ArraySeq$ofUnit.prototype.constructor = $c_scm_ArraySeq$ofUnit;
/** @constructor */
function $h_scm_ArraySeq$ofUnit() {
}
$h_scm_ArraySeq$ofUnit.prototype = $c_scm_ArraySeq$ofUnit.prototype;
$c_scm_ArraySeq$ofUnit.prototype.length__I = (function() {
  return $n(this.scm_ArraySeq$ofUnit__f_array).u.length;
});
$c_scm_ArraySeq$ofUnit.prototype.hashCode__I = (function() {
  var this$1 = $m_s_util_hashing_MurmurHash3$();
  var a = this.scm_ArraySeq$ofUnit__f_array;
  return this$1.arrayHash$mVc$sp__Ajl_Void__I__I(a, this$1.s_util_hashing_MurmurHash3$__f_seqSeed);
});
$c_scm_ArraySeq$ofUnit.prototype.equals__O__Z = (function(that) {
  if ((that instanceof $c_scm_ArraySeq$ofUnit)) {
    var x2 = $as_scm_ArraySeq$ofUnit(that);
    return ($n(this.scm_ArraySeq$ofUnit__f_array).u.length === $n($n(x2).scm_ArraySeq$ofUnit__f_array).u.length);
  } else {
    return $c_scm_ArraySeq.prototype.equals__O__Z.call(this, that);
  }
});
$c_scm_ArraySeq$ofUnit.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_ArrayOps$ArrayIterator$mcV$sp(this.scm_ArraySeq$ofUnit__f_array);
});
$c_scm_ArraySeq$ofUnit.prototype.apply$mcVI$sp__I__V = (function(index) {
  $n(this.scm_ArraySeq$ofUnit__f_array).get(index);
});
$c_scm_ArraySeq$ofUnit.prototype.apply__O__O = (function(v1) {
  var index = $uI(v1);
  this.apply$mcVI$sp__I__V(index);
});
$c_scm_ArraySeq$ofUnit.prototype.apply__I__O = (function(i) {
  this.apply$mcVI$sp__I__V(i);
});
$c_scm_ArraySeq$ofUnit.prototype.array__O = (function() {
  return this.scm_ArraySeq$ofUnit__f_array;
});
function $as_scm_ArraySeq$ofUnit(obj) {
  return (((obj instanceof $c_scm_ArraySeq$ofUnit) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.mutable.ArraySeq$ofUnit"));
}
function $isArrayOf_scm_ArraySeq$ofUnit(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.scm_ArraySeq$ofUnit)));
}
function $asArrayOf_scm_ArraySeq$ofUnit(obj, depth) {
  return (($isArrayOf_scm_ArraySeq$ofUnit(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.mutable.ArraySeq$ofUnit;", depth));
}
var $d_scm_ArraySeq$ofUnit = new $TypeData().initClass($c_scm_ArraySeq$ofUnit, "scala.collection.mutable.ArraySeq$ofUnit", ({
  scm_ArraySeq$ofUnit: 1,
  scm_ArraySeq: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_SeqOps: 1,
  scm_Cloneable: 1,
  jl_Cloneable: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  scm_IndexedSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  Ljava_io_Serializable: 1
}));
/** @constructor */
function $c_sci_$colon$colon(head, next) {
  this.sci_$colon$colon__f_head = null;
  this.sci_$colon$colon__f_next = null;
  this.sci_$colon$colon__f_head = head;
  this.sci_$colon$colon__f_next = next;
}
$c_sci_$colon$colon.prototype = new $h_sci_List();
$c_sci_$colon$colon.prototype.constructor = $c_sci_$colon$colon;
/** @constructor */
function $h_sci_$colon$colon() {
}
$h_sci_$colon$colon.prototype = $c_sci_$colon$colon.prototype;
$c_sci_$colon$colon.prototype.head__O = (function() {
  return this.sci_$colon$colon__f_head;
});
$c_sci_$colon$colon.prototype.productPrefix__T = (function() {
  return "::";
});
$c_sci_$colon$colon.prototype.productArity__I = (function() {
  return 2;
});
$c_sci_$colon$colon.prototype.productElement__I__O = (function(x$1) {
  switch (x$1) {
    case 0: {
      return this.sci_$colon$colon__f_head;
      break;
    }
    case 1: {
      return this.sci_$colon$colon__f_next;
      break;
    }
    default: {
      return $m_sr_Statics$().ioobe__I__O(x$1);
    }
  }
});
$c_sci_$colon$colon.prototype.productIterator__sc_Iterator = (function() {
  return new $c_sr_ScalaRunTime$$anon$1(this);
});
$c_sci_$colon$colon.prototype.tail__O = (function() {
  return this.sci_$colon$colon__f_next;
});
var $d_sci_$colon$colon = new $TypeData().initClass($c_sci_$colon$colon, "scala.collection.immutable.$colon$colon", ({
  sci_$colon$colon: 1,
  sci_List: 1,
  sci_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_SeqOps: 1,
  sci_LinearSeq: 1,
  sc_LinearSeq: 1,
  sc_LinearSeqOps: 1,
  sci_LinearSeqOps: 1,
  sc_StrictOptimizedLinearSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  sci_StrictOptimizedSeqOps: 1,
  scg_DefaultSerializable: 1,
  Ljava_io_Serializable: 1,
  s_Product: 1
}));
/** @constructor */
function $c_sci_Nil$() {
}
$c_sci_Nil$.prototype = new $h_sci_List();
$c_sci_Nil$.prototype.constructor = $c_sci_Nil$;
/** @constructor */
function $h_sci_Nil$() {
}
$h_sci_Nil$.prototype = $c_sci_Nil$.prototype;
$c_sci_Nil$.prototype.head__E = (function() {
  throw new $c_ju_NoSuchElementException("head of empty list");
});
$c_sci_Nil$.prototype.tail__E = (function() {
  throw new $c_jl_UnsupportedOperationException("tail of empty list");
});
$c_sci_Nil$.prototype.knownSize__I = (function() {
  return 0;
});
$c_sci_Nil$.prototype.iterator__sc_Iterator = (function() {
  return $m_sc_Iterator$().sc_Iterator$__f_scala$collection$Iterator$$_empty;
});
$c_sci_Nil$.prototype.productPrefix__T = (function() {
  return "Nil";
});
$c_sci_Nil$.prototype.productArity__I = (function() {
  return 0;
});
$c_sci_Nil$.prototype.productElement__I__O = (function(x$1) {
  return $m_sr_Statics$().ioobe__I__O(x$1);
});
$c_sci_Nil$.prototype.productIterator__sc_Iterator = (function() {
  return new $c_sr_ScalaRunTime$$anon$1(this);
});
$c_sci_Nil$.prototype.tail__O = (function() {
  this.tail__E();
});
$c_sci_Nil$.prototype.head__O = (function() {
  this.head__E();
});
var $d_sci_Nil$ = new $TypeData().initClass($c_sci_Nil$, "scala.collection.immutable.Nil$", ({
  sci_Nil$: 1,
  sci_List: 1,
  sci_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_SeqOps: 1,
  sci_LinearSeq: 1,
  sc_LinearSeq: 1,
  sc_LinearSeqOps: 1,
  sci_LinearSeqOps: 1,
  sc_StrictOptimizedLinearSeqOps: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  sci_StrictOptimizedSeqOps: 1,
  scg_DefaultSerializable: 1,
  Ljava_io_Serializable: 1,
  s_Product: 1
}));
var $n_sci_Nil$;
function $m_sci_Nil$() {
  if ((!$n_sci_Nil$)) {
    $n_sci_Nil$ = new $c_sci_Nil$();
  }
  return $n_sci_Nil$;
}
function $ct_scm_StringBuilder__jl_StringBuilder__($thiz, underlying) {
  $thiz.scm_StringBuilder__f_underlying = underlying;
  return $thiz;
}
function $ct_scm_StringBuilder__($thiz) {
  $ct_scm_StringBuilder__jl_StringBuilder__($thiz, new $c_jl_StringBuilder());
  return $thiz;
}
/** @constructor */
function $c_scm_StringBuilder() {
  this.scm_StringBuilder__f_underlying = null;
}
$c_scm_StringBuilder.prototype = new $h_scm_AbstractSeq();
$c_scm_StringBuilder.prototype.constructor = $c_scm_StringBuilder;
/** @constructor */
function $h_scm_StringBuilder() {
}
$h_scm_StringBuilder.prototype = $c_scm_StringBuilder.prototype;
$c_scm_StringBuilder.prototype.stringPrefix__T = (function() {
  return "IndexedSeq";
});
$c_scm_StringBuilder.prototype.iterator__sc_Iterator = (function() {
  var this$1 = new $c_sc_IndexedSeqView$Id(this);
  return new $c_sc_IndexedSeqView$IndexedSeqViewIterator(this$1);
});
$c_scm_StringBuilder.prototype.lengthCompare__I__I = (function(len) {
  var x = $n(this.scm_StringBuilder__f_underlying).length__I();
  return ((x === len) ? 0 : ((x < len) ? (-1) : 1));
});
$c_scm_StringBuilder.prototype.addAll__sc_IterableOnce__scm_Growable = (function(elems) {
  return $f_scm_Growable__addAll__sc_IterableOnce__scm_Growable(this, elems);
});
$c_scm_StringBuilder.prototype.length__I = (function() {
  return $n(this.scm_StringBuilder__f_underlying).length__I();
});
$c_scm_StringBuilder.prototype.knownSize__I = (function() {
  return $n(this.scm_StringBuilder__f_underlying).length__I();
});
$c_scm_StringBuilder.prototype.addOne__C__scm_StringBuilder = (function(x) {
  var this$1 = $n(this.scm_StringBuilder__f_underlying);
  var str = ("" + $cToS(x));
  this$1.jl_StringBuilder__f_java$lang$StringBuilder$$content = (this$1.jl_StringBuilder__f_java$lang$StringBuilder$$content + str);
  return this;
});
$c_scm_StringBuilder.prototype.toString__T = (function() {
  return $n(this.scm_StringBuilder__f_underlying).jl_StringBuilder__f_java$lang$StringBuilder$$content;
});
$c_scm_StringBuilder.prototype.isEmpty__Z = (function() {
  return ($n(this.scm_StringBuilder__f_underlying).length__I() === 0);
});
$c_scm_StringBuilder.prototype.result__O = (function() {
  return $n(this.scm_StringBuilder__f_underlying).jl_StringBuilder__f_java$lang$StringBuilder$$content;
});
$c_scm_StringBuilder.prototype.addOne__O__scm_Growable = (function(elem) {
  return this.addOne__C__scm_StringBuilder($uC(elem));
});
$c_scm_StringBuilder.prototype.apply__O__O = (function(v1) {
  var i = $uI(v1);
  return $bC($n(this.scm_StringBuilder__f_underlying).charAt__I__C(i));
});
$c_scm_StringBuilder.prototype.apply__I__O = (function(i) {
  return $bC($n(this.scm_StringBuilder__f_underlying).charAt__I__C(i));
});
var $d_scm_StringBuilder = new $TypeData().initClass($c_scm_StringBuilder, "scala.collection.mutable.StringBuilder", ({
  scm_StringBuilder: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_SeqOps: 1,
  scm_Cloneable: 1,
  jl_Cloneable: 1,
  scm_ReusableBuilder: 1,
  scm_Builder: 1,
  scm_Growable: 1,
  scm_Clearable: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  scm_IndexedSeqOps: 1,
  jl_CharSequence: 1,
  Ljava_io_Serializable: 1
}));
function $p_scm_ListBuffer__copyElems__V($thiz) {
  var buf = new $c_scm_ListBuffer().scala$collection$mutable$ListBuffer$$freshFrom__sc_IterableOnce__scm_ListBuffer($thiz);
  $thiz.scm_ListBuffer__f_first = $n(buf).scm_ListBuffer__f_first;
  $thiz.scm_ListBuffer__f_last0 = $n(buf).scm_ListBuffer__f_last0;
  $thiz.scm_ListBuffer__f_aliased = false;
}
function $p_scm_ListBuffer__ensureUnaliased__V($thiz) {
  $thiz.scm_ListBuffer__f_mutationCount = ((1 + $thiz.scm_ListBuffer__f_mutationCount) | 0);
  if ($thiz.scm_ListBuffer__f_aliased) {
    $p_scm_ListBuffer__copyElems__V($thiz);
  }
}
/** @constructor */
function $c_scm_ListBuffer() {
  this.scm_ListBuffer__f_mutationCount = 0;
  this.scm_ListBuffer__f_first = null;
  this.scm_ListBuffer__f_last0 = null;
  this.scm_ListBuffer__f_aliased = false;
  this.scm_ListBuffer__f_len = 0;
  this.scm_ListBuffer__f_mutationCount = 0;
  this.scm_ListBuffer__f_first = $m_sci_Nil$();
  this.scm_ListBuffer__f_last0 = null;
  this.scm_ListBuffer__f_aliased = false;
  this.scm_ListBuffer__f_len = 0;
}
$c_scm_ListBuffer.prototype = new $h_scm_AbstractBuffer();
$c_scm_ListBuffer.prototype.constructor = $c_scm_ListBuffer;
/** @constructor */
function $h_scm_ListBuffer() {
}
$h_scm_ListBuffer.prototype = $c_scm_ListBuffer.prototype;
$c_scm_ListBuffer.prototype.iterator__sc_Iterator = (function() {
  return new $c_scm_MutationTracker$CheckedIterator($n(this.scm_ListBuffer__f_first).iterator__sc_Iterator(), new $c_sjsr_AnonFunction0((() => this.scm_ListBuffer__f_mutationCount)));
});
$c_scm_ListBuffer.prototype.apply__I__O = (function(i) {
  var this$1 = $n(this.scm_ListBuffer__f_first);
  return $f_sc_LinearSeqOps__apply__I__O(this$1, i);
});
$c_scm_ListBuffer.prototype.length__I = (function() {
  return this.scm_ListBuffer__f_len;
});
$c_scm_ListBuffer.prototype.knownSize__I = (function() {
  return this.scm_ListBuffer__f_len;
});
$c_scm_ListBuffer.prototype.isEmpty__Z = (function() {
  return (this.scm_ListBuffer__f_len === 0);
});
$c_scm_ListBuffer.prototype.toList__sci_List = (function() {
  this.scm_ListBuffer__f_aliased = (!this.isEmpty__Z());
  return this.scm_ListBuffer__f_first;
});
$c_scm_ListBuffer.prototype.addOne__O__scm_ListBuffer = (function(elem) {
  $p_scm_ListBuffer__ensureUnaliased__V(this);
  var last1 = new $c_sci_$colon$colon(elem, $m_sci_Nil$());
  if ((this.scm_ListBuffer__f_len === 0)) {
    this.scm_ListBuffer__f_first = last1;
  } else {
    $n(this.scm_ListBuffer__f_last0).sci_$colon$colon__f_next = last1;
  }
  this.scm_ListBuffer__f_last0 = last1;
  this.scm_ListBuffer__f_len = ((1 + this.scm_ListBuffer__f_len) | 0);
  return this;
});
$c_scm_ListBuffer.prototype.scala$collection$mutable$ListBuffer$$freshFrom__sc_IterableOnce__scm_ListBuffer = (function(xs) {
  var it = $n(xs).iterator__sc_Iterator();
  if ($n(it).hasNext__Z()) {
    var len = 1;
    var last0 = new $c_sci_$colon$colon($n(it).next__O(), $m_sci_Nil$());
    this.scm_ListBuffer__f_first = last0;
    while ($n(it).hasNext__Z()) {
      var last1 = new $c_sci_$colon$colon($n(it).next__O(), $m_sci_Nil$());
      $n(last0).sci_$colon$colon__f_next = last1;
      last0 = last1;
      len = ((1 + len) | 0);
    }
    this.scm_ListBuffer__f_len = len;
    this.scm_ListBuffer__f_last0 = last0;
  }
  return this;
});
$c_scm_ListBuffer.prototype.addAll__sc_IterableOnce__scm_ListBuffer = (function(xs) {
  var it = $n(xs).iterator__sc_Iterator();
  if ($n(it).hasNext__Z()) {
    var fresh = new $c_scm_ListBuffer().scala$collection$mutable$ListBuffer$$freshFrom__sc_IterableOnce__scm_ListBuffer(it);
    $p_scm_ListBuffer__ensureUnaliased__V(this);
    if ((this.scm_ListBuffer__f_len === 0)) {
      this.scm_ListBuffer__f_first = $n(fresh).scm_ListBuffer__f_first;
    } else {
      $n(this.scm_ListBuffer__f_last0).sci_$colon$colon__f_next = $n(fresh).scm_ListBuffer__f_first;
    }
    this.scm_ListBuffer__f_last0 = $n(fresh).scm_ListBuffer__f_last0;
    this.scm_ListBuffer__f_len = ((this.scm_ListBuffer__f_len + $n(fresh).scm_ListBuffer__f_len) | 0);
  }
  return this;
});
$c_scm_ListBuffer.prototype.stringPrefix__T = (function() {
  return "ListBuffer";
});
$c_scm_ListBuffer.prototype.addAll__sc_IterableOnce__scm_Growable = (function(elems) {
  return this.addAll__sc_IterableOnce__scm_ListBuffer(elems);
});
$c_scm_ListBuffer.prototype.addOne__O__scm_Growable = (function(elem) {
  return this.addOne__O__scm_ListBuffer(elem);
});
$c_scm_ListBuffer.prototype.result__O = (function() {
  return this.toList__sci_List();
});
$c_scm_ListBuffer.prototype.apply__O__O = (function(v1) {
  var i = $uI(v1);
  var this$1 = $n(this.scm_ListBuffer__f_first);
  return $f_sc_LinearSeqOps__apply__I__O(this$1, i);
});
var $d_scm_ListBuffer = new $TypeData().initClass($c_scm_ListBuffer, "scala.collection.mutable.ListBuffer", ({
  scm_ListBuffer: 1,
  scm_AbstractBuffer: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_SeqOps: 1,
  scm_Cloneable: 1,
  jl_Cloneable: 1,
  scm_Buffer: 1,
  scm_Growable: 1,
  scm_Clearable: 1,
  scm_Shrinkable: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  scm_ReusableBuilder: 1,
  scm_Builder: 1,
  scg_DefaultSerializable: 1,
  Ljava_io_Serializable: 1
}));
function $ct_sjs_js_WrappedArray__sjs_js_Array__($thiz, array) {
  $thiz.sjs_js_WrappedArray__f_scala$scalajs$js$WrappedArray$$array = array;
  return $thiz;
}
function $ct_sjs_js_WrappedArray__($thiz) {
  $ct_sjs_js_WrappedArray__sjs_js_Array__($thiz, []);
  return $thiz;
}
/** @constructor */
function $c_sjs_js_WrappedArray() {
  this.sjs_js_WrappedArray__f_scala$scalajs$js$WrappedArray$$array = null;
}
$c_sjs_js_WrappedArray.prototype = new $h_scm_AbstractBuffer();
$c_sjs_js_WrappedArray.prototype.constructor = $c_sjs_js_WrappedArray;
/** @constructor */
function $h_sjs_js_WrappedArray() {
}
$h_sjs_js_WrappedArray.prototype = $c_sjs_js_WrappedArray.prototype;
$c_sjs_js_WrappedArray.prototype.stringPrefix__T = (function() {
  return "IndexedSeq";
});
$c_sjs_js_WrappedArray.prototype.iterator__sc_Iterator = (function() {
  var this$1 = new $c_sc_IndexedSeqView$Id(this);
  return new $c_sc_IndexedSeqView$IndexedSeqViewIterator(this$1);
});
$c_sjs_js_WrappedArray.prototype.lengthCompare__I__I = (function(len) {
  var x = $uI(this.sjs_js_WrappedArray__f_scala$scalajs$js$WrappedArray$$array.length);
  return ((x === len) ? 0 : ((x < len) ? (-1) : 1));
});
$c_sjs_js_WrappedArray.prototype.apply__I__O = (function(index) {
  return this.sjs_js_WrappedArray__f_scala$scalajs$js$WrappedArray$$array[index];
});
$c_sjs_js_WrappedArray.prototype.length__I = (function() {
  return $uI(this.sjs_js_WrappedArray__f_scala$scalajs$js$WrappedArray$$array.length);
});
$c_sjs_js_WrappedArray.prototype.knownSize__I = (function() {
  return $uI(this.sjs_js_WrappedArray__f_scala$scalajs$js$WrappedArray$$array.length);
});
$c_sjs_js_WrappedArray.prototype.className__T = (function() {
  return "WrappedArray";
});
$c_sjs_js_WrappedArray.prototype.result__O = (function() {
  return this;
});
$c_sjs_js_WrappedArray.prototype.addOne__O__scm_Growable = (function(elem) {
  this.sjs_js_WrappedArray__f_scala$scalajs$js$WrappedArray$$array.push(elem);
  return this;
});
$c_sjs_js_WrappedArray.prototype.apply__O__O = (function(v1) {
  var index = $uI(v1);
  return this.sjs_js_WrappedArray__f_scala$scalajs$js$WrappedArray$$array[index];
});
function $as_sjs_js_WrappedArray(obj) {
  return (((obj instanceof $c_sjs_js_WrappedArray) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.scalajs.js.WrappedArray"));
}
function $isArrayOf_sjs_js_WrappedArray(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sjs_js_WrappedArray)));
}
function $asArrayOf_sjs_js_WrappedArray(obj, depth) {
  return (($isArrayOf_sjs_js_WrappedArray(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.scalajs.js.WrappedArray;", depth));
}
var $d_sjs_js_WrappedArray = new $TypeData().initClass($c_sjs_js_WrappedArray, "scala.scalajs.js.WrappedArray", ({
  sjs_js_WrappedArray: 1,
  scm_AbstractBuffer: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_Iterable: 1,
  sc_IterableOnce: 1,
  sc_IterableOps: 1,
  sc_IterableOnceOps: 1,
  sc_IterableFactoryDefaults: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_SeqOps: 1,
  s_Equals: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_SeqOps: 1,
  scm_Cloneable: 1,
  jl_Cloneable: 1,
  scm_Buffer: 1,
  scm_Growable: 1,
  scm_Clearable: 1,
  scm_Shrinkable: 1,
  sc_StrictOptimizedSeqOps: 1,
  sc_StrictOptimizedIterableOps: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqOps: 1,
  scm_IndexedSeqOps: 1,
  scm_IndexedBuffer: 1,
  scm_Builder: 1,
  Ljava_io_Serializable: 1
}));
$L0 = new $c_RTLong(0, 0);
$d_J.zero = $L0;
$s_LMain__main__AT__V(new ($d_T.getArrayOf().constr)([]));
//# sourceMappingURL=client-fastopt.js.map
