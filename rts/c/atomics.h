// Start of atomics.h

inline int32_t atomic_add_i32_global(volatile __global int32_t *p, int32_t x) {
#ifdef FUTHARK_CUDA
  return atomicAdd((int32_t*)p, x);
#else
  return atomic_add(p, x);
#endif
}

inline int32_t atomic_add_i32_local(volatile __local int32_t *p, int32_t x) {
#ifdef FUTHARK_CUDA
  return atomicAdd((int32_t*)p, x);
#else
  return atomic_add(p, x);
#endif
}

inline float atomic_fadd_f32_global(volatile __global float *p, float x) {
#ifdef FUTHARK_CUDA
  return atomicAdd((float*)p, x);
#else
  union { int32_t i; float f; } old;
  union { int32_t i; float f; } assumed;
  old.f = *p;
  do {
    assumed.f = old.f;
    old.f = old.f + x;
    old.i = atomic_cmpxchg((volatile __global int32_t*)p, assumed.i, old.i);
  } while (assumed.i != old.i);
  return old.f;
#endif
}

inline float atomic_fadd_f32_local(volatile __local float *p, float x) {
#ifdef FUTHARK_CUDA
  return atomicAdd((float*)p, x);
#else
  union { int32_t i; float f; } old;
  union { int32_t i; float f; } assumed;
  old.f = *p;
  do {
    assumed.f = old.f;
    old.f = old.f + x;
    old.i = atomic_cmpxchg((volatile __local int32_t*)p, assumed.i, old.i);
  } while (assumed.i != old.i);
  return old.f;
#endif
}

inline int32_t atomic_smax_i32_global(volatile __global int32_t *p, int32_t x) {
#ifdef FUTHARK_CUDA
  return atomicMax((int32_t*)p, x);
#else
  return atomic_max(p, x);
#endif
}

inline int32_t atomic_smax_i32_local(volatile __local int32_t *p, int32_t x) {
#ifdef FUTHARK_CUDA
  return atomicMax((int32_t*)p, x);
#else
  return atomic_max(p, x);
#endif
}

inline int32_t atomic_smin_i32_global(volatile __global int32_t *p, int32_t x) {
#ifdef FUTHARK_CUDA
  return atomicMin((int32_t*)p, x);
#else
  return atomic_min(p, x);
#endif
}

inline int32_t atomic_smin_i32_local(volatile __local int32_t *p, int32_t x) {
#ifdef FUTHARK_CUDA
  return atomicMin((int32_t*)p, x);
#else
  return atomic_min(p, x);
#endif
}

inline uint32_t atomic_umax_i32_global(volatile __global uint32_t *p, uint32_t x) {
#ifdef FUTHARK_CUDA
  return atomicMax((uint32_t*)p, x);
#else
  return atomic_max(p, x);
#endif
}

inline uint32_t atomic_umax_i32_local(volatile __local uint32_t *p, uint32_t x) {
#ifdef FUTHARK_CUDA
  return atomicMax((uint32_t*)p, x);
#else
  return atomic_max(p, x);
#endif
}

inline uint32_t atomic_umin_i32_global(volatile __global uint32_t *p, uint32_t x) {
#ifdef FUTHARK_CUDA
  return atomicMin((uint32_t*)p, x);
#else
  return atomic_min(p, x);
#endif
}

inline uint32_t atomic_umin_i32_local(volatile __local uint32_t *p, uint32_t x) {
#ifdef FUTHARK_CUDA
  return atomicMin((uint32_t*)p, x);
#else
  return atomic_min(p, x);
#endif
}

inline int32_t atomic_and_i32_global(volatile __global int32_t *p, int32_t x) {
#ifdef FUTHARK_CUDA
  return atomicAnd((int32_t*)p, x);
#else
  return atomic_and(p, x);
#endif
}

inline int32_t atomic_and_i32_local(volatile __local int32_t *p, int32_t x) {
#ifdef FUTHARK_CUDA
  return atomicAnd((int32_t*)p, x);
#else
  return atomic_and(p, x);
#endif
}

inline int32_t atomic_or_i32_global(volatile __global int32_t *p, int32_t x) {
#ifdef FUTHARK_CUDA
  return atomicOr((int32_t*)p, x);
#else
  return atomic_or(p, x);
#endif
}

inline int32_t atomic_or_i32_local(volatile __local int32_t *p, int32_t x) {
#ifdef FUTHARK_CUDA
  return atomicOr((int32_t*)p, x);
#else
  return atomic_or(p, x);
#endif
}

inline int32_t atomic_xor_i32_global(volatile __global int32_t *p, int32_t x) {
#ifdef FUTHARK_CUDA
  return atomicXor((int32_t*)p, x);
#else
  return atomic_xor(p, x);
#endif
}

inline int32_t atomic_xor_i32_local(volatile __local int32_t *p, int32_t x) {
#ifdef FUTHARK_CUDA
  return atomicXor((int32_t*)p, x);
#else
  return atomic_xor(p, x);
#endif
}

inline int32_t atomic_xchg_i32_global(volatile __global int32_t *p, int32_t x) {
#ifdef FUTHARK_CUDA
  return atomicExch((int32_t*)p, x);
#else
  return atomic_xor(p, x);
#endif
}

inline int32_t atomic_xchg_i32_local(volatile __local int32_t *p, int32_t x) {
#ifdef FUTHARK_CUDA
  return atomicExch((int32_t*)p, x);
#else
  return atomic_xor(p, x);
#endif
}

inline int32_t atomic_cmpxchg_i32_global(volatile __global int32_t *p,
                                         int32_t cmp, int32_t val) {
#ifdef FUTHARK_CUDA
  return atomicCAS((int32_t*)p, cmp, val);
#else
  return atomic_cmpxchg(p, cmp, val);
#endif
}

inline int32_t atomic_cmpxchg_i32_local(volatile __local int32_t *p,
                                         int32_t cmp, int32_t val) {
#ifdef FUTHARK_CUDA
  return atomicCAS((int32_t*)p, cmp, val);
#else
  return atomic_cmpxchg(p, cmp, val);
#endif
}

// End of atomics.h
