## Jai Binary BVH Tree

A dynamic AABB tree broad-phase bvh tree written natively in jai.

Implemented using notes from Erin Catto `https://box2d.org/files/ErinCatto_DynamicBVH_GDC2019.pdf` and box2d `https://github.com/erincatto/box2d/`.

Which is inspired by Nathanael Presson btDbvt from bullet `https://github.com/bulletphysics/bullet3`.

### Dependencies

This depends in [https://github.com/kujukuju/jaimath](https://github.com/kujukuju/jaimath) which needs a function naming refactor sooner or later.

### API

```jai
Read the code.
```

### Notes

It's basically expected that you construct the tree with a type of Triangle3. If not you will have to manually provide collision information as the callback api requests it.

> NOTE: This library is still experimental and in development, the API may change and it may not function exactly as intended.

See `examples/` for usage information. (Might be outdated.)
