# Native DOM component (JUN-93)

The JVM 17/21 setup-java/setup-clojure test/lint workflow has been deleted
under JUN-290. Do not restore JVM/nbb or a hosted/C-loader fallback.

The native facade and dom-core use typed `:document` values. Their nine
declared exports are checked as imported library functions, not external
container exports. Test `test/native/dom.kotoba` through the shared Browser
native gate using an Amu-produced compiler and exact source locks; Python may
stage/hash/observe, never replace the native verdict. Record ARM and x86
execution separately. See kotoba-lang/browser/scripts/check-shared-native.py.

The native golden covers document construction, root/children/attribute/style
changes, tree construction and operation consumption. The former full hosted
DOM/HTML/GPU test and lint matrix is no longer covered by CI. CSSOM, GPU output,
Browser surface and the kernel desktop remain separate qualification gates.
