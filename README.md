# kami-engine-modeling

Portable CLJC polygon-editing engine. Meshes and edit operations are immutable
EDN; the first vertical slice provides a validated mesh contract and face
extrusion. `kami-app-modeler` owns interaction, selection, gizmos, history UI,
and USD/glTF import/export.

Run `clojure -M:test`.
