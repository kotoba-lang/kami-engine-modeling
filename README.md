# kami-engine-modeling

Portable CLJC polygon-editing engine. Meshes, scenes, edit operations and
modifier stacks are immutable EDN. The current kernel includes validated mesh
contracts, component transforms, extrusion/inset/bevel/loop/knife operations,
BSP Boolean operations, normals/UVs/material data, and non-destructive mirror,
array and subdivision modifiers. `kami-app-modeler` owns interaction,
selection, gizmos, history UI and interchange workflows.

This repository is the tessellated mesh layer of the wider exact-CAD plan. It
does not claim NURBS/B-rep, STEP, assembly, manufacturing drawing or qualified
CAE support. Their shared document graph, adapter boundaries and 1/5–5/5 gates
are defined in
[`kami-engine` ADR-0049](../kami-engine/90-docs/adr/0049-kotoba-3d-suite-commercial-cad-maturity.md).

Run `clojure -M:test` (currently 21 tests / 122 assertions).
