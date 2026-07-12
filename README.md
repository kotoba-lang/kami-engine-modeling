# kami-engine-modeling

Portable CLJC polygon-editing engine. Meshes, scenes, edit operations and
modifier stacks are immutable EDN. The current kernel includes validated mesh
contracts, component transforms, extrusion/inset/bevel/loop/knife operations,
BSP Boolean operations, normals/UVs/material data, and non-destructive mirror,
array and subdivision modifiers. `kami-app-modeler` owns interaction,
selection, gizmos, history UI and interchange workflows.

This repository is the tessellated mesh layer of the wider exact-CAD plan.
`kami.modeling.nurbs` now provides validated rational NURBS curve/surface
evaluation and provenance-carrying surface tessellation. `kami.modeling.brep`
adds stable vertex/edge/coedge/loop/face/shell/body topology, analytic surfaces,
closed-manifold validation and B-rep face provenance on derived meshes. It does
not yet claim a complete feature kernel or production assembly, manufacturing
drawing or qualified CAE support. `kami.modeling.step` implements a fail-closed ISO
10303-21/AP242 geometric subset for planar, line-edged closed B-rep topology;
it is explicitly not full AP242 conformance. The shared document graph, adapter
boundaries and 1/5–5/5 gates
are defined in
[`kami-engine` ADR-0049](../kami-engine/90-docs/adr/0049-kotoba-3d-suite-commercial-cad-maturity.md).

`kami.modeling.assembly` provides immutable part definitions and occurrences,
configurations/suppression, grounded state, deterministic coincident/distance
mate solving, DOF and over-constraint diagnostics, and AABB interference
depth/volume. Rotational, gear and full kinematic solving remain explicit
unsolved constraints until their production solvers land.

`kami.modeling.drawing` adds model-revision-associated sheets, orthographic box
views, semantic dimensions, stale/orphan diagnostics, assembly BOM generation,
ISO/ASME-oriented paper/projection settings and deterministic vector SVG output.
Section/detail views, hidden-line removal, GD&T and DXF/PDF remain subsequent gates.

`kami.modeling.document` is the first shared CAD foundation: a versioned
immutable document graph with stable UUID nodes, explicit units and modeling
tolerance, deterministic revision IDs, projection provenance/staleness checks,
and a non-destructive adapter from existing polygon scenes. Exact geometry,
drawings, CAE and collaboration history build on this contract rather than
using renderer meshes as their source of truth.

Run `clojure -M:test` (currently 37 tests / 198 assertions).
