# kami-engine-modeling

Portable CLJC polygon-editing engine. Meshes, scenes, edit operations and
modifier stacks are immutable EDN. The current kernel includes validated mesh
contracts, component transforms, extrusion/inset/bevel/loop/knife operations,
BSP Boolean operations, normals/UVs/material data, and non-destructive mirror,
array and subdivision modifiers. `kami-app-modeler` owns interaction,
selection, gizmos, history UI and interchange workflows.

This repository is the tessellated mesh layer of the wider exact-CAD plan.
`kami.modeling.nurbs` now provides validated rational NURBS curve/surface
evaluation, outer/inner UV trimming and provenance-carrying surface
tessellation. `kami.modeling.brep`
adds stable vertex/edge/coedge/loop/face/shell/body topology, analytic surfaces,
closed-manifold validation and B-rep face provenance on derived meshes. It does
not yet claim a complete feature kernel or production assembly, manufacturing
drawing or qualified CAE support. `kami.modeling.step` implements a fail-closed ISO
10303-21/AP242 geometric subset for planar, line-edged closed B-rep topology;
its deterministic internal gate round-trips 100 generated closed bodies. This
is explicitly not an external interoperability corpus or full AP242
conformance. The shared document graph, adapter
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

`kami.modeling.cae` adds solver-neutral, revision-bound studies with SI units,
materials, boundary conditions, loads, adapter provenance and stale-result
detection. Its transparent 1D linear-static bar FEM reference is verified
against the analytic solution, reaction/energy balance and mesh refinement. It
is a verification oracle, not a safety-certified production solver.

`kami.modeling.feature-graph` generalizes the linear modifier stack into a typed
dependency DAG with cycle/type diagnostics, deterministic cache keys, partial
invalidation, disabled nodes, structured evaluation failures and last-valid
preview retention. The existing UI stack remains a compatible projection.

`kami.modeling.large-scene` separates shared geometry from instances and adds
AABB BVH construction, frustum culling, distance LOD, spatial chunks,
resident-byte-budget streaming plans and portable stable picking IDs. The data
gate covers 10,000 instances collapsed to one shared upload and instanced draw;
browser/device p95 and 100-million-triangle streaming gates remain outstanding.

`kami.modeling.collaboration` adds actor/logical-time/parent/precondition
operations, deterministic replay, offline branches, semantic-path merge,
explainable conflicts, selective inverse operations, checkpoints and audit.
The gate covers 10,000 operations without requiring full-log replay after a
checkpoint. Authorization, signatures, network transport and ephemeral presence
remain integration gates.

`kami.modeling.document` is the first shared CAD foundation: a versioned
immutable document graph with stable UUID nodes, explicit units and modeling
tolerance, deterministic revision IDs, projection provenance/staleness checks,
and a non-destructive adapter from existing polygon scenes. Exact geometry,
drawings, CAE and collaboration history build on this contract rather than
using renderer meshes as their source of truth.

Run `clojure -M:test` (currently 52 tests / 461 assertions).
