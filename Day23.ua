Split ← ⊜∘≠,
Tagged ← ⍜(♭)(⊗:"#.>v")

Offsets ← [0_1 1_0 0_¯1 ¯1_0]
Arrows ← (∘|∘|+0_1|+1_0)

# Tagged -- [Node]
Nodes ← ⊂⊂⊃⊃(0_1)(⊚ >2 /+ ≡(↻) Offsets ¤ >1)(⊟:-1.-1⧻)
# Pos Tagged - [Neighbour(s)]
Next ← ▽⊃(>0 ⍣⊡(⋅⋅⋅0))(∘) +Offsets ¤
# Path Tagged - Path'
Step ← ⊂ ⍜(:)(▽≥⊃⊃(⋅⧻)(⊗)(⊙∘)) ⊃∘ (Next ⊢ ⇌)
# Path Tagged - On path (= exactly 1 next step)
OnPath ← =1 ⧻ ; ⍜(:)(▽≥⊃⊃(⋅⧻)(⊗)(⊙∘)) ⊃∘ (Next ⊢ ⇌)
# Path Tagged - Path' (until end or node)
Walk ← ⊙(;) ⍢(⊃Step(⋅∘))(OnPath)
# [Path] Tagged -- [Path'] compatible
Forward ← ▽ ⊃≡(¬/×/=⍜(⊡1)(Arrows ⊃⊡ ∘)) ∘
# [Path] Nodes -- [From To Length]
Edge ← [⊃⊃(⊗ ⊢)(⊗ ⊢ ⇌)(-1 ⧻)]
# [Nodes] Tagged -- [[Path]] one-step paths
StepAll ← ∧(⊂: ≡(⊂:∩¤) ⊃Next(¤∘)) ⊙⊙[] ⊙(¤) ⍜⇌(↘1)

# Tagged -- [From To Length]
ToWeightedEdges ← (
  ⊃Nodes ∘
  ⊃StepAll (∩∘)
  ⊃(Forward ⊙(¤) ⊙⋅∘)(⋅⊙∘)
  ⊙(∩¤ :)
  ≡(Edge Walk)
)

From ← ▽⊡0⍉⌕⊙.
AtEndNode ← =⊃(⋅(/↥ ≡(⊡1)))(⊡1)
FollowEdge ← ⊂: + ⊃(From⊡1)(¤ [0 0 ⊡ 2])
FollowAll ← ∧((FollowEdge|⊂:⊙(⋅∘)) ⊃(AtEndNode)⊙∘) ⊙⊙[] ⊙(¤) ⊙(.)

# Converting grid to weighted edges
Sample ← Tagged Split @\n &fras "day23_sample.txt"
SampleGraph ← [0_2_15 1_3_30 1_5_24 2_1_22 2_4_22 3_7_10 4_5_12 4_6_38 5_3_18 5_6_10 6_7_10 7_8_5]
⍤ ⊃⋅∘ ≍ [1_1] Next 0_1 Sample
⍤ ⊃⋅∘ ≍ [0_1 1_1] Step [0_1] Sample
⍤ ⊃⋅∘ ≍ 16 ⧻ Walk [0_1] Sample
⍤ ⊃⋅∘ ≍ SampleGraph ToWeightedEdges Sample

# Recursively walking paths in graph
⍤ ⊃⋅∘ ≍ [0_2_15] From 0 SampleGraph
⍤ ⊃⋅∘ ≍ [2_1_22 2_4_22] From 2 SampleGraph
⍤ ⊃⋅∘ ≍ [0_2_15] FollowEdge 0_0_0 SampleGraph []
⍤ ⊃⋅∘ ≍ [2_1_37 2_4_37] FollowEdge 0_2_15 SampleGraph []
⍤ ⊃⋅∘ ≍ [2_1_37 2_4_37] ⊙(;) FollowAll [0_2_15] SampleGraph
⍤ ⊃⋅∘ ≍ [1_3_67 1_5_61 4_5_49 4_6_75] ⊙(;) FollowAll [2_1_37 2_4_37] SampleGraph

# Part One
Start ← [0_0_0]
MaxHike ← /↥≡(⊡2)
FollowAllToEnd ← ⊙(;) ⍢(FollowAll)(¬/× ≡(AtEndNode) ⊙(¤))
PartOne ← MaxHike FollowAllToEnd Start ToWeightedEdges Tagged Split @\n

&fras "day23.txt"
PartOne
