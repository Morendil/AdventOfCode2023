# Solve Day 25 using the Stoer-Wagner min-cut
# https://en.wikipedia.org/wiki/Stoer%E2%80%93Wagner_algorithm
# Super slow, should be optimized, but does work…

Split ← ⊜□≠,
Words ← ⊜□ ∊:+@a⇡26.
WordMatrix ← ⬚(□ "   ")≡(Words ⍘□)
Label ← ⍜(♭)(↘ 1 ⊛ ⊂ □ "   ")
Adjacencies ← ≡(⊂ :) :¤⊙(⊏ ⊚ ≠ 0. ↘ 1)⊡ 0.
AllAdjacencies ← /⊐⊂ ≡(□Adjacencies)
ToMatrix ← ⍘⊚-1
TimesItself ← ≡(/+×)¤
MakeSquare ← ⬚0↙↥⇌.△.
Undirect ← +⍉.

# MergedArray AdjacencyMatrix -- MergedArray AdjacencyMatrix
RememberMerged ← ⊙⊂ : ¤
# VertexSet -- ArrayOfRows
LastTwoVertices ← ⇌ ↙ 2 ⇌
# Array N -- Array
RemoveRow ← ⬚1 ▽ ¬ ⍘⊚ ¤
RemoveCol ← ⍜⍉ RemoveRow
# Array Row ArrayOfRows -- Array
ReplaceRow ← ⍜⊡ ; ⊙(:) ⊢
# Array ArrayOfRows -- Array MergedRow
MergeRows ← /+ ⊡ ≡(¤) ⊙(.)

# WeightsFromVertexSet ← |3 /+⋅⊏ # slow version: use whole vertex set
WeightsFromVertexSet ← |3 +⊙(⊡⊢⇌) # fast version: add only latest vertex
# RemoveVertexSet ← ⊙(⍜⊡ (×0) ≡(¤)). # slow: fix and pick
RemoveVertexSet ← ⊙(⍜⊏ (×0)). # fast: select
# TightAndCut ← ⊙(⊃(⊢ ⇌ ⍏)(⊏ ⊢ ⇌ ⍏ .)) # slow: sort twice
TightAndCut ← ⊙(⊃∘⊡ ⊢⇌⍏.) # fast: sort once
MinCutPhaseStep ← (
  ⊙;
  # weightsacc vertexset adjmatrix
  ⊃(.WeightsFromVertexSet|⋅⊙∘)
  # weightsacc' weights vertexset adjmatrix
  ⊙(: ⊂ TightAndCut RemoveVertexSet :)
)
# Without the CutOfPhase:
# MinCutPhaseStep ← ⊂ ⊙(⊢ ⇌ ⍏ ⍜⊡ (×0) ≡(¤)). ⊙(/+⊏).⊙(.)
ZeroDiagonal ← ∧(⍜⊡ (×0)⊟.)⇡ ⧻ .
MergeTwo ← ⊙(RemoveRow ⊢ ⇌) . ⊙(ReplaceRow) . ⊙(MergeRows) .
PerformMerge ← ⊙(ZeroDiagonal) ⊙(⍉) MergeTwo ⊙(⍉) MergeTwo
MinCutPhaseFinish ← ⊙(RememberMerged PerformMerge LastTwoVertices)
MinCutPhaseInit ← ⊙(0 [0]) ⊃(-1⧻)∘
MinCutPhaseInit ← ⊃(-1⧻|▽:0 ⧻|0 [0]|∘)
# Merges AdjacencyMatrix -- Merges' AdjacencyMatrix' CutOfPhase
MinCutPhase ← MinCutPhaseFinish ; ⍥(MinCutPhaseStep) MinCutPhaseInit
# MinMerges MinCut Merges AdjacencyMatrix Cut -- MinMerges' MinCut' Merges AdjacencyMatrix
CompareToRecord ← ⊃(
  ⋅⊙∘ # keep AdjacencyMatrix & Merges
| ⊃(
    >⊙⋅⋅∘ # compare Cut to MinCut
  | ⊙⋅⊙⊙∘ # drop AdjacencyMatrix
  )
  (⋅⋅⊙∘|⊙∘) # keep MinCut & MinMerges, or copy Cut & Merges
)

# Merge -- Array of merge sizes
CountPartitions ← ∧(;MergeTwo)⊙(⬚1 ↯ : [] ¤.)⇌↘ 1

AdjacencyMatrix ← Undirect MakeSquare ToMatrix AllAdjacencies Label WordMatrix

StoerOriginal ← Undirect [
  0_2_0_0_3_0_0_0
  0_0_3_0_2_2_0_0
  0_0_0_4_0_0_2_0
  0_0_0_0_0_0_2_2
  0_0_0_0_0_3_0_0
  0_0_0_0_0_0_1_0
  0_0_0_0_0_0_0_3
  0_0_0_0_0_0_0_0]
StoerFirstMerge ← [
  0_3_0_4_2_0_0
  3_0_4_0_0_2_0
  0_4_0_0_0_2_2
  4_0_0_0_3_0_0
  2_0_0_3_0_1_0
  0_2_2_0_1_0_3
  0_0_2_0_0_3_0]
⍤ ⊃⋅∘ ≍ StoerFirstMerge ; PerformMerge 4_0 StoerOriginal

PartOne ← (
  Split @\n
  AdjacencyMatrix
  ⧻ .
  # Keep length way up the stack for last phase
  ⊙(:).
  ⊙⊙([]∞[])
  -2
  ⍥(CompareToRecord MinCutPhase)
  # Discard last matrix, last merge, and min cut value; keep min-cut merge
  ;;
  ⍤"min cut should be 3"=3
  CountPartitions
  # Get the max
  /↥
  # Subtract from total count and multiply together
  × ⊃∘-
)

&fras "day25.txt"
PartOne
