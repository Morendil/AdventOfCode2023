# Solve Day 25 using the Stoer-Wagner min-cut
# https://en.wikipedia.org/wiki/Stoer%E2%80%93Wagner_algorithm
# Super slow, should be optimized, but does work…

Split ← ⊜□≠,
Words ← ⊜□ ∊:+@a⇡26.
WordMatrix ← ⬚(□ "   ")≡(Words ⊔)
Label ← ⍜(♭)(↘ 1 ⊛ ⊂ □ "   ")
Adjacencies ← ≡(⊂ :):¤⊙(⊏ ⊚ ≠ 0. ↘ 1)⊡ 0.
AllAdjacencies ← ⊐(/⊂ ≡(Adjacencies))
ToMatrix ← ⍘⊚-1
TimesItself ← ≡(/+×)¤
MakeSquare ← ⬚0+↯:0⊂.⊢△.
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
# AdjacencyMatrix VertexSet CutOfPhase -- AdjacencyMatrix VertexSet' CutOfPhase'
WeightsFromVertexSet ← ⊙(/+⊏).⊙(.)
RemoveVertexSet ← ⊙(⍜⊡ (×0) ≡(¤)).
TightAndCut ← ⊙(⊃(⊢ ⇌ ⍏)(⊏ ⊢ ⇌ ⍏ .))
MinCutPhaseStep ← : ⊂ TightAndCut RemoveVertexSet WeightsFromVertexSet ;
# Without the CutOfPhase:
# MinCutPhaseStep ← ⊂ ⊙(⊢ ⇌ ⍏ ⍜⊡ (×0) ≡(¤)). ⊙(/+⊏).⊙(.)
ZeroDiagonal ← ∧(⍜⊡ (×0)⊟.)⇡ ⧻ .
MergeTwo ← ⊙(RemoveRow ⊢ ⇌) . ⊙(ReplaceRow) . ⊙(MergeRows) .
PerformMerge ← ⊙(ZeroDiagonal) ⊙(⍉) MergeTwo ⊙(⍉) MergeTwo
MinCutPhaseFinish ← ⊙(RememberMerged PerformMerge LastTwoVertices)
MinCutPhaseInit ← ⊙(0 [0]) - 1 ⧻ .
# Merges AdjacencyMatrix -- Merges' AdjacencyMatrix' CutOfPhase
MinCutPhase ← MinCutPhaseFinish ⍥(MinCutPhaseStep) MinCutPhaseInit
# MinMerges MinCut Merges AdjacencyMatrix Cut -- MinMerges' MinCut' Merges AdjacencyMatrix
CompareToRecord ← ⊙(⊙((;;|⊙⊙(;;))⊙(:)) : ⊙(.) : ⊙(<:,,):):

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
  0_0_0_0_0_0_0_0
]
StoerFirstMerge ← [
  0_3_0_4_2_0_0
  3_0_4_0_0_2_0
  0_4_0_0_0_2_2
  4_0_0_0_3_0_0
  2_0_0_3_0_1_0
  0_2_2_0_1_0_3
  0_0_2_0_0_3_0
]
⍤ ⊃⋅∘ ≍ StoerFirstMerge ; PerformMerge 4_0 StoerOriginal

&fras "day25_sample.txt"
Split @\n
AdjacencyMatrix
⧻ .
# Keep length way up the stack for last phase
⊙(:).
⊙⊙([]∞[])
-2
⍥(dump△ CompareToRecord MinCutPhase)
# Discard last matrix, last merge, and min cut value; keep min-cut merge
;;;
CountPartitions
# Get the max
⊏⊢⇌⍏.
# Subtract from total count and multiply together
× ⊙(-) .
