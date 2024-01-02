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
MinCutPhaseFinish ← ⊙(RememberMerged ⊙(RemoveRow ⊢ ⇌) . ⊙(ReplaceRow) . ⊙(MergeRows) . ⊙(RemoveCol ⊢ ⇌) . LastTwoVertices)
MinCutPhaseInit ← ⊙(0 [0]) - 1 ⧻ .
# Merges AdjacencyMatrix -- Merges' AdjacencyMatrix' CutOfPhase
MinCutPhase ← MinCutPhaseFinish ⍥(MinCutPhaseStep) MinCutPhaseInit
# MinMerges MinCut Merges AdjacencyMatrix Cut -- MinMerges' MinCut' Merges AdjacencyMatrix
CompareToRecord ← ⊙(⊙((;;|⊙⊙(;;))⊙(:)) : ⊙(.) : ⊙(<:,,):):

AdjacencyMatrix ← Undirect MakeSquare ToMatrix AllAdjacencies Label WordMatrix

&fras "day25.txt"
Split @\n
AdjacencyMatrix
⧻ .
⊙⊙([]∞[])
-1
⍥(dump△ CompareToRecord MinCutPhase)
