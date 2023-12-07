Split ← ⊜□≠,
SplitOn ← ⊐⊜∘ ¬ ⌕ : ,
NumArray ← ≡(parse ⊔) Split @\s
ParseMaps ← ≡(□ ≡(NumArray ⊔) Split @\n ⊔)
Prepare ← : ⊙(↘ 1) ↙ 1 . ≡(⊡ 1 Split @: ⊔) SplitOn "\n\n"
PairSeeds ← ↯ ⊂ : 2 ÷2⧻.
MakePairs ← ≡(⊂:1)

WithinStart ← ↥ ⊙(⊡ 0) ⊡ 1
Within ← ⊂ ⊃(+⊃(WithinStart)(-:0/-↙ 2))(-⊃(WithinStart)(↧ ⊙(/+) /+ ↘ 1))
BelowStart ← ↧ ⊙(⊡ 0) ⊡ 1
Below ← ⊂ ⊃(BelowStart)(-⊃(BelowStart)(↧ ⊙(/+) ⊡ 1))
AboveStart ← ↥ ⊙(⊡ 0) /+ ↘ 1
Above ← ⊂ ⊃(AboveStart)(-⊃(AboveStart)(/+;))
Good ← ▽ ≡(> 0 ⊡ 1) .
⍤ ⊃⋅∘ ≍ 50_29 Below [81 79 14] [50 50]
⍤ ⊃⋅∘ ≍ 93_7 Above [81 79 14] [50 50]
⍤ ⊃⋅∘ ≍ 81_14 Within [81 79 14] [50 50]

Reset ← :[]⊂
Step ← ⊙⊂ ⊃(Good ⊂ ≡(⊃(Below)(Above) :))(Good ≡(Within :)) : ¤
Step 52_50_48 [79_14 50_2] []
⍤ ⊃⋅∘ ≍ ↯ 0_2 []
⍤ ⊃⋅∘ ≍ [81_14 52_2]

Generic! ← ⊡ 0_0 ⊏ ⍏. ; : ∧(Reset ∧(Step) ⊔) ⊙(: []) ^2.2 ParseMaps ⊙(NumArray ⊔ ⊡ 0) Prepare
PartOne ← Generic!⊙(MakePairs)
PartTwo ← Generic!⊙(PairSeeds)

&fras "day05.txt"
PartOne

&fras "day05.txt"
PartTwo
