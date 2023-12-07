Split ← ⊜□≠,
SplitOn ← ⊐⊜∘ ¬ ⌕ : ,
NumArray ← ≡(parse ⊔) Split @\s
ParseMaps ← ≡(□ ≡(NumArray ⊔) Split @\n ⊔)
Prepare ← : ⊙(↘ 1) ↙ 1 . ≡(⊡ 1 Split @: ⊔) SplitOn "\n\n"
PairSeeds ← ↯ ⊂ : 2 ÷2⧻.

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

Step ← ⊙⊂ ⊃(Good ⊂ ≡(⊃(Below)(Above) :))(Good ≡(Within :)) : ¤
Step 52_50_48 [79_14 50_2] []
⍤ ⊃⋅∘ ≍ ↯ 0_2 []
⍤ ⊃⋅∘ ≍ [81_14 52_2]

&fras "day05_sample.txt"
Prepare
ParseMaps ⊙(NumArray ⊔ ⊡ 0)
⊙(PairSeeds)
⊙(: [])
∧(∧(: [] ⊂ Step) ⊔)
