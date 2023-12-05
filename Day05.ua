Split ← ⊜□≠,
SplitOn ← ⊐⊜∘ ¬ ⌕ : ,
NumArray ← ≡(parse ⊔) Split @ 
ParseMaps ← ≡(□ ≡(NumArray ⊔) Split @\n ⊔)
Prepare ← : ⊙(↘ 1) ↙ 1 . ≡(⊡ 1 Split @: ⊔) SplitOn "\n\n"

# &fras "day05_sample.txt"
# Prepare
# ParseMaps ⊙(NumArray ⊔ ⊡ 0)
# :

# ⍤ ⊃⋅∘ ≍ ⊟ □ [] □ [79 14] Transform [50 98 2] [79 14]
Overlap ← ×⊃(< /+ ⊙(⊡ 1)) (> ⊙(/+) ⊡ 1)
⍤ ⊃⋅∘ ≍ 0 Overlap [50 98 2] [79 14]

WithinStart ← ↥ ⊙(⊡ 0) ⊡ 1
Within ← ⊟ ⊃(+⊃(WithinStart)(-:0/-↙ 2))(-⊃(WithinStart)(↧ ⊙(/+) /+ ↘ 1))
BelowStart ← ↧ ⊙(⊡ 0) ⊡ 1
Below ← ⊟ ⊃(BelowStart)(-⊃(BelowStart)(↧ ⊙(/+) ⊡ 1))
AboveStart ← ↥ ⊙(⊡ 0) /+ ↘ 1
Above ← ⊟ ⊃(AboveStart)(-⊃(AboveStart)(/+;))
⍤ ⊃⋅∘ ≍ 50_29 Below [81 79 14] [50 50]
⍤ ⊃⋅∘ ≍ 93_7 Above [81 79 14] [50 50]
⍤ ⊃⋅∘ ≍ 81_14 Within [81 79 14] [50 50]
# ⊂ ⊃(□ 81_14;;) (□ ⊟ ⊃(50_29;;) (93_7;;))
# ⍤ ⊃⋅∘ ≍ [□ 81_14 □ [50_29 93_7]]
