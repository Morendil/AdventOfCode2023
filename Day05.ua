Split ← ⊜□≠,
SplitOn ← ⊐⊜∘ ¬ ⌕ : ,
NumArray ← ≡(parse ⊔) Split @\s
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
Within ← ⊂ ⊃(+⊃(WithinStart)(-:0/-↙ 2))(-⊃(WithinStart)(↧ ⊙(/+) /+ ↘ 1))
BelowStart ← ↧ ⊙(⊡ 0) ⊡ 1
Below ← ⊂ ⊃(BelowStart)(-⊃(BelowStart)(↧ ⊙(/+) ⊡ 1))
AboveStart ← ↥ ⊙(⊡ 0) /+ ↘ 1
Above ← ⊂ ⊃(AboveStart)(-⊃(AboveStart)(/+;))
Good ← ▽ ≡(> 0 ⊡ 1) .
⍤ ⊃⋅∘ ≍ 50_29 Below [81 79 14] [50 50]
⍤ ⊃⋅∘ ≍ 93_7 Above [81 79 14] [50 50]
⍤ ⊃⋅∘ ≍ 81_14 Within [81 79 14] [50 50]

# [□ [[79 14]] □ []]
# [50_98_2 52_50_48]
# fold(Step)
# [□ [[79 14]] □ []]
# 50_98_2
# Step
# 52_50_48
# ⊔ ⊡ 0 ,¤
# ≡(⊃Unmapped Mapped :)

[]           # Mapped
[79_14 50_2] # Unmapped
52_50_48
≡(⊃(Good [Within])(Good ⊟ ⊃Below Above):) : ¤
# ⊙(⊂≡(⊢)) :
# ⊢
# ⍤ ⊃⋅∘ ≍ ↯ 0_2[]
# ⍤ ⊃⋅∘ ≍ [81_14 52_2]

# []           # Mapped
# [79_14 50_2] # Unmapped
# 50_98_2
# ≡(⊃(Good [Within])(Good ⊟ ⊃Below Above):) : ¤
# ⊙(⊂≡(⊢)) :
# ⊢
