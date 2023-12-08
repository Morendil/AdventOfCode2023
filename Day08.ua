IsAlphaNum ← ∊:⊂+@0⇡10+@A⇡26
Split ← ⊜□≠,
SplitOn ← ⊐⊜∘ ¬ ⌕ : ,
Prepare ← : ⊙(↘ 1) ↙ 1 . ≡(⊡ 1 Split @: ⊔) SplitOn "\n\n"

Step ← (
  +1=@R
  ⊙(:⊙(:⊙(⊗ ⊡ 0). :) : ⊡ 0 .)
  ⊂
  ⊡ ⊙(.)
  :
  ⊙⊂
)

&fras "day08_sample2.txt"
: ⍘⊟ SplitOn "\n\n"
SplitOn "\n" ⊔
≡(⊜□ IsAlphaNum.)
⊔ : ⍉
: ⊙(⊡ 0 .)
⊙(¤ ⊟∩□)
dump∘
≡(1;dump∘)
