IsAlphaNum ← ∊:⊂+@0⇡10+@A⇡26
Split ← ⊜□≠,
SplitOn ← ⊐⊜∘ ¬ ⌕ : ,
Directions ← ⊔:⍉ ≡(⊜□ IsAlphaNum.) SplitOn "\n" ⊔ : ⍘⊟ SplitOn "\n\n"

# On stack: <@L or @R> Directions [box "ONE"]
# After: [box "TWO" box "ONE"] Directions
Step ← ⊙⊂ : ⊡ ⊙(.) ⊂ ⊙(:⊙(:⊙(⊗ ⊡ 0). :) : ⊡ 0 .) +1=@R
# On stack : "BEG" "LRLRR" Directions
# After : Directions Path
Path ← ∧(Step) ⊙: : ¤
# Directions -> Table
Table ← ≡(⇌ ; Path ⊙(∩⊔ ⍘⊟ ⊔)) ⊙(¤ □ ⊟ ∩□) : ⊙(⊢.)
# box BEG Table -> box END (of row) Table
StartToEnd ← ⊢ ⊡ ⊚ ≡(⇌)⌕ ⊙(..)
# box BEG Table -> box END Table
FollowFast ← ⍢(StartToEnd)(≠@Z ⊢ ⇌ ⊔ dump∘)
# Table -> Start nodes
Starts ← ⊏ ⊚ ≡(=@A ⊢ ⇌ ⊔) . ⊢ ⍉

&fras "day08_sample2.txt"
Directions
Table
Starts .
⊡ 1
StartToEnd
StartToEnd
