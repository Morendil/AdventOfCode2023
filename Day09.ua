Numbers ← ⊜⋕ ≠,@ 
Lines ← ⊜□ ≠,@\n
Derive ← - ↯ ¤ ⊙: ⧻ . ↘ 1 .
Derivatives ← ;⍢(⊃(Derive °□) (⊂ □))(>0/+≠0.°□) : [] □
Extrapolate ← /+≡(⊢ ⇌ °□)
Backpolate ← /-≡(⊢°□)
⍤ ⊃⋅∘ ≍ 18 Extrapolate Derivatives [0 3 6 9 12 15]

PartOne ← /+ ≡(Extrapolate Derivatives Numbers °□) Lines
PartTwo ← /+ ≡(Backpolate Derivatives Numbers °□) Lines

&fras "day09.txt"
PartOne

&fras "day09.txt"
PartTwo
