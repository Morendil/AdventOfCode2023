Split ← ⊜□≠,
IsDigit ← ∊:+@0⇡10
Ints ← ⊜⋕ IsDigit .
ParseOne ← Ints °□ ⊡ 1 Split @: °□
ParseTwo ← ⋕ ▽ IsDigit . °□ ⊡ 1 Split @: °□

WaysToBeat ← /+ > : × ⊙- . ⇡ +1 .
⍤ ⊃⋅∘ ≍ 4 WaysToBeat 7 9
⍤ ⊃⋅∘ ≍ 8 WaysToBeat 15 40
⍤ ⊃⋅∘ ≍ 9 WaysToBeat 30 200

PartOne ← /× ≡(WaysToBeat °⊟) ⍉ ≡(ParseOne) Split @\n
PartTwo ← /× WaysToBeat °⊟ ≡(ParseTwo) Split @\n

&fras "day06.txt"
PartOne

&fras "day06.txt"
PartTwo
