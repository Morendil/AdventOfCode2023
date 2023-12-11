Grid ← ⊜∘≠,@\n
Coordinates ← ⊚ =@#
Manhattan ← /+ ⌵ -
ExpandBy ← ⊙(;)⇌ ↘ 1∧(⊂ + ⊙(⊢ .) (,|1) > 0 /+ ⌕ @#) ⊙([0]) :
SumDistances ← ÷ 2 /+/+ ⊠Manhattan .
CoordinatesExpandedBy ← ⍜⍉ ≡⊏ ; ⊃(⊙Coordinates) (⊟ ⊃(ExpandBy) (ExpandBy ⊙⍉))

PartOne ← SumDistances CoordinatesExpandedBy 2 Grid
PartTwo ← SumDistances CoordinatesExpandedBy 1000000 Grid

&fras "day11.txt"
PartOne

&fras "day11.txt"
PartTwo
