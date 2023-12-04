IsDigit ← ∊:+@0⇡10
Split ← ⊜□≠,
Card ← ≡(□ ⊜parse IsDigit. ⊔) Split @| ⊔ ⊡ 1 Split @:
Winners ← |1 ⧻▽∊,/⊔
Value ← (ⁿ : 2 - 1|0)=0.

Effects ← ;:⬚0≡(⊂⊙(▽ : 1)▽ :0+1,) :⇡⧻.
Step ← +1⊙(+×⊙:)⊙(⊡).⊙(.)
Run ← |1 ;⍥(Step) : ⊙(⧻.) 0 ▽:1 ⧻. /∘ ⇌ Effects

PartOne ← /+ ≡(Value Winners Card ⊔) Split @\n
PartTwo ← /+ Run ≡(Winners Card ⊔) Split @\n

⍤ ⊃⋅∘ ≍ [□ [41 48 83 86 17] □ [83 86 6 31 17 9 48 53]] Card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
⍤ ⊃⋅∘ ≍ 8 Value Winners Card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"

&fras "day04.txt"
PartOne

&fras "day04.txt"
PartTwo
