IsDigit ← ∊:+@0⇡10
IsSpecial ← × ⊃(≠@.)(¬ IsDigit)
Grid ← ⊜∘≠,@\n
Offsets ← ▽ ¬ ≡≍ ¤ 0_0 . ☇ 1 ⊠⊟ . -1 [0 1 2]
Neighbours ← /↥≡(⬚0 ↻) Offsets ¤ IsSpecial

TaggedNumbers ← ▽⊜/↥:⊙IsDigit⊙(: ⊜parse IsDigit ..)
⍤ ⊃⋅∘ ≍ [467] TaggedNumbers [0 0 1 1 1 0 0 0 1 0] "467..114.#"

PartOne ← /+ ♭ ⬚0 ≡TaggedNumbers Neighbours . Grid

&fras "day03.txt"
PartOne
