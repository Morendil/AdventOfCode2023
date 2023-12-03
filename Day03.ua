IsDigit ← ∊:+@0⇡10
IsSpecial ← × ⊃(≠@.)(¬ IsDigit)
Grid ← ⊜∘≠,@\n
Offsets ← ▽ ¬ ≡≍ ¤ 0_0 . ☇ 1 ⊠⊟ . -1 [0 1 2]
Neighbours ← /↥≡(⬚0 ↻) Offsets ¤

TaggedNumbers ← ▽⊜/↥:⊙IsDigit⊙(: ⊜parse IsDigit ..)
⍤ ⊃⋅∘ ≍ [467] TaggedNumbers [0 0 1 1 1 0 0 0 1 0] "467..114.#"

PartOne ← /+ ♭ ⬚0 ≡TaggedNumbers Neighbours IsSpecial . Grid
PartTwo ← (
  Grid
  ⊚=@*. # Enumerate gears
  # For each gear, get the part numbers it touches
  ≡(□ ⊐/⊂ ≡(□ TaggedNumbers) : ⊔ , Neighbours ⬚0 + ⍘⊚ ¤ : ≠.⊔,) ⊙□
  # Keep where length is two
  ▽=2≡(⧻ ⊔).
  # Unbox, multiply, sum
  /+≡(/×⊔)
)

&fras "day03.txt"
PartOne

&fras "day03.txt"
PartTwo
