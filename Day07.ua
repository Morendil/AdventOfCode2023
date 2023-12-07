Lines ← ⊜□ ≠@\n.
Words ← ⊜□ ≠@\s.⊔
Prepare ← ≡(⊔) ⊙(≡(parse ⊔)) ⍘⊟ ⍉
CardValue ← ⊗ : "23456789TJQKA"
HandType ← ⇌ ⊏ ⍏ . ⊕⧻ ⊛ ⊏ ⍏ .
TypeValue ← ⊗□:⊐[1_1_1_1_1 2_1_1_1 2_2_1 3_1_1 3_2 4_1 5]
SortBidsByHandValues ← ⊏⍏
Ranks ← +1 ⇡ ⧻ .
Winnings ← /+×

PartOne ← Winnings Ranks SortBidsByHandValues ≡(⊂ ⊃(TypeValue HandType) (CardValue)). Prepare ≡(Words) Lines

&fras "day07_sample.txt"
PartOne
