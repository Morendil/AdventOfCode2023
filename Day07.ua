Lines ← ⊜□ ≠@\n.
Words ← ⊜□ ≠@\s.⊔
Prepare ← ≡(⊔) ⊙(≡(parse ⊔)) ⍘⊟ ⍉
CardValue ← ⊗ : "23456789TJQKA"
JokerCardValue ← ⊗ : "J23456789TQKA"
HandType ← ⇌ ⊏ ⍏ . ⊕⧻ . ⊛ ⊏ ⍏ .
JokerHandType ← ⬚0 + ¤ ⧻ : ⊙(([]|⊔ ⊡ 1)-1 ⧻.) HandType⊔⊢. ⊕□ = @J.
TypeValue ← ⊗□:⊐[1_1_1_1_1 2_1_1_1 2_2_1 3_1_1 3_2 4_1 5]
SortBidsByHandValues ← ⊏⍏
Ranks ← +1 ⇡ ⧻
Winnings ← /+×

PartOne ← Winnings Ranks . SortBidsByHandValues ≡(⊂ ⊃(TypeValue HandType) (CardValue)) Prepare ≡(Words) Lines
PartTwo ← Winnings Ranks . SortBidsByHandValues ≡(⊂ ⊃(TypeValue JokerHandType) (JokerCardValue)) Prepare ≡(Words) Lines

&fras "day07.txt"
PartOne

&fras "day07.txt"
PartTwo
