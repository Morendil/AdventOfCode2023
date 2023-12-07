Lines ← ⊜□ ≠@\n.
Words ← ⊜□ ≠@\s.⊔
Prepare ← ≡(⊔) ⊙(≡(parse ⊔)) ⍘⊟ ⍉
CardValue ← ⊗ : "23456789TJQKA"
JokerCardValue ← ⊗ : "J23456789TQKA"
HandType ← ⇌ ⊏ ⍏ . ⊕⧻ . ⊛ ⊏ ⍏ .
JokerHandType ← ⬚0 + ¤ ⧻ : ⊙(([]|⊔ ⊡ 1)-1 ⧻.) HandType⊔⊢. ⊕□ = @J.
SortBidsByHandValues ← ⊏⍏
Ranks ← +1 ⇡ ⧻
Winnings ← /+×

PartOne ← Winnings Ranks . SortBidsByHandValues ≡(⊂ ⊃(□ HandType) (CardValue)) Prepare ≡(Words) Lines
PartTwo ← Winnings Ranks . SortBidsByHandValues ≡(⊂ ⊃(□ JokerHandType) (JokerCardValue)) Prepare ≡(Words) Lines

&fras "day07.txt"
PartOne

&fras "day07.txt"
PartTwo
