Digits ← {"zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"}
📜 ← ⊜□≠@\n.
👆 ← ∊:+@0⇡10
ParseFirstAndLast ← parse⊟⊃⊢(⊢⇌)
Nip ← ;:
🛏️ ← ⬚@_ ↙ Nip ⧻.
Tails ← ≡(□ ↘) ⊙¤ ⇡ ⧻ .
IsPrefix ← ≍ ⊃∘🛏️
StartsWithWhichDigit ← (⊗ 1 ≡(IsPrefix ⊔) Digits ¤|parse ↙ 1)👆⊢.

PartOneDigits ← ▽👆.
PartTwoDigits ← +@0 ▽ <10 . ≡(StartsWithWhichDigit ⊔) Tails

PartOne ← /+ ≡(ParseFirstAndLast PartOneDigits ⊔)
PartTwo ← /+ ≡(ParseFirstAndLast PartTwoDigits ⊔)

$ 1abc2
$ pqr3stu8vwx
$ a1b2c3d4e5f
$ treb7uchet
📜
;
@1
👆
;
$ 1abc2
parse▽👆.
;
$ 1abc2
PartOneDigits
ParseFirstAndLast
;
$ 1abc2
$ pqr3stu8vwx
$ a1b2c3d4e5f
$ treb7uchet

📜
PartOne
⍤ ⊃⋅∘ ≍ 142

$ two
⊗ : Digits □
⍤ ⊃⋅∘ ≍ 2

⍤ ⊃⋅∘ ≍ 1 IsPrefix "two" "twok"
⍤ ⊃⋅∘ ≍ 0 IsPrefix "twok" "two"

⍤ ⊃⋅∘ ≍ 2 StartsWithWhichDigit "twok"
⍤ ⊃⋅∘ ≍ 10 StartsWithWhichDigit "zilch"
⍤ ⊃⋅∘ ≍ 1 StartsWithWhichDigit "1zilch"

$ twok
Tails
⍤ ⊃⋅∘ ≍ {"twok" "wok" "ok" "k"}

$ xtwone3four
PartTwoDigits
⍤ ⊃⋅∘ ≍ "2134"

&fras "day01.txt"
📜
PartOne
&p

&fras "day01.txt"
📜
PartTwo
&p
