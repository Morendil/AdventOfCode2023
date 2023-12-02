Split ← ⊜□≠,
Get ← (0|parse ⊔ ⊢ Split @ )/+⌕ ⊙.
Possible ← = 1 ¬ /+≡(/+ > 12_13_14)
Lookup ← /+≡(Get ⊙⊔) ¤
Sample ← ≡(Lookup ⊔){"red" "green" "blue"} ¤ Split @,
Game ← ≡(Sample ⊔) Split @; ⊔⊢⇌ Split @:

# Domain (part 1)
⍤ ⊃⋅∘ ≍ 1 Possible [6_3_1 1_2_2]
⍤ ⊃⋅∘ ≍ 0 Possible [20_8_6 6_3_1 1_2_2]
# Parsing
⍤ ⊃⋅∘ ≍ 12 Get "red" "12 red"
⍤ ⊃⋅∘ ≍ 6_3_1 Sample "6 red, 1 blue, 3 green"
⍤ ⊃⋅∘ ≍ [6_3_1 1_2_2] Game "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

# Part 1
&fras "day02.txt"
Split @\n
/+ +1 ⊚ ≡(Possible Game ⊔)
