Lines ← ⊜□ ≠@\n.
Words ← ⊜□ ≠@\s.⊔
Numbers ← ⊜parse ≠@,.
Prepare ← ≡(⊔) ⊙(≡(parse ⊔)) ⍘⊟ ⍉

# Cells in the state space are represented as four integers:
# - 0 = Working, 1 = Broken;
# - number of groups remaining to match
# - number remaining in current group
# - count of that cell
Working ← 0
Broken ← 1

WorkingBrokenEnd ← /×⊂⊙(=0_0↙2)=@#
WorkingUnknown ← ×⊙(×⊃(=0⊡0)(>0⊡1))=@?
WorkingBroken ← ×⊙(×⊃(=0⊡0)(>0⊡1))=@#
WorkingOther ← =0⊡0;
BrokenWorkingSome ← ×⊙(×⊃(=1⊡0)(>0⊡2))=@.
BrokenWorkingOther ← ×⊙(×⊃(=1⊡0)(>0⊡2))≠@.
BrokenBrokenDone ← ××⊙(⊃(=1⊡0)(=0⊡2))=@#
Case ← ⊗ 1 [⊃⊃⊃⊃⊃⊃WorkingBrokenEnd WorkingUnknown WorkingBroken WorkingOther BrokenWorkingSome BrokenWorkingOther BrokenBrokenDone]

Remove ← ⍜(⊡ 3)(-:0)
DoWorkingBrokenEnd ← ⊙(;) [Remove]
DoWorkingUnknown ← ⊟ ⊃∘ ([1 ⊃⊃(-1⊡ 1)(-1 ⊢ ↘ ⊙(⇌)-1 ⊡1)(⊡ 3)])
DoWorkingBroken ← ⊟ ⊃Remove ([1 ⊃⊃(-1⊡ 1)(-1 ⊢ ↘ ⊙(⇌)-1 ⊡1)(⊡ 3)])
DoWorkingOther ← [∘]
DoBrokenWorkingSome ← ⊙(;) [Remove]
DoBrokenOtherSome ← ⊙(;) ⊟ ⊃(Remove)(⍜(⊡ 2)(-1))
DoBrokenBrokenDone ← ⊙(;) [Remove]
DoBrokenOtherDone ← ⊙(;) ⊟ ⊃(Remove)(⍜(⊡ 0)(0;))

Switch ← (
  DoWorkingBrokenEnd
| DoWorkingUnknown
| DoWorkingBroken
| DoWorkingOther
| DoBrokenWorkingSome
| DoBrokenOtherSome
| DoBrokenBrokenDone
| DoBrokenOtherDone
) Case ⊙(.)

Initial ← ⍜(⊡ 1)(∘;) [0 0 0 1] ⧻
Cut ← ▽ ≡(>0⊡3).
Finish ← /+≡(⊡ 3) ▽ ≡(/×=0_0↙ 2 ↘ 1).
Merge ← ⊕(⊂ : ⊙(↙3 ⊢) /+ ≡(⊡ 3).) ⊛ ≡(↙ 3).

Arrangements ← (
  # Prepare the stack for folding and rowing
  ⊙(¤ ¤ : ¤ Initial .)
  # Compute final state
  ∧(Cut Merge ⊔ /⊐⊂ ≡(□ Cut Switch) ⊙:)
  # Cut off any states where there are broken springs to match
  Finish
)

Expand ← ⊓(⊂: ⊙(⊔)⊐/⊂⊙(□)≡(□⊂@?)[⍥.3].)(/⊂[⍥.4])

PartOne ← /+ ≡(Arrangements ∩⊔ ⍘⊟ ⍜(⊡ 1)(⍜(⊔)Numbers)Words) Lines
PartTwo ← /+ ≡(Arrangements Expand ∩⊔ ⍘⊟ ⍜(⊡ 1)(⍜(⊔)Numbers)Words) Lines

⍤ ⊃⋅∘ ≍ 0 Case @# [Working 0 0 1]
⍤ ⊃⋅∘ ≍ 1 Case @? [Working 1 0 1]
⍤ ⊃⋅∘ ≍ 2 Case @# [Working 1 0 1]
⍤ ⊃⋅∘ ≍ 3 Case @. [Working 1 0 1]
⍤ ⊃⋅∘ ≍ 4 Case @. [Broken 1 1 1]
⍤ ⊃⋅∘ ≍ 5 Case @? [Broken 1 1 1]
⍤ ⊃⋅∘ ≍ 5 Case @# [Broken 1 1 1]
⍤ ⊃⋅∘ ≍ 6 Case @# [Broken 1 0 1]
⍤ ⊃⋅∘ ≍ 7 Case @? [Broken 1 0 1]
⍤ ⊃⋅∘ ≍ 7 Case @. [Broken 1 0 1]

⍤ ⊃⋅∘ ≍ [[Working 0 0 ¯2]] DoWorkingBrokenEnd [Working 0 0 2] 1_1_5_3
⍤ ⊃⋅∘ ≍ [[Working 2 0 3][Broken 1 4 3]] DoWorkingUnknown [Working 2 0 3] 1_1_5_3
⍤ ⊃⋅∘ ≍ [[Working 2 0 ¯7][Broken 1 4 7]] DoWorkingBroken [Working 2 0 7] 1_3_5_7
⍤ ⊃⋅∘ ≍ [[Broken 2 4 ¯7]] DoBrokenWorkingSome [Broken 2 4 7] 1_3_5_7
⍤ ⊃⋅∘ ≍ [[Broken 2 4 ¯7][Broken 2 3 7]] DoBrokenOtherSome [Broken 2 4 7] 1_3_5_7
⍤ ⊃⋅∘ ≍ [[Broken 2 0 ¯7]] DoBrokenBrokenDone [Broken 2 0 7] 1_3_5_7
⍤ ⊃⋅∘ ≍ [[Broken 2 0 ¯7][Working 2 0 7]] DoBrokenOtherDone [Broken 2 0 7] 1_3_5_7

⍤ ⊃⋅∘ ≍ [[Working 0 0 ¯2]] Switch @# [Working 0 0 2] 1_1_5_3
⍤ ⊃⋅∘ ≍ [[Working 2 0 3][Broken 1 4 3]] Switch @? [Working 2 0 3] 1_1_5_3
⍤ ⊃⋅∘ ≍ [[Working 2 0 ¯7][Broken 1 4 7]] Switch @# [Working 2 0 7] 1_3_5_7
⍤ ⊃⋅∘ ≍ [[Broken 2 4 ¯7]] Switch @. [Broken 2 4 7] 1_3_5_7
⍤ ⊃⋅∘ ≍ [[Broken 2 4 ¯7][Broken 2 3 7]] Switch @? [Broken 2 4 7] 1_3_5_7
⍤ ⊃⋅∘ ≍ [[Broken 2 0 ¯7]] Switch @# [Broken 2 0 7] 1_3_5_7
⍤ ⊃⋅∘ ≍ [[Broken 2 0 ¯7][Working 2 0 7]] Switch @? [Broken 2 0 7] 1_3_5_7

⍤ ⊃⋅∘ ≍ [[Working 3 0 1]] Cut Merge Switch @? [Broken 3 0 1] 1_1_5_3
⍤ ⊃⋅∘ ≍ 195 Arrangements "?????.?????#????????" 1_1_5_3

&fras "day12.txt"
PartOne

&fras "day12.txt"
PartTwo
