# Parsing
Numbers ← ⊜⋕ ∊:⊂@-+@0⇡10.
Matrix ← ⊜Numbers≠, @\n
RandomPick ← ⊏ ⌊× ⊙(⧻.) [⍥⚂]
PickThree ← ⍢((⊙(;)|;) ≠2/↥ ⊛ . RandomPick 3 .)(≠2/↥ ⊛)

# Coordinate pair manipulations:
DropX ← ≡(▽ 0_1_1_0_1_1)
InsertX ← × 0_1_1_0_1_1 ▽ 2_1_2_1
DropY ← ≡(▽ 1_0_1_1_0_1)
InsertY ← × 1_0_1_1_0_1 ▽ 1_2_1_2
DropZ ← ≡(▽ 1_1_0_1_1_0)
InsertZ ← × 1_1_0_1_1_0 ▽ 1_2_1_2

# Final assembly:
Coeffs ← × ¯1_1_1_¯1 ≡(-:°⊟) ⇌ ⍉
ComputeK ← -:∩(/- ≡(/×)⍉) ⊃(↙ 2)(↘ 2) ≡(↻)0_1_0_1 ↯ 4_2
DoXY ← ⊂ ⊃(InsertZ Coeffs)(ComputeK)DropZ
DoXZ ← ⊂ ⊃(InsertY Coeffs)(ComputeK)DropY
DoYZ ← ⊂ ⊃(InsertX Coeffs)(ComputeK)DropX
DoCoeffs ← ⊃⊃DoXY DoXZ DoYZ
ToEquations ← [⊃(DoCoeffs ↙ 2)(DoCoeffs ↘ 1)]

# Gaussian elimination

Swap ← (⍜⊏⇌⊟|⋅⋅∘)=,,
⍤ ⊃⋅∘ ≍ 5_2_3_4_1_6 Swap 0 4 1_2_3_4_5_6
⍤ ⊃⋅∘ ≍ 1_2_3_4_5_6 Swap 0 0 1_2_3_4_5_6

# N Matrix -- First pivot of rank N
FindPivot ← ⊃∘ + ⊃∘ (⊢ ⊚↘) ⊃∘ (≠0⊡) ⊙(⍉.)
# N Matrix -- Normalized at N
Normalize ← ⍜(⊙⊡)(÷⊡⊙.).
# N Normalized -- Reduced
Reduce ← ⊂: ≡(- ⊃(×⊃(⊡ :)(⋅⊡))∘) ⊙(∩¤)⊙⊙(.) : ⊙(⊃↘↙ +1).
# N Matrix -- Matrix
Substitute ← ⊙(⊂) ⊃(⊙∘)(-×⊃(↙1⇌⋅∘)(⊡⊙(⇌ ⍉.)⊙⋅∘)) ⊃∘ ⊃↙↘
# Solved -- Solutions
Solutions ← ⇌⊢⇌⍉

RowReduction ← (
  -1 ⧻.
  ⊙(0)
  ⍥(
    FindPivot
    ⊃∘ Swap
    ⊃∘ Normalize
    ⊃∘ Reduce
    +1
  )
  Normalize
)

BackSubstitution ← (
  ⇌
  -1 ⧻.
  ⊙(1)
  ⍥(+1 Substitute)
  ;
)

GaussianSolve ← Solutions BackSubstitution RowReduction
PartTwoSum ← /+⁅↙ 3
PartTwo ← PartTwoSum GaussianSolve ToEquations PickThree Matrix

⍤ ⊃⋅∘ ≍ ¯15_8_2 GaussianSolve [1_3_¯2_5 3_5_6_7 2_4_3_8]

&fras "day24.txt"
PartTwo
