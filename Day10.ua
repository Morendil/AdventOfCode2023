Grid ← ⊜∘≠,@\n
Start ← ⊢ ⊚ = @S
Pipes ← "FJL7-|"
Directions ← [[0_1 1_0][0_¯1 ¯1_0][0_1 ¯1_0][0_1 0_¯1][0_¯1 0_1][¯1_0 1_0]]
Compatible ← [["7-J" "J|L"]["L-F" "7|F"]["7-J" "7|F"]["L-F" "J|L"]["7-J" "L-F"]["7|F" "J|L"]]

# Stack : Compatible x n, Pos x n, Grid
# Out: Bool
EdgeCompatible ← /×≠0≡(⊗): ⊙(≡(⊡ ⊙⊔)⊙(¤ □))

&fras "day10_sample.txt"
Grid
Start .
# Assume the @S is:
⊙(⍜⊡;) . ⊙⊙@F
# Take start value
⊡¤,,

# Value -> Directions Compatibility
⊗ : Pipes
⍉
⊃(⊡ : Compatible)(⊡ : Directions)
⊙(≡(+)⊙(¤))

⊙⊙.
# Directions Compatibility Grid -> Valid Nodes or []
# Keep if compatible
([]|∘) : ⊙(EdgeCompatible) ,

⊡¤,,
⊗ : Pipes
⍉
≡(⊃(⊡ : Compatible)(⊡ : Directions))
⊙(≡(+)⊙(¤))
⊙⊙.
([]|∘) : ⊙(EdgeCompatible) ,
