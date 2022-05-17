# Yeah.

# It's probably better if you don't ask.

kw = """KW_attribute | KW_const | KW_uniform | KW_varying | KW_centroid
| KW_break | KW_continue | KW_do | KW_for | KW_while | KW_if | KW_else
| KW_in | KW_out | KW_inout
| KW_float | KW_int | KW_void | KW_bool
| KW_true | KW_false | KW_invariant | KW_discard | KW_return
| KW_mat2 | KW_mat3 | KW_mat4
| KW_vec2 | KW_vec3 | KW_vec4
| KW_ivec2 | KW_ivec3 | KW_ivec4
| KW_bvec2 | KW_bvec3 | KW_bvec4
| KW_sampler1D | KW_sampler2D | KW_sampler3D | KW_samplerCube"""

kws = [y.strip()[3:] for y in kw.split("|")]
kws.sort(key=len, reverse=True)
for kw in kws:
    print(f"readKW \"{kw}\" = Just KW_{kw}")

ops = """O_LParen | O_RParen | O_LBracket | O_RBracket
| O_Dot | O_PlusPlus | O_MinusMinus | O_Plus | O_Minus
| O_Tilde | O_Not | O_Mult | O_Div | O_Mod | O_LShift | O_RShift 
| O_LT | O_GT | O_LE | O_GE | O_EQ | O_NE 
| O_And | O_Xor | O_Or | O_LAnd | O_LXor | O_LOr
| O_QMark | O_Colon | O_Assign | O_PlusAssign | O_MinusAssign
| O_MultAssign | O_DivAssign | O_ModAssign | O_Comma"""

ops = [y.strip()[2:] for y in ops.split("|")]

for op in ops:
    print(f"show O_{op} = \"\"")