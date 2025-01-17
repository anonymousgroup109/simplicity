/* This file has been automatically generated. */
[SCALAR_MULTIPLY] =
{ .tag = JET
, .jet = scalar_multiply
, .sourceIx = ty_w512
, .targetIx = ty_w256
}
,[MULTIPLY_32] =
{ .tag = JET
, .jet = multiply_32
, .sourceIx = ty_w64
, .targetIx = ty_w64
}
,[GEJ_INFINITY] =
{ .tag = JET
, .jet = gej_infinity
, .sourceIx = ty_u
, .targetIx = ty_pw512w256
}
,[SCALAR_IS_ZERO] =
{ .tag = JET
, .jet = scalar_is_zero
, .sourceIx = ty_w256
, .targetIx = ty_b
}
,[FULL_ADD_32] =
{ .tag = JET
, .jet = full_add_32
, .sourceIx = ty_pbw64
, .targetIx = ty_pbw32
}
,[GEJ_GE_ADD_EX] =
{ .tag = JET
, .jet = gej_ge_add_ex
, .sourceIx = ty_ppw512w256w512
, .targetIx = ty_pw256pw512w256
}
,[FE_IS_ODD] =
{ .tag = JET
, .jet = fe_is_odd
, .sourceIx = ty_w256
, .targetIx = ty_b
}
,[LINEAR_COMBINATION_1] =
{ .tag = JET
, .jet = linear_combination_1
, .sourceIx = ty_ppw256pw512w256w256
, .targetIx = ty_pw512w256
}
,[FE_IS_ZERO] =
{ .tag = JET
, .jet = fe_is_zero
, .sourceIx = ty_w256
, .targetIx = ty_b
}
,[GEJ_X_EQUIV] =
{ .tag = JET
, .jet = gej_x_equiv
, .sourceIx = ty_pw256pw512w256
, .targetIx = ty_b
}
,[SCALAR_ADD] =
{ .tag = JET
, .jet = scalar_add
, .sourceIx = ty_w512
, .targetIx = ty_w256
}
,[SCALAR_NEGATE] =
{ .tag = JET
, .jet = scalar_negate
, .sourceIx = ty_w256
, .targetIx = ty_w256
}
,[FULL_SUBTRACT_32] =
{ .tag = JET
, .jet = full_subtract_32
, .sourceIx = ty_pbw64
, .targetIx = ty_pbw32
}
,[GEJ_NEGATE] =
{ .tag = JET
, .jet = gej_negate
, .sourceIx = ty_pw512w256
, .targetIx = ty_pw512w256
}
,[GEJ_ADD] =
{ .tag = JET
, .jet = gej_add
, .sourceIx = ty_ppw512w256pw512w256
, .targetIx = ty_pw512w256
}
,[GEJ_DOUBLE] =
{ .tag = JET
, .jet = gej_double
, .sourceIx = ty_pw512w256
, .targetIx = ty_pw512w256
}
,[FE_NEGATE] =
{ .tag = JET
, .jet = fe_negate
, .sourceIx = ty_w256
, .targetIx = ty_w256
}
,[FE_SQUARE] =
{ .tag = JET
, .jet = fe_square
, .sourceIx = ty_w256
, .targetIx = ty_w256
}
,[SCALAR_NORMALIZE] =
{ .tag = JET
, .jet = scalar_normalize
, .sourceIx = ty_w256
, .targetIx = ty_w256
}
,[GENERATE] =
{ .tag = JET
, .jet = generate
, .sourceIx = ty_w256
, .targetIx = ty_pw512w256
}
,[BIP_0340_VERIFY] =
{ .tag = JET
, .jet = bip_0340_verify
, .sourceIx = ty_w1024
, .targetIx = ty_u
}
,[SCALE] =
{ .tag = JET
, .jet = scale
, .sourceIx = ty_pw256pw512w256
, .targetIx = ty_pw512w256
}
,[GE_NEGATE] =
{ .tag = JET
, .jet = ge_negate
, .sourceIx = ty_w512
, .targetIx = ty_w512
}
,[GEJ_IS_ON_CURVE] =
{ .tag = JET
, .jet = gej_is_on_curve
, .sourceIx = ty_pw512w256
, .targetIx = ty_b
}
,[FULL_MULTIPLY_32] =
{ .tag = JET
, .jet = full_multiply_32
, .sourceIx = ty_w128
, .targetIx = ty_w64
}
,[DECOMPRESS] =
{ .tag = JET
, .jet = decompress
, .sourceIx = ty_pbw256
, .targetIx = ty_mw512
}
,[FE_MULTIPLY] =
{ .tag = JET
, .jet = fe_multiply
, .sourceIx = ty_w512
, .targetIx = ty_w256
}
,[GEJ_NORMALIZE] =
{ .tag = JET
, .jet = gej_normalize
, .sourceIx = ty_pw512w256
, .targetIx = ty_mw512
}
,[FE_INVERT] =
{ .tag = JET
, .jet = fe_invert
, .sourceIx = ty_w256
, .targetIx = ty_w256
}
,[FE_NORMALIZE] =
{ .tag = JET
, .jet = fe_normalize
, .sourceIx = ty_w256
, .targetIx = ty_w256
}
,[FE_ADD] =
{ .tag = JET
, .jet = fe_add
, .sourceIx = ty_w512
, .targetIx = ty_w256
}
,[SCALAR_INVERT] =
{ .tag = JET
, .jet = scalar_invert
, .sourceIx = ty_w256
, .targetIx = ty_w256
}
,[POINT_VERIFY_1] =
{ .tag = JET
, .jet = point_verify_1
, .sourceIx = ty_pppw256pbw256w256pbw256
, .targetIx = ty_u
}
,[GEJ_Y_IS_ODD] =
{ .tag = JET
, .jet = gej_y_is_odd
, .sourceIx = ty_pw512w256
, .targetIx = ty_b
}
,[SCALAR_SQUARE] =
{ .tag = JET
, .jet = scalar_square
, .sourceIx = ty_w256
, .targetIx = ty_w256
}
,[GE_IS_ON_CURVE] =
{ .tag = JET
, .jet = ge_is_on_curve
, .sourceIx = ty_w512
, .targetIx = ty_b
}
,[GEJ_GE_ADD] =
{ .tag = JET
, .jet = gej_ge_add
, .sourceIx = ty_ppw512w256w512
, .targetIx = ty_pw512w256
}
,[SHA_256_BLOCK] =
{ .tag = JET
, .jet = sha_256_block
, .sourceIx = ty_pw256w512
, .targetIx = ty_w256
}
,[GEJ_IS_INFINITY] =
{ .tag = JET
, .jet = gej_is_infinity
, .sourceIx = ty_pw512w256
, .targetIx = ty_b
}
,[ADD_32] =
{ .tag = JET
, .jet = add_32
, .sourceIx = ty_w64
, .targetIx = ty_pbw32
}
,[FE_MULTIPLY_BETA] =
{ .tag = JET
, .jet = fe_multiply_beta
, .sourceIx = ty_w256
, .targetIx = ty_w256
}
,[LINEAR_VERIFY_1] =
{ .tag = JET
, .jet = linear_verify_1
, .sourceIx = ty_pppw256w512w256w512
, .targetIx = ty_u
}
,[SCALAR_MULTIPLY_LAMBDA] =
{ .tag = JET
, .jet = scalar_multiply_lambda
, .sourceIx = ty_w256
, .targetIx = ty_w256
}
,[SUBTRACT_32] =
{ .tag = JET
, .jet = subtract_32
, .sourceIx = ty_w64
, .targetIx = ty_pbw32
}
,[FE_SQUARE_ROOT] =
{ .tag = JET
, .jet = fe_square_root
, .sourceIx = ty_w256
, .targetIx = ty_mw256
}
,[GEJ_RESCALE] =
{ .tag = JET
, .jet = gej_rescale
, .sourceIx = ty_ppw512w256w256
, .targetIx = ty_pw512w256
}
,[VERSION] =
{ .tag = JET
, .jet = version
, .sourceIx = ty_u
, .targetIx = ty_w32
}
,[LOCK_TIME] =
{ .tag = JET
, .jet = lock_time
, .sourceIx = ty_u
, .targetIx = ty_w32
}
,[INPUT_IS_PEGIN] =
{ .tag = JET
, .jet = input_is_pegin
, .sourceIx = ty_w32
, .targetIx = ty_mb
}
,[INPUT_PREV_OUTPOINT] =
{ .tag = JET
, .jet = input_prev_outpoint
, .sourceIx = ty_w32
, .targetIx = ty_mpw256w32
}
,[INPUT_ASSET] =
{ .tag = JET
, .jet = input_asset
, .sourceIx = ty_w32
, .targetIx = ty_mspbw256w256
}
,[INPUT_AMOUNT] =
{ .tag = JET
, .jet = input_amount
, .sourceIx = ty_w32
, .targetIx = ty_mspbw256w64
}
,[INPUT_SCRIPT_HASH] =
{ .tag = JET
, .jet = input_script_hash
, .sourceIx = ty_w32
, .targetIx = ty_mw256
}
,[INPUT_SEQUENCE] =
{ .tag = JET
, .jet = input_sequence
, .sourceIx = ty_w32
, .targetIx = ty_mw32
}
,[INPUT_ISSUANCE_BLINDING] =
{ .tag = JET
, .jet = input_issuance_blinding
, .sourceIx = ty_w32
, .targetIx = ty_mmw256
}
,[INPUT_ISSUANCE_CONTRACT] =
{ .tag = JET
, .jet = input_issuance_contract
, .sourceIx = ty_w32
, .targetIx = ty_mmw256
}
,[INPUT_ISSUANCE_ENTROPY] =
{ .tag = JET
, .jet = input_issuance_entropy
, .sourceIx = ty_w32
, .targetIx = ty_mmw256
}
,[INPUT_ISSUANCE_ASSET_AMT] =
{ .tag = JET
, .jet = input_issuance_asset_amt
, .sourceIx = ty_w32
, .targetIx = ty_mmspbw256w64
}
,[INPUT_ISSUANCE_TOKEN_AMT] =
{ .tag = JET
, .jet = input_issuance_token_amt
, .sourceIx = ty_w32
, .targetIx = ty_mmspbw256w64
}
,[INPUT_ISSUANCE_ASSET_PROOF] =
{ .tag = JET
, .jet = input_issuance_asset_proof
, .sourceIx = ty_w32
, .targetIx = ty_mw256
}
,[INPUT_ISSUANCE_TOKEN_PROOF] =
{ .tag = JET
, .jet = input_issuance_token_proof
, .sourceIx = ty_w32
, .targetIx = ty_mw256
}
,[OUTPUT_ASSET] =
{ .tag = JET
, .jet = output_asset
, .sourceIx = ty_w32
, .targetIx = ty_mspbw256w256
}
,[OUTPUT_AMOUNT] =
{ .tag = JET
, .jet = output_amount
, .sourceIx = ty_w32
, .targetIx = ty_mspbw256w64
}
,[OUTPUT_NONCE] =
{ .tag = JET
, .jet = output_nonce
, .sourceIx = ty_w32
, .targetIx = ty_mmspbw256w256
}
,[OUTPUT_SCRIPT_HASH] =
{ .tag = JET
, .jet = output_script_hash
, .sourceIx = ty_w32
, .targetIx = ty_mw256
}
,[OUTPUT_NULL_DATUM] =
{ .tag = JET
, .jet = output_null_datum
, .sourceIx = ty_w64
, .targetIx = ty_mmspw2w256sbw4
}
,[OUTPUT_SURJECTION_PROOF] =
{ .tag = JET
, .jet = output_surjection_proof
, .sourceIx = ty_w32
, .targetIx = ty_mw256
}
,[OUTPUT_RANGE_PROOF] =
{ .tag = JET
, .jet = output_range_proof
, .sourceIx = ty_w32
, .targetIx = ty_mw256
}
,[SCRIPT_CMR] =
{ .tag = JET
, .jet = script_cmr
, .sourceIx = ty_u
, .targetIx = ty_w256
}
,[CURRENT_INDEX] =
{ .tag = JET
, .jet = current_index
, .sourceIx = ty_u
, .targetIx = ty_w32
}
,[CURRENT_IS_PEGIN] =
{ .tag = JET
, .jet = current_is_pegin
, .sourceIx = ty_u
, .targetIx = ty_b
}
,[CURRENT_PREV_OUTPOINT] =
{ .tag = JET
, .jet = current_prev_outpoint
, .sourceIx = ty_u
, .targetIx = ty_pw256w32
}
,[CURRENT_ASSET] =
{ .tag = JET
, .jet = current_asset
, .sourceIx = ty_u
, .targetIx = ty_spbw256w256
}
,[CURRENT_AMOUNT] =
{ .tag = JET
, .jet = current_amount
, .sourceIx = ty_u
, .targetIx = ty_spbw256w64
}
,[CURRENT_SCRIPT_HASH] =
{ .tag = JET
, .jet = current_script_hash
, .sourceIx = ty_u
, .targetIx = ty_w256
}
,[CURRENT_SEQUENCE] =
{ .tag = JET
, .jet = current_sequence
, .sourceIx = ty_u
, .targetIx = ty_w32
}
,[CURRENT_ISSUANCE_BLINDING] =
{ .tag = JET
, .jet = current_issuance_blinding
, .sourceIx = ty_u
, .targetIx = ty_mw256
}
,[CURRENT_ISSUANCE_CONTRACT] =
{ .tag = JET
, .jet = current_issuance_contract
, .sourceIx = ty_u
, .targetIx = ty_mw256
}
,[CURRENT_ISSUANCE_ENTROPY] =
{ .tag = JET
, .jet = current_issuance_entropy
, .sourceIx = ty_u
, .targetIx = ty_mw256
}
,[CURRENT_ISSUANCE_ASSET_AMT] =
{ .tag = JET
, .jet = current_issuance_asset_amt
, .sourceIx = ty_u
, .targetIx = ty_mspbw256w64
}
,[CURRENT_ISSUANCE_TOKEN_AMT] =
{ .tag = JET
, .jet = current_issuance_token_amt
, .sourceIx = ty_u
, .targetIx = ty_mspbw256w64
}
,[CURRENT_ISSUANCE_ASSET_PROOF] =
{ .tag = JET
, .jet = current_issuance_asset_proof
, .sourceIx = ty_u
, .targetIx = ty_w256
}
,[CURRENT_ISSUANCE_TOKEN_PROOF] =
{ .tag = JET
, .jet = current_issuance_token_proof
, .sourceIx = ty_u
, .targetIx = ty_w256
}
,[TAPLEAF_VERSION] =
{ .tag = JET
, .jet = tapleaf_version
, .sourceIx = ty_u
, .targetIx = ty_w8
}
,[TAPBRANCH] =
{ .tag = JET
, .jet = tapbranch
, .sourceIx = ty_w8
, .targetIx = ty_mw256
}
,[INTERNAL_KEY] =
{ .tag = JET
, .jet = internal_key
, .sourceIx = ty_u
, .targetIx = ty_w256
}
,[ANNEX_HASH] =
{ .tag = JET
, .jet = annex_hash
, .sourceIx = ty_u
, .targetIx = ty_mw256
}
,[INPUTS_HASH] =
{ .tag = JET
, .jet = inputs_hash
, .sourceIx = ty_u
, .targetIx = ty_w256
}
,[OUTPUTS_HASH] =
{ .tag = JET
, .jet = outputs_hash
, .sourceIx = ty_u
, .targetIx = ty_w256
}
,[NUM_INPUTS] =
{ .tag = JET
, .jet = num_inputs
, .sourceIx = ty_u
, .targetIx = ty_w32
}
,[NUM_OUTPUTS] =
{ .tag = JET
, .jet = num_outputs
, .sourceIx = ty_u
, .targetIx = ty_w32
}
,[FEE] =
{ .tag = JET
, .jet = fee
, .sourceIx = ty_w256
, .targetIx = ty_w64
}
