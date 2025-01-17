#include "primitive/elements/jets.h"

#define WRAP_(jet)                                                                                                            \
bool c_##jet(frameItem* dst, const frameItem* src, const txEnv* env) {                                                        \
  return jet(dst, *src, env);                                                                                                 \
}

WRAP_(version)
WRAP_(lock_time)
WRAP_(input_is_pegin)
WRAP_(input_prev_outpoint)
WRAP_(input_asset)
WRAP_(input_amount)
WRAP_(input_script_hash)
WRAP_(input_sequence)
WRAP_(input_issuance_blinding)
WRAP_(input_issuance_contract)
WRAP_(input_issuance_entropy)
WRAP_(input_issuance_asset_amt)
WRAP_(input_issuance_token_amt)
WRAP_(input_issuance_asset_proof)
WRAP_(input_issuance_token_proof)
WRAP_(output_asset)
WRAP_(output_amount)
WRAP_(output_nonce)
WRAP_(output_script_hash)
WRAP_(output_null_datum)
WRAP_(output_surjection_proof)
WRAP_(output_range_proof)
WRAP_(script_cmr)
WRAP_(current_index)
WRAP_(current_is_pegin)
WRAP_(current_prev_outpoint)
WRAP_(current_asset)
WRAP_(current_amount)
WRAP_(current_script_hash)
WRAP_(current_sequence)
WRAP_(current_issuance_blinding)
WRAP_(current_issuance_contract)
WRAP_(current_issuance_entropy)
WRAP_(current_issuance_asset_amt)
WRAP_(current_issuance_token_amt)
WRAP_(current_issuance_asset_proof)
WRAP_(current_issuance_token_proof)
WRAP_(tapleaf_version)
WRAP_(tapbranch)
WRAP_(internal_key)
WRAP_(annex_hash)
WRAP_(inputs_hash)
WRAP_(outputs_hash)
WRAP_(num_inputs)
WRAP_(num_outputs)
