/*
 * Closed-world guard over the signature of every libhegel function zizek
 * binds, compiled into the ffi test suite.
 *
 * Each line below copies a function's prototype from the libhegel header
 * that Hegel.Internal.Foreign.Raw was last checked against, and initialises
 * a function pointer of exactly that type with the real function. Once a
 * newer hegel.h changes a bound function's signature in any way, including
 * what a pointer argument points to, its line no longer type-checks and the
 * build fails. Update the Haskell binding and its line here together.
 *
 * The functions zizek reaches through a cbits shim are listed under their
 * libhegel names, since the shim forwards to them unchanged.
 *
 * Every function bound in Raw.hsc needs a line here.
 */
#include <hegel.h>

/* The callback Raw.hsc's OutputSink wraps. */
_Static_assert(__builtin_types_compatible_p(hegel_output_callback_t,
                                            void (*)(void *, const char *, size_t)),
               "hegel_output_callback_t changed; update OutputSink in Raw.hsc");

hegel_result_t (*const zizek_guard_hegel_collection_free)(hegel_context_t *ctx, hegel_collection_t *collection) = hegel_collection_free;
hegel_result_t (*const zizek_guard_hegel_collection_more)(hegel_context_t *ctx, hegel_test_case_t *tc, hegel_collection_t *collection, bool *out_more) = hegel_collection_more;
hegel_result_t (*const zizek_guard_hegel_collection_reject)(hegel_context_t *ctx, hegel_test_case_t *tc, hegel_collection_t *collection, const hegel_string_t *why) = hegel_collection_reject;
hegel_result_t (*const zizek_guard_hegel_context_free)(hegel_context_t *ctx) = hegel_context_free;
hegel_string_t (*const zizek_guard_hegel_context_last_error)(const hegel_context_t *ctx) = hegel_context_last_error;
hegel_context_t *(*const zizek_guard_hegel_context_new)(void) = hegel_context_new;
hegel_result_t (*const zizek_guard_hegel_event)(hegel_context_t *ctx, hegel_test_case_t *tc, hegel_string_t label) = hegel_event;
hegel_result_t (*const zizek_guard_hegel_event_value)(hegel_context_t *ctx, hegel_test_case_t *tc, double value, hegel_string_t label) = hegel_event_value;
hegel_result_t (*const zizek_guard_hegel_failure_caveat)(hegel_context_t *ctx, const hegel_failure_t *f, const hegel_string_t **out_caveat) = hegel_failure_caveat;
hegel_result_t (*const zizek_guard_hegel_failure_free)(hegel_context_t *ctx, hegel_failure_t *f) = hegel_failure_free;
hegel_result_t (*const zizek_guard_hegel_failure_origin)(hegel_context_t *ctx, const hegel_failure_t *f, hegel_string_t *out_origin) = hegel_failure_origin;
hegel_result_t (*const zizek_guard_hegel_failure_reproduction_blob)(hegel_context_t *ctx, const hegel_failure_t *f, const hegel_string_t **out_blob) = hegel_failure_reproduction_blob;
hegel_result_t (*const zizek_guard_hegel_generate_boolean)(hegel_context_t *ctx, hegel_test_case_t *tc, double p, bool forced, bool has_forced, bool *out_value) = hegel_generate_boolean;
hegel_result_t (*const zizek_guard_hegel_generate_bytes)(hegel_context_t *ctx, hegel_test_case_t *tc, uint64_t min_size, uint64_t max_size, hegel_generate_bytes_result_t *out_result) = hegel_generate_bytes;
hegel_result_t (*const zizek_guard_hegel_generate_bytes_result_free)(hegel_context_t *ctx, hegel_generate_bytes_result_t *result) = hegel_generate_bytes_result_free;
hegel_result_t (*const zizek_guard_hegel_generate_date)(hegel_context_t *ctx, hegel_test_case_t *tc, hegel_date_t min_value, hegel_date_t max_value, hegel_date_t *out_value) = hegel_generate_date;
hegel_result_t (*const zizek_guard_hegel_generate_datetime)(hegel_context_t *ctx, hegel_test_case_t *tc, hegel_datetime_t min_value, hegel_datetime_t max_value, hegel_datetime_t *out_value) = hegel_generate_datetime;
hegel_result_t (*const zizek_guard_hegel_generate_float)(hegel_context_t *ctx, hegel_test_case_t *tc, uint32_t width, double min_value, double max_value, bool allow_nan, bool allow_infinity, bool exclude_min, bool exclude_max, double smallest_nonzero_magnitude, double *out_value) = hegel_generate_float;
hegel_result_t (*const zizek_guard_hegel_generate_integer)(hegel_context_t *ctx, hegel_test_case_t *tc, int64_t min_value, int64_t max_value, int64_t *out_value) = hegel_generate_integer;
hegel_result_t (*const zizek_guard_hegel_generate_integer_big)(hegel_context_t *ctx, hegel_test_case_t *tc, const uint8_t *min_value, size_t min_value_len, const uint8_t *max_value, size_t max_value_len, uint8_t *out_value, size_t out_value_cap, size_t *out_value_len) = hegel_generate_integer_big;
hegel_result_t (*const zizek_guard_hegel_generate_string)(hegel_context_t *ctx, hegel_test_case_t *tc, const hegel_string_generator_t *generator, hegel_owned_string_t *out_result) = hegel_generate_string;
hegel_result_t (*const zizek_guard_hegel_generate_time)(hegel_context_t *ctx, hegel_test_case_t *tc, hegel_time_t min_value, hegel_time_t max_value, hegel_time_t *out_value) = hegel_generate_time;
hegel_result_t (*const zizek_guard_hegel_generate_uuid)(hegel_context_t *ctx, hegel_test_case_t *tc, uint8_t version, bool has_version, uint8_t *out_bytes) = hegel_generate_uuid;
hegel_result_t (*const zizek_guard_hegel_label_combine)(hegel_context_t *ctx, const uint64_t *labels, size_t len, uint64_t *out_label) = hegel_label_combine;
hegel_result_t (*const zizek_guard_hegel_label_from_name)(hegel_context_t *ctx, hegel_string_t name, uint64_t *out_label) = hegel_label_from_name;
hegel_result_t (*const zizek_guard_hegel_mark_complete)(hegel_context_t *ctx, hegel_test_case_t *tc, uint32_t status, const hegel_string_t *origin) = hegel_mark_complete;
hegel_result_t (*const zizek_guard_hegel_new_collection)(hegel_context_t *ctx, hegel_test_case_t *tc, uint64_t min_size, uint64_t max_size, hegel_collection_t **out_collection) = hegel_new_collection;
hegel_result_t (*const zizek_guard_hegel_new_pool)(hegel_context_t *ctx, hegel_test_case_t *tc, hegel_pool_t **out_pool) = hegel_new_pool;
hegel_result_t (*const zizek_guard_hegel_new_recursion)(hegel_context_t *ctx, hegel_test_case_t *tc, uint64_t max_depth, uint64_t max_leaves, HegelRecursion **out_recursion) = hegel_new_recursion;
hegel_result_t (*const zizek_guard_hegel_new_state_machine)(hegel_context_t *ctx, hegel_test_case_t *tc, const hegel_string_t *rule_names, const int64_t *rule_groups, const double *rule_weights, size_t num_rules, const hegel_string_t *invariant_names, const bool *invariant_always_check, size_t num_invariants, int64_t min_concurrency, int64_t max_concurrency, int64_t step_count, hegel_state_machine_t **out_state_machine, int64_t *out_concurrency) = hegel_new_state_machine;
hegel_result_t (*const zizek_guard_hegel_next_test_case)(hegel_context_t *ctx, hegel_run_t *run, hegel_test_case_t **out_test_case) = hegel_next_test_case;
hegel_result_t (*const zizek_guard_hegel_owned_string_free)(hegel_context_t *ctx, hegel_owned_string_t *result) = hegel_owned_string_free;
hegel_result_t (*const zizek_guard_hegel_pool_add)(hegel_context_t *ctx, hegel_test_case_t *tc, hegel_pool_t *pool, int64_t *out_variable_id) = hegel_pool_add;
hegel_result_t (*const zizek_guard_hegel_pool_free)(hegel_context_t *ctx, hegel_pool_t *pool) = hegel_pool_free;
hegel_result_t (*const zizek_guard_hegel_pool_generate)(hegel_context_t *ctx, hegel_test_case_t *tc, hegel_pool_t *pool, bool consume, int64_t *out_variable_id) = hegel_pool_generate;
hegel_result_t (*const zizek_guard_hegel_recursion_branch)(hegel_context_t *ctx, hegel_test_case_t *tc, HegelRecursion *recursion, uint64_t depth, bool *out_branch) = hegel_recursion_branch;
hegel_result_t (*const zizek_guard_hegel_recursion_finish)(hegel_context_t *ctx, hegel_test_case_t *tc, HegelRecursion *recursion) = hegel_recursion_finish;
hegel_result_t (*const zizek_guard_hegel_recursion_free)(hegel_context_t *ctx, HegelRecursion *recursion) = hegel_recursion_free;
hegel_result_t (*const zizek_guard_hegel_recursion_leaf)(hegel_context_t *ctx, hegel_test_case_t *tc, HegelRecursion *recursion) = hegel_recursion_leaf;
hegel_result_t (*const zizek_guard_hegel_recursion_retry)(hegel_context_t *ctx, hegel_test_case_t *tc, HegelRecursion *recursion) = hegel_recursion_retry;
hegel_result_t (*const zizek_guard_hegel_run_free)(hegel_context_t *ctx, hegel_run_t *run) = hegel_run_free;
hegel_result_t (*const zizek_guard_hegel_run_result)(hegel_context_t *ctx, hegel_run_t *run, hegel_run_result_t **out_result) = hegel_run_result;
hegel_result_t (*const zizek_guard_hegel_run_result_error)(hegel_context_t *ctx, const hegel_run_result_t *r, const hegel_string_t **out_error) = hegel_run_result_error;
hegel_result_t (*const zizek_guard_hegel_run_result_failure)(hegel_context_t *ctx, const hegel_run_result_t *r, size_t index, hegel_failure_t **out_failure) = hegel_run_result_failure;
hegel_result_t (*const zizek_guard_hegel_run_result_failure_count)(hegel_context_t *ctx, const hegel_run_result_t *r, size_t *out_count) = hegel_run_result_failure_count;
hegel_result_t (*const zizek_guard_hegel_run_result_free)(hegel_context_t *ctx, hegel_run_result_t *r) = hegel_run_result_free;
hegel_result_t (*const zizek_guard_hegel_run_result_status)(hegel_context_t *ctx, const hegel_run_result_t *r, hegel_run_status_t *out_status) = hegel_run_result_status;
hegel_result_t (*const zizek_guard_hegel_run_start)(hegel_context_t *ctx, const hegel_settings_t *settings, hegel_output_callback_t callback, void *user_data, hegel_run_t **out_run) = hegel_run_start;
hegel_result_t (*const zizek_guard_hegel_run_start_blob)(hegel_context_t *ctx, const hegel_settings_t *settings, hegel_string_t blob, hegel_output_callback_t callback, void *user_data, hegel_run_t **out_run) = hegel_run_start_blob;
hegel_result_t (*const zizek_guard_hegel_set_default_profile)(hegel_context_t *ctx, hegel_string_t name) = hegel_set_default_profile;
hegel_result_t (*const zizek_guard_hegel_set_unicode_version)(hegel_context_t *ctx, hegel_string_t version) = hegel_set_unicode_version;
hegel_result_t (*const zizek_guard_hegel_settings_free)(hegel_context_t *ctx, hegel_settings_t *s) = hegel_settings_free;
hegel_result_t (*const zizek_guard_hegel_settings_get_backend)(hegel_context_t *ctx, const hegel_settings_t *s, hegel_backend_t *out) = hegel_settings_get_backend;
hegel_result_t (*const zizek_guard_hegel_settings_get_database)(hegel_context_t *ctx, const hegel_settings_t *s, const hegel_string_t **out_database) = hegel_settings_get_database;
hegel_result_t (*const zizek_guard_hegel_settings_get_derandomize)(hegel_context_t *ctx, const hegel_settings_t *s, bool *out) = hegel_settings_get_derandomize;
hegel_result_t (*const zizek_guard_hegel_settings_get_nondeterminism_strictness)(hegel_context_t *ctx, const hegel_settings_t *s, hegel_nondeterminism_strictness_t *out) = hegel_settings_get_nondeterminism_strictness;
hegel_result_t (*const zizek_guard_hegel_settings_get_phases)(hegel_context_t *ctx, const hegel_settings_t *s, uint32_t *out) = hegel_settings_get_phases;
hegel_result_t (*const zizek_guard_hegel_settings_get_print_blob)(hegel_context_t *ctx, const hegel_settings_t *s, bool *out) = hegel_settings_get_print_blob;
hegel_result_t (*const zizek_guard_hegel_settings_get_report_multiple_failures)(hegel_context_t *ctx, const hegel_settings_t *s, bool *out) = hegel_settings_get_report_multiple_failures;
hegel_result_t (*const zizek_guard_hegel_settings_get_seed)(hegel_context_t *ctx, const hegel_settings_t *s, uint64_t *out_seed, bool *out_has_seed) = hegel_settings_get_seed;
hegel_result_t (*const zizek_guard_hegel_settings_get_show_statistics)(hegel_context_t *ctx, const hegel_settings_t *s, bool *out) = hegel_settings_get_show_statistics;
hegel_result_t (*const zizek_guard_hegel_settings_get_suppress_health_check)(hegel_context_t *ctx, const hegel_settings_t *s, uint32_t *out) = hegel_settings_get_suppress_health_check;
hegel_result_t (*const zizek_guard_hegel_settings_get_test_cases)(hegel_context_t *ctx, const hegel_settings_t *s, uint64_t *out) = hegel_settings_get_test_cases;
hegel_result_t (*const zizek_guard_hegel_settings_get_unbounded_choices)(hegel_context_t *ctx, const hegel_settings_t *s, bool *out) = hegel_settings_get_unbounded_choices;
hegel_result_t (*const zizek_guard_hegel_settings_get_verbosity)(hegel_context_t *ctx, const hegel_settings_t *s, hegel_verbosity_t *out) = hegel_settings_get_verbosity;
hegel_result_t (*const zizek_guard_hegel_settings_new)(hegel_context_t *ctx, hegel_settings_t **out_settings) = hegel_settings_new;
hegel_result_t (*const zizek_guard_hegel_settings_new_for_profile)(hegel_context_t *ctx, hegel_string_t name, hegel_settings_t **out_settings) = hegel_settings_new_for_profile;
hegel_result_t (*const zizek_guard_hegel_settings_register_profile)(hegel_context_t *ctx, hegel_string_t name, const hegel_settings_t *settings) = hegel_settings_register_profile;
hegel_result_t (*const zizek_guard_hegel_settings_set_backend)(hegel_context_t *ctx, hegel_settings_t *s, uint32_t backend) = hegel_settings_set_backend;
hegel_result_t (*const zizek_guard_hegel_settings_set_database)(hegel_context_t *ctx, hegel_settings_t *s, const hegel_string_t *database) = hegel_settings_set_database;
hegel_result_t (*const zizek_guard_hegel_settings_set_database_key)(hegel_context_t *ctx, hegel_settings_t *s, const hegel_string_t *key) = hegel_settings_set_database_key;
hegel_result_t (*const zizek_guard_hegel_settings_set_derandomize)(hegel_context_t *ctx, hegel_settings_t *s, bool derandomize) = hegel_settings_set_derandomize;
hegel_result_t (*const zizek_guard_hegel_settings_set_nondeterminism_strictness)(hegel_context_t *ctx, hegel_settings_t *s, uint32_t strictness) = hegel_settings_set_nondeterminism_strictness;
hegel_result_t (*const zizek_guard_hegel_settings_set_phases)(hegel_context_t *ctx, hegel_settings_t *s, uint32_t phases) = hegel_settings_set_phases;
hegel_result_t (*const zizek_guard_hegel_settings_set_print_blob)(hegel_context_t *ctx, hegel_settings_t *s, bool yes) = hegel_settings_set_print_blob;
hegel_result_t (*const zizek_guard_hegel_settings_set_report_multiple_failures)(hegel_context_t *ctx, hegel_settings_t *s, bool yes) = hegel_settings_set_report_multiple_failures;
hegel_result_t (*const zizek_guard_hegel_settings_set_seed)(hegel_context_t *ctx, hegel_settings_t *s, uint64_t seed, bool has_seed) = hegel_settings_set_seed;
hegel_result_t (*const zizek_guard_hegel_settings_set_show_statistics)(hegel_context_t *ctx, hegel_settings_t *s, bool yes) = hegel_settings_set_show_statistics;
hegel_result_t (*const zizek_guard_hegel_settings_set_suppress_health_check)(hegel_context_t *ctx, hegel_settings_t *s, uint32_t checks) = hegel_settings_set_suppress_health_check;
hegel_result_t (*const zizek_guard_hegel_settings_set_test_cases)(hegel_context_t *ctx, hegel_settings_t *s, uint64_t n) = hegel_settings_set_test_cases;
hegel_result_t (*const zizek_guard_hegel_settings_set_test_location)(hegel_context_t *ctx, hegel_settings_t *s, hegel_string_t file, uint32_t begin_line, hegel_string_t class_name, hegel_string_t function) = hegel_settings_set_test_location;
hegel_result_t (*const zizek_guard_hegel_settings_set_unbounded_choices)(hegel_context_t *ctx, hegel_settings_t *s, bool yes) = hegel_settings_set_unbounded_choices;
hegel_result_t (*const zizek_guard_hegel_settings_set_verbosity)(hegel_context_t *ctx, hegel_settings_t *s, uint32_t v) = hegel_settings_set_verbosity;
hegel_result_t (*const zizek_guard_hegel_start_span)(hegel_context_t *ctx, hegel_test_case_t *tc, uint64_t label) = hegel_start_span;
hegel_result_t (*const zizek_guard_hegel_state_machine_free)(hegel_context_t *ctx, hegel_state_machine_t *state_machine) = hegel_state_machine_free;
hegel_result_t (*const zizek_guard_hegel_state_machine_next_group)(hegel_context_t *ctx, hegel_test_case_t *tc, hegel_state_machine_t *state_machine, int64_t *out_group_id) = hegel_state_machine_next_group;
hegel_result_t (*const zizek_guard_hegel_state_machine_next_rule)(hegel_context_t *ctx, hegel_test_case_t *tc, hegel_state_machine_t *state_machine, int64_t worker_index, int64_t *out_rule_index) = hegel_state_machine_next_rule;
hegel_result_t (*const zizek_guard_hegel_state_machine_rule_rejected)(hegel_context_t *ctx, hegel_test_case_t *tc, hegel_state_machine_t *state_machine, int64_t worker_index) = hegel_state_machine_rule_rejected;
hegel_result_t (*const zizek_guard_hegel_state_machine_should_check_invariant)(hegel_context_t *ctx, hegel_test_case_t *tc, hegel_state_machine_t *state_machine, int64_t invariant_index, bool *out_should_check) = hegel_state_machine_should_check_invariant;
hegel_result_t (*const zizek_guard_hegel_stop_span)(hegel_context_t *ctx, hegel_test_case_t *tc, bool discard) = hegel_stop_span;
hegel_result_t (*const zizek_guard_hegel_string_generator_domain)(hegel_context_t *ctx, uint64_t max_length, hegel_string_generator_t **out_generator) = hegel_string_generator_domain;
hegel_result_t (*const zizek_guard_hegel_string_generator_email)(hegel_context_t *ctx, hegel_string_generator_t **out_generator) = hegel_string_generator_email;
hegel_result_t (*const zizek_guard_hegel_string_generator_free)(hegel_context_t *ctx, hegel_string_generator_t *generator) = hegel_string_generator_free;
hegel_result_t (*const zizek_guard_hegel_string_generator_regex)(hegel_context_t *ctx, hegel_string_t pattern, bool fullmatch, const hegel_string_generator_t *alphabet, hegel_string_generator_t **out_generator) = hegel_string_generator_regex;
hegel_result_t (*const zizek_guard_hegel_string_generator_text)(hegel_context_t *ctx, uint64_t min_size, uint64_t max_size, const hegel_string_t *codec, uint32_t min_codepoint, uint32_t max_codepoint, const hegel_string_t *categories, size_t categories_len, const hegel_string_t *exclude_categories, size_t exclude_categories_len, hegel_string_t include_characters, hegel_string_t exclude_characters, hegel_string_generator_t **out_generator) = hegel_string_generator_text;
hegel_result_t (*const zizek_guard_hegel_string_generator_url)(hegel_context_t *ctx, hegel_string_generator_t **out_generator) = hegel_string_generator_url;
hegel_result_t (*const zizek_guard_hegel_target)(hegel_context_t *ctx, hegel_test_case_t *tc, double value, hegel_string_t label) = hegel_target;
hegel_result_t (*const zizek_guard_hegel_test_case_clone)(hegel_context_t *ctx, const hegel_test_case_t *tc, hegel_test_case_t **out_test_case) = hegel_test_case_clone;
hegel_result_t (*const zizek_guard_hegel_test_case_free)(hegel_context_t *ctx, hegel_test_case_t *tc) = hegel_test_case_free;
hegel_result_t (*const zizek_guard_hegel_test_case_from_blob)(hegel_context_t *ctx, const hegel_settings_t *s, hegel_string_t blob, hegel_output_callback_t callback, void *user_data, hegel_test_case_t **out_test_case) = hegel_test_case_from_blob;
hegel_result_t (*const zizek_guard_hegel_test_case_should_capture)(hegel_context_t *ctx, const hegel_test_case_t *tc, bool *out_should_capture) = hegel_test_case_should_capture;
hegel_result_t (*const zizek_guard_hegel_version)(hegel_context_t *ctx, hegel_string_t *out_version) = hegel_version;
