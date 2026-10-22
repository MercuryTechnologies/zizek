#include "zizek_shim.h"

void hegel_zizek_context_last_error(const hegel_context_t *ctx,
                                    hegel_string_t *out_error) {
  *out_error = hegel_context_last_error(ctx);
}

hegel_result_t hegel_zizek_settings_new_for_profile(hegel_context_t *ctx,
                                                    const hegel_string_t *name,
                                                    hegel_settings_t **out_settings) {
  return hegel_settings_new_for_profile(ctx, *name, out_settings);
}

hegel_result_t hegel_zizek_settings_set_test_location(hegel_context_t *ctx,
                                                      hegel_settings_t *s,
                                                      const hegel_string_t *file,
                                                      uint32_t begin_line,
                                                      const hegel_string_t *class_name,
                                                      const hegel_string_t *function) {
  return hegel_settings_set_test_location(ctx, s, *file, begin_line, *class_name, *function);
}

hegel_result_t hegel_zizek_settings_register_profile(hegel_context_t *ctx,
                                                     const hegel_string_t *name,
                                                     const hegel_settings_t *settings) {
  return hegel_settings_register_profile(ctx, *name, settings);
}

hegel_result_t hegel_zizek_set_default_profile(hegel_context_t *ctx,
                                               const hegel_string_t *name) {
  return hegel_set_default_profile(ctx, *name);
}

hegel_result_t hegel_zizek_set_unicode_version(hegel_context_t *ctx,
                                               const hegel_string_t *version) {
  return hegel_set_unicode_version(ctx, *version);
}

hegel_result_t hegel_zizek_run_start_blob(hegel_context_t *ctx,
                                          const hegel_settings_t *settings,
                                          const hegel_string_t *blob,
                                          hegel_output_callback_t callback,
                                          void *user_data,
                                          hegel_run_t **out_run) {
  return hegel_run_start_blob(ctx, settings, *blob, callback, user_data, out_run);
}

hegel_result_t hegel_zizek_test_case_from_blob(hegel_context_t *ctx,
                                               const hegel_settings_t *s,
                                               const hegel_string_t *blob,
                                               hegel_output_callback_t callback,
                                               void *user_data,
                                               hegel_test_case_t **out_test_case) {
  return hegel_test_case_from_blob(ctx, s, *blob, callback, user_data, out_test_case);
}

hegel_result_t hegel_zizek_label_from_name(hegel_context_t *ctx,
                                           const hegel_string_t *name,
                                           uint64_t *out_label) {
  return hegel_label_from_name(ctx, *name, out_label);
}

hegel_result_t hegel_zizek_string_generator_text(hegel_context_t *ctx,
                                                 uint64_t min_size,
                                                 uint64_t max_size,
                                                 const hegel_string_t *codec,
                                                 uint32_t min_codepoint,
                                                 uint32_t max_codepoint,
                                                 const hegel_string_t *categories,
                                                 size_t categories_len,
                                                 const hegel_string_t *exclude_categories,
                                                 size_t exclude_categories_len,
                                                 const hegel_string_t *include_characters,
                                                 const hegel_string_t *exclude_characters,
                                                 hegel_string_generator_t **out_generator) {
  return hegel_string_generator_text(ctx, min_size, max_size, codec, min_codepoint, max_codepoint,
                                     categories, categories_len, exclude_categories,
                                     exclude_categories_len, *include_characters,
                                     *exclude_characters, out_generator);
}

hegel_result_t hegel_zizek_string_generator_regex(hegel_context_t *ctx,
                                                  const hegel_string_t *pattern,
                                                  bool fullmatch,
                                                  const hegel_string_generator_t *alphabet,
                                                  hegel_string_generator_t **out_generator) {
  return hegel_string_generator_regex(ctx, *pattern, fullmatch, alphabet, out_generator);
}

hegel_result_t hegel_zizek_grammar_builder_new(hegel_context_t *ctx,
                                               const hegel_string_t *text,
                                               hegel_grammar_builder_t **out_builder) {
  return hegel_grammar_builder_new(ctx, *text, out_builder);
}

hegel_result_t hegel_zizek_target(hegel_context_t *ctx,
                                  hegel_test_case_t *tc,
                                  double value,
                                  const hegel_string_t *label) {
  return hegel_target(ctx, tc, value, *label);
}

hegel_result_t hegel_zizek_event(hegel_context_t *ctx,
                                 hegel_test_case_t *tc,
                                 const hegel_string_t *label) {
  return hegel_event(ctx, tc, *label);
}

hegel_result_t hegel_zizek_event_value(hegel_context_t *ctx,
                                       hegel_test_case_t *tc,
                                       double value,
                                       const hegel_string_t *label) {
  return hegel_event_value(ctx, tc, value, *label);
}
