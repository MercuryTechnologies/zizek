/*
 * Prototypes for zizek's C shims over libhegel.
 *
 * GHC's FFI cannot pass or return a C struct by value, so each shim takes a
 * pointer to every struct argument the libhegel function takes by value, and
 * writes a by-value result through an out-pointer.
 */
#ifndef ZIZEK_SHIM_H
#define ZIZEK_SHIM_H

#include <hegel.h>

/* datetime_shim.c */

hegel_result_t hegel_zizek_generate_date(hegel_context_t *ctx,
                                         hegel_test_case_t *tc,
                                         const hegel_date_t *min_value,
                                         const hegel_date_t *max_value,
                                         hegel_date_t *out_value);

hegel_result_t hegel_zizek_generate_time(hegel_context_t *ctx,
                                         hegel_test_case_t *tc,
                                         const hegel_time_t *min_value,
                                         const hegel_time_t *max_value,
                                         hegel_time_t *out_value);

hegel_result_t hegel_zizek_generate_datetime(hegel_context_t *ctx,
                                             hegel_test_case_t *tc,
                                             const hegel_datetime_t *min_value,
                                             const hegel_datetime_t *max_value,
                                             hegel_datetime_t *out_value);

/* string_shim.c */

void hegel_zizek_context_last_error(const hegel_context_t *ctx,
                                    hegel_string_t *out_error);

hegel_result_t hegel_zizek_settings_new_for_profile(hegel_context_t *ctx,
                                                    const hegel_string_t *name,
                                                    hegel_settings_t **out_settings);

hegel_result_t hegel_zizek_settings_set_test_location(hegel_context_t *ctx,
                                                      hegel_settings_t *s,
                                                      const hegel_string_t *file,
                                                      uint32_t begin_line,
                                                      const hegel_string_t *class_name,
                                                      const hegel_string_t *function);

hegel_result_t hegel_zizek_settings_register_profile(hegel_context_t *ctx,
                                                     const hegel_string_t *name,
                                                     const hegel_settings_t *settings);

hegel_result_t hegel_zizek_set_default_profile(hegel_context_t *ctx,
                                               const hegel_string_t *name);

hegel_result_t hegel_zizek_set_unicode_version(hegel_context_t *ctx,
                                               const hegel_string_t *version);

hegel_result_t hegel_zizek_run_start_blob(hegel_context_t *ctx,
                                          const hegel_settings_t *settings,
                                          const hegel_string_t *blob,
                                          hegel_output_callback_t callback,
                                          void *user_data,
                                          hegel_run_t **out_run);

hegel_result_t hegel_zizek_test_case_from_blob(hegel_context_t *ctx,
                                               const hegel_settings_t *s,
                                               const hegel_string_t *blob,
                                               hegel_output_callback_t callback,
                                               void *user_data,
                                               hegel_test_case_t **out_test_case);

hegel_result_t hegel_zizek_label_from_name(hegel_context_t *ctx,
                                           const hegel_string_t *name,
                                           uint64_t *out_label);

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
                                                 hegel_string_generator_t **out_generator);

hegel_result_t hegel_zizek_string_generator_regex(hegel_context_t *ctx,
                                                  const hegel_string_t *pattern,
                                                  bool fullmatch,
                                                  const hegel_string_generator_t *alphabet,
                                                  hegel_string_generator_t **out_generator);

hegel_result_t hegel_zizek_target(hegel_context_t *ctx,
                                  hegel_test_case_t *tc,
                                  double value,
                                  const hegel_string_t *label);

hegel_result_t hegel_zizek_event(hegel_context_t *ctx,
                                 hegel_test_case_t *tc,
                                 const hegel_string_t *label);

hegel_result_t hegel_zizek_event_value(hegel_context_t *ctx,
                                       hegel_test_case_t *tc,
                                       double value,
                                       const hegel_string_t *label);

#endif /* ZIZEK_SHIM_H */
