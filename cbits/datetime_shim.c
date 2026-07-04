#include <hegel.h>

hegel_result_t hegel_zizek_generate_date(hegel_context_t *ctx,
                                          hegel_test_case_t *tc,
                                          const hegel_date_t *min_value,
                                          const hegel_date_t *max_value,
                                          hegel_date_t *out_value) {
  return hegel_generate_date(ctx, tc, *min_value, *max_value, out_value);
}

hegel_result_t hegel_zizek_generate_time(hegel_context_t *ctx,
                                          hegel_test_case_t *tc,
                                          const hegel_time_t *min_value,
                                          const hegel_time_t *max_value,
                                          hegel_time_t *out_value) {
  return hegel_generate_time(ctx, tc, *min_value, *max_value, out_value);
}

hegel_result_t hegel_zizek_generate_datetime(hegel_context_t *ctx,
                                              hegel_test_case_t *tc,
                                              const hegel_datetime_t *min_value,
                                              const hegel_datetime_t *max_value,
                                              hegel_datetime_t *out_value) {
  return hegel_generate_datetime(ctx, tc, *min_value, *max_value, out_value);
}
