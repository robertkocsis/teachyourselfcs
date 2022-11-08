#include <stdint.h>

int tmult_ok(int x, int y)
{
    int64_t p = x * y;

    return p <= INT32_MAX;
}