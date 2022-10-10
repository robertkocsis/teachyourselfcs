#include <stdio.h>

void a(int x) {
    printf("%X \n", x & 0xFF);
}

void b(int x) {
    printf("%X \n", x ^ ~0xFF );
}

void c(int x) {
    printf("%X \n", x | 0xFF);
}

int main()
{
    int x = 0x87654321;
    a(x);
    b(x);
    c(x);

    return 0;
}