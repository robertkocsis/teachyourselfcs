#include <stdio.h>
#include <string.h>

int strlonger(char *s, char *t)
{
    return strlen(s) > strlen(t);
}

int main()
{
    float array[3] = {1, 2, 3};

    printf("%d \n", strlonger("123", "1234"));

    return 0;
}
