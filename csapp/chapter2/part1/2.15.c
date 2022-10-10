#include <stdio.h>


int main()
{
    int a = 0x46;
    int b = 0x46;
    
    printf("%X \n", !(a ^ b));

    return 0;
}