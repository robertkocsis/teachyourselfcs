#include <stdio.h>

int fun1(unsigned word)
{
    printf("fun1 with:0x%X \n", word);
    printf("\n");
    unsigned shifted = word << 24;
    printf("(word << 24): %u ", shifted);
    printf("\n");

    shifted = shifted >> 24;
    printf("(word << 24) >> 24: %u ", shifted);
    printf("\n");

    int shiftedInt = (int)shifted;

    printf("(int) ((word << 24) >> 24): %d ", shiftedInt);
    printf("\n");
    printf("\n");

    return shiftedInt;
}
int fun2(unsigned word)
{
    printf("fun2 with:0x%X \n", word);
    printf("\n");

    unsigned shifted = word << 24;
    printf("(word << 24): %u ", shifted);
    printf("\n");

    int shiftedInt = (int)shifted;
    printf("(int)word << 24: %d ", shiftedInt);
    printf("\n");

    shiftedInt = shiftedInt >> 24;

    printf("((int)word << 24) >> 24: %d ", shiftedInt);
    printf("\n");
    printf("\n");

    return shiftedInt;
}

void printResult(unsigned word)
{
    fun1(word);
    fun2(word);
}

int main()
{
    printResult(0x00000076);
    printResult(0x87654321);
    printResult(0x000000C9);
    printResult(0xEDCBA987);

    return 0;
}