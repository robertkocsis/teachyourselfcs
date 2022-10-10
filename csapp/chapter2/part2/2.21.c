#include <stdio.h>

void printResult(int comparisonResult)
{

    printf("%d ", comparisonResult);
    printf("\n");
}

int main()
{
    printResult(-2147483647-1 == 2147483648U);
    printResult(-2147483647-1 < 2147483647);
    printResult(-2147483647-1U < 2147483647);
    printResult(-2147483647-1 < -2147483647);
    printResult(-2147483647-1U < -2147483647);

    return 0;
}