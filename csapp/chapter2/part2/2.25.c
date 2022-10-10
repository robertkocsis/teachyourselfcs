#include <stdio.h>

float sum_elements(float a[], unsigned length)
{
    int i;
    float result = 0;

    if (length == 0)
    {
        return result;
    }

    for (i = 0; i <= length - 1; i++)
        result += a[i];
    return result;
}

int main()
{
    float array[3] = {1, 2, 3.5};

    printf("%f \n", sum_elements(array, 3U));

    return 0;
}
