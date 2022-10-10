#include <stdio.h>

void inplace_swap(int *x, int *y)
{
    *y = *x ^ *y; /* Step 1 */
    *x = *x ^ *y; /* Step 2 */
    *y = *x ^ *y; /* Step 3 */
}

void reverse_array(int a[], int cnt)
{
    int first, last;
    for (first = 0, last = cnt - 1;
         first < last;
         first++, last--)
    {
        printf("first: %d last: %d \n", first, last);
        inplace_swap(&a[first], &a[last]);
    }

    for (size_t i = 0; i < cnt; i++)
    {
        printf("%d ", a[i]);
    }
    printf("\n");
}

int main()
{
    int a[] = {1, 2, 3, 4};
    reverse_array(a, 4);

    int b[] = {1, 2, 3, 4, 5};
    reverse_array(b, 5);
    return 0;
}