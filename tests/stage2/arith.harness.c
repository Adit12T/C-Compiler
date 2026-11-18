/* Calls the function compiled by pcc3 and prints the globals it sets. */
#include <stdio.h>

extern int x, y;
void compute(void);

int main(void)
{
    compute();
    printf("%d %d\n", x, y);
    return 0;
}
