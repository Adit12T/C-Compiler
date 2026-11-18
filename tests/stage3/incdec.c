int i;
int j;
int main()
{
    i = 0;
    j = i++;        /* j = 0, i = 1 */
    j = ++i + j;    /* i = 2, j = 2 */
    i--;            /* i = 1 */
    if (i <= 1)
        j = j + 10;
    if (i >= 2)
        j = j + 100;
    return j;       /* 12 */
}
