int printf(const char *fmt, ...);
int g;
int i;
int main()
{
    i = 0;
    g = 1;
    while (i < 5) {
        g = g * 2;
        i++;
    }
    if (g == 32)
        printf("loop ok %d\n", g);
    else
        printf("loop bad %d\n", g);
    for (i = 0; i < 3; i++)
        printf("for %d\n", i);
    return 0;
}
