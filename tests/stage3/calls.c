int printf(const char *fmt, ...);
int puts(const char *s);
int n;
int main()
{
    puts("calls");
    for (n = 1; n <= 3; n++)
        printf("%d squared is %d\n", n, n * n);
    printf("%s and %c\n", "text", 65);
    return 0;
}
