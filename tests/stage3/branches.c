int printf(const char *fmt, ...);
int a;
int b;
int main()
{
    a = 7;
    b = 3;
    if (a > b && b != 0)
        printf("max %d\n", a);
    if (a < b || b == 3)
        printf("or ok\n");
    a = a % b;
    printf("mod %d\n", a);
    return 0;
}
