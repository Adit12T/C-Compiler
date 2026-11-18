int printf(const char *fmt, ...);
int a;
int b;
int main()
{
    a = 7;
    b = 3;
    if (a > b)
        printf("max %d\n", a);
    else
        printf("max %d\n", b);
    if (a != b)
        printf("differ\n");
    a = a % b;
    printf("mod %d\n", a);
    return 0;
}
