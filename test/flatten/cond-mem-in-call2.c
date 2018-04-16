
int foo(int x, int y)
{
  printf("hello, %d", x);
  return 0;
}

typedef int(*fty)(int, int);
int main(int argc, char **argv)
{

  int *p0 = 0;
  fty p;
  fty a[10];
  a[1] = p;
  int x = 1;
  if (x == 0) {
    x = (a[*p0])(1, x);
  } else {
    x = 10;
  }

  printf("%d\n", x);
  return 0;
  
}
