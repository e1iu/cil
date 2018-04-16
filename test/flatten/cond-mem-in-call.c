
int foo(int x, int y)
{
  printf("hello, %d", x);
  return 0;
}

int main(int argc, char **argv)
{

  int *p = 0;
  int x = 1;
  if (x == 0) {
    x = foo(*p, x);
  } else {
    x = 10;
  }

  printf("%d\n", x);
  return 0;
  
}
