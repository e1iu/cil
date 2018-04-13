
int main(int argc, char **argv)
{

  int *p = 0;
  int x = 1;
  if (x == 0) {
    x = *p + 1;
  } else {
    x = 10;
  }

  printf("%d\n", x);
  return 0;
  
}
