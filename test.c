int s = 5;

int foo(int a, int b, int c, int d, int e, int f, int g, int h)
{
    int k = a + b + c + d + e + f + g + h;
    return k * 3;
}

int fib(int len) {
    int f1 = 0;
    int f2 = 1;
    int f3 = 0;
    int i = 3;
    while(i<=len)           
    {
        f3=f1+f2;                     
        f1=f2;
        f2=f3;
        i=i+1;                  
    }

    return f3;
}

int main(int argc, int argv)
{
    int firsta = s;
    int s = 1;
    int g = 4 + (s * 5 / (2 * 3 - 1) * 3 * firsta - 9) * 2;
    int f = foo(1,2,3,4,5,6,7,8);
    while (firsta < 100)
    {
        firsta = firsta + f + g;
        if (firsta >= 200)
        {
            f = 2;
        }
        else if(firsta <= 110)
        {
            f = 3;
        }
        else if(firsta >= 111)
        {
            f = 4;
        }
    }

    int result;

    if (f == 2)
    {
        result = 10;
    }
    else if(f == 3) {
        result = 11;
    }
    else {
        result = 12;
    }

    return fib(result);
}
