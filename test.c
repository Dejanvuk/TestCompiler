int s = 12;

int foo(int first) {
    int k = first * 2;
    return k * 3;
}

int main(int argc, int argv) {
    int f1 = 0;
    int f2 = 1;
    int f3 = 0;
    int i = 3;
    int len = 10;
    while(i<=len)           
    {
        f3=f1+f2;                     
        f1=f2;
        f2=f3;
        i=i+1;                  
    }

    return f3;
}
