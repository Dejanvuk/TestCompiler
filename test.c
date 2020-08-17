int s = 2;

int gas() {
    return s;
}

int sev(int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8) {
    int c = arg1 + arg2 + arg3 + arg4 + arg5 + arg6 + arg7 + arg8;
    int newf = 928;
    return c;
}

int foo(int first) {
    int k = first * 2;
    return k * 3;
}

int main(int argc, int argv) {
    int firsta = s * 2;
    int s = 1;
    int d = sev(s,2,3,4,5,6,7,firsta);
    int g = foo(d);
    int n;
    n = s + d + g;
    return n;
}

