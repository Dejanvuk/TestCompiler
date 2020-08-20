int s = 12;

int foo(int first) {
    int k = first * 2;
    return k * 3;
}

int main(int argc, int argv) {
    int firsta = 7;
    int g;
    int f = 44;
    if(firsta != 7) {
        g = 1;
        f = 45;
    }
    else if(firsta <= 6) {
        g = 2;
        f = 46;
    }
    else if(firsta > 9) {
        g = 3;
    }
    else if(firsta == 7){
        g = 4;
    }
    return g;
}

