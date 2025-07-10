#include <iostream>
using namespace std;

int foo(int n)
{
    if (n == 0)
        return 42;
    foo(n - 1); // no return
}

int main()
{
    cout << foo(5) << endl; // prints garbage or nothing meaningful
}
