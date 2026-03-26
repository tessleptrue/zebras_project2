/*!tests!
 *
 * {
 *   "input": [],
 *   "output": ["1"]
 * }
 *
 */

int f() {
    int x = 1;
    return x;
}

int g() {
    int x = 2;
    return x;
}

void main() {
    int a = f();
    int b = g();
    fprintf(stdout, "%d", a);
    return ;
}