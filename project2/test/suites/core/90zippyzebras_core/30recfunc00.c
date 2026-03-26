/*!tests!
 *
 * {
 *   "input": ["3"],
 *   "output": ["3"]
 * }
 *
 */
#include <stdio.h>

int f(int n) {
    if (n == 0) return 0 ;
    return 1 + f(n - 1) ;
}

void main() {
    int n ;
    fscanf(stdin, "%d", &n) ;
    fprintf(stdout, "%d\n", f(n)) ;
    return ;
}