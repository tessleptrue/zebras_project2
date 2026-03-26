/*!tests!
 *
 * {
 *   "input": ["5"],
 *   "output": ["6"]
 * }
 *
 */
#include <stdio.h>
#include <stdbool.h>

int f(int x, int y) {
    return x + y ;
}

void main() {
    int n ;
    fscanf(stdin, "%d", &n) ;

    fprintf(stdout, "%d\n", f(n, 1)) ;
    return ;
}