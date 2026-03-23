/*!tests!
 *
 * {
 *   "input": ["5"],
 *   "output": ["5"]
 * }
 *
 */
#include <stdio.h>

int f() {
    int x ;
    fscanf(stdin, "%d", &x) ;
    return x ;
}

void main() {
    fprintf(stdout, "%d\n", f()) ;
}