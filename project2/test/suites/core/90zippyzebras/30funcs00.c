/*!tests!
 *
 * {
 *   "input": [],
 *   "output": ["3"]
 * }
 *
 */
#include <stdio.h>

int f(int x) { return x + 1 ; }
int g(int y) { return f(y) + 1 ; }

void main() {
    fprintf(stdout, "%d\n", g(1)) ;
    return ;
}