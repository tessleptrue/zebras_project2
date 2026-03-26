/*!tests!
 *
 * {
 *   "input": [],
 *   "output": ["6"]
 * }
 *
 */
#include <stdio.h>
#include <stdbool.h>

int f() { return 5 ; }

void main() {
    int x = f() + 1 ;
    fprintf(stdout, "%d\n", x) ;
    return ;
}