/*!tests!
 *
 * {
 *   "input": [],
 *   "exception": "UnboundVariable"
 * }
 *
 */
#include <stdio.h>

void main() {
    int x ;
    fprintf(stdout, "%d\n", x) ;
    return ;
}