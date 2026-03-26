/*!tests!
 *
 * {
 *  "input":        [],
 *  "exception":    "UnboundVariable"
 * }
 *
 */


#include <stdio.h>

void main() {
    fprintf(stdout, "%d\n", x) ;
    return ;
}
