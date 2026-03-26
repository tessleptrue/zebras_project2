/*!tests!
 *
 * {
 *    "input":      [],
 *    "output":     ["true"]
 * }
 *
 */
#include <stdio.h>
#include <stdbool.h>


bool f() {
    if (true) return true ;

    return false ;
}

void main() {
    fprintf(stdout, "%b\n", f()) ;

    return ;
}
