/*!tests!
 *
 * {
 *   "input": [],
 *   "exception": "NoReturn"
 * }
 *
 */
#include <stdio.h>
#include <stdbool.h>


bool f() {
    if (false) return true ;
}

void main() {
    f() ;
    return ;
}