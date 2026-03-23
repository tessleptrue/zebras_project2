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
    if (true) return true ;
}

void main() {
    f() ;
    return ;
}