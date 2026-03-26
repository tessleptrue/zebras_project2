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
    int y = x + 1 ;
    return ;
}