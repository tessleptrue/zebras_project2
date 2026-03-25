/*!tests!
 *
 * {
 *   "input": [],
 *   "exception": "TypeError"
 * }
 *
 */
#include <stdio.h>
#include <stdbool.h>

void main() {
    fprintf(stdout, "%d", true) ;
    return ;
}