/*!tests!
 *
 * {
 *   "input": [],
 *   "exception": "TypeError"
 * }
 *
 */
#include <stdio.h>

void main() {
    int x ;
    fscanf(stdin, "%x", &x) ;
    return ;
}