/*!tests!
 *
 * {
 *   "input": ["5"],
 *   "exception": "TypeError"
 * }
 *
 */
#include <stdio.h>

void main() {
    int x ;
    fscanf(stdin, "%d", &x) ;

    if (x) return ;
}