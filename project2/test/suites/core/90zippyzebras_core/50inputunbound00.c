/*!tests!
 *
 * {
 *   "input": ["5"],
 *   "exception": "UnboundVariable"
 * }
 *
 */
#include <stdio.h>

void main() {
    fscanf(stdin, "%d\n", &x) ;
}