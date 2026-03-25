/*!tests!
 *
 * {
 *   "input": [],
 *   "output": ["2"]
 * }
 *
 */
#include <stdio.h>

void main() {
    int x = 1 ;

    {
        x = 2 ;
    }

    fprintf(stdout, "%d\n", x) ;
    return ;
}