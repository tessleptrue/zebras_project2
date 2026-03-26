/*!tests!
 *
 * { "input": [], 
 *   "output": [
 *     "0",
 *     "1",
 *     "2"
 * ] 
 * }
 *
 */
#include <stdio.h>
#include <stdbool.h>

void main() {
    int x = 0 ;
    while (x < 3) {
        fprintf(stdout, "%d\n", x) ;
        x = x + 1 ;
    }
    return ;
}