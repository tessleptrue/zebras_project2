/*!tests!
 *
 * { "input": [], 
 *   "output": ["5"] }
 *
 */
#include <stdio.h>
#include <stdbool.h>

void main() {
    while (false) {
        fprintf(stdout, "%d\n", 1) ;
    }
    fprintf(stdout, "%d\n", 5) ;
    return ;
}