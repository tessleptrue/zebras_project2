/*!tests!
 *
 * { "input": [], 
 *   "output": ["2"] }
 *
 */
#include <stdio.h>
#include <stdbool.h>

void main() {
    if (false) 
        fprintf(stdout, "%d\n", 1) ;
    else
        fprintf(stdout, "%d\n", 2) ;
    return ;
}