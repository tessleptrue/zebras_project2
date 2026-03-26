/*!tests!
 *
 * { "input": [], 
 *   "output": ["false","true", "true"]
 * }
 *
 */
#include <stdio.h>
#include <stdbool.h>

void main() {
    fprintf(stdout, "%b\n", false && true) ;
    fprintf(stdout, "%b\n", true || false) ;
    fprintf(stdout, "%b\n", true && true) ;
    return ;
}