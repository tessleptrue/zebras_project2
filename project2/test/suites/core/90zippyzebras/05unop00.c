/*!tests!
 *
 * { "input": [],
 *   "output": [ "-5", "false"] }
 *
 */
#include <stdio.h>
#include <stdbool.h>

void main() {
    fprintf(stdout, "%d\n", -5) ;
    fprintf(stdout, "%b\n", !true) ;
    return ;
}