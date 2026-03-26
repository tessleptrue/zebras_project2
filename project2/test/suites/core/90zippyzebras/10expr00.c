/*!tests!
 *
 * { "input": [], 
 * "output": ["5"] }
 *
 */
#include <stdio.h>
#include <stdbool.h>


int f() { return 5 ; }

void main() {
    f() ; 
    fprintf(stdout, "%d\n", 5) ;
    return ;
}