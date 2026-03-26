/*!tests!
 *
 * {
 *   "input": [],
 *   "output": ["None"]
 * }
 *
 */
#include <stdio.h>

void f() {
    fprintf(stdout, "None");
    return ;
}

void main() {
    f();
    return ;
}