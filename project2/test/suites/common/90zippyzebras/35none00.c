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
}

void main() {
    f();
    return ;
}