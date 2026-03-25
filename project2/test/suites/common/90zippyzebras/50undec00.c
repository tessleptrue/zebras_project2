/*!tests!
 *
 * {
 *  "input": [],
 *  "exception": "UndefinedFunction"
 * }
 *
 */
#include <stdio.h>

void main() {
    f();
    return ;
}

void f() {
    fprintf(stdout, "hello");
}