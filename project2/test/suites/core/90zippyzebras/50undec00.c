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
}

void f() {
    fprintf(stdout, "hello");
}