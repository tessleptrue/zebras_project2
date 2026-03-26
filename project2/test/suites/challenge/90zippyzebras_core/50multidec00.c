/*!tests!
 *
 * {
 *   "input": [],
 *   "exception": "MultipleDeclaration"
 * }
 *
 */
#include <stdio.h>

void f(int x, int y, int x) {  
    return;
}

void main() {
    f(1, 2, 3);
    return;
}