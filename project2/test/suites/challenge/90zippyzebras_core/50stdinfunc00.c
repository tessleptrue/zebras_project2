/*!tests!
 *
 * {
 *   "input": ["true"],
 *   "exception": "TypeError"
 * }
 *
 */
#include <stdio.h>
#include <stdbool.h>

int f(int x) {
    return x + 1 ;
}

void main() {
    bool b ;
    fscanf(stdin, "%b", &b) ;
    f(b) ;
}