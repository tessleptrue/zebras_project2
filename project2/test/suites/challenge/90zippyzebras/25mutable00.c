/*!tests!
 *
 * {
 *   "input": [],
 *   "output": ["99"]
 * }
 *
 */
#include <stdio.h>

void set(int xs[]) {
    xs[0] = 99;
    return;
}

void main() {
    int xs[2];
    xs[0] = 1;

    set(xs);

    fprintf(stdout, "%d\n", xs[0]);
    return;
}