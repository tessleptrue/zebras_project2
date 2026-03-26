/*!tests!
 *
 * {
 *   "input": [],
 *   "exception": "SegmentationError"
 * }
 *
 */
#include <stdio.h>

void main() {
    int xs[4];
    fprintf(stdout, "%d\n", xs[5]);
    return;
}