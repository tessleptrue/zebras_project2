/*!tests!
 *
 * {
 *   "input": [],
 *   "output": [
 *      "8",
 *      "2",
 *      "15",
 *      "3",
 *      "1"]
 * }
 *
 */
#include <stdio.h>

void main() {
    fprintf(stdout, "%d\n", 5 + 3) ;
    fprintf(stdout, "%d\n", 5 - 3) ;
    fprintf(stdout, "%d\n", 5 * 3) ;
    fprintf(stdout, "%d\n", 9 / 3) ;
    fprintf(stdout, "%d\n", 10 % 3) ;
    return ;
}