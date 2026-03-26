/*!tests!
 *
 * {
 *   "input": [],
 *   "output": [
 *      "true",
 *      "true",
 *      "false",
 *      "true",
 *      "true",
 *      "true",
 *      "false"]
 * }
 *
 */
#include <stdio.h>

void main() {
    fprintf(stdout, "%b\n", 5 == 5) ;
    fprintf(stdout, "%b\n", 5 != 3) ;
    fprintf(stdout, "%b\n", 3 > 5) ;
    fprintf(stdout, "%b\n", 5 <= 5) ;
    fprintf(stdout, "%b\n", 6 > 2) ;
    fprintf(stdout, "%b\n", 5 >= 5) ;
    fprintf(stdout, "%b\n", 6 <= 5) ;
    return ;
}