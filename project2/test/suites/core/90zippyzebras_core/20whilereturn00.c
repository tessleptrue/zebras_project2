/*!tests!
 *
 * {
 *   "input": [],
 *   "output": []
 * }
 *
 */
#include <stdio.h>
#include <stdbool.h>

void main() {
    int x = 0 ;

    while (x < 3) {
        while (true) {
            return ;
        }
        x = x + 1 ;
    }

    fprintf(stdout, "%d\n", 5) ;
}