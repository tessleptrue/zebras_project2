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
    f(3, 2, 4); 
    return; 
}