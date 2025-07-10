/*
 * Author: Jehadur Rahman
 * Organization: CyArm
 * GitHub: https://github.com/JehadurRE
 * Website: jehadurre.me
 * Date: 2025-07-10 09:43 (Thu)
 */

// by Alexander Nikolskiy

const readline = require('readline');
const rl = readline.createInterface({
    input: process.stdin,
    terminal: false
});

process.stdin.setEncoding('utf8');
rl.on('line', readLine);

function readLine(line) {
    console.log(fib(parseInt(line, 10)));
    process.exit();
}

function fibNaive(n) {
    if (n <= 1) return n;
    return fibNaive(n - 1) + fibNaive(n - 2);
}

function fib(n) {
    if (n <= 1) return n;
    
    const arr = new Array(n + 1);
    arr[0] = 0;
    arr[1] = 1;
    
    for (let i = 2; i <= n; i++) {
        arr[i] = arr[i - 1] + arr[i - 2];
    }
    
    return arr[n];
}

function testSolution() {
    console.assert(fib(3) === 2);
    console.assert(fib(10) === 55);
    for (let n = 0; n < 20; n++) {
        console.assert(fib(n) === fibNaive(n));
    }
}

module.exports = fib;
