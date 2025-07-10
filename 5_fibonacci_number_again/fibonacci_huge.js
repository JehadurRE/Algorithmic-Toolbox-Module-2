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
    if (line !== "\n") {
        const n = parseInt(line.toString().split(' ')[0], 10);
        const m = parseInt(line.toString().split(' ')[1], 10);

        console.log(getFibMod(n, m));
        process.exit();
    }
}

function findPisanoPeriod(m) {
    let prev = 0;
    let current = 1;
    
    for (let i = 0; i < m * m; i++) {
        let temp = current;
        current = (prev + temp) % m;
        prev = temp;
        
        if (prev === 0 && current === 1) {
            return i + 1;
        }
    }
    return 0;
}

function getFibMod(n, m) {
    const p = findPisanoPeriod(m);
    n = n % p;
    
    if (n <= 1) return n;
    
    let previous = 0;
    let current = 1;
    
    for (let i = 0; i < n - 1; i++) {
        let tmp_previous = previous;
        previous = current;
        current = (tmp_previous + current) % m;
    }
    
    return current;
}

module.exports = getFibMod;
