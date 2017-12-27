import fs = require('fs')
import path = require('path')
import * as lib from './lib';


const INPUT = fs.readFileSync(path.join(__dirname, '../../input.txt'), 'utf8')


function main() {
    console.log(`day1-p1: ${lib.part1(INPUT)}`);
    console.log(`day1-p2: ${lib.part2(INPUT)}`);
}

main()

