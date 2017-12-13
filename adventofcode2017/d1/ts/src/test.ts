import * as assert from 'assert';
import * as lib from './lib';


(() => {
    const p1cases = [
        ["1122", 3],
        ["1111", 4],
        ["1234", 0],
        ["91212129", 9],
    ];
    console.log("Part one...");
    p1cases.forEach(function([input, expected]: [string, number]) {
        process.stdout.write(`  ${input} -> ${expected} ... `);
        const out = lib.part1(input);
        process.stdout.write(`${out} ... `);
        assert(out === expected);
        console.log("Ok");
    })

    const p2cases = [
        ["1212", 6],
        ["1221", 0],
        ["123425", 4],
        ["123123", 12],
        ["12131415", 4],
    ];
    console.log("Part two...");
    p2cases.forEach(function([input, expected]: [string, number]) {
        process.stdout.write(`  ${input} -> ${expected} ... `);
        const out = lib.part2(input);
        process.stdout.write(`${out} ... `);
        assert(out === expected);
        console.log("Ok");
    })
})()

