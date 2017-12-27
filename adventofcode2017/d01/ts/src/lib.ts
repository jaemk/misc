
export function part1(input: string): number {
    const chars = input.trim().split('');
    let [front, offset] = [chars.slice(0, 1), chars.slice(1)];
    offset = offset.concat(front);
    let sum = 0;
    for (let i = 0; i < chars.length; i++) {
        if (chars[i] == offset[i]) {
            sum += parseInt(chars[i])
        }
    }
    return sum
}


export function part2(input: string): number {
    const chars = input.trim().split('');
    const half = chars.length / 2;

    let sum = 0
    chars.forEach((c, ind) => {
        let other_ind = ind + half;
        while (other_ind >= chars.length) { other_ind -= chars.length; }
        if (c == chars[other_ind]) {
            sum += parseInt(c);
        }
    });
    return sum
}

