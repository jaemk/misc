/// Perform base64 encoding on str of ascii text
pub fn encode_content(content: &str) -> Vec<(u32, String)> {
    content.as_bytes().chunks(3).fold(vec![], |mut acc, chunk| {
        // chunk is a vec of three (or less, since .chunks() doesn't pad) bytes.
        // Join chunk into a single u32 so we can pull out
        // overlapping 6-bit groups.
        let mut chars_encoded = 0u32;  // keep track of total chars encoded in this chunk
        let mut chunks_as_one = 0u32;
        for part in chunk.iter() {
            chars_encoded += 1;
            chunks_as_one = chunks_as_one << 8;  // make room for byte
            chunks_as_one |= *part as u32;       // tack on the byte
        }
        // Again, .chunks() doesn't add padding for undersized groups
        // so if there's only 1 item left, .chunks(3) returns a vec w/ len=1
        // To make sure the ending characters are correct, we need to shift
        // the chunks_as_one over as if chunk.len() == 3
        let trailing_nil = 3 - chars_encoded; // keep track of fake shifts required
        for _ in 0..trailing_nil {
            chunks_as_one = chunks_as_one << 8;
        }

        // Break the lower 24 bits into 4 6-bit groups, add 32,
        // convert to chars to be saved into output lines.
        // Skip the first n(trailing_nil)-items - these were alignment shifts,
        // if we didn't skip, then there would be extra trailing whitespace on groups
        // encoded from less than 3 chars (really just the last group).
        let mut enc_groups = vec![];
        for i in 0..4 {
            let group = chunks_as_one & 63;     // pull out the lower 6 bits
            chunks_as_one = chunks_as_one >> 6; // shift out the lower 6 bits
            if i < trailing_nil { continue; }
            enc_groups.push(std::char::from_u32(group + 32).unwrap());
        }
        let encoded = enc_groups.into_iter().rev().collect::<String>();

        // store 45 byte lines -- there's 45 source(!!) bytes per line.
        // the encoded lines (the output lines) are 60 bytes long.
        let len = acc.len();
        if len > 0 && acc[len-1].0 < 45 {
            // there's an existing line and there's room for more bytes
            acc[len-1].1.push_str(encoded.as_str());
            acc[len-1].0 += chars_encoded;
        } else {
            // new line
            acc.push((chars_encoded, encoded));
        }
        acc
    })
}

pub fn format_lines(enc_content: Vec<(u32, String)>, source: &str) -> String {
    let mut content = format!("begin 644 {}.txt", source);
    for &(count, ref line) in enc_content.iter() {
        let lead_char = std::char::from_u32(count + 32).unwrap();
        let leader = char::to_string(&lead_char);
        content.push_str(format!("\n{}{}", leader, line).as_str());
    }
    content.push_str("\n`\nend");
    content
}

/// Encode `content` as a base64 string. Provide an optional
/// `source` name to appear as the header .txt file.
pub fn encode(content: &str, source: Option<&str>) -> String {
    let src = match source {
        Some(name) => name,
        None => "file",
    };
    let enc_content = encode_content(content);
    format_lines(enc_content, src)
}

