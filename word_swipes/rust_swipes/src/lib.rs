/// `contained_by_raw(file: &str, word: &str)` is the fastest
/// of the three for single (or a few) lookups since it doesn't
/// save any intermediate values when reading the dictionary file.

use std::collections::HashMap;
use std::io::BufReader;
use std::io::prelude::*;
use std::fs::File;
use std::path::Path;


/// Checks if 'maybe's letters are contained (in proper order) in 'word'
fn chars_match(word: &str, maybe: &str) -> bool {
    let mut word_iter = word.chars();
    let mut found = 0;
    let mut prev_char = ' ';
    let to_find = maybe.len();
    for maybe_char in maybe.chars() {
        loop {
            match word_iter.next() {
                Some(word_char) => {
                    if maybe_char == word_char || maybe_char == prev_char {
                        found += 1;
                        prev_char = maybe_char;
                        if to_find == found { return true; }
                        break;
                    }
                },
                None => { return to_find == found },
            };
        };
    }
    return false;
}


/// Don't save any intermediate values, just read from the file and
/// save any matches. Break early, once the first letters are no longer matching.
pub fn contained_by_raw(file: &str, word: &str) -> Option<Vec<String>> {
    let word_first_last: String;
    let word_first: char;
    {
        let mut word_iter = word.chars();
        let first = word_iter.next().unwrap();
        let last = word_iter.rev().next().unwrap();
        word_first_last = format!("{}{}", first, last);
        word_first = first;
    }

    let path = Path::new(file);
    let file = File::open(&path).unwrap();
    let buf = BufReader::new(file);

    let mut matches = vec![];
    let mut found_matching_first = false;
    for line in buf.lines() {
        let maybe = line.unwrap();
        if maybe.len() < 5 { continue; }
        let maybe_first_last: String;
        let maybe_first: char;
        {
            let mut maybe_iter = maybe.chars();
            let first = maybe_iter.next().unwrap();
            let last = maybe_iter.rev().next().unwrap();
            maybe_first_last = format!("{}{}", first, last);
            maybe_first = first;
        }
        if found_matching_first && word_first != maybe_first { break; }
        if word_first_last != maybe_first_last { continue; }
        found_matching_first = true;

        if chars_match(word, &maybe) {
            matches.push(maybe.to_string());
        }
    }
    if matches.len() > 0 {
        Some(matches)
    } else {
        None
    }
}

/// MatcherHash stores the list of words in a hashmap
/// grouped by their starting and ending letters.
/// Lookups are super fast, but reading the file and loading
/// the hashmap are, of course, pretty slow if you're only
/// doing a single lookup.
pub struct MatcherHash(HashMap<String, Vec<String>>);

impl MatcherHash {
    pub fn new() -> MatcherHash {
        MatcherHash(HashMap::new())
    }
    pub fn load(&mut self, file: &str) {
        let &mut MatcherHash(ref mut map) = self;
        let path = Path::new(file);
        let file = File::open(&path).unwrap();
        let buf = BufReader::new(file);

        for line in buf.lines() {
            let word = line.unwrap().trim().to_string();
            if word.len() < 5 { continue; }
            let first_last: String;
            {
                let mut word_iter = word.chars();
                let first = word_iter.next().unwrap();
                let last = word_iter.rev().next().unwrap();
                first_last = format!("{}{}", first, last);
            }
            let mut group = map.entry(first_last).or_insert(vec![]);
            group.push(word);
        }
    }
    pub fn contained_by(&self, word: &str) -> Option<Vec<String>> {
        let &MatcherHash(ref map) = self;
        let first_last: String;
        {
            let mut word_iter = word.chars();
            let first = word_iter.next().unwrap();
            let last = word_iter.rev().next().unwrap();
            first_last = format!("{}{}", first, last);
        }
        match map.get(&first_last) {
            Some(maybes) => {
                let mut matches = vec![];
                for maybe in maybes.iter() {
                    if chars_match(word, maybe) {
                        matches.push(maybe.to_string());
                    }
                }
                Some(matches)
            },
            None => None
        }
    }
}

/// Same thing as MatcherHash, but saves words in a vec.
/// Half the load time, but lookups are the same speed as
/// just reading from the file. Doing a binary-search to
/// find the starting slice where the first letters match
/// would probably speed this up a bit.
pub struct MatcherVec(Vec<String>);

impl MatcherVec {
    pub fn new() -> MatcherVec {
        MatcherVec(Vec::new())
    }
    pub fn load(&mut self, file: &str) {
        let &mut MatcherVec(ref mut vec) = self;
        let path = Path::new(file);
        let file = File::open(&path).unwrap();
        let buf = BufReader::new(file);

        for line in buf.lines() {
            let word = line.unwrap().trim().to_string();
            if word.len() >= 5 {
                vec.push(word);
            }
        }
    }
    pub fn contained_by(&self, word: &str) -> Option<Vec<String>> {
        let &MatcherVec(ref words) = self;
        let word_first_last: String;
        let word_first: char;
        {
            let mut word_iter = word.chars();
            let first = word_iter.next().unwrap();
            let last = word_iter.rev().next().unwrap();
            word_first_last = format!("{}{}", first, last);
            word_first = first;
        }
        let mut matches = vec![];
        let mut found_matching_first = false;
        for maybe in words.iter() {
            let maybe_first_last: String;
            let maybe_first: char;
            {
                let mut maybe_iter = maybe.chars();
                let first = maybe_iter.next().unwrap();
                let last = maybe_iter.rev().next().unwrap();
                maybe_first_last = format!("{}{}", first, last);
                maybe_first = first;
            }
            if found_matching_first && word_first != maybe_first { break; }
            if word_first_last != maybe_first_last { continue; }
            found_matching_first = true;

            if chars_match(word, maybe) {
                matches.push(maybe.to_string());
            }
        }
        if matches.len() > 0 {
            Some(matches)
        } else {
            None
        }
    }
}

