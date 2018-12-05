use utils::{StdResult, StdError};
use std::collections::HashMap;
use chrono::{DateTime, Utc, TimeZone, Timelike};
use regex::Regex;


#[derive(Hash, PartialEq, Debug)]
struct Window {
    start: DateTime<Utc>,
    end: DateTime<Utc>,
}
impl Window {
    fn minutes(&self) -> i64 {
        self.end.signed_duration_since(self.start).num_minutes()
    }
}


/// collection of shifts by guard id
type Shifts = HashMap<u32, Vec<Shift>>;


#[derive(Hash, PartialEq)]
struct Shift {
    guard_id: u32,
    start: DateTime<Utc>,
    asleep: Vec<Window>,
}
impl Shift {
    fn total_minutes_asleep(&self) -> i64 {
        self.asleep.iter().map(|w| w.minutes()).sum()
    }

    /// array of minutes with 0 for awake minutes and 1 for asleep minutes
    fn minute_array(&self) -> StdResult<[u32; 60]> {
        let mut arr = [0; 60];
        for window in &self.asleep {
            if window.start.hour() != 0 {
                Err(format!("Found someone asleep before midnight! {:?}", window))?;
            }
            let start_minute = window.start.minute() as usize;
            let end_minute = window.end.minute() as usize;
            for i in start_minute..end_minute {
                arr[i] += 1;
            }
        }
        Ok(arr)
    }

    /// parse a full shift from an ordered collection of asleep/awake entries
    fn from_entries(entries: &[Entry]) -> StdResult<Self> {
        lazy_static! {
            static ref ID_RE: Regex = Regex::new(r"#(\d+)").unwrap();
        }
        let start = &entries[0];
        let cap = ID_RE.captures(&start.text).unwrap();
        let id = cap[1].parse::<u32>()?;

        let mut windows = vec![];
        for chunk in entries[1..].chunks(2) {
            let sleep_start = { assert!(chunk[0].text.starts_with("falls")); chunk[0].date };
            let sleep_end = { assert!(chunk[1].text.starts_with("wakes")); chunk[1].date };
            windows.push(Window {
                start: sleep_start,
                end: sleep_end,
            });
        }

        Ok(Shift {
            guard_id: id,
            start: start.date,
            asleep: windows,
        })
    }
}


fn parse_shifts(entries: &[Entry]) -> StdResult<Shifts> {
    let mut map = map!{};

    let mut buf = vec![];
    for e in entries {
        if e.text.starts_with("Guard") && !buf.is_empty() {
            let shift = Shift::from_entries(&buf)?;
            let e = map.entry(shift.guard_id).or_insert(vec![]);
            e.push(shift);
            buf.clear();
        }
        buf.push(e.clone());
    }
    if !buf.is_empty() {
        let shift = Shift::from_entries(&buf)?;
        let e = map.entry(shift.guard_id).or_insert(vec![]);
        e.push(shift);
    }
    Ok(map)
}


#[derive(Hash, PartialEq, Clone)]
struct Entry {
    date: DateTime<Utc>,
    text: String,
}
impl std::str::FromStr for Entry {
    type Err = StdError;
    fn from_str(s: &str) -> StdResult<Self> {
        lazy_static! {
            static ref ENTRY_RE: Regex = Regex::new(r"\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2})\]\s+(.*)").unwrap();
        }
        let cap = ENTRY_RE.captures(s).unwrap();
        let date = &cap[1];
        let msg = &cap[2];
        Ok(Entry {
            date: Utc.datetime_from_str(date, "%Y-%m-%d %H:%M")?,
            text: msg.trim().into(),
        })
    }
}


fn parse_entries(input: &str) -> StdResult<Vec<Entry>> {
    let mut entries = input
        .trim()
        .lines()
        .map(|line| {
            let e = line.parse::<Entry>()?;
            Ok(e)
        }).collect::<StdResult<Vec<_>>>()?;
    entries.sort_by_key(|e| e.date);
    Ok(entries)
}


struct NapStats {
    max_minute: u32,
    max_count: u32,
}

fn combine_minutes_max(guard_shifts: &[Shift]) -> StdResult<NapStats> {
    let mut minutes = [0; 60];
    for shift in guard_shifts {
        let arr = shift.minute_array()?;
        minutes.iter_mut().zip(arr.iter()).for_each(|(a, &b)| { *a += b });
    }
    let (max_minute, max_count) = minutes.iter()
        .enumerate()
        .max_by_key(|(_idx, count)| *count)
        .ok_or_else(|| "no max count found")?;
    Ok(NapStats {
        max_minute: max_minute.clone() as u32,
        max_count: max_count.clone(),
    })
}


fn part_1(shifts: &Shifts) -> StdResult<u32> {
    let (guard_id, guard_shifts) = shifts.iter().max_by_key(|(_id, shifts)| -> i64 {
        shifts.iter().map(|shift| shift.total_minutes_asleep()).sum()
    }).unwrap();

    let stats = combine_minutes_max(guard_shifts)?;
    Ok(guard_id * stats.max_minute as u32)
}


fn part_2(shifts: &Shifts) -> StdResult<u32> {
    let (guard, stats) = shifts.iter()
        .map(|(guard, guard_shifts)| -> StdResult<(u32, NapStats)> {
            let stats: NapStats = combine_minutes_max(guard_shifts)?;
            Ok((*guard, stats))
        })
        .filter_map(Result::ok)
        .max_by_key(|(_, stats)| stats.max_count)
        .ok_or_else(|| "no max count found")?;
    Ok(guard * stats.max_minute as u32)
}


pub fn run() -> StdResult<()> {
    info!("*** Day 4 ***");

    let (parse_ms, shifts) = time!({
        let entries = parse_entries(input_file!("d04.txt"))?;
        parse_shifts(&entries)?
    });

    let (ms1, res) = time!({ part_1(&shifts)? });
    info!("p1: {}", res);

    let (ms2, res) = time!({ part_2(&shifts)? });
    info!("p2: {}", res);
    info!("[Day 4 runtimes] parsing: {}ms, p1: {}ms, p2: {}ms\n", parse_ms, ms1, ms2);
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;
    static INPUT: &'static str = r##"
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
"##;

    #[test]
    fn test_part_1() {
        let entries = parse_entries(INPUT).unwrap();
        let shifts = parse_shifts(&entries).unwrap();
        let res = part_1(&shifts).unwrap();
        assert_eq!(res, 240)
    }

    #[test]
    fn test_part_2() {
        let entries = parse_entries(INPUT).unwrap();
        let shifts = parse_shifts(&entries).unwrap();
        let res = part_2(&shifts).unwrap();
        assert_eq!(res, 4455)
    }
}
