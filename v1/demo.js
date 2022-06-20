var user_input = [
  ["This", "%", "60"],
  ["Lusian Website", "%", "30"],
  ["Cooking Class", "17:30", "180"],
  ["Dog Walk", "11:30", "25"],
  ["Learn", "%", "45"],
  ["Lunch", "12:30", "%"]
]

var timeline = [],
    timed_events = [],
    wild_events = []



const str_to_time = (str) => {
  let time = (str).split(':');
  let now = new Date();
  return new Date(now.getFullYear(), now.getMonth(), now.getDate(), ...time);
}

// sort events
for (let input in user_input) {
  input = user_input[input]
  if (input[1] !== "%") {
    input[1] = str_to_time(input[1])
    timed_events.push(input)
  }
  else {
    wild_events.push(input)
  }
}




const remove_item_from_timed_events = (event) => {
  var index = timed_events.indexOf(event)
  timed_events = timed_events.slice(0, index).concat(timed_events.slice(index + 1));
}

// sort timed event list by time
while(timed_events.length > 0) {
  var min_time = str_to_time("23:59"),
      min_event = null;
  for (let event in timed_events) {
    event = timed_events[event]
    if (event[1] < min_time) {
      min_time = event[1]
      min_event = event
    }
  }
  // listify
  remove_item_from_timed_events(min_event)
  timeline.push(min_event)
}
console.log(timeline)
