var user_input = [
  ["This", "%", "60"],
  ["Lusian Website", "%", "30"],
  ["Cooking Class", "17:30", "180"],
  ["Dog Walk", "13:30", "25"],
  ["Learn", "%", "45"],
  ["Lunch", "12:30", "%"]
]



// helpers

const str_to_time = (str) => {
  let time = (str).split(':');
  let now = new Date();
  return new Date(now.getFullYear(), now.getMonth(), now.getDate(), ...time);
}

function AddMinutesToDate(date, minutes) {
  return new Date(new Date(date).getTime() + minutes * 60000);
}



var timeline = [["Start", str_to_time("09:00"), "0"]],
    timed_events = [],
    wild_events = []

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

// Time specific assignment
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


// Wildcard assignment


// assign duration specified wildcards
console.log(wild_events)
var last_wildcard = 0 // a simple itterator for new wilds assignment
// query for gaps in timline to fit wildcards
for (let event in timeline) {
  if (event<timeline.length-1) {
    event = timeline[event]
    var next_event = timeline[timeline.indexOf(event)+1]
    var time_gap = next_event[1] - event[1]
    time_gap /= 60000
    let event_end_shift = parseInt(event[2])+5
    time_gap -= event_end_shift
    // check if wildcard would fit
    // First check is the 0th index of the wildcards
    var selected_wildcard = wild_events[last_wildcard]
    if(selected_wildcard!=undefined && parseInt(selected_wildcard[2])<time_gap) {
      console.log("gap found")
      selected_wildcard[1] = AddMinutesToDate(event[1], event_end_shift)
      console.log(selected_wildcard)
      timeline.splice(timeline.indexOf(event)+1, 0, selected_wildcard)
      last_wildcard+=1
    }
    console.log(time_gap)
  }
  else {
    // no future event
  }

}

console.log(timeline)

function addZ(n){return n<10? '0'+n:''+n;}

function stamp_date(date) {
  var d = date
  let year = d.getFullYear(),
      month = addZ(d.getMonth()+1),
      day = addZ(d.getDate()),
      day_name = d.toString().split(' ')[0],
      hour = addZ(d.getHours()),
      min = addZ(d.getMinutes())
  return `<${year}-${month}-${day} ${day_name} ${hour}:${min}>`
}

function orgify_event(event) { // generates event heading
  var r = `* TODO ${event[0]}
SCHEDULED: ${stamp_date(event[1])}`
  return r
}

function generate_org_markdown(line) { // line should be the timeline var
  var agenda = ""
  for (let event in timeline) {
    event = timeline[event]
    console.log(event)
    agenda+="\n"+orgify_event(event)
  }
  console.log(agenda)
}

generate_org_markdown(timeline)
