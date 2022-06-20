var schedule = []

function add_to_schedule(event, time) {
  schedule.push(event, time)
}


var sleep_time = 10
var work_time = 6

var precision = 5 // minutes

var day = 24 - sleep_time

var template = [
  "Wakeup Time",
  "Breakfast",
  "Morning Hygine",
  "%DAY",
  "Dinner",
  "Evening Entertainment",
  "Evening Hygine",
  "Sleep"
]

var day_template = [
  "Work",
  "Lunch",
  "Fitness",
  "Work",
  "Social Events"
]


wake_up_time = sleep_time/2
go_to_sleep_time = 24 - sleep_time/2


non_work_time = day - work_time


