// it is now 1700


var global = {
  "waking up duration": 10,
  "breakfast min": 15,
  "departure time": 10
}

var schedule = []

var params = {
  "wake up time": 600,
  "work in the morning": true,
  "leave the house": 745,
}

var statistics = {
  "time in the morning": params['leave the house'] - params['wake up time'],
}

var hours = parseInt(statistics['time in the morning']/100)
var minutes = hours * 60 + (statistics['time in the morning']%100)

var clock = minutes
var todo = ["waking up", "breakfast", "hygine", "work", "departure"]

//waking up
var wud = global['waking up duration']
clock-=wud
schedule.push("+"+wud+"\tWaking Up")






console.log(statistics)
