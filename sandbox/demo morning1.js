
class Timestamp {
  constructor(hour, min){
    this.hour = hour
    this.min = min
  }
  time(){
    return this.min + this.hour*60
  }
}

var global = {
  "waking up duration": 10,
  "breakfast min": 15,
  "departure time": 10,
  "hygine duration": 20
}

var morning_structure = ["wake up", "breakfast", "hygine", "work", "departure"]

const morning_params = {
  "work": true,
  "uptime": new Timestamp(6,0),
  "depart": new Timestamp(8,0)
}


var schedule = {

}

// proc



var morning_time = () => {
  var d = morning_params['depart'].time(),
      w = morning_params['uptime'].time();

  return d-w
}

morning_time = morning_time()

console.log(morning_structure)
console.log(morning_time)

var times = [
  [0, global['waking up duration']],
  [-1, global['breakfast min']],
  [-1, global['hygine duration']]
]

var departure = [morning_params['depart'].time() - global['departure time'], morning_params['depart'].time()]

if (morning_params['work'] == false){
    const index = morning_structure.indexOf('work');
    if (index > -1) {
        morning_structure.splice(index, 1); // 2nd parameter means remove one item only
    }
    times.push([-1, departure[0]])
}

times.push(departure)


var ut =  morning_params['uptime'].time()
for(item in morning_structure) {
  var i = item
  item = morning_structure[item]
  var time = times[i]
  if (time[0]<0) {
    time[0] = times[i-1][1]
  }
  schedule[item] = {
    "start":ut+time[0],
    "end": ut+time[1]
  }
}


console.log(schedule)
