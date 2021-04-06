
// https://stackoverflow.com/questions/667555/how-to-detect-idle-time-in-javascript-elegantly

var idleTime = 0;

$(document).ready(function () {

  var idleInterval = setInterval(timerIncrement, 1000); // millisec

  //Zero the idle timer on mouse movement.
  $(this).mousemove(function (e) {
    idleTime = 0;
  });
  $(this).keypress(function (e) {
    idleTime = 0;
  });
});

function timerIncrement() {
  idleTime = idleTime + 1; // number of minutes passed

  Shiny.setInputValue("idle_timeout-app_idle_time", idleTime);

}
