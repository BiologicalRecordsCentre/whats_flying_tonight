$(document).ready(function () {
  navigator.geolocation.getCurrentPosition(onSuccess, onError);

  function onError (err) {
    setTimeout(function () {
        Shiny.onInputChange("geolocation", false);
    }, 1100);
  }

 function onSuccess (position) {
    setTimeout(function () {
        var coords = position.coords;
        console.log(coords.latitude + ", " + coords.longitude);
        Shiny.onInputChange("geolocation", true);
        Shiny.onInputChange("lat", coords.latitude);
        Shiny.onInputChange("long", coords.longitude);
    }, 1100);
}
});