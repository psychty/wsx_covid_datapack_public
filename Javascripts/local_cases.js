
//search event
$(document).on('click','#btnPostcode',function(){
    var input = $('#txtPostcode').val();
    var url  = "https://api.postcodes.io/postcodes/"+input;

    post(url).done(function(postcode){
      displayData(postcode);
    });
});

//random on load
$(document).ready(function(){
   var url  = "https://api.postcodes.io/random/postcodes";

    post(url).done(function(postcode){
      displayData(postcode);
    });
});


//enter event - search
$("#txtPostcode").keypress(function(e) {
  if (e.which === 13) {
    $('#btnPostcode').click();
  }
});


//display results on page
function displayData(postcode){
  var html = "";
  $('#result_text').hide();
      for(var index in postcode['result']){
        html += "<div class='row'>";
        html += "<div class='cell'>";
        html += index.replace(/_/g,' ').strFirstUpper();
        html += "</div><div class='cell'>";
        html += postcode['result'][index];
        html += "</div></div>";
      }
      $('#result_text').html(html).fadeIn(300);
}

//ajax call
function post(url){
    return $.ajax({
        url: url,
        success: function(){
          //woop
        },
        error: function(desc, err) {
            $('#result_text').html("Details: " + desc.responseText);
        }
    });
}


//uppercase
String.prototype.strFirstUpper = function() {
    return this.charAt(0).toUpperCase() + this.slice(1);
}
