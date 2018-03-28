 $( function() {

    var addresssearchkeyVar = JSON.parse(jQuery('#addresssearchid').html());
    var addresssearchkey = addresssearchkeyVar['addresssearchkey'];

    function populateAddress( message ) {
    message = message.replace(/, /g, "\r");
    $("#companyAddress").val(message);
    }

    $( "#address" ).autocomplete({
      source: function( request, response ) {
        $.ajax( {
          url: "https://api.ordnancesurvey.co.uk/places/v1/addresses/find?key=" + addresssearchkey,
          dataType: "JSON",
          data: {
            query: request.term
          },
          success: function( data ) {
            var addressResults = data.results
            var addressArr = [];
            $.each(addressResults, function (index, address) {
                addressArr.push(address.DPA.ADDRESS); //push values here
            });

            response( addressArr );
          }
        } );
      },
      minLength: 2,
      select: function( event, ui ) {
      populateAddress( ui.item.value);
      }
    } );
  } );