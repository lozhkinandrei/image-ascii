convertToAscii = (e) => {
    let imageUrl = $("#imageUrl").val()
    let color = $("#asciiColored").attr("checked") === "checked"

    $.ajax({
        url: `${window.SERVER_URL}/convert-url`,
        type: 'GET',
        data: {
            imageUrl: imageUrl,
            color: color,
        },
        accepts: { json: "application/json, text/javascript" },
        success: function (res) {
            $(".ascii-art").empty().append(res['res']);
        },
        error: function (res) {
            console.log(res.responseJSON)
        }
    });
}

handleColorSwitch = (e) => {
    let toggle = $("#asciiColored")

    if (toggle.attr("checked")) {
        toggle.removeAttr("checked")
    } else {
        toggle.attr("checked", true)
    }
}
