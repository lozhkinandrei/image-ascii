function convertToAscii(e) {
    e.preventDefault()

    form = document.querySelector('#ascii-form');
    let formData = new FormData(form);

    formData.append("width", 150)
    formData.append("color", true)

    $(form).find(".ascii-form-error").empty()
    $(form).find(".ascii-form-error").addClass('d-none')

    $(form).find("button").attr('disabled', true)
    $(form).find("button .spinner-border").removeClass('d-none')
    $(form).find("button .btn-text").text('Loading ...')
    $('.ascii-art').empty()


    $.ajax({
        url: `${window.SERVER_URL}/convert`,
        type: 'POST',
        data: formData,
        accepts: { json: "application/json, text/javascript" },
        success: function (res) {
            $(".ascii-art").append(res['res'])

            currentWidth = $(".ascii-art").width()
            currentHeight = $(".ascii-art").height()
            scale = $('body .container .container').width() / currentWidth

            $('.ascii-art').css('transform', `scale(${scale})`)
            $('.ascii-art').css('margin-bottom', currentHeight * scale - currentHeight + 'px')
            $('.ascii-art').css('margin-right', currentWidth * scale - currentWidth + 'px')
        },
        error: function (res) {
            errorMessage = res.status == 400 ? res.responseJSON.err : res.responseText
            $(form).find(".ascii-form-error").removeClass('d-none')
            $(form).find(".ascii-form-error").text(errorMessage)

        },
        complete: function () {
            $(form).find("button").attr('disabled', false)
            $(form).find("button .spinner-border").addClass('d-none')
            $(form).find("button .btn-text").text('Submit')
        },
        contentType: false,
        processData: false
    });

    form.reset();
}


handleColorSwitch = (e) => {
    let toggle = $("#asciiColored")

    if (toggle.attr("checked")) {
        toggle.removeAttr("checked")
    } else {
        toggle.attr("checked", true)
    }
}
