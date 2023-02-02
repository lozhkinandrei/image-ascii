function resizeAscii(width) {
    currentWidth = $(".ascii-art").width()
    currentHeight = $(".ascii-art").height()
    scale = width / currentWidth

    $('.ascii-art').css('transform', `scale(${scale})`)
    $('.ascii-art').css('margin-bottom', currentHeight * scale - currentHeight + 'px')
    $('.ascii-art').css('margin-right', currentWidth * scale - currentWidth + 'px')
}

function convertToAscii(e) {
    e.preventDefault()

    form = document.querySelector('#ascii-form');

    let formData = new FormData(form);
    formData.set("color", $('#color').is(':checked'))

    $.ajax({
        url: `${window.SERVER_URL}/convert`,
        type: 'POST',
        data: formData,
        accepts: { json: "application/json, text/javascript" },
        beforeSend: function () {
            $(form).find(".ascii-form-error").empty()
            $(form).find(".ascii-form-error").addClass('d-none')

            $(form).find("button").attr('disabled', true)
            $(form).find("button .spinner-border").removeClass('d-none')
            $(form).find("button .btn-text").text('Loading ...')
        },
        success: function (res) {
            $('.ascii-art-dummy').remove()
            const w = $('#description').width()
            $(".ascii-art").empty().append(res['res'])
            resizeAscii(w)
        },
        error: function (res) {
            errorMessage = res.status == 400 ? res.responseJSON.err : res.responseText || "Error occured"
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
}
