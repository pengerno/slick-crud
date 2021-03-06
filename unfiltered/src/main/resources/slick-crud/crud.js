var no = no || {};
no.penger = no.penger || {};
no.penger.crud = no.penger.crud || {};

/*
    This code is... terrible, i know.

    The only reasonable thing to do would be to build a better API, and
     let the frontend handle json only, and kill all of this.

    I don't have time for that now. Hacking a few lines here reminded
     me well why i hate jquery so much
*/

no.penger.crud.makeEditable = function(editable, onChange){
    var $editable = $(editable);
    var current;

    $editable.focus(function(){
        current = this.value;
    });

    $editable.blur(function(){
        if(current != this.value && onChange){
            onChange.call(this, this.value, current);
        }
    });

    $editable.keydown(function(event){
        if(event.which == 13){ //ENTER
            event.preventDefault();
            $(this).blur();
        } else if(event.which == 27){ // ESC
            event.preventDefault();
            $(this).text(current);
            $(this).blur();
        }
    })
};

no.penger.crud.single = function(path, root){

    function setup(){
        $(root + ' input[type!=checkbox]').each(function(i, e){
            no.penger.crud.makeEditable(e, function(text, old){
                 var elem   = $(this);
                 var id     = $(elem.closest('table')).attr('db-id');
                 var key    = elem.closest('tr').find('td:first').text();

                 var data  = {};
                 data[key] = text;
                 no.penger.crud.send(elem, no.penger.crud.updateUrl(path, id), data, function(){
                     elem.text(old);
                 });
             });
        });

        $(root + ' ' + 'select').each(function(i, e){
            no.penger.crud.makeEditable(e, function(text, old){
                 var elem   = $(this);
                 var id     = $(elem.closest('table')).attr('db-id');
                 var key    = elem.closest('tr').find('td:first').text();

                 var data  = {};
                 data[key] = text;
                 no.penger.crud.send(elem, no.penger.crud.updateUrl(path, id), data, function(){
                     elem.text(old);
                 });
             });
        });

        $(root + ' input[type=checkbox]').click(function(){
            var self    = this;
            var elem   = $(this);
            var id     = $(elem.closest('table')).attr('db-id');
            var key    = elem.closest('tr').find('td:first').text();
            var checked = this.checked;
            var data    = {};
            data[key]   = checked;
            no.penger.crud.send(elem, no.penger.crud.updateUrl(path, id), data, function(){
                self.checked = !checked;
            });
        });

        $(root + ' .delete').click(function(e){
            e.preventDefault();
            var self    = this;
            var elem   = $(this);
            var id     = $(elem.closest('table')).attr('db-id');
            if(confirm("Are you sure?")) {
                no.penger.crud.delete(path + "/delete/" + id, function () {
                    alert("deleted " + id + " from " + path);
                    window.location = path;
                });
            }
        })
    }

    $(setup)
};

no.penger.crud.neew = function(url, root){
    function setup(){
        $(root + "submit").click(submit);
    }

    function submit(){

        var form = document.createElement("form");
        form.setAttribute("method", "POST");
        form.setAttribute("action", url + "/create");

        $(root).find('tr').each(function(index) {
            var $self = $(this);

            var id     = $self.find("th").text();
            var $input = $self.find("input");

            var value;

            if ($input.attr('type') === 'checkbox'){
                if ($input[0].checked) {
                    value = "true";
                } else {
                    value = "false";
                }
            } else{
                value = $input.val();
            }

            var hiddenField = document.createElement("input");
            hiddenField.setAttribute("type", "hidden");
            hiddenField.setAttribute("name", id);
            hiddenField.setAttribute("value", value);

            form.appendChild(hiddenField);

        });


        document.body.appendChild(form);
        form.submit();

    }
    $(setup)
};

no.penger.crud.view = function(path, root){

    function setup(){
        no.penger.crud.makeEditable($(root + ' input[type!=checkbox]'), function(text, old){
            var elem   = $(this);
            var column = elem.closest('table').find('th:eq('+this.parentElement.cellIndex+')').text();
            var id     = $(elem.closest('tr')).attr('db-id');

            var data     = {};
            data[column] = text;

            no.penger.crud.send(elem, no.penger.crud.updateUrl(path, id), data, function(){
                elem.text(old);
            });
        });

        no.penger.crud.makeEditable($(root + ' select'), function(text, old){
            var elem   = $(this);
            var column = elem.closest('table').find('th:eq('+this.parentElement.cellIndex+')').text();
            var id     = $(elem.closest('tr')).attr('db-id');

            var data     = {};
            data[column] = text;

            no.penger.crud.send(elem, no.penger.crud.updateUrl(path, id), data, function(){
                elem.text(old);
            });
        });

        $(root +' input[type=checkbox]').click(function(){
            var self   = this;
            var elem   = $(this);
            var column = elem.closest('table').find('th:eq('+this.parentElement.cellIndex+')').text();
            var id     = $(elem.closest('tr')).attr('db-id');

            var checked  = this.checked;
            var data     = {};
            data[column] = checked;

            no.penger.crud.send(elem, no.penger.crud.updateUrl(path, id), data, function(){
                self.checked = !checked;
            })
        })
    }

    $(setup)
};

no.penger.crud.updateUrl = function(path, id) {
    return path + "/update/" + id;
};

no.penger.crud.send = function($elem, url, data, undo){
    $elem.addClass("disabled");
    $.ajax({
        url: url,
        type: 'post',
        data: $.param(data),
        success:function(msg){
            $elem.removeClass("disabled");
            $elem.addClass("status_ok");
            console.debug('success' + msg);
            setTimeout(function() {
                $elem.removeClass("status_ok");
            }, 400);
        },
        error:function(error){
            $elem.removeClass("disabled");
            $elem.addClass("status_failed");
            alert(error.responseText);
            undo();
            setTimeout(function() {
                $elem.removeClass("status_failed");
            }, 1000);

        }
    });
};

no.penger.crud.delete = function(url, deleted){
    $.ajax({
        url: url,
        type: 'delete',
        data: "",
        success:function(msg){
            console.debug('success' + msg);
            deleted();
        },
        error:function(error){
            alert(error.responseText);
        }
    });
};