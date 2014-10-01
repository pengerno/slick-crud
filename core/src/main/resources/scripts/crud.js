var no = no || {};
no.penger = no.penger || {};
no.penger.crud = no.penger.crud || {};

no.penger.crud.makeEditable = function(editable, onChange){
    var current;

    editable.focus(function(){
        current = $(this).text();
    });

    editable.blur(function(){
        var self = $(this);
        if(current != self.text()){
            onChange.call(this, self.text(), current);
        }
    });

    editable.keydown(function(event){
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

no.penger.crud.single = function(url, root){

    function setup(){
        no.penger.crud.makeEditable($(root + ' [contenteditable=true]'), function(text, old){
            var elem  = $(this);
            var id    = elem.closest('table').find('td:first + td').text();
            var key   = elem.closest('tr').find('td:first').text();
            var data  = {};
            data[key] = text;
            no.penger.crud.send(url + "/" + id, data, function(){
                elem.text(old);
            });
        });

        $(root + ' input[type=checkbox]').click(function(){
            var self    = this;
            var elem    = $(this);
            var id      = elem.closest('table').find('td:first + td').text();
            var key     = elem.closest('tr').find('td:first').text();
            var checked = this.checked;
            var data    = {};
            data[key]   = checked;
            no.penger.crud.send(url + "/" + id, data, function(){
                self.checked = !checked;
            });
        })
    }

    $(setup)
};

no.penger.crud.view = function(path, root){

    function setup(){
        no.penger.crud.makeEditable($(root + ' [contenteditable=true]'), function(text, old){
            var elem   = $(this);
            var column = elem.closest('table').find('th:eq('+this.cellIndex+')').text();
            var idTd   = elem.parent().find('td:first');
            var id     = idTd[0] === this ? old : idTd.text();

            var data     = {};
            data[column] = text;

            no.penger.crud.send(path + "/" + id, data, function(){
                elem.text(old);
            });
        });

        $(root +' input[type=checkbox]').click(function(){
            var self   = this;
            var elem   = $(this);
            var column = elem.closest('table').find('th:eq('+this.parentElement.cellIndex+')').text();
            var id     = elem.closest('tr').find('td:first').text();

            var checked  = this.checked;
            var data     = {};
            data[column] = checked;

            no.penger.crud.send(path + "/" + id, data, function(){
                self.checked = !checked;
            })
        })
    }

    $(setup)
};

no.penger.crud.send = function(url, data, undo){
    $.ajax({
        url: url,
        type: 'post',
        data: $.param(data),
        success:function(msg){
            console.debug('success' + msg);
        },
        error:function(error){
            alert(error.responseText);
            undo();
        }
    });
};