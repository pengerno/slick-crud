var FINN     = FINN || {};
FINN.pf      = FINN.pf || {};
FINN.pf.crud = FINN.pf.crud || {};

FINN.pf.crud.makeEditable = function(editable, onChange){
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

FINN.pf.crud.single = function(url, root){

    function setup(){
        FINN.pf.crud.makeEditable($(root + ' [contenteditable=true]'), function(text, old){
            var elem  = $(this);
            var id    = elem.closest('table').find('td:first + td').text();
            var key   = elem.closest('tr').find('td:first').text();
            var data  = {};
            data[key] = text;
            FINN.pf.crud.send(url + "/" + id, data, function(){
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
            FINN.pf.crud.send(url + "/" + id, data, function(){
                self.checked = !checked;
            });
        })
    }

    $(setup)
};

FINN.pf.crud.view = function(path, root){

    function setup(){
        FINN.pf.crud.makeEditable($(root + ' [contenteditable=true]'), function(text, old){
            var elem   = $(this);
            var column = elem.closest('table').find('th:eq('+this.cellIndex+')').text();
            var idTd   = elem.parent().find('td:first');
            var id     = idTd[0] === this ? old : idTd.text();

            var data     = {};
            data[column] = text;

            FINN.pf.crud.send(path + "/" + id, data, function(){
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

            FINN.pf.crud.send(path + "/" + id, data, function(){
                self.checked = !checked;
            })
        })
    }

    $(setup)
};

FINN.pf.crud.send = function(url, data, undo){
    $.ajax({
        url: url,
        type: 'post',
        data: $.param(data),
        success:function(){
            console.debug('success');
        },
        error:function(){
            undo();
        }
    });
};