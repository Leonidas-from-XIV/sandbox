// ==UserScript==
// @name          DokuWiki Headerbuttons
// @namespace     http://marek.homelinux.net/
// @description	  Moves the DokuWiki edit buttons into the headers
// @include       http://www.whgdev.de/*
// @exclude       http://www.whgdev.de/*do=admin*
// @exclude       http://www.whgdev.de/*do=edit*
// ==/UserScript==

function modify() {
    // get all forms on the page
    var forms = document.getElementsByTagName('form');
    // iterate from last to first
    for (var i = 0; i < forms.length; i++) {
        // give the form a name
        var form = forms[i];
        // do not edit the edit-form
        if (form.name != 'editform') {
            // it isn't the edit-form, so modify
            if (i == 0) {
                // it is the first form: delete it
                parent = form.parentNode;
                parent.removeChild(form);
            } else {
                // just give it an ID
                form.id = 'form' + (i - 1).toString();
            }
        }
    }
    
    // again: get the forms
    var forms = document.getElementsByTagName('form');
    
    // iterate through the forms
    for (var i = 0; i < forms.length; i++) {
        // give the form a name
        var form = forms[i];
        
        // get the previous div-element
        var previous_div = form.parentNode.previousSibling.previousSibling;
        // now get the previous heading
        var previous_heading = previous_div.previousSibling.previousSibling;
        //alert(previous_heading.nodeValue);
        
        // get all input elements of the form
        var inputs = form.getElementsByTagName('input');
        // get the submit button
        var submit = inputs[2];
        
        // set the style: small text, invisible border
        submit.style.fontSize = 'x-small';
        submit.style.color = '#01B5EE';
        submit.style.padding = '0px';
        submit.style.backgroundColor = '#EEEEEE';
        submit.style.border = '0px solid #000000';
        
        // move the form node into the heading node
        previous_heading.appendChild(form);
    }
}

// start the script
modify()