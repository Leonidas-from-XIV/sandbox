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
    // iterate from first to last
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
            }
        }
    }
    
    // again: get the forms
    var forms = document.getElementsByTagName('form');
    
    // iterate through the forms
    for (var i = 0; i < forms.length; i++) {
        // give the form a name
        var form = forms[i];
        
        // look for the previous heading
        var heading_found = false;
        var last_element = form.parentNode;
        // begin saerching
        while (heading_found == false) {
            // get the previous DOM node
            last_element = last_element.previousSibling;
            // set a shortcut variable
            var tag_name = last_element.tagName
            // is the element a heading?
            if (tag_name == 'H1' || tag_name == 'H2' ||
                tag_name == 'H3' || tag_name == 'H4' || 
                tag_name == 'H5' || tag_name == 'H6') {
                // yes, it *is* a heading.. found it. quit the loop
                heading_found = true;
            }
        }
        // set the previous heading to the last_element (hope it was found)
        var previous_heading = last_element;
        
        // get all input elements of the form
        var inputs = form.getElementsByTagName('input');
        
        // find the submit button element
        var submit = false;
        for (var j = 0; j < inputs.length; j++) {
            if (inputs[j].type == 'submit') {
                // found the submit button, set the variable
                submit = inputs[j];
            }
        }
        
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
