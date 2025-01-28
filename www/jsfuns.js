function toggleChildren(tabid) {
    const children = document.querySelectorAll(`.${tabid}-children`);
    if (children.length > 0) {
        children.forEach(child => {
            if (child.style.display === "none" || child.style.display === "") {
                child.style.display = "block";
            } else {
                child.style.display = "none";
            }
        });
    }
}

function opentab(tabsetid, tabid) {
    const tabs = document.querySelectorAll("." + tabsetid);
    const contents = document.querySelectorAll("." + tabsetid + "-content");
    
    // Set active tab
    tabs.forEach(tab => {
        tab.classList.remove("active");
        if (tab.id === tabid) tab.classList.add("active");
    });

    // Hide all content and show the content of the active tab
    contents.forEach(c => {
        c.style.display = "none";
        if (c.id === tabid + "-content") {
            c.style.display = "block";
            $(c).trigger("shown");
        }
    });
    
}

//*register this input
var selectedaccountInput = new Shiny.InputBinding();

$.extend(selectedaccountInput, {
  // Find elements with the "selectedaccountInput" class
  find: function(scope) {
    return $(scope).find(".selectedaccountInput");
  },
  
  // Get the value of the input element
  getValue: function(el) {
    return $(el).val();  // Use .val() instead of .prop("value") for input elements
  },
  
  // Set the value of the input element
  setValue: function(el, value) {
    $(el).val(value).trigger("change");  // Use .val() and trigger "change" for reactivity
  },
  
  // Handle messages sent from the server
  receiveMessage: function(el, value) {
    this.setValue(el, value);
  },
  
  // Subscribe to changes on the input element
  subscribe: function(el, callback) {
    $(el).on("change.selectedaccountInput", function() {
      callback();
    });
  },
  
  // Unsubscribe from changes
  unsubscribe: function(el) {
    $(el).off(".selectedaccountInput");
  }
});

// Register the custom input binding
Shiny.inputBindings.register(selectedaccountInput, "mutindabindings");


function notifyServerselected_tab(tabid) {
  // Use Shiny's setInputValue to update the custom input binding
  Shiny.setInputValue("selected_tab", tabid);
}


