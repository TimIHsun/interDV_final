document.addEventListener('DOMContentLoaded', function() {
    var el_tabs = document.querySelector(".tabs");
    var instance_tabs = M.Tabs.init(el_tabs);
    var elems = document.querySelectorAll('.modal');
    var instances = M.Modal.init(
      elems, {onOpenStart: null});
});