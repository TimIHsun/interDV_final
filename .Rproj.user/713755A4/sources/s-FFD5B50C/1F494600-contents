document.addEventListener('DOMContentLoaded', function() {
    var el_tabs = document.querySelector(".tabs");
    var instance_tabs = M.Tabs.init(el_tabs);
    var elems_carousel = document.querySelectorAll('.carousel');
    var instances_carousel = M.Carousel.init(elems_carousel,{
      dist: 0,
      numVisible:3,
      shift:100,
      indicators: true
    }); 
    /*, {
      // specify options here
    });*/
    var elems_fltbtn = document.querySelectorAll('.fixed-action-btn');
    var instances_fltbtn = M.FloatingActionButton.init(elems_fltbtn);
    var slideIndex = 1;
    showSlides(slideIndex);

    // Next/previous controls
    function plusSlides(n) {
      showSlides(slideIndex += n);
    }

    // Thumbnail image controls
    function currentSlide(n) {
     showSlides(slideIndex = n);
    }

    function showSlides(n) {
      var i;
      var slides = document.getElementsByClassName("mySlides");
      var dots = document.getElementsByClassName("dot");
      if (n > slides.length) {slideIndex = 1}
      if (n < 1) {slideIndex = slides.length}
      for (i = 0; i < slides.length; i++) {
          slides[i].style.display = "none";
      }
      for (i = 0; i < dots.length; i++) {
        dots[i].className = dots[i].className.replace(" active", "");
      }
      slides[slideIndex-1].style.display = "block";
      dots[slideIndex-1].className += " active";
    }
});

