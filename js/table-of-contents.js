$(document).ready(() => {
  var $toc = $('.toc');
  if ($toc.length > 0) {
    var rootElem = $(document.documentElement);
    var positions = new Array();
    $('.toc>ul>li>a').each(function() {
      positions.unshift(decodeURI(this.getAttribute('href')));
    });
    var setActive = href => {
      $toc.find('a').removeClass('active');
      var x = $toc.find('a[href="' + href + '"]');
      x.addClass('active');
      var l = $toc[0].offsetHeight;
      var s = $toc[0].scrollTop;
      var maxs = x[0].offsetTop - 0.2 * l;
      var mins = x[0].offsetTop - l + x[0].offsetHeight + 0.2 * l;
      if (s > maxs)
        $toc[0].scrollTo({top: maxs, behavior: 'smooth'});
      else if (s < mins)
        $toc[0].scrollTo({top: mins, behavior: 'smooth'});
    }
    $(window).scroll(() => {
      $(positions).each((_, id) => {
        if (window.scrollY + 1 >= $(id)[0].offsetTop) {
          setActive(encodeURI(id));
          return false;
        }
      });
    });

    if (window.getComputedStyle($toc[0], '::-webkit-scrollbar')['width'] == "") {
      // not webkit
    } else {
      // webkit
      $toc.addClass('webkit');
    }
  }
});
