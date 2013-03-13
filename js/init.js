$(function(){
	$(".thumbnails a.thumbnail").attr('rel', 'gallery').fancybox();

	/*$("#nav-list li.scroll, #scroll_up").click(function(e) {
		e.preventDefault();
		 $('html, body').animate({
				scrollTop: $($(this).children("a").attr("href")).offset().top
		 },1500);
	 });*/
  //track outbound links
  var a = document.getElementsByTagName('a');
  for(i = 0; i < a.length; i++){
    if (a[i].href.indexOf(location.host) == -1 && a[i].href.match(/^http:\/\//i)){
      a[i].onclick = function(){
        _gaq.push(['_trackEvent', 'outgoing_links', this.href.replace(/^http:\/\//i, '')]);
      }
    }
  }
 });
