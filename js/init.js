$(function(){
//	$(".thumbnails a.thumbnail").attr('rel', 'gallery').fancybox();

  var _gaq = _gaq || [];
  var pluginUrl = '//www.google-analytics.com/plugins/ga/inpage_linkid.js';
  _gaq.push(['_require', 'inpage_linkid', pluginUrl]);
  _gaq.push(['_setAccount', 'UA-37346831-1']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();
	/*$("#nav-list li.scroll, #scroll_up").click(function(e) {
		e.preventDefault();
		 $('html, body').animate({
				scrollTop: $($(this).children("a").attr("href")).offset().top
		 },1500);
	 });*/
  //track outbound links
  var a = document.getElementsByTagName('a');
  for(i = 0; i < a.length; i++){
    if (a[i].href.indexOf(location.host) == -1 && a[i].href.match(/^(http|https):\/\//i)){
      a[i].onclick = function(){
        alert(this.href)
        _gaq.push(['_trackEvent', 'outgoing_links', this.href.replace(/^(http|https):\/\//i, '')]);
      }
    }
  }
 });
