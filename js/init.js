$(function(){
	$(".thumbnails a.thumbnail").attr('rel', 'gallery').fancybox();

	$("#nav-list li.scroll, #scroll_up").click(function(e) {
		e.preventDefault();
		 $('html, body').animate({
				scrollTop: $($(this).children("a").attr("href")).offset().top
		 },1500);
	 });
 });
