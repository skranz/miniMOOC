$(document).on('click','.nextBtn',function(e){
  var id = e.currentTarget.id;
  var curtab  = parseInt(id.split('-')[1]);
  var nexttab = curtab+1;

  var tabsetid = $("#sectionTabset").data("tabsetid");

  $("#sectionTabset li:nth-child("+curtab+")").removeClass('active');
  $("#sectionTabset li:nth-child("+nexttab+")").addClass('active');

  $("#tab-"+tabsetid+"-"+curtab).removeClass('active');
  $("#tab-"+tabsetid+"-"+nexttab).addClass('active');

  $(window).scrollTop(0);

  //alert('You clicked on the nextBtn ' + curtab);
});
