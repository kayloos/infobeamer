$(function() {
  $("#add_feed_link").click(function() {
    $("#add_feed_form").show();
    $("#add_feed_form input").first().focus();
    hideModal();
    return false;
  });

  $("#add_feed_form").submit(function() {
    $("#add_feed_form").hide();
  });

  $(".feed").each(function(idx, elem) {
    href = $(elem).data("href");
    renderFeed(href, elem);
  });
});

function renderFeed(link, feedDiv) {
  var content = "";
  $.ajax({
    url: "/feed",
    type: "POST",
    data: { feed: link },
    dataType: 'json',
    success: function(obj) {
      $(obj.rss).each(function(idx, elem) {
        var newDesc = "";
        if (elem.desc.indexOf("news.ycombinator.com") == -1)
          newDesc = elem.desc.replace(/<[^>]*>/g, "");
        else 
          newDesc = elem.desc;
        content += renderItem(elem.title, elem.link, newDesc);
      });
      $(feedDiv).prepend(content);
    },
    error: function() {
      alert("Error for '" + link + "'");
    }
  });
}

function renderItem(title, link, description) {
  var str = "<a href='" + link + "' target='_blank'><h1>" + title +"</h1></a><p>" + description + "</p>";
  return str;
}

function hideModal() {
  // $("body").click(function() {
    // $("#add_feed_form").hide();
  // });
}

