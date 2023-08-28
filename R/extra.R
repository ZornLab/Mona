#15 to 375
#100 and 65
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, c = 100, l = 65)[1:n]
}

plot_inputs <- "
    function(el, x){
      var id = el.getAttribute('id');
      var gd = document.getElementById(id);
      var d3 = Plotly.d3;
      Shiny.setInputValue('plot_rendered',id,{priority: 'event'})
      Plotly.update(id).then(attach);
      function attach() {
        gd.addEventListener('click', function(evt) {
          Shiny.setInputValue('plot_clicked',id,{priority: 'event'})
        });
      };
    }"

plot_render <- "
    function(el, x){
      var id = el.getAttribute('id');
      Shiny.setInputValue('plot_rendered',id,{priority: 'event'})
    }"

theme <- create_theme(
  bs4dash_layout(
    sidebar_width = "10%",
    control_sidebar_width = "15%"
  ),
  bs4dash_color(
    lightblue = "#b9c5fd",
    blue = "#96a8fc",
    teal="#fcfcff"
  ),
  bs4dash_sidebar_light(
    bg = "#fcfcff"
  )
)

# Triggered whenever plots are sorted to check their order
sortable_custom_input <- function() {
  js_text <- "function(evt) {
    if (typeof Shiny !== \"undefined\") {
      var plots = [...this.el.querySelectorAll('.plotly.html-widget')];
      var ids = Array.from(plots, node => node.id);
      Shiny.setInputValue('sort_info',ids,{priority: 'event'})
    }
  }"
  htmlwidgets::JS(js_text)
}