// Start of the hover-hyperlink.js file
function(el) {
  el.on('plotly_hover', function(d) {
    var url = d.points[0].customdata;
    var ann = {
      text: url,
      x: 0,
      y: 0,
      xref: "paper",
      yref: "paper",
      yshift: -100,
      showarrow: false
    };
    Plotly.relayout(el.id, {annotations: [ann]});
 });
}
