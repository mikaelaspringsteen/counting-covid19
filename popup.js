// Start of the hover-hyperlink.js file
function(el) {
  el.on('plotly_hover', function(d) {
    var ann = {
      text: "text",
      x: 0,
      y: 0,
      xref: "paper",
      yref: "paper",
      yshift: -40,
      showarrow: false
    };
    Plotly.relayout(el.id, {annotations: [ann]});
 });
}
