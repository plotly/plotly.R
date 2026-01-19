// Unit test for VIS-992
// Check that resize() works even when element is not attached to the DOM.
// We create a mock of the Plotly object that has the relayout function that throws an
// error if an ID is supplied for the element and it is not attached to the DOM.

const widgetDefinition = require('./widgetdefinition.js')

global.Plotly = {
  relayout: el => {
    if (typeof el === 'string') {
      elementInDocument = document.getElementById(el);
      if (elementInDocument === null) {
          throw new Error('No DOM element with id \'' + el + '\' exists on the page.');
      }
    }
  }
}

test('test that', () => {
  const div = document.createElement('div')
  div.id = 'ID'
  expect(document.getElementById(div.id)).toBe(null)

  const instance = {autosize: true, width: 0, height: 0}
  widgetDefinition.resize(div, 0, 0, instance) // this used to error before VIS-992
})
