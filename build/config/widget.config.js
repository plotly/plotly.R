const cliArgs = require('yargs').argv
const _ = require('lodash')

const config = {
  widgetEntryPoint: 'theSrc/scripts/plotly.js',
  widgetFactory: 'theSrc/scripts/plotly.factory.js',
  widgetName: 'plotly',
}

const commandLineOverides = _.omit(cliArgs, ['_', '$0'])
module.exports = _.merge(config, commandLineOverides)