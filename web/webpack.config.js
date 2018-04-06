const metaWebpackConfig = require('./metaWebpackConfig.js');
const path = require('path');

module.exports = metaWebpackConfig({
    basePath: './',
    outPath: path.resolve(__dirname, 'dist')
});
