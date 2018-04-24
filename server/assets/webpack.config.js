const metaWebpackConfig = require('../../web/metaWebpackConfig.js');
const path = require('path');

module.exports = metaWebpackConfig({
    basePath: '../../web/',
    outPath: path.resolve(__dirname, '../priv/static/'),
    additionalCopy: [{ from: 'lib/', to: 'lib/' }]
});
