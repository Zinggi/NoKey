const path = require('path');
const CopyWebpackPlugin = require('copy-webpack-plugin');

const elmSource = path.resolve(__dirname, '../../web/');
const out = path.resolve(__dirname, '../priv/static/');
module.exports = {
    entry: '../../web/index.js',
    output: {
        filename: 'bundle.js',
        path: out
    },
    module: {
        rules: [{
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'file-loader?name=[name].[ext]'
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                // This is what you need in your own work
                loader: "elm-webpack-loader",
                options: {
                    debug: true,
                    warn: true,
                    cwd: elmSource
                }
            }
        ],
        noParse: [/.elm$/]
    },
    devServer: {
        // inline: true,
        contentBase: out,
        // stats: 'errors-only'
    },
    plugins: [new CopyWebpackPlugin([{ from: 'static' }], {})]
};
