const path = require('path');

module.exports = () => ({
    entry: './index.js',
    output: {
        filename: 'bundle.js',
        path: path.resolve(__dirname, 'dist')
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
                loader: "elm-webpack-loader",
                options: {
                    debug: argv.mode === 'production' ? false : true,
                    warn: true
                }
            }
        ],
        noParse: [/.elm$/]
    },
    devServer: {
        // inline: true,
        contentBase: './dist',
        // stats: 'errors-only'
    }
});
