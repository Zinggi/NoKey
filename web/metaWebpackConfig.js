const path = require('path');
const SWPrecacheWebpackPlugin = require('sw-precache-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');

// TODO: Test production!!!
// const PUBLIC_PATH =
//     argv === 'production' ?
//         'https://virt35.ethz.ch/webApp/' :
//         'http://localhost:3001/'

module.exports = (config) => {
    return (env, argv) => ({
        entry: path.resolve(config.basePath, 'index.js'),
        output: {
            filename: 'bundle.js',
            path: config.outPath,
            // publicPath : PUBLIC_PATH
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
                        debug: argv.mode !== 'production',
                        cwd: config.basePath
                    }
                },
                {
                    test: /\.js$/,
                    exclude: [/node_modules/],
                    use: {
                        loader: "babel-loader"
                    }
                }
            ],
            noParse: [/.elm$/]
        },
        devServer: {
            // inline: true,
            contentBase: config.outPath,
            // stats: 'errors-only'
        },
        plugins: [
            new SWPrecacheWebpackPlugin({
                cacheId: 'noKey',
                dontCacheBustUrlsMatching: /\.\w{8}\./,
                filename: 'service-worker.js',
                minify: argv.mode === 'production',
                // navigateFallback: PUBLIC_PATH + 'index.html',
                navigateFallback: '/main.html',
                staticFileGlobsIgnorePatterns: [/\.map$/, /asset-manifest\.json$/],
            }),
            new CopyWebpackPlugin([{ from: path.resolve(config.basePath, 'staticFiles/') }], {})
        ]
    });
};
