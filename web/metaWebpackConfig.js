const path = require('path');
const SWPrecacheWebpackPlugin = require('sw-precache-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = (config) => {
    return (env, argv) => ({
        entry: path.resolve(config.basePath, 'index.js'),
        output: {
            filename: 'bundle.js',
            path: config.outPath,
            // publicPath : '/webApp/'
        },
        module: {
            rules: [{
                    test: /\.html$|\.css$/,
                    exclude: /node_modules/,
                    loader: 'file-loader?name=[name].[ext]'
                },
                {
                    test: /\.elm$/,
                    exclude: [/elm-stuff/, /node_modules/],
                    loader: "elm-webpack-loader",
                    options: {
                        debug: false, // argv.mode !== 'production',
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
            new CopyWebpackPlugin([{ from: path.resolve(config.basePath, 'staticFiles/') }], {}),
            new SWPrecacheWebpackPlugin({
                cacheId: 'noKey',
                dontCacheBustUrlsMatching: /\.\w{8}\./,
                filename: 'service-worker.js',
                minify: argv.mode === 'production',
                // For client site routing, fall back to the root if URL not in cache:
                // navigateFallback: '/main.html',
                // Here we could restrict the fallback to only matching URLs.
                // We should probably do this, as without that, every wrong URL will just load main.html!
                // navigateFallbackWhitelist: [/^\/articles/],
                staticFileGlobsIgnorePatterns: [/\.map$/, /asset-manifest\.json$/],
                runtimeCaching: [{
                    urlPattern: /^https:\/\/fonts.googleapis.com\/.*/,
                    handler: 'cacheFirst'
                }]
            })
        ]
    });
};
