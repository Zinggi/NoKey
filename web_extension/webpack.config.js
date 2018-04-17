const path = require('path');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const CleanWebpackPlugin = require('clean-webpack-plugin');

module.exports = (env, argv) => ({
    entry: {
        content: './content_scripts/pageUtils.js',
        fillForm: './content_scripts/fillForm.js',
        newPassword: './content_scripts/newPassword.js',
        background: './background.js',
        popup: './popup/setup.js'
    },
    output: {
        filename: '[name]/bundle.js',
        path: path.resolve(__dirname, 'addon')
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
                    debug: false,
                    warn: false,
                    files: [
                        path.resolve(__dirname, "elm/MainBackground.elm"),
                        path.resolve(__dirname, "elm/FillLogin.elm"),
                        path.resolve(__dirname, "elm/GeneratePassword.elm"),
                        path.resolve(__dirname, "elm/Popup.elm")
                    ]

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
    plugins: [
        new CopyWebpackPlugin([
            { from: './manifest.json' },
            { from: './icons', to: 'icons/' },
            { from: './popup/main.html', to: 'popup/' },
            { from: './content_scripts/*.html' },
            { from: './node_modules/webextension-polyfill/dist/browser-polyfill.min.js', to: 'dist/webextensionPolyfill.js' },
            { from: './../web/styles.css', to: 'dist/styles.css' }
        ], {}),
        new CleanWebpackPlugin(['addon'])
    ],
    optimization: {
        splitChunks: {
            cacheGroups: {
                commons: {
                    name: 'commons',
                    chunks: 'all',
                    minChunks: 2,
                    enforce: true
                }
            }
        }
    }
});
