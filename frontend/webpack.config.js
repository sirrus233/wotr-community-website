const prod = process.env.NODE_ENV === "production";

const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

const path = require("path");

module.exports = {
    mode: prod ? "production" : "development",
    entry: "./src/index.tsx",
    output: {
        path: path.resolve(__dirname + "/dist"),
        filename: "bundle.js",
    },
    target: "web",
    resolve: {
        extensions: [".ts", ".tsx", ".js", ".json"],
    },
    module: {
        rules: [
            {
                test: /\.(ts|tsx)$/,
                exclude: /node_modules/,
                resolve: {
                    extensions: [".ts", ".tsx", ".json"],
                },
                use: "ts-loader",
            },
            {
                test: /\.css$/,
                use: [MiniCssExtractPlugin.loader, "css-loader"],
            },
        ],
    },
    devServer: {
        static: path.resolve(__dirname, "dist"),
        open: true,
    },
    plugins: [
        new HtmlWebpackPlugin({
            template: "./public/index.html",
            favicon: "./public/favicon.ico",
        }),
        new MiniCssExtractPlugin(),
    ],
};
