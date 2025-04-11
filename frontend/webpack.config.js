const isDev = process.env.NODE_ENV === "development";

const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

const path = require("path");

module.exports = {
    mode: isDev ? "development" : "production",
    entry: "./src/index.tsx",
    output: {
        path: path.resolve(__dirname + "/dist"),
        filename: "bundle.js",
    },
    target: "web",
    resolve: {
        alias: {
            env: path.resolve(
                __dirname,
                "src",
                "env",
                isDev ? "dev.ts" : "prod.ts"
            ),
        },
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
            {
                test: /\.(png|svg|jpg|jpeg|gif)$/i,
                type: "asset/resource",
                generator: {
                    filename: "assets/[name][hash][ext][query]",
                },
            },
        ],
    },
    devServer: {
        static: path.resolve(__dirname, "dist"),
        historyApiFallback: true,
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
