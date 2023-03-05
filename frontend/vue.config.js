module.exports = {
  css: {
    loaderOptions: {
      sass: {
        additionalData: `@import "@/styles/_variables.scss";`
      }
    }
  },
  devServer: {
    proxy: {
      '^/api': {
        target: 'http://127.0.0.1:3002',
        ws: true,
        changeOrigin: true
      },
    }
  }
};
