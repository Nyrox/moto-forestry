

[%%raw {|
if (module.hot) {
  module.hot.accept()
}
|}]



[%%raw {|
    
const { app, BrowserWindow } = require('electron')

const { default: installExtension, REACT_DEVELOPER_TOOLS} = require("electron-devtools-installer")

app.whenReady().then(() => {
  installExtension(REACT_DEVELOPER_TOOLS)
        .then((name) => console.log(`Added Extension:  ${name}`))
        .catch((err) => console.log('An error occurred: ', err));
})

function createWindow () {
  // Create the browser window.
  let win = new BrowserWindow({
    width: 1500,
    height: 1000,
    webPreferences: {
      nodeIntegration: true,
      webSecurity: false,
    }
  })


  let file = "http://localhost:" + process.env.ELECTRON_WEBPACK_WDS_PORT
  win.loadURL(file)

}

app.whenReady().then(createWindow)
|}]




let mainEntry () =
    Js.log "Main"