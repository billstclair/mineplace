var httpPort = (process.env.PORT || 8081),
    verbose = (process.env.VERBOSE || null),
    noListen = (process.env.NOLISTEN || null),
    startRepl = (process.env.REPL || null),
    http = require('http'),
    ecstatic = require('ecstatic'),
    repl = require('repl')
    server = http.createServer(
      ecstatic({ root: __dirname })
    ),
    WebSocketServer = require('./WebSocketServer.js'),
    app = require('./server.js').Elm.Mineplace.Server.Server.init({ flags: verbose }),
    wss = new WebSocketServer(
      server,
      app.ports.inputPort,
      app.ports.outputPort,
      verbose
    );

function listen(port = httpPort) {
  server.listen(httpPort);
  console.log(`Listening on :${httpPort}`);
}

if (!noListen || !startRepl) {
  listen();
}

if (startRepl) {
  global.httpPort = httpPort;
  global.verbose = verbose;
  global.server = server;

  global.exit = function() {
    process.exit();
  }
  global.quit = global.exit;
  global.listen = listen;

  repl.start();
}
