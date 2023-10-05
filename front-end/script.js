// npx eslint draw-board.ts
var StoneColor;
(function (StoneColor) {
    StoneColor[StoneColor["Black"] = 0] = "Black";
    StoneColor[StoneColor["White"] = 1] = "White";
})(StoneColor || (StoneColor = {}));
var gameConfig = {
    socketUrl: 'ws://127.0.0.1:1234',
    htmlCanvasName: 'goBoard',
    backgroundPath: 'images/wood_full_original.jpg',
    whiteStonePath: 'images/white_00.png',
    blackStonePath: 'images/black_00.png',
    // whiteHolePath: 'images/white_hole.png',
    // blackHolePath: 'images/black_hole.png',
    boardSize: 19,
    starPoints: [3 + 1, 9 + 1, 15 + 1],
};
function initGameIO(cfg) {
    var canvas = document.getElementById(cfg.htmlCanvasName);
    var visibleCtx;
    var visibleCtxMaybe = canvas.getContext('2d');
    if (visibleCtxMaybe !== null) {
        visibleCtx = visibleCtxMaybe;
    }
    else {
        throw new Error("draw-board initGame: visibleCtxMaybe in null");
    }
    var bufferCanvas = document.createElement('canvas');
    bufferCanvas.width = canvas.width;
    bufferCanvas.height = canvas.height;
    var bufferCanvasCtxMaybe = bufferCanvas.getContext('2d');
    var bufferCanvasCtx;
    if (bufferCanvasCtxMaybe !== null) {
        bufferCanvasCtx = bufferCanvasCtxMaybe;
    }
    else {
        throw new Error("draw-board initGame: bufferCanvasCtxMaybe in null");
    }
    var background = new Image();
    background.src = cfg.backgroundPath;
    var blackStone = new Image();
    blackStone.src = cfg.blackStonePath;
    var whiteStone = new Image();
    whiteStone.src = cfg.whiteStonePath;
    // const blackHole = new Image();
    // blackHole.src = cfg.blackHolePath;
    // const whiteHole = new Image();
    // whiteHole.src = cfg.whiteHolePath;
    // Handle image loading
    background.onload = function () {
        whiteStone.onload = function () { };
        blackStone.onload = function () { };
        // whiteHole.onload = () => { }
        // blackHole.onload = () => { }
    };
    var canvases = {
        canvas: canvas,
        visibleCtx: visibleCtx,
        bufferCanvas: bufferCanvas,
        ctx: bufferCanvasCtx,
    };
    var assets = {
        backgroundImg: background,
        blackStoneImg: blackStone,
        whiteStoneImg: whiteStone,
        // blackHoleImg: blackHole,
        // whiteHoleImg: whiteHole,
    };
    var sizes = {
        gridSize: canvas.width / (cfg.boardSize + 1),
        borderSize: canvas.width / (cfg.boardSize + 1),
        boardSize: cfg.boardSize,
        starPoints: cfg.starPoints,
    };
    var inputBuffer = {
        mouseX: 0,
        mouseY: 0,
        canvasWidth: 0,
        canvasHeight: 0,
    };
    var boardMatrix = [];
    for (var i = 0; i < cfg.boardSize; i++) {
        boardMatrix[i] = [];
        for (var j = 0; j < cfg.boardSize; j++) {
            boardMatrix[i][j] = null;
        }
    }
    var boardState = {
        currentMoveColor: StoneColor.White,
        currentMoveNumber: 0,
        boardMatrix: boardMatrix,
        newStoneCandidate: null,
    };
    var webSocket = new WebSocket(cfg.socketUrl);
    var allGameData = {
        webSocket: webSocket,
        boardState: boardState,
        inputData: getInputDataIO(inputBuffer),
        inputBuffer: inputBuffer,
        gameEntityDatas: [],
        sizes: sizes,
        canvases: canvases,
        assets: assets,
        gameLoopId: null,
    };
    document.addEventListener('mousemove', function (event) {
        // Retrieve the mouse cursor coordinates from the event object
        var canvasRect = allGameData.canvases.canvas.getBoundingClientRect();
        inputBuffer.mouseX = event.clientX - canvasRect.left; // X-coordinate relative to the viewport
        inputBuffer.mouseY = event.clientY - canvasRect.top; // Y-coordinate relative to the viewport
        // Log or use the cursor coordinates
        console.log("Mouse X: ".concat(inputBuffer.mouseX, ", Mouse Y: ").concat(inputBuffer.mouseY));
    });
    // Event handler when the WebSocket connection is opened
    webSocket.addEventListener('open', function (event) {
        console.log('WebSocket connection opened:', event);
        // Send a message to the server (optional)
        webSocket.send('Hello, server!');
    });
    // Event handler when a message is received from the server
    webSocket.addEventListener('message', function (event) {
        var message = event.data;
        console.log('Message from server:', message);
        // Handle the received message as needed
        // You can update the canvas or perform other actions here
    });
    // Event handler when the WebSocket connection is closed
    webSocket.addEventListener('close', function (event) {
        console.log('WebSocket connection closed:', event);
        // You can handle reconnection logic here if needed
    });
    // Event handler for WebSocket errors
    webSocket.addEventListener('error', function (event) {
        console.error('WebSocket error:', event);
    });
    allGameData.boardState.boardMatrix[10 - 1][10 - 1] = StoneColor.Black;
    allGameData.boardState.boardMatrix[1 - 1][1 - 1] = StoneColor.White;
    allGameData.boardState.boardMatrix[19 - 1][19 - 1] = StoneColor.White;
    allGameData.boardState.boardMatrix[1 - 1][19 - 1] = StoneColor.White;
    allGameData.boardState.boardMatrix[19 - 1][1 - 1] = StoneColor.White;
    return allGameData;
}
// Draw grid lines
function drawGridIO(ctx, sizes, canvas) {
    var gridSize = sizes.gridSize, borderSize = sizes.borderSize;
    ctx.strokeStyle = 'black';
    ctx.lineWidth = 1;
    for (var i = 0; i <= sizes.boardSize; i++) {
        var pos = i * gridSize;
        // Vertical lines
        ctx.beginPath();
        ctx.moveTo(pos, borderSize);
        ctx.lineTo(pos, canvas.height - borderSize);
        ctx.stroke();
        // Horizontal lines
        ctx.beginPath();
        ctx.moveTo(borderSize, pos);
        ctx.lineTo(canvas.width - borderSize, pos);
        ctx.stroke();
    }
    // Draw star points (for a 19x19 board)
    ctx.fillStyle = 'black';
    sizes.starPoints.forEach(function (x) {
        sizes.starPoints.forEach(function (y) {
            var centerX = x * gridSize;
            var centerY = y * gridSize;
            ctx.beginPath();
            ctx.arc(centerX, centerY, 3, 0, 2 * Math.PI);
            ctx.fill();
        });
    });
}
function placeStoneIO(ctx, assets, sizes, x, y, color) {
    if (color !== null) {
        var gridSize = sizes.gridSize, borderSize = sizes.borderSize;
        var coordX = x * gridSize - gridSize / 2 + borderSize;
        var coordY = y * gridSize - gridSize / 2 + borderSize;
        var image = color == StoneColor.Black ? assets.blackStoneImg : assets.whiteStoneImg;
        ctx.drawImage(image, coordX, coordY, gridSize, gridSize);
    }
}
function drawBackgroundIO(ctx, backgroundImg, canvas) {
    ctx.drawImage(backgroundImg, 0, 0, canvas.width, canvas.height);
}
function findNearestToMouseCoord(mouseX, mouseY, canvas, sizes) {
    var gridSize = sizes.gridSize, borderSize = sizes.borderSize, boardSize = sizes.boardSize;
    if (mouseX < 0 || mouseY < 0 || mouseX > canvas.width || mouseY > canvas.height) {
        return null;
    }
    else {
        var row = Math.round((mouseX - borderSize) / gridSize);
        var col = Math.round((mouseY - borderSize) / gridSize);
        if (row >= 0 && col >= 0 && row < boardSize && col < boardSize) {
            return [row, col];
        }
        else {
            return null;
        }
    }
}
function drawStoneNearMouseIO(ctx, mouseX, mouseY, canvas, sizes, assets, boardState) {
    if (boardState.newStoneCandidate == null) {
        return;
    }
    else {
        var _a = boardState.newStoneCandidate, row = _a[0], col = _a[1];
        if (boardState.boardMatrix[row][col] == null) {
            var img = boardState.currentMoveColor == StoneColor.White ? assets.whiteStoneImg : assets.blackStoneImg;
            var gridSize = sizes.gridSize, borderSize = sizes.borderSize;
            var x = row * gridSize + borderSize - gridSize / 3;
            var y = col * gridSize + borderSize - gridSize / 3;
            ctx.drawImage(img, x, y, sizes.gridSize * 2 / 3, sizes.gridSize * 2 / 3);
        }
    }
}
function getInputDataIO(inputBuffer) {
    var inputData = {
        mouseX: inputBuffer.mouseX,
        mouseY: inputBuffer.mouseY,
        canvasWidth: inputBuffer.canvasWidth,
        canvasHeight: inputBuffer.canvasHeight,
    };
    return inputData;
}
function processGameLogic(allGameData) {
    var inputData = allGameData.inputData, canvases = allGameData.canvases, sizes = allGameData.sizes;
    allGameData.boardState.newStoneCandidate = findNearestToMouseCoord(inputData.mouseX, inputData.mouseY, canvases.canvas, sizes);
    return allGameData;
}
function renderStateIO(allGameData, canvases, assets, sizes, inputData) {
    drawBackgroundIO(canvases.ctx, assets.backgroundImg, canvases.canvas);
    drawGridIO(canvases.ctx, sizes, canvases.canvas);
    for (var r = 0; r < sizes.boardSize; r++) {
        for (var c = 0; c < sizes.boardSize; c++) {
            var stoneColor = allGameData.boardState.boardMatrix[r][c];
            placeStoneIO(canvases.ctx, assets, sizes, r, c, stoneColor);
        }
    }
    drawStoneNearMouseIO(canvases.ctx, inputData.mouseX, inputData.mouseY, canvases.canvas, sizes, assets, allGameData.boardState);
    canvases.visibleCtx.drawImage(canvases.bufferCanvas, 0, 0);
}
function gameLoopIO(allGameData) {
    allGameData.inputData = getInputDataIO(allGameData.inputBuffer);
    allGameData = processGameLogic(allGameData);
    renderStateIO(allGameData, allGameData.canvases, allGameData.assets, allGameData.sizes, allGameData.inputData);
}
function sendMessageIO(ws, msg) {
    // Wait until the state of the socket is not ready and send the message when it is...
    waitForSocketConnectionIO(ws, function () {
        console.log("message sent!!!");
        ws.send(msg);
    });
}
// Make the function wait until the connection is made...
function waitForSocketConnectionIO(socket, callback) {
    setTimeout(function () {
        if (socket.readyState === 1) {
            console.log("Connection is made");
            if (callback != null) {
                callback();
            }
        }
        else {
            console.log("wait for connection...");
            waitForSocketConnectionIO(socket, callback);
        }
    }, 5); // wait 5 milisecond for the connection...
}
function startGameLoopIO(allGameData) {
    sendMessageIO(allGameData.webSocket, 'Init');
    if (!allGameData.gameLoopId) {
        allGameData.gameLoopId = setInterval(function () {
            gameLoopIO(allGameData);
        }, 1000 / 60); // Set the desired frame rate (e.g., 60 FPS)
    }
}
var allGameData = initGameIO(gameConfig);
startGameLoopIO(allGameData);
