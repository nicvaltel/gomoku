"use strict";
// npx eslint draw-board.ts
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        if (typeof b !== "function" && b !== null)
            throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
// MODULE script
var StoneColor;
(function (StoneColor) {
    StoneColor[StoneColor["Black"] = 0] = "Black";
    StoneColor[StoneColor["White"] = 1] = "White";
})(StoneColor || (StoneColor = {}));
var MessageProcessorState;
(function (MessageProcessorState) {
    MessageProcessorState[MessageProcessorState["MPSHandshake"] = 0] = "MPSHandshake";
    MessageProcessorState[MessageProcessorState["MPSNormal"] = 1] = "MPSNormal";
})(MessageProcessorState || (MessageProcessorState = {}));
var gameConfig = {
    socketUrl: 'ws://127.0.0.1:1234',
    htmlCanvasName: 'goBoard',
    backgroundPath: 'images/wood_full_original.jpg',
    whiteStonePath: 'images/white_00.png',
    blackStonePath: 'images/black_00.png',
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
    // Handle image loading
    background.onload = function () {
        whiteStone.onload = function () { };
        blackStone.onload = function () { };
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
        messageProcessorState: MessageProcessorState.MPSHandshake,
    };
    document.addEventListener('mousemove', function (event) {
        // Retrieve the mouse cursor coordinates from the event object
        var canvasRect = allGameData.canvases.canvas.getBoundingClientRect();
        inputBuffer.mouseX = event.clientX - canvasRect.left; // X-coordinate relative to the viewport
        inputBuffer.mouseY = event.clientY - canvasRect.top; // Y-coordinate relative to the viewport
        // Log or use the cursor coordinates
        // console.log(`Mouse X: ${inputBuffer.mouseX}, Mouse Y: ${inputBuffer.mouseY}`);
    });
    // Event handler when the WebSocket connection is opened
    webSocket.addEventListener('open', function (event) {
        console.log('WebSocket connection opened:', event);
        // Send a message to the server (optional)
        // webSocket.send('Hello, server!');
    });
    // Event handler when a message is received from the server
    webSocket.addEventListener('message', function (event) {
        var message = event.data;
        console.log('Message from server:', message);
        console.log(decodeWebSocketOutputMessage(message));
        allGameData.messageProcessorState = messageProcessor(allGameData, message);
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
// MODULE render
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
// MODULE cookie
var keyUserId = 'keyUserId';
var keyConnId = 'keyConnId';
var keyTempAnonPasswd = 'keyTempAnonPasswd';
var keyUserRegOrAnon = 'keyUserRegOrAnon';
// function setCookie(key: string, value: string, expirationDays: number = 365) {
//     const date = new Date();
//     date.setTime(date.getTime() + expirationDays * 24 * 60 * 60 * 1000); // Calculate expiration time
//     const expires = `expires=${date.toUTCString()}`;
//     document.cookie = `${key}=${value}; ${expires}; path=/`;
// }
function setCookie(key, value, expirationDays) {
    if (expirationDays === void 0) { expirationDays = 365; }
    var date = new Date();
    date.setTime(date.getTime() + expirationDays * 24 * 60 * 60 * 1000); // Calculate expiration time
    var expires = "expires=".concat(date.toUTCString());
    // Set the "SameSite" attribute to "None" for cross-site requests
    var sameSite = 'SameSite=None';
    // Set the "Secure" attribute to ensure it's sent over secure connections
    var secure = 'Secure';
    // Combine all attributes
    var cookieAttributes = [expires, sameSite, secure, 'path=/'];
    document.cookie = "".concat(key, "=").concat(value, "; ").concat(cookieAttributes.join('; '));
}
function getCookie(key) {
    var cookies = document.cookie.split(';');
    for (var _i = 0, cookies_1 = cookies; _i < cookies_1.length; _i++) {
        var cookie = cookies_1[_i];
        var _a = cookie.split('='), cookieKey = _a[0], cookieValue = _a[1];
        var trimmedKey = cookieKey.trim();
        if (trimmedKey === key) {
            return decodeURIComponent(cookieValue);
        }
    }
    return null;
}
var Handshake = /** @class */ (function () {
    function Handshake(connId, userId, password) {
        this.connId = connId;
        this.userId = userId;
        this.password = password;
    }
    return Handshake;
}());
var ExistingAnonConn = /** @class */ (function (_super) {
    __extends(ExistingAnonConn, _super);
    function ExistingAnonConn(connId, userId, password) {
        return _super.call(this, connId, userId, password) || this;
    }
    return ExistingAnonConn;
}(Handshake));
var ExistingRegisteredUserAndConn = /** @class */ (function (_super) {
    __extends(ExistingRegisteredUserAndConn, _super);
    function ExistingRegisteredUserAndConn(connId, userId, password) {
        return _super.call(this, connId, userId, password) || this;
    }
    return ExistingRegisteredUserAndConn;
}(Handshake));
var ExistingRegisteredUserNewConn = /** @class */ (function (_super) {
    __extends(ExistingRegisteredUserNewConn, _super);
    function ExistingRegisteredUserNewConn(userId, password) {
        return _super.call(this, '', userId, password) || this;
    }
    return ExistingRegisteredUserNewConn;
}(Handshake));
var NonExisting = /** @class */ (function (_super) {
    __extends(NonExisting, _super);
    function NonExisting() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    return NonExisting;
}(Handshake));
// Add other WebSocketInputMessage subtypes here
function encodeHandshake(payload) {
    var result = 'Handshake';
    if (payload instanceof ExistingAnonConn) {
        result += ';ExistingAnonConn';
        result += ";".concat(payload.connId);
        result += ";".concat(payload.userId);
        result += ";".concat(payload.password);
    }
    else if (payload instanceof ExistingRegisteredUserAndConn) {
        result += ';ExistingRegisteredUserAndConn';
        result += ";".concat(payload.connId);
        result += ";".concat(payload.userId);
        result += ";".concat(payload.password);
    }
    else if (payload instanceof ExistingRegisteredUserNewConn) {
        result += ';ExistingRegisteredUserNewConn';
        result += ";".concat(payload.userId);
        result += ";".concat(payload.password);
    }
    else if (payload instanceof NonExisting) {
        result += ';NonExisting';
    }
    else {
        // Handle unknown types here, or throw an error
        result = '';
    }
    return result;
}
function encodeWebSocketInputMessage(message) {
    var result = '';
    if (message.type === 'HandshakeInMsg') {
        result = encodeHandshake(message.payload);
    }
    else {
        result = '';
    }
    return result;
}
function decodeLoginLogoutOutMsg(input) {
    var parts = input.split(';');
    var msgType = parts[0];
    if (msgType === 'AskForExistingUser') {
        return 'AskForExistingUser';
    }
    else if (msgType === 'RegisterError') {
        return 'RegisterError';
    }
    else if (msgType === 'RegisteredSuccessfully' && parts.length === 3) {
        var userId = parts[1];
        var connId = parts[2];
        return ['RegisteredSuccessfully', userId, connId];
    }
    else if (msgType === 'LoginError') {
        return 'LoginError';
    }
    else if (msgType === 'LoginSuccessfully' && parts.length === 3) {
        var userId = parts[1];
        var connId = parts[2];
        return ['LoginSuccessfully', userId, connId];
    }
    else if (msgType === 'LogoutSuccessfully' && parts.length === 3) {
        var userId = parts[1];
        var connId = parts[2];
        return ['LogoutSuccessfully', userId, connId];
    }
    else if (msgType === 'NewAnonUser' && parts.length === 4) {
        var userId = parts[1];
        var connId = parts[2];
        var passwd = parts[3];
        return ['NewAnonUser', userId, connId, passwd];
    }
    else if (msgType === 'OldAnonUser' && parts.length === 3) {
        var userId = parts[1];
        var connId = parts[2];
        return ['OldAnonUser', userId, connId];
    }
    return null; // Invalid input or unrecognized message type
}
function decodeWebSocketOutputMessage(input) {
    var parts = input.split(';');
    var msgType = parts[0];
    if (msgType === 'LoginLogout') {
        var loginLogoutMsg = decodeLoginLogoutOutMsg(parts.slice(1).join(';'));
        if (loginLogoutMsg) {
            return { type: 'LoginLogoutOutMsg', payload: loginLogoutMsg };
        }
    }
    return null; // Invalid input or unrecognized message type
}
// MODULE handshake
function messageProcessorLoginLogout(allGameData, msg) {
    var newState = allGameData.messageProcessorState;
    if (msg === 'AskForExistingUser') {
        var connId = getCookie(keyConnId);
        var userId = getCookie(keyUserId);
        var password = getCookie(keyTempAnonPasswd);
        var regOrAnon = getCookie(keyUserRegOrAnon);
        if (regOrAnon && regOrAnon === 'RegUser' && connId && userId && password) {
            var messageAns = {
                type: 'HandshakeInMsg',
                payload: new ExistingRegisteredUserAndConn(connId, userId, password),
            };
            sendMessageIO(allGameData.webSocket, encodeWebSocketInputMessage(messageAns));
        }
        else if (regOrAnon && regOrAnon === 'AnonUser' && connId && userId && password) {
            var messageAns = {
                type: 'HandshakeInMsg',
                payload: new ExistingAnonConn(connId, userId, password),
            };
            sendMessageIO(allGameData.webSocket, encodeWebSocketInputMessage(messageAns));
        }
        else if (!connId && regOrAnon && regOrAnon === 'RegUser' && userId && password) {
            var messageAns = {
                type: 'HandshakeInMsg',
                payload: new ExistingRegisteredUserNewConn(userId, password),
            };
            sendMessageIO(allGameData.webSocket, encodeWebSocketInputMessage(messageAns));
        }
        else {
            var messageAns = {
                type: 'HandshakeInMsg',
                payload: new NonExisting,
            };
            sendMessageIO(allGameData.webSocket, encodeWebSocketInputMessage(messageAns));
        }
    }
    else if (msg[0] === 'NewAnonUser') {
        setCookie(keyUserId, msg[1].toString());
        setCookie(keyConnId, msg[2].toString());
        setCookie(keyTempAnonPasswd, msg[3].toString());
        setCookie(keyUserRegOrAnon, 'AnonUser');
        var xxx = getCookie(keyTempAnonPasswd);
        console.log("SALAM");
        console.log(xxx);
    }
    else if (msg[0] === 'OldAnonUser') {
        setCookie(keyUserId, msg[1].toString());
        setCookie(keyConnId, msg[2].toString());
        setCookie(keyUserRegOrAnon, 'AnonUser');
    }
    return newState;
}
function messageProcessor(allGameData, message) {
    var wsMsg = decodeWebSocketOutputMessage(message);
    if (wsMsg === null) {
        // TODO RESEND MSG
        return allGameData.messageProcessorState;
    }
    else {
        if (wsMsg.type === 'LoginLogoutOutMsg') {
            return messageProcessorLoginLogout(allGameData, wsMsg.payload);
        }
        else {
            return allGameData.messageProcessorState;
        }
    }
}
// MDULE run-script
// const messageToEncode1: WebSocketInputMessage = {
//     type: 'HandshakeInMsg',
//     payload: new ExistingAnonConn(777, 888),
//   };
// const messageToEncode2: WebSocketInputMessage = {
//     type: 'HandshakeInMsg',
//     payload: new ExistingRegisteredUserAndConn(777, 888,"hello there!"),
//   };
// const messageToEncode3: WebSocketInputMessage = {
//     type: 'HandshakeInMsg',
//     payload: new ExistingRegisteredUserNewConn(777, "hello there!"),
//   };
//   const messageToEncode4: WebSocketInputMessage = {
//     type: 'HandshakeInMsg',
//     payload: new NonExisting(),
//   };
function startGameLoopIO(allGameData) {
    var connId = getCookie(keyConnId);
    var userId = getCookie(keyUserId);
    var password = getCookie(keyTempAnonPasswd);
    var regOrAnon = getCookie(keyUserRegOrAnon);
    console.log(connId, userId, password, regOrAnon);
    // const encodedInMessage = encodeWebSocketInputMessage(messageToEncode4);
    // // sendMessageIO(allGameData.webSocket,'Init')
    // sendMessageIO(allGameData.webSocket,encodedInMessage)
    // const decodedMessage1 = decodeWebSocketOutputMessage('LoginLogout;RegisteredSuccessfully;123;456');
    // console.log(decodedMessage1); // Output: { type: 'LoginLogoutOutMsg', payload: ['RegisteredSuccessfully', 123, 456] }
    // const decodedMessage2 = decodeLoginLogoutOutMsg('RegisteredSuccessfully;123;456');
    // console.log(decodedMessage2); // Output: ['RegisteredSuccessfully', 123, 456]
    if (!allGameData.gameLoopId) {
        allGameData.gameLoopId = setInterval(function () {
            gameLoopIO(allGameData);
        }, 1000 / 60); // Set the desired frame rate (e.g., 60 FPS)
    }
}
// Module entryPoint
// DOMContentLoaded uses for enshure that cookies are loaded
document.addEventListener('DOMContentLoaded', function () {
    var allGameData = initGameIO(gameConfig);
    startGameLoopIO(allGameData);
});
