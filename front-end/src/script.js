// npx eslint draw-board.ts
// MODULE script


import('../output/MessageProcessor/index.js').then(MessageProcessor => {
import('../output/GameProcessor/index.js').then(GameProcessor => {
import('./cookies.js').then(Cookies => {
import('./render.js').then(Render => {
const { initRenderIO, renderConfig, renderStateIO, initGameState } = Render;    
const { messageProcessor, updateConnectionConfig } = MessageProcessor;
const { setCookie, getCookie, keyUserId, keyConnId, keyTempAnonPasswd, keyUserRegOrAnon } = Cookies;


var connectionConfig = {
    webSocketUrl: 'ws://127.0.0.1:1234',
    userId : '',
    connId : '',
    userRegOrAnon : 'AnonUser',
    tempAnonPasswd : '',
}

var inputBuffer = {
    mouseX: 0,
    mouseY: 0,
    canvasWidth: 0,
    canvasHeight: 0,
};

var appData = {
    connConf : connectionConfig,
    updateCookieFlag : false,
    renderConf : renderConfig,
    renderData : null,
    gameLoopId: null,
    gameState: null,
    inputBuffer: inputBuffer,
    inputData: getInputDataIO(inputBuffer),
    
    // htmlCanvasName: 'goBoard',
    // backgroundPath: 'images/wood_full_original.jpg',
    // whiteStonePath: 'images/white_00.png',
    // blackStonePath: 'images/black_00.png',
    // boardSize: 19,
    // starPoints: [3 + 1, 9 + 1, 15 + 1],
};


function loadCookies(appData){
    appData.connConf.userId = getCookie(keyUserId);
    appData.connConf.connId = getCookie(keyConnId);
    appData.connConf.userRegOrAnon = getCookie(keyUserRegOrAnon);
    appData.connConf.tempAnonPasswd = getCookie(keyTempAnonPasswd);
}

function saveCookies(appData){
    if (appData.updateCookieFlag) {
        setCookie(keyUserId,appData.connConf.userId);
        setCookie(keyConnId,appData.connConf.connId);
        setCookie(keyUserRegOrAnon,appData.connConf.userRegOrAnon);
        setCookie(keyTempAnonPasswd,appData.connConf.tempAnonPasswd);
        appData.updateCookieFlag = false;
    }
}


function initWebSocketIO(appData){
    // Create a new WebSocket object
    const socket = new WebSocket(appData.connConf.webSocketUrl);

    // Connection opened
    socket.addEventListener('open', (event) => {
        console.log('WebSocket connection opened:', event);

        // Send a message to the server
        // socket.send('Hello, server!');
    });

    // Listen for messages from the server
    socket.addEventListener('message', (event) => {
        console.log('Received message from server:', event.data);
        const messageProcessorResult = messageProcessor(appData)(event.data);

        console.log("messageProcessorResult: ", messageProcessorResult);

        appData = messageProcessorResult.value0;
        console.log("newAppData: ", messageProcessorResult);
        saveCookies(appData);

        if (messageProcessorResult.value1 != "") {
            console.log('Sending message to server:', messageProcessorResult.value1);
            socket.send(messageProcessorResult.value1);
        }
        
    });

    // Listen for WebSocket errors
    socket.addEventListener('error', (event) => {
        console.error('WebSocket error:', event);
    });

    // Listen for WebSocket closures
    socket.addEventListener('close', (event) => {
        console.log('WebSocket connection closed:', event);
        // You can handle reconnection logic here if needed
    });

    // // Close the WebSocket connection after 5 seconds (for demonstration purposes)
    // setTimeout(() => {
    //     socket.close();
    // }, cfg.wsTimeout);

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


function initGameIO(appData) {

    document.addEventListener('mousemove', function (event) {
        // Retrieve the mouse cursor coordinates from the event object
        var canvasRect = appData.renderData.canvases.canvas.getBoundingClientRect();
        inputBuffer.mouseX = event.clientX - canvasRect.left; // X-coordinate relative to the viewport
        inputBuffer.mouseY = event.clientY - canvasRect.top; // Y-coordinate relative to the viewport
        // Log or use the cursor coordinates
        console.log(`Mouse X: ${inputBuffer.mouseX}, Mouse Y: ${inputBuffer.mouseY}`);
    });

    loadCookies(appData);
    appData.renderData = initRenderIO(appData.renderConf);
    appData.gameState = initGameState(appData.renderConf);
    initWebSocketIO(appData, messageProcessor);    
}


function sendWSMessageIO(ws, msg) {
    // Wait until the state of the socket is not ready and send the message when it is...
    waitForSocketConnectionIO(ws, function () {
        console.log("ws message sent: ", msg);
        ws.send(msg);
    });
}



// function gameLoopIO(allGameData) {
//     // allGameData.inputData = getInputDataIO(allGameData.inputBuffer);
//     // allGameData = processGameLogic(allGameData);
//     // renderStateIO(allGameData, allGameData.canvases, allGameData.assets, allGameData.sizes, allGameData.inputData);
// }

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


// MDULE run-script
// function startGameLoopIO(allGameData) {
//     var connId = getCookie(keyConnId);
//     var userId = getCookie(keyUserId);
//     var password = getCookie(keyTempAnonPasswd);
//     var regOrAnon = getCookie(keyUserRegOrAnon);
//     console.log(connId, userId, password, regOrAnon);
//     if (!allGameData.gameLoopId) {
//         allGameData.gameLoopId = setInterval(function () {
//             gameLoopIO(allGameData);
//         }, 1000 / 60); // Set the desired frame rate (e.g., 60 FPS)
//     }
// }



// Module GameLoop 

function gameLoopIO(appData) {
    appData.inputData = getInputDataIO(appData.inputBuffer);
    // findNearestToMouseCoord
    // allGameData = processGameLogic(allGameData);
    // renderStateIO(allGameData, allGameData.canvases, allGameData.assets, allGameData.sizes, allGameData.inputData);
    renderStateIO(appData.renderData, appData.gameState);
}

function startGameLoopIO(appData) {
    if (!appData.gameLoopId) {
        appData.gameLoopId = setInterval(function () {
            gameLoopIO(appData);
        }, 1000 / 60); // Set the desired frame rate (e.g., 60 FPS)
    }
}

// Module entryPoint




initGameIO(appData);
startGameLoopIO(appData);
// startGameLoopIO(allGameData);

});
});
});
});