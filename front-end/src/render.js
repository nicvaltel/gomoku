// Draw grid lines

// ------------------ RENDER VARS ------------------------

var renderConfig = {
    htmlCanvasName: 'goBoard',
    backgroundPath: '../images/wood_full_original.jpg',
    whiteStonePath: '../images/white_00.png',
    blackStonePath: '../images/black_00.png',
    boardSize: 19,
    starPoints: [3 + 1, 9 + 1, 15 + 1],
};

// ------------------ GAME VARS ------------------------

const STONE_COLOR_BLACK = 0;
const STONE_COLOR_WHITE = 1;

var gameState = {
    currentMoveColor: STONE_COLOR_WHITE,
    currentMoveNumber: 0,
    boardMatrix: null,
    newStoneCandidate: null,
};

// ------------------ RENDER LOGIC ------------------------

function initRenderIO(renderConf) {
    var canvas = document.getElementById(renderConf.htmlCanvasName);
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
    background.src = renderConf.backgroundPath;
    var blackStone = new Image();
    blackStone.src = renderConf.blackStonePath;
    var whiteStone = new Image();
    whiteStone.src = renderConf.whiteStonePath;
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
        gridSize: canvas.width / (renderConf.boardSize + 1),
        borderSize: canvas.width / (renderConf.boardSize + 1),
        boardSize: renderConf.boardSize,
        starPoints: renderConf.starPoints,
    };

    var renderData = {
        sizes: sizes,
        canvases: canvases,
        assets: assets,
    };


    // // Test stones
    // renderData.boardState.boardMatrix[10 - 1][10 - 1] = STONE_COLOR_BLACK;
    // renderData.boardState.boardMatrix[1 - 1][1 - 1] = STONE_COLOR_WHITE;
    // renderData.boardState.boardMatrix[19 - 1][19 - 1] = STONE_COLOR_WHITE;
    // renderData.boardState.boardMatrix[1 - 1][19 - 1] = STONE_COLOR_WHITE;
    // renderData.boardState.boardMatrix[19 - 1][1 - 1] = STONE_COLOR_WHITE;
    

    return renderData;
}

function drawBackgroundIO(renderData) {
    const ctx = renderData.canvases.ctx;
    const backgroundImg = renderData.assets.backgroundImg;
    const canvas = renderData.canvases.canvas;
    ctx.drawImage(backgroundImg, 0, 0, canvas.width, canvas.height);
}

function drawGridIO(renderData) {
    const ctx = renderData.canvases.ctx;
    const sizes = renderData.sizes;
    const canvas = renderData.canvases.canvas;

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
        var image = color == STONE_COLOR_BLACK ? assets.blackStoneImg : assets.whiteStoneImg;
        ctx.drawImage(image, coordX, coordY, gridSize, gridSize);
    }
}

function renderStonesIO(renderData, gameState) {
    const ctx = renderData.canvases.ctx;
    const sizes = renderData.sizes;
    for (var r = 0; r < renderConfig.boardSize; r++) {
        for (var c = 0; c < renderConfig.boardSize; c++) {
            var stoneColor = gameState.boardMatrix[r][c];
            placeStoneIO(ctx, renderData.assets, sizes, r, c, stoneColor);
        }
    }
}

function renderStateIO(renderData, gameState) {
    drawBackgroundIO(renderData);
    drawGridIO(renderData);
    renderStonesIO(renderData, gameState);

    // drawStoneNearMouseIO(canvases.ctx, inputData.mouseX, inputData.mouseY, canvases.canvas, sizes, assets, allGameData.boardState);
    renderData.canvases.visibleCtx.drawImage(renderData.canvases.bufferCanvas, 0, 0);
}


// ------------------ GAME LOGIC ------------------------

function initGameState(renderConfig){
    var boardMatrix = [];
    for (var i = 0; i < renderConfig.boardSize; i++) {
        boardMatrix[i] = [];
        for (var j = 0; j < renderConfig.boardSize; j++) {
            boardMatrix[i][j] = null;
        }
    }
    gameState.currentMoveColor = STONE_COLOR_WHITE;
    gameState.currentMoveNumber = 0;
    gameState.boardMatrix = boardMatrix;
    gameState.newStoneCandidate = null;


    // Test stones
    gameState.boardMatrix[10 - 1][10 - 1] = STONE_COLOR_BLACK;
    gameState.boardMatrix[1 - 1][1 - 1] = STONE_COLOR_WHITE;
    gameState.boardMatrix[19 - 1][19 - 1] = STONE_COLOR_WHITE;
    gameState.boardMatrix[1 - 1][19 - 1] = STONE_COLOR_WHITE;
    gameState.boardMatrix[19 - 1][1 - 1] = STONE_COLOR_WHITE;

    return gameState;
}


export {
    initRenderIO,
    renderConfig,
    renderStateIO,
    initGameState
};




// function drawBackgroundIO(ctx, backgroundImg, canvas) {
//     ctx.drawImage(backgroundImg, 0, 0, canvas.width, canvas.height);
// }

// function drawStoneNearMouseIO(ctx, mouseX, mouseY, canvas, sizes, assets, boardState) {
//     if (boardState.newStoneCandidate == null) {
//         return;
//     }
//     else {
//         var _a = boardState.newStoneCandidate, row = _a[0], col = _a[1];
//         if (boardState.boardMatrix[row][col] == null) {
//             var img = boardState.currentMoveColor == StoneColor.White ? assets.whiteStoneImg : assets.blackStoneImg;
//             var gridSize = sizes.gridSize, borderSize = sizes.borderSize;
//             var x = row * gridSize + borderSize - gridSize / 3;
//             var y = col * gridSize + borderSize - gridSize / 3;
//             ctx.drawImage(img, x, y, sizes.gridSize * 2 / 3, sizes.gridSize * 2 / 3);
//         }
//     }
// }


// function renderStateIO(allGameData, canvases, assets, sizes, inputData) {
//     drawBackgroundIO(canvases.ctx, assets.backgroundImg, canvases.canvas);
//     drawGridIO(canvases.ctx, sizes, canvases.canvas);
//     for (var r = 0; r < sizes.boardSize; r++) {
//         for (var c = 0; c < sizes.boardSize; c++) {
//             var stoneColor = allGameData.boardState.boardMatrix[r][c];
//             placeStoneIO(canvases.ctx, assets, sizes, r, c, stoneColor);
//         }
//     }
//     drawStoneNearMouseIO(canvases.ctx, inputData.mouseX, inputData.mouseY, canvases.canvas, sizes, assets, allGameData.boardState);
//     canvases.visibleCtx.drawImage(canvases.bufferCanvas, 0, 0);
// }
