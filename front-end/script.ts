// npx eslint draw-board.ts

enum StoneColor {
    Black,
    White,
}

interface GameConfig {
    socketUrl: string,
    htmlCanvasName: string,
    backgroundPath: string,
    blackStonePath: string,
    whiteStonePath: string,
    // whiteHolePath: string,
    // blackHolePath: string,
    boardSize: number,
    starPoints: number[],
}

const gameConfig: GameConfig = {
    socketUrl: 'ws://127.0.0.1:1234', // 'ws://95.140.155.123:1234/ws',
    htmlCanvasName: 'goBoard',
    backgroundPath: 'images/wood_full_original.jpg',
    whiteStonePath: 'images/white_00.png',
    blackStonePath: 'images/black_00.png',
    // whiteHolePath: 'images/white_hole.png',
    // blackHolePath: 'images/black_hole.png',
    boardSize: 19,
    starPoints: [3 + 1, 9 + 1, 15 + 1],
}

interface GameAssets {
    backgroundImg: HTMLImageElement,
    whiteStoneImg: HTMLImageElement,
    blackStoneImg: HTMLImageElement,
    // whiteHoleImg: HTMLImageElement,
    // blackHoleImg: HTMLImageElement,
}

interface Canvases {
    canvas: HTMLCanvasElement;
    visibleCtx: CanvasRenderingContext2D;
    bufferCanvas: HTMLCanvasElement;
    ctx: CanvasRenderingContext2D;
}

interface Sizes {
    gridSize: number,
    borderSize: number,
    boardSize: number,
    starPoints: number[],
}

interface InputData {
    mouseX: number,
    mouseY: number,
    canvasWidth: number,
    canvasHeight: number,
}

type GameEntity = {
    type: 'GameBoard';
} | {
    type: 'Stone';
    value: StoneColor;
};

interface GameEntityData {
    gameObj: GameEntity
    x: number,
    y: number,
    z: number,
    width: number,
    height: number,
}

type MaybeStone = StoneColor | null

interface BoardState {
    currentMoveColor: StoneColor,
    currentMoveNumber: number,
    boardMatrix: MaybeStone[][],
    newStoneCandidate: [number, number] | null
}

interface AllGameData {
    webSocket: WebSocket,
    boardState: BoardState,
    inputData: InputData,
    inputBuffer: InputData,
    gameEntityDatas: GameEntityData[]
    sizes: Sizes,
    canvases: Canvases,
    assets: GameAssets,
    gameLoopId: number | null,
}

function initGameIO(cfg: GameConfig): AllGameData {

    const canvas = document.getElementById(cfg.htmlCanvasName) as HTMLCanvasElement;
    let visibleCtx: CanvasRenderingContext2D;
    const visibleCtxMaybe = canvas.getContext('2d');
    if (visibleCtxMaybe !== null) {
        visibleCtx = visibleCtxMaybe;
    } else {
        throw new Error("draw-board initGame: visibleCtxMaybe in null");
    }

    const bufferCanvas = document.createElement('canvas');
    bufferCanvas.width = canvas.width;
    bufferCanvas.height = canvas.height;

    const bufferCanvasCtxMaybe = bufferCanvas.getContext('2d');
    let bufferCanvasCtx: CanvasRenderingContext2D;
    if (bufferCanvasCtxMaybe !== null) {
        bufferCanvasCtx = bufferCanvasCtxMaybe;
    } else {
        throw new Error("draw-board initGame: bufferCanvasCtxMaybe in null");
    }

    const background = new Image();
    background.src = cfg.backgroundPath;

    const blackStone = new Image();
    blackStone.src = cfg.blackStonePath;

    const whiteStone = new Image();
    whiteStone.src = cfg.whiteStonePath;

    // const blackHole = new Image();
    // blackHole.src = cfg.blackHolePath;

    // const whiteHole = new Image();
    // whiteHole.src = cfg.whiteHolePath;

    // Handle image loading
    background.onload = () => {
        whiteStone.onload = () => { }
        blackStone.onload = () => { }
        // whiteHole.onload = () => { }
        // blackHole.onload = () => { }
    };

    const canvases: Canvases = {
        canvas: canvas,
        visibleCtx: visibleCtx,
        bufferCanvas: bufferCanvas,
        ctx: bufferCanvasCtx,
    }

    const assets: GameAssets = {
        backgroundImg: background,
        blackStoneImg: blackStone,
        whiteStoneImg: whiteStone,
        // blackHoleImg: blackHole,
        // whiteHoleImg: whiteHole,
    }

    const sizes: Sizes = {
        gridSize: canvas.width / (cfg.boardSize + 1),
        borderSize: canvas.width / (cfg.boardSize + 1),
        boardSize: cfg.boardSize,
        starPoints: cfg.starPoints,
    }

    const inputBuffer: InputData = {
        mouseX: 0,
        mouseY: 0,
        canvasWidth: 0,
        canvasHeight: 0,
    }

    const boardMatrix: MaybeStone[][] = [];
    for (let i = 0; i < cfg.boardSize; i++) {
        boardMatrix[i] = [];
        for (let j = 0; j < cfg.boardSize; j++) {
            boardMatrix[i][j] = null;
        }
    }
    const boardState: BoardState = {
        currentMoveColor: StoneColor.White,
        currentMoveNumber: 0,
        boardMatrix: boardMatrix,
        newStoneCandidate: null,
    }

    const webSocket = new WebSocket(cfg.socketUrl);

    const allGameData: AllGameData = {
        webSocket: webSocket,
        boardState: boardState,
        inputData: getInputDataIO(inputBuffer),
        inputBuffer: inputBuffer,
        gameEntityDatas: [],
        sizes: sizes,
        canvases: canvases,
        assets: assets,
        gameLoopId: null,
    }

    document.addEventListener('mousemove', (event: MouseEvent) => {
        // Retrieve the mouse cursor coordinates from the event object
        const canvasRect = allGameData.canvases.canvas.getBoundingClientRect();
        inputBuffer.mouseX = event.clientX - canvasRect.left; // X-coordinate relative to the viewport
        inputBuffer.mouseY = event.clientY - canvasRect.top; // Y-coordinate relative to the viewport

        // Log or use the cursor coordinates
        console.log(`Mouse X: ${inputBuffer.mouseX}, Mouse Y: ${inputBuffer.mouseY}`);
    });


    // Event handler when the WebSocket connection is opened
    webSocket.addEventListener('open', (event) => {
        console.log('WebSocket connection opened:', event);

        // Send a message to the server (optional)
        webSocket.send('Hello, server!');
    });

    // Event handler when a message is received from the server
    webSocket.addEventListener('message', (event) => {
        const message = event.data;
        console.log('Message from server:', message);

        // Handle the received message as needed
        // You can update the canvas or perform other actions here
    });

    // Event handler when the WebSocket connection is closed
    webSocket.addEventListener('close', (event) => {
        console.log('WebSocket connection closed:', event);

        // You can handle reconnection logic here if needed
    });

    // Event handler for WebSocket errors
    webSocket.addEventListener('error', (event) => {
        console.error('WebSocket error:', event);
    });


    allGameData.boardState.boardMatrix[10 - 1][10 - 1] = StoneColor.Black;
    allGameData.boardState.boardMatrix[1 - 1][1 - 1] = StoneColor.White;
    allGameData.boardState.boardMatrix[19 - 1][19 - 1] = StoneColor.White;
    allGameData.boardState.boardMatrix[1 - 1][19 - 1] = StoneColor.White;
    allGameData.boardState.boardMatrix[19 - 1][1 - 1] = StoneColor.White;

    return allGameData
}


// Draw grid lines
function drawGridIO(ctx: CanvasRenderingContext2D, sizes: Sizes, canvas: HTMLCanvasElement) {
    const { gridSize, borderSize } = sizes

    ctx.strokeStyle = 'black';
    ctx.lineWidth = 1;

    for (let i = 0; i <= sizes.boardSize; i++) {
        const pos = i * gridSize;

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

    sizes.starPoints.forEach(x => {
        sizes.starPoints.forEach(y => {
            const centerX = x * gridSize;
            const centerY = y * gridSize;
            ctx.beginPath();
            ctx.arc(centerX, centerY, 3, 0, 2 * Math.PI);
            ctx.fill();
        });
    });

}

function placeStoneIO(ctx: CanvasRenderingContext2D, assets: GameAssets, sizes: Sizes, x: number, y: number, color: MaybeStone) {
    if (color !== null) {
        const { gridSize, borderSize } = sizes

        const coordX = x * gridSize - gridSize / 2 + borderSize;
        const coordY = y * gridSize - gridSize / 2 + borderSize;

        const image = color == StoneColor.Black ? assets.blackStoneImg : assets.whiteStoneImg
        ctx.drawImage(image, coordX, coordY, gridSize, gridSize);
    }
}


function drawBackgroundIO(ctx: CanvasRenderingContext2D, backgroundImg: HTMLImageElement, canvas: HTMLCanvasElement) {
    ctx.drawImage(backgroundImg, 0, 0, canvas.width, canvas.height);
}


function findNearestToMouseCoord(mouseX: number, mouseY: number, canvas: HTMLCanvasElement, sizes: Sizes): [number, number] | null {
    const { gridSize, borderSize, boardSize } = sizes

    if (mouseX < 0 || mouseY < 0 || mouseX > canvas.width || mouseY > canvas.height) {
        return null
    } else {
        const row = Math.round((mouseX - borderSize) / gridSize);
        const col = Math.round((mouseY - borderSize) / gridSize);
        if (row >= 0 && col >= 0 && row < boardSize && col < boardSize) {
            return [row, col];
        } else {
            return null;
        }
    }
}

function drawStoneNearMouseIO(ctx: CanvasRenderingContext2D, mouseX: number, mouseY: number, canvas: HTMLCanvasElement, sizes: Sizes, assets: GameAssets, boardState: BoardState) {
    if (boardState.newStoneCandidate == null) {
        return
    } else {
        const [row, col] = boardState.newStoneCandidate;
        if (boardState.boardMatrix[row][col] == null) {
            const img = boardState.currentMoveColor == StoneColor.White ? assets.whiteStoneImg : assets.blackStoneImg;
            const { gridSize, borderSize } = sizes
            const x = row * gridSize + borderSize - gridSize / 3;
            const y = col * gridSize + borderSize - gridSize / 3;
            ctx.drawImage(img, x, y, sizes.gridSize * 2 / 3, sizes.gridSize * 2 / 3);
        }
    }
}


function getInputDataIO(inputBuffer: InputData): InputData {
    const inputData: InputData = {
        mouseX: inputBuffer.mouseX,
        mouseY: inputBuffer.mouseY,
        canvasWidth: inputBuffer.canvasWidth,
        canvasHeight: inputBuffer.canvasHeight,
    }
    return inputData
}

function processGameLogic(allGameData): AllGameData {
    const { inputData, canvases, sizes } = allGameData;
    allGameData.boardState.newStoneCandidate = findNearestToMouseCoord(inputData.mouseX, inputData.mouseY, canvases.canvas, sizes);
    return allGameData;
}

function renderStateIO(allGameData: AllGameData, canvases: Canvases, assets: GameAssets, sizes: Sizes, inputData: InputData): void {
    drawBackgroundIO(canvases.ctx, assets.backgroundImg, canvases.canvas);
    drawGridIO(canvases.ctx, sizes, canvases.canvas);

    for (let r = 0; r < sizes.boardSize; r++) {
        for (let c = 0; c < sizes.boardSize; c++) {
            const stoneColor = allGameData.boardState.boardMatrix[r][c];
            placeStoneIO(canvases.ctx, assets, sizes, r, c, stoneColor);
        }
    }

    drawStoneNearMouseIO(canvases.ctx, inputData.mouseX, inputData.mouseY, canvases.canvas, sizes, assets, allGameData.boardState);

    canvases.visibleCtx.drawImage(canvases.bufferCanvas, 0, 0);
}

function gameLoopIO(allGameData: AllGameData): void {
    allGameData.inputData = getInputDataIO(allGameData.inputBuffer);
    allGameData = processGameLogic(allGameData);
    renderStateIO(allGameData, allGameData.canvases, allGameData.assets, allGameData.sizes, allGameData.inputData);
}

function sendMessageIO(ws: WebSocket, msg : string){
    // Wait until the state of the socket is not ready and send the message when it is...
    waitForSocketConnectionIO(ws, function(){
        console.log("message sent!!!");
        ws.send(msg);
    });
}

// Make the function wait until the connection is made...
function waitForSocketConnectionIO(socket : WebSocket, callback : () => void){
    setTimeout(
        function () {
            if (socket.readyState === 1) {
                console.log("Connection is made")
                if (callback != null){
                    callback();
                }
            } else {
                console.log("wait for connection...")
                waitForSocketConnectionIO(socket, callback);
            }
        }, 5); // wait 5 milisecond for the connection...
}

function startGameLoopIO(allGameData: AllGameData): void {

    sendMessageIO(allGameData.webSocket,'Init')

    if (!allGameData.gameLoopId) {
        allGameData.gameLoopId = setInterval(() => {
            gameLoopIO(allGameData);
        }, 1000 / 60); // Set the desired frame rate (e.g., 60 FPS)
    }
}

const allGameData = initGameIO(gameConfig);
startGameLoopIO(allGameData)