declare enum StoneColor {
    Black = 0,
    White = 1
}
declare enum MessageProcessorState {
    MPSHandshake = 0,
    MPSNormal = 1
}
interface GameConfig {
    socketUrl: string;
    htmlCanvasName: string;
    backgroundPath: string;
    blackStonePath: string;
    whiteStonePath: string;
    boardSize: number;
    starPoints: number[];
}
declare const gameConfig: GameConfig;
interface GameAssets {
    backgroundImg: HTMLImageElement;
    whiteStoneImg: HTMLImageElement;
    blackStoneImg: HTMLImageElement;
}
interface Canvases {
    canvas: HTMLCanvasElement;
    visibleCtx: CanvasRenderingContext2D;
    bufferCanvas: HTMLCanvasElement;
    ctx: CanvasRenderingContext2D;
}
interface Sizes {
    gridSize: number;
    borderSize: number;
    boardSize: number;
    starPoints: number[];
}
interface InputData {
    mouseX: number;
    mouseY: number;
    canvasWidth: number;
    canvasHeight: number;
}
type GameEntity = {
    type: 'GameBoard';
} | {
    type: 'Stone';
    value: StoneColor;
};
interface GameEntityData {
    gameObj: GameEntity;
    x: number;
    y: number;
    z: number;
    width: number;
    height: number;
}
type MaybeStone = StoneColor | null;
interface BoardState {
    currentMoveColor: StoneColor;
    currentMoveNumber: number;
    boardMatrix: MaybeStone[][];
    newStoneCandidate: [number, number] | null;
}
interface AllGameData {
    webSocket: WebSocket;
    boardState: BoardState;
    inputData: InputData;
    inputBuffer: InputData;
    gameEntityDatas: GameEntityData[];
    sizes: Sizes;
    canvases: Canvases;
    assets: GameAssets;
    gameLoopId: number | null;
    messageProcessorState: MessageProcessorState;
}
declare function initGameIO(cfg: GameConfig): AllGameData;
declare function drawGridIO(ctx: CanvasRenderingContext2D, sizes: Sizes, canvas: HTMLCanvasElement): void;
declare function placeStoneIO(ctx: CanvasRenderingContext2D, assets: GameAssets, sizes: Sizes, x: number, y: number, color: MaybeStone): void;
declare function drawBackgroundIO(ctx: CanvasRenderingContext2D, backgroundImg: HTMLImageElement, canvas: HTMLCanvasElement): void;
declare function findNearestToMouseCoord(mouseX: number, mouseY: number, canvas: HTMLCanvasElement, sizes: Sizes): [number, number] | null;
declare function drawStoneNearMouseIO(ctx: CanvasRenderingContext2D, mouseX: number, mouseY: number, canvas: HTMLCanvasElement, sizes: Sizes, assets: GameAssets, boardState: BoardState): void;
declare function getInputDataIO(inputBuffer: InputData): InputData;
declare function processGameLogic(allGameData: AllGameData): AllGameData;
declare function renderStateIO(allGameData: AllGameData, canvases: Canvases, assets: GameAssets, sizes: Sizes, inputData: InputData): void;
declare function gameLoopIO(allGameData: AllGameData): void;
declare function sendMessageIO(ws: WebSocket, msg: string): void;
declare function waitForSocketConnectionIO(socket: WebSocket, callback: () => void): void;
declare const keyUserId = "keyUserId";
declare const keyConnId = "keyConnId";
declare const keyTempAnonPasswd = "keyTempAnonPasswd";
declare const keyUserRegOrAnon = "keyUserRegOrAnon";
declare function setCookie(key: string, value: string, expirationDays?: number): void;
declare function getCookie(key: string): string | null;
type ConnId = string;
type UserId = string;
type Password = string;
type UserRegOrAnon = 'RegUser' | 'AnonUser';
declare class Handshake {
    connId?: string | undefined;
    userId?: string | undefined;
    password?: string | undefined;
    constructor(connId?: string | undefined, userId?: string | undefined, password?: string | undefined);
}
declare class ExistingAnonConn extends Handshake {
    constructor(connId: ConnId, userId: UserId, password: Password);
}
declare class ExistingRegisteredUserAndConn extends Handshake {
    constructor(connId: ConnId, userId: UserId, password: Password);
}
declare class ExistingRegisteredUserNewConn extends Handshake {
    constructor(userId: UserId, password: Password);
}
declare class NonExisting extends Handshake {
}
type WebSocketInputMessage = {
    type: 'HandshakeInMsg';
    payload: Handshake;
};
declare function encodeHandshake(payload: Handshake): string;
declare function encodeWebSocketInputMessage(message: WebSocketInputMessage): string;
type WebSocketOutputMessage = {
    type: 'LoginLogoutOutMsg';
    payload: LoginLogoutMsg;
};
type LoginLogoutMsg = 'AskForExistingUser' | 'RegisterError' | ['RegisteredSuccessfully', UserId, ConnId] | 'LoginError' | ['LoginSuccessfully', UserId, ConnId] | ['LogoutSuccessfully', UserId, ConnId] | ['NewAnonUser', UserId, ConnId, Password] | ['OldAnonUser', UserId, ConnId];
declare function decodeLoginLogoutOutMsg(input: string): LoginLogoutMsg | null;
declare function decodeWebSocketOutputMessage(input: string): WebSocketOutputMessage | null;
declare function messageProcessorLoginLogout(allGameData: AllGameData, msg: LoginLogoutMsg): MessageProcessorState;
declare function messageProcessor(allGameData: AllGameData, message: string): MessageProcessorState;
declare function startGameLoopIO(allGameData: AllGameData): void;
