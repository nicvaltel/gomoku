type ConnId = number;
type UserId = number;
type Password = string;

class Handshake {
  constructor(
    public connId: ConnId,
    public userId: UserId,
    public password?: Password
  ) {}
}

class ExistingAnonConn extends Handshake {
  constructor(connId: ConnId, userId: UserId) {
    super(connId, userId);
  }
}

class ExistingRegisteredUserAndConn extends Handshake {
  constructor(connId: ConnId, userId: UserId, password: Password) {
    super(connId, userId, password);
  }
}

class ExistingRegisteredUserNewConn extends Handshake {
  constructor(userId: UserId, password: Password) {
    super(0, userId, password);
  }
}

class NonExisting extends Handshake {}

// Define the WebSocketInputMessage union type
type WebSocketInputMessage =
  | { type: 'HandshakeInMsg'; payload: Handshake };
  // Add other WebSocketInputMessage subtypes here

// Custom encoding function
function encodeWebSocketInputMessage(message: WebSocketInputMessage): string {
    if (message.type === 'HandshakeInMsg') {
      const payload = message.payload;
  
      if (payload instanceof ExistingAnonConn) {
        // Handle ExistingAnonConn case
        return JSON.stringify({
          tag: 'HandshakeInMsg',
          contents: {
            tag: 'ExistingAnonConn',
            contents: [
              { unConnId: payload.connId },
              { unUserId: payload.userId },
            ],
          },
        });
      } else if (payload instanceof ExistingRegisteredUserAndConn) {
        // Handle ExistingRegisteredUserAndConn case
        return JSON.stringify({
          tag: 'HandshakeInMsg',
          contents: {
            tag: 'ExistingRegisteredUserAndConn',
            unConnId: payload.connId,
            unUserId: payload.userId,
            unPassword: payload.password,
          },
        });
      } else if (payload instanceof ExistingRegisteredUserNewConn) {
        // Handle ExistingRegisteredUserNewConn case
        return JSON.stringify({
          tag: 'HandshakeInMsg',
          contents: {
            tag: 'ExistingRegisteredUserNewConn',
            unUserId: payload.userId,
            unPassword: payload.password,
          },
        });
      } else if (payload instanceof NonExisting) {
        // Handle NonExisting case
        return JSON.stringify({
          tag: 'HandshakeInMsg',
          contents: { tag: 'NonExisting' },
        });
      }
    }
  
    // Return an empty string if the message type is not recognized
    return '';
  }
  


// // Example usages:

const messageToEncode: WebSocketInputMessage = {
    type: 'HandshakeInMsg',
    payload: new ExistingAnonConn(777, 888),
  };
  
// Encode the message to JSON
export const encodedMessage = encodeWebSocketInputMessage(messageToEncode);
  