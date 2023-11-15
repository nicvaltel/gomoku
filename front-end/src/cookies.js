var keyUserId = 'keyUserId';
var keyConnId = 'keyConnId';
var keyTempAnonPasswd = 'keyTempAnonPasswd';
var keyUserRegOrAnon = 'keyUserRegOrAnon';

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

export {
    setCookie,
    getCookie,
    keyUserId,
    keyConnId,
    keyTempAnonPasswd,
    keyUserRegOrAnon,
};