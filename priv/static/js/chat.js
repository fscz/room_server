var RTCPeerConnection = webkitRTCPeerConnection || mozRTCPeerConnection;
var peerConnection;
var configuration = {
   iceServers: [
    {url: 'stun:stun.l.google.com:19302'},
    {url: 'stun:stun1.l.google.com:19302'},
    {url: 'stun:stun2.l.google.com:19302'},
    {url: 'stun:stun3.l.google.com:19302'},
    {url: 'stun:stun4.l.google.com:19302'}
   ]
};
var DtlsSrtpKeyAgreement = {
   DtlsSrtpKeyAgreement: true
};
var optional = {
   optional: [DtlsSrtpKeyAgreement]
};


function logError(error) {
    console.log('error: '+error);
}

var $console = $('#console');
function log(msg) {
    //$console.append('<div>'+msg+'</div>');
    console.log(msg);
}




function makeRoomServerClient(callbacks) {
    function WebSocketRoomServerClient(url) {
        this.ws = new WebSocket(url);        
    }
    WebSocketRoomServerClient.prototype.send = function(type, msg) {
        msg = msg || {};
        msg["type"] = type;
        this.ws.send(JSON.stringify(msg));
    }
    WebSocketRoomServerClient.prototype.message = function(content) {
        log('<out: '+JSON.stringify(content));
        this.send("message", {content: content});    
    }
    WebSocketRoomServerClient.prototype.join = function(roomId) {
        this.send("join", {room: roomId});
    }
    WebSocketRoomServerClient.prototype.leave = function() {
        this.send("leave");
    }
    WebSocketRoomServerClient.prototype.create = function() {
        this.send("create");
    }
    WebSocketRoomServerClient.prototype.setCallbacks = function(callbacks) {
        this.ws.onopen = callbacks.onopen || undefined;
        this.ws.onclose = callbacks.onclose || undefined;
        this.ws.onmessage = callbacks.onmessage || undefined;
        this.ws.onerror = callbacks.onerror || undefined;
    } 

    var roomServerClient = new WebSocketRoomServerClient('ws://' + window.location.host + '/websocket/room');
    roomServerClient.setCallbacks(
        {
            onopen: function(event) {
                roomServerClient.create();
            },
            onclose: function() {
                log('Connection closed.');
            },
            onerror: function(event) {
                logError(event.reason);
            },
            onmessage: function(event) {
                log('in> '+event.data);
                var message = JSON.parse(event.data);
                var type = message['type'];
                var status = message['status'];
                var reason = message['reason'];

                if (type === 'message') {
                    var content = message['content'];
                    var type = content['type'];

                    if (type === 'offer') {

                        var remoteDescription = new RTCSessionDescription(content);
                        peerConnection.setRemoteDescription(remoteDescription, function() {
                            peerConnection.createAnswer(function(answerSDP) {
                                peerConnection.setLocalDescription(answerSDP, function() {
                                    roomServerClient.message(peerConnection.localDescription);
                                });                                
                            }, logError);
                        }, logError);        
                        
                    }
                    else if (type === 'candidate') {
                        peerConnection.addIceCandidate(new RTCIceCandidate(content));
                    }
                }
                else if (type === 'create') {
                    if (status === 'ok') {
                        var room = message['room'];
                        var $room = $('#room');
                        $room.text($room.text()+room);

                        peerConnection = new RTCPeerConnection(configuration);                          
                        
                        peerConnection.onicecandidate = function(event) {
                            var candidate = event.candidate;
                                      
                            if(candidate) {
                                var msg = {
                                    type: 'candidate',
                                    candidate: candidate.candidate,
                                    sdpMLineIndex: candidate.sdpMLineIndex,
                                    sdpMid: candidate.sdpMid
                                };
                                roomServerClient.message(msg);
                            }
                        }
                        callbacks.peerConnectionCreated(peerConnection);

                    } else {
                        logError(reason);
                    }
                }
            }
        }
    );
    return roomServerClient;
}

/*
function disconnectPeers() {
 
    // Close the RTCDataChannels if they're open.
    
    sendChannel.close();
    receiveChannel.close();
    
    // Close the RTCPeerConnections
    
    localConnection.close();
    remoteConnection.close();

    sendChannel = null;
    receiveChannel = null;
    localConnection = null;
    remoteConnection = null;
    
    // Update user interface elements
    
    connectButton.disabled = false;
    disconnectButton.disabled = true;
    sendButton.disabled = true;
    
    messageInputBox.value = "";
    messageInputBox.disabled = true;
  }
*/

/*
            var localChannel = pc.createDataChannel("1", {ordered: false, maxRetransmits: 0});

            localChannel.onerror = function (error) {
                log("localChannel onerror:", error);
            };

            localChannel.onmessage = function (event) {
                log("localChannel onmessage:", event.data);
            };

            localChannel.onopen = function () {
                log("localChannel onopen");
                var $send = $('#send');
                $send.prop('disabled', false);
                $send.click(function() {
                    localChannel.send('foo');
                }); 
            };

            localChannel.onclose = function () {
                log("localChannel onclose");
            };
            */
function handleInput(input) {
    log(JSON.stringify(input));
    if (input.tiltLeft) {
        
    } 
    if (input.tiltRight) {
        
    }
    if (input.tiltUp) {
        
    }
    if (input.tiltDown) {
        
    }
    if (input.buttonLeft) {
        
    }
    if (input.buttonRight) {
        
    }
}        

function arrayBufferToBase64( buffer ) {
    var binary = '';
    var bytes = new Uint8Array( buffer );
    var len = bytes.byteLength;
    for (var i = 0; i < len; i++) {
        binary += String.fromCharCode( bytes[ i ] );
    }
    return window.btoa( binary );
}


$(document).ready(function() {
    
    var $body = $('body');
    var $wrapper = $('.wrapper'); 
    function animateBody() {
        sweep($body[0], 'backgroundColor', '#a8f', '#a8f', {callback: animateBody, direction: -1, duration: 20000});
    }
    animateBody();
    var urlCreator = window.URL || window.webkitURL;


    var roomServerClient = makeRoomServerClient({
        peerConnectionCreated: function(pc) { 

            pc.onaddstream = function (event) {
                log('stream added: '+event.stream);
                var $video = $('<video width="800" height="600" controls/>');
                $video.attr('src', URL.createObjectURL(event.stream));
                $body.empty().append($video);
            };  

            pc.ondatachannel = function (event) {
                var remoteChannel = event.channel;  
                var $img = $('<img width="100%" height="100%"/>');
                //$body.empty().append($img);

                remoteChannel.onmessage = function (event) {
                    var blob = new Blob([new Uint8Array(event.data)], {type: 'image/jpeg'});

                    var objectURL = urlCreator.createObjectURL(blob);
                    $img.attr('src', objectURL);
                    //var input = JSON.parse(String.fromCharCode.apply(null, new Uint8Array(event.data)));                    
                    //handleInput(input);
                };
                remoteChannel.onopen = function () {
                    log("remoteChannel onopen");
                    
                };
                remoteChannel.onclose = function() {
                    log("remoteChannel onclose");
                    $send.prop('disabled', true);
                };
                remoteChannel.onerror = function() {
                    log("remoteChannel onerror");
                };
            };
        }
    });

    
    
});
