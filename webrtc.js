// WebRTC implementation for Liar's Dice with Firestore signaling

class WebRTCManager {
  constructor() {
    this.peerConnection = null;
    this.localStream = null;
    this.remoteStream = null;
    this.gameId = null;
    this.clientId = null;
    this.isInitiator = false;
    this.firestoreConfig = {
      apiKey: 'AIzaSyDzpJvn3EOxsm9EZPpNwaRDIrEXdsEGp7I',
      projectId: 'liars-dice-3fd4e'
    };
    this.baseUrl = `https://firestore.googleapis.com/v1/projects/${this.firestoreConfig.projectId}/databases/(default)/documents`;
    
    // ICE servers configuration
    this.iceServers = {
      iceServers: [
        { urls: 'stun:stun.l.google.com:19302' },
        { urls: 'stun:stun1.l.google.com:19302' },
        { urls: 'stun:stun2.l.google.com:19302' }
      ]
    };

    // Callbacks
    this.onRemoteStreamCallback = null;
    this.onConnectionStateChangeCallback = null;
    this.onErrorCallback = null;
  }

  // Initialize WebRTC connection
  async initialize(gameId, clientId, isInitiator) {
    console.log('Initializing WebRTC:', { gameId, clientId, isInitiator });
    this.gameId = gameId;
    this.clientId = clientId;
    this.isInitiator = isInitiator;

    try {
      // Get user media (audio and video)
      await this.setupLocalMedia();
      
      // Create peer connection
      this.createPeerConnection();
      
      // Add local stream to peer connection
      this.localStream.getTracks().forEach(track => {
        this.peerConnection.addTrack(track, this.localStream);
      });

      // Start listening for signaling messages
      this.startSignalingListener();

      if (this.isInitiator) {
        // Create and send offer
        await this.createOffer();
      }

      return { success: true };
    } catch (error) {
      console.error('Error initializing WebRTC:', error);
      if (this.onErrorCallback) {
        this.onErrorCallback(error.message);
      }
      return { success: false, error: error.message };
    }
  }

  // Setup local media (camera and microphone)
  async setupLocalMedia() {
    try {
      this.localStream = await navigator.mediaDevices.getUserMedia({
        video: {
          width: { ideal: 640 },
          height: { ideal: 480 }
        },
        audio: {
          echoCancellation: true,
          noiseSuppression: true,
          autoGainControl: true
        }
      });

      // Display local video
      const localVideo = document.getElementById('localVideo');
      if (localVideo) {
        localVideo.srcObject = this.localStream;
        localVideo.muted = true; // Mute local audio to prevent feedback
      }

      console.log('Local media setup successful');
    } catch (error) {
      console.error('Error accessing media devices:', error);
      throw new Error('Could not access camera/microphone: ' + error.message);
    }
  }

  // Create RTCPeerConnection
  createPeerConnection() {
    this.peerConnection = new RTCPeerConnection(this.iceServers);

    // Handle ICE candidates
    this.peerConnection.onicecandidate = (event) => {
      if (event.candidate) {
        console.log('New ICE candidate:', event.candidate);
        this.sendSignalingMessage({
          type: 'ice-candidate',
          candidate: event.candidate
        });
      }
    };

    // Handle remote stream
    this.peerConnection.ontrack = (event) => {
      console.log('Received remote track:', event.track.kind);
      if (!this.remoteStream) {
        this.remoteStream = new MediaStream();
        const remoteVideo = document.getElementById('remoteVideo');
        if (remoteVideo) {
          remoteVideo.srcObject = this.remoteStream;
        }
        if (this.onRemoteStreamCallback) {
          this.onRemoteStreamCallback(this.remoteStream);
        }
      }
      this.remoteStream.addTrack(event.track);
    };

    // Handle connection state changes
    this.peerConnection.onconnectionstatechange = () => {
      console.log('Connection state:', this.peerConnection.connectionState);
      if (this.onConnectionStateChangeCallback) {
        this.onConnectionStateChangeCallback(this.peerConnection.connectionState);
      }
    };

    // Handle ICE connection state changes
    this.peerConnection.oniceconnectionstatechange = () => {
      console.log('ICE connection state:', this.peerConnection.iceConnectionState);
    };
  }

  // Create and send offer
  async createOffer() {
    try {
      const offer = await this.peerConnection.createOffer();
      await this.peerConnection.setLocalDescription(offer);
      
      console.log('Created offer:', offer);
      await this.sendSignalingMessage({
        type: 'offer',
        sdp: offer.sdp
      });
    } catch (error) {
      console.error('Error creating offer:', error);
      throw error;
    }
  }

  // Handle received offer
  async handleOffer(offerSdp) {
    try {
      console.log('Handling offer');
      const offer = new RTCSessionDescription({
        type: 'offer',
        sdp: offerSdp
      });
      
      await this.peerConnection.setRemoteDescription(offer);
      
      const answer = await this.peerConnection.createAnswer();
      await this.peerConnection.setLocalDescription(answer);
      
      console.log('Created answer:', answer);
      await this.sendSignalingMessage({
        type: 'answer',
        sdp: answer.sdp
      });
    } catch (error) {
      console.error('Error handling offer:', error);
      throw error;
    }
  }

  // Handle received answer
  async handleAnswer(answerSdp) {
    try {
      console.log('Handling answer');
      const answer = new RTCSessionDescription({
        type: 'answer',
        sdp: answerSdp
      });
      
      await this.peerConnection.setRemoteDescription(answer);
    } catch (error) {
      console.error('Error handling answer:', error);
      throw error;
    }
  }

  // Handle received ICE candidate
  async handleIceCandidate(candidateData) {
    try {
      const candidate = new RTCIceCandidate(candidateData);
      await this.peerConnection.addIceCandidate(candidate);
      console.log('Added ICE candidate');
    } catch (error) {
      console.error('Error adding ICE candidate:', error);
    }
  }

  // Send signaling message to Firestore
  async sendSignalingMessage(message) {
    try {
      const signalPath = `games/${this.gameId}/signals/${this.clientId}_${Date.now()}`;
      const url = `${this.baseUrl}/${signalPath}?key=${this.firestoreConfig.apiKey}`;
      
      const data = {
        fields: {
          from: { stringValue: this.clientId },
          timestamp: { integerValue: Date.now().toString() },
          type: { stringValue: message.type },
          data: { stringValue: JSON.stringify(message) }
        }
      };

      const response = await fetch(url, {
        method: 'PATCH',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(data)
      });

      if (!response.ok) {
        throw new Error(`Failed to send signaling message: ${response.status}`);
      }

      console.log('Sent signaling message:', message.type);
    } catch (error) {
      console.error('Error sending signaling message:', error);
      throw error;
    }
  }

  // Listen for signaling messages from Firestore
  startSignalingListener() {
    const checkInterval = 1000; // Check every second
    
    this.signalingInterval = setInterval(async () => {
      try {
        await this.checkForSignalingMessages();
      } catch (error) {
        console.error('Error checking signaling messages:', error);
      }
    }, checkInterval);
  }

  // Check for new signaling messages
  async checkForSignalingMessages() {
    try {
      const signalsPath = `games/${this.gameId}/signals`;
      const url = `${this.baseUrl}/${signalsPath}?key=${this.firestoreConfig.apiKey}`;
      
      const response = await fetch(url);
      if (!response.ok) {
        return;
      }

      const data = await response.json();
      if (!data.documents) {
        return;
      }

      // Process signals not from this client
      for (const doc of data.documents) {
        const fields = doc.fields;
        if (!fields) continue;

        const from = fields.from?.stringValue;
        const messageData = fields.data?.stringValue;
        
        if (from === this.clientId) continue; // Skip own messages
        if (!messageData) continue;

        try {
          const message = JSON.parse(messageData);
          await this.handleSignalingMessage(message);
          
          // Delete processed message
          await fetch(`${this.baseUrl}/${doc.name.split('/databases/(default)/documents/')[1]}?key=${this.firestoreConfig.apiKey}`, {
            method: 'DELETE'
          });
        } catch (error) {
          console.error('Error processing signal:', error);
        }
      }
    } catch (error) {
      console.error('Error checking signaling messages:', error);
    }
  }

  // Handle incoming signaling message
  async handleSignalingMessage(message) {
    console.log('Received signaling message:', message.type);

    switch (message.type) {
      case 'offer':
        if (!this.isInitiator) {
          await this.handleOffer(message.sdp);
        }
        break;
      case 'answer':
        if (this.isInitiator) {
          await this.handleAnswer(message.sdp);
        }
        break;
      case 'ice-candidate':
        if (message.candidate) {
          await this.handleIceCandidate(message.candidate);
        }
        break;
      default:
        console.warn('Unknown message type:', message.type);
    }
  }

  // Toggle local video
  toggleVideo(enabled) {
    if (this.localStream) {
      this.localStream.getVideoTracks().forEach(track => {
        track.enabled = enabled;
      });
      return true;
    }
    return false;
  }

  // Toggle local audio
  toggleAudio(enabled) {
    if (this.localStream) {
      this.localStream.getAudioTracks().forEach(track => {
        track.enabled = enabled;
      });
      return true;
    }
    return false;
  }

  // Close connection and cleanup
  close() {
    console.log('Closing WebRTC connection');

    // Stop signaling listener
    if (this.signalingInterval) {
      clearInterval(this.signalingInterval);
      this.signalingInterval = null;
    }

    // Stop local stream
    if (this.localStream) {
      this.localStream.getTracks().forEach(track => track.stop());
      this.localStream = null;
    }

    // Close peer connection
    if (this.peerConnection) {
      this.peerConnection.close();
      this.peerConnection = null;
    }

    // Clear video elements
    const localVideo = document.getElementById('localVideo');
    const remoteVideo = document.getElementById('remoteVideo');
    if (localVideo) localVideo.srcObject = null;
    if (remoteVideo) remoteVideo.srcObject = null;

    this.remoteStream = null;
    this.gameId = null;
    this.clientId = null;
  }

  // Set callbacks
  onRemoteStream(callback) {
    this.onRemoteStreamCallback = callback;
  }

  onConnectionStateChange(callback) {
    this.onConnectionStateChangeCallback = callback;
  }

  onError(callback) {
    this.onErrorCallback = callback;
  }
}

// Global instance
window.webRTCManager = new WebRTCManager();

// Export functions for OCaml to call
window.webrtc_initialize = async (gameId, clientId, isInitiator) => {
  return await window.webRTCManager.initialize(gameId, clientId, isInitiator);
};

window.webrtc_toggle_video = (enabled) => {
  return window.webRTCManager.toggleVideo(enabled);
};

window.webrtc_toggle_audio = (enabled) => {
  return window.webRTCManager.toggleAudio(enabled);
};

window.webrtc_close = () => {
  window.webRTCManager.close();
};

window.webrtc_set_remote_stream_callback = (callback) => {
  window.webRTCManager.onRemoteStream(() => callback());
};

window.webrtc_set_connection_state_callback = (callback) => {
  window.webRTCManager.onConnectionStateChange((state) => callback(state));
};

window.webrtc_set_error_callback = (callback) => {
  window.webRTCManager.onError((error) => callback(error));
};

console.log('WebRTC module loaded');

// Setup video controls when DOM is ready
document.addEventListener('DOMContentLoaded', () => {
  const videoContainer = document.getElementById('video-container');
  const toggleVideoBtn = document.getElementById('toggleVideo');
  const toggleAudioBtn = document.getElementById('toggleAudio');

  if (toggleVideoBtn) {
    toggleVideoBtn.addEventListener('click', () => {
      const manager = window.webRTCManager;
      if (manager && manager.localStream) {
        const videoTrack = manager.localStream.getVideoTracks()[0];
        if (videoTrack) {
          const newState = !videoTrack.enabled;
          manager.toggleVideo(newState);
          toggleVideoBtn.classList.toggle('disabled', !newState);
          toggleVideoBtn.textContent = newState ? '📹' : '📹❌';
        }
      }
    });
  }

  if (toggleAudioBtn) {
    toggleAudioBtn.addEventListener('click', () => {
      const manager = window.webRTCManager;
      if (manager && manager.localStream) {
        const audioTrack = manager.localStream.getAudioTracks()[0];
        if (audioTrack) {
          const newState = !audioTrack.enabled;
          manager.toggleAudio(newState);
          toggleAudioBtn.classList.toggle('disabled', !newState);
          toggleAudioBtn.textContent = newState ? '🎤' : '🎤❌';
        }
      }
    });
  }

  // Show video container when WebRTC is active
  window.showVideoContainer = () => {
    if (videoContainer) {
      videoContainer.classList.add('active');
      videoContainer.style.display = 'flex';
    }
  };

  // Hide video container
  window.hideVideoContainer = () => {
    if (videoContainer) {
      videoContainer.classList.remove('active');
      videoContainer.style.display = 'none';
    }
  };
});

// Override initialize to show video container
const originalInitialize = window.webrtc_initialize;
window.webrtc_initialize = async (gameId, clientId, isInitiator) => {
  const result = await originalInitialize(gameId, clientId, isInitiator);
  if (result.success && window.showVideoContainer) {
    window.showVideoContainer();
  }
  return result;
};

// Override close to hide video container
const originalClose = window.webrtc_close;
window.webrtc_close = () => {
  originalClose();
  if (window.hideVideoContainer) {
    window.hideVideoContainer();
  }
};
