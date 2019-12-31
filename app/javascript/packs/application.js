// This file is automatically compiled by Webpack, along with any other files
// present in this directory. You're encouraged to place your actual application logic in
// a relevant structure within app/javascript and only use these pack files to reference
// that code so it'll be compiled.

// require("@rails/ujs").start()
// require("@rails/activestorage").start()
// require("channels")


// Uncomment to copy all static images under ../images to the output folder and reference
// them with the image_pack_tag helper in views (e.g <%= image_pack_tag 'rails.png' %>)
// or the `imagePath` JavaScript helper below.
//
// const images = require.context('../images', true)
// const imagePath = (name) => images(name, true)

import {
    Elm
} from '../Main.elm'

import consumer from '../channels/consumer';

document.addEventListener('DOMContentLoaded', () => {
    const target = document.createElement('div');

    document.body.appendChild(target);
    const app = Elm.Main.init({
        node: target,
        flags: {
            storedJWT: window.localStorage.getItem("jwt")
        }
    });

    app.ports.storeAuthToken.subscribe(token => {
        window.localStorage.setItem("jwt", token);
    });

    app.ports.clearAuthToken.subscribe(() => {
        window.localStorage.removeItem("jwt");
    });

    consumer.subscriptions.create("TakeFeedChannel", {
        connected() {
            // Called when the subscription is ready for use on the server
            console.debug("Connected to Action Cable")
        },

        disconnected() {
            // Called when the subscription has been terminated by the server
            console.debug("Disconnected from Action Cable")
        },

        received(data) {
            // Called when there's incoming data on the websocket for this channel
            console.debug(`Got data from Action Cable`, JSON.stringify(data))
            try {
                app.ports.newTakeInfo.send(data);
            }
            catch (e) {
                console.error("Failed to send info from the take feed to elm", e);
            }
        }
    });
});
