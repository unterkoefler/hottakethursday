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
});
