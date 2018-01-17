console.log("start background.js");

setup(Elm.MainBackground.worker, (app) => {
    console.log("started", app);
    app.ports.sendOutNewState.subscribe((state) => {
        // console.log("new state arrived!!!", state);
        if (app.onSendOutNewState) {
            try {
                app.onSendOutNewState(state);
            } catch (e) {
                console.log(e);
            };
        }
    });

    window.backgroundApp = app;
});


browser.browserAction.setBadgeText({text: "test"});
browser.browserAction.setBadgeBackgroundColor({color: "green"});
