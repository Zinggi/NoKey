// taken from: https://gist.github.com/pablen/c07afa6a69291d771699b0e8c91fe547
const DEFAULT_ROOT_ELEMENT = document;
const DEFAULT_OBSERVER_CONFIG = { childList: true, subtree: true };

const createObserver = (reactors, config) => {
    config = config || {};
    const {
        observerConfig = DEFAULT_OBSERVER_CONFIG,
        rootElement = DEFAULT_ROOT_ELEMENT,
    } = config;

    const observer = new MutationObserver(mutations => {
        mutations.forEach(mutation => {
            // Handle added nodes
            mutation.addedNodes.forEach(addedNode => {
                doOnMount(true, addedNode, reactors);
            });

            mutation.removedNodes.forEach(removedNode => {
                doOnMount(false, removedNode, reactors);

            });
        });
    });

    observer.observe(rootElement, observerConfig);

    return observer;
};

// Returns an iterator containing elements that were part of a DOM mutation & matches the selector
const doOnMount = (isMount, rootElement, reactors) => {
    if (!(rootElement.querySelectorAll && rootElement.matches)) {
        return [];
    }
    reactors.forEach(reactor => {
        let matchingElements = [];
        const event = isMount ? reactor.onMount : reactor.onUnmount;
        if (event) {
            if (rootElement.matches(reactor.selector)) {
                matchingElements = [rootElement];
            } else {
                matchingElements = rootElement.querySelectorAll(reactor.selector);
            }
            if (matchingElements.length >= 1) {
                matchingElements.forEach(event);
            }
        }
    });
};

module.exports = createObserver;
