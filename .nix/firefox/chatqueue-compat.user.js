// ==UserScript==
// @name         solo098 ChatQueue ChatGPT send-button compatibility
// @namespace    https://github.com/klarkc/dotfiles
// @version      1.0.0
// @description  Expose the real ChatGPT submit button to signed ChatQueue 1.0.1; never send a message.
// @match        https://chatgpt.com/*
// @run-at       document-idle
// @grant        none
// ==/UserScript==

(() => {
	"use strict";
	// ChatQueue 1.0.1 looks for #composer-submit-button. The current ChatGPT
	// composer instead exposes form[data-chatgpt-composer] button[type=submit].
	// Do not patch an installed XPI: that would invalidate Mozilla's signature.
	const marker = "composer-submit-button";
	const compat = () => {
		for (const form of document.querySelectorAll(
			"form[data-chatgpt-composer]",
		)) {
			const candidates = [...form.querySelectorAll('button[type="submit"]')];
			if (candidates.length !== 1) continue; // ambiguous form: fail closed
			const button = candidates[0];
			const aria = (button.getAttribute("aria-label") || "").toLowerCase();
			// Never give a Stop/Cancel button the Send identity during generation.
			if (/stop|cancel|pause|parar|interromper|cancelar/.test(aria)) {
				if (button.id === marker) button.removeAttribute("id");
				continue;
			}
			if (button.id && button.id !== marker) continue; // don't overwrite a site ID
			if (!button.id) button.id = marker;
		}
	};
	const observer = new MutationObserver(compat);
	observer.observe(document.documentElement, {
		subtree: true,
		childList: true,
		attributes: true,
		attributeFilter: ["aria-label", "type", "id"],
	});
	compat();
})();
