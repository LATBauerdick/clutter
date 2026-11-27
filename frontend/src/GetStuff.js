"use strict";

export const _encodeURIComponent = encodeURIComponent;

// browser history: one entry per view, numbered in its state; popstate tells the app
// which entry the browser moved to (Back or Forward)
export const initHistory = () => {
  history.replaceState({ i: 0 }, "");
  // links that only carry an onClick (href="#", so they look and act like links): the
  // browser must not follow "#" -- that adds a history entry without state, and popstate
  // then reads it as entry 0, the start page. The onClick still runs.
  document.addEventListener("click", (e) => {
    const a = e.target && e.target.closest && e.target.closest('a[href="#"]');
    if (a) e.preventDefault();
  });
};
export const pushHistory = (i) => () => { history.pushState({ i: i }, ""); };

// the last view shown, kept in this browser (a convenience: a refresh or a restart of clutter
// opens it again); storage may be off or empty (a private window), then the start page
export const saveView = (v) => () => {
  try { localStorage.setItem("clutter.view", v); } catch (e) {}
};
export const loadView = () => {
  try { return localStorage.getItem("clutter.view") || ""; } catch (e) { return ""; }
};
export const historyBack = () => { history.back(); };
export const historyForward = () => { history.forward(); };
export const onPopState = (cb) => () => {
  window.addEventListener("popstate", (e) => cb((e.state && e.state.i) || 0)());
};
// within two screens of the page's end: cb, at most once per animation frame
export const onNearBottom = (cb) => () => {
  let queued = false;
  window.addEventListener("scroll", () => {
    if (queued) return;
    queued = true;
    requestAnimationFrame(() => {
      queued = false;
      const doc = document.documentElement;
      if (window.innerHeight + window.scrollY >= doc.scrollHeight - 2 * window.innerHeight) cb();
    });
  }, { passive: true });
};

// is the first element matching the selector taller inside than it shows (line clamp)?
export const isClamped = (sel) => () => {
  const e = document.querySelector(sel);
  return !!e && e.scrollHeight > e.clientHeight + 1;
};

// the browser's own "are you sure?" dialog (native on iPad and iPhone too): true for OK
export const confirmBox = (msg) => () => window.confirm(msg);
