import { writable } from 'svelte/store';

const { subscribe, set } = writable(window.location.pathname);

export const route = {
  subscribe,
  init: () => {
    const update = () => set(window.location.pathname);
    window.addEventListener('popstate', update);
    return () => window.removeEventListener('popstate', update);
  }
};

export const navigate = (path) => {
  window.history.pushState({}, '', path);
  set(path);
};