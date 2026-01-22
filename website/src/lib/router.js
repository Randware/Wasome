import { writable } from 'svelte/store';

const { subscribe, set } = writable(window.location.hash.slice(1) || '/');

export const route = {
  subscribe,
  init: () => {
    const update = () => set(window.location.hash.slice(1) || '/');
    window.addEventListener('hashchange', update);
    return () => window.removeEventListener('hashchange', update);
  }
};

export const navigate = (path) => {
  window.location.hash = path;
};