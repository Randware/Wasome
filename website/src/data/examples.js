// This file loads fallback examples from the generated JS file (created by scripts/fetch-examples.js).
// If the generated file doesn't exist (e.g. fetch failed), it provides empty defaults.
// These fallbacks are ONLY used when the GitHub API rate-limits the client.

// Use import.meta.glob to optionally load the generated file (won't error if missing)
const modules = import.meta.glob('./generated/fallback-examples.js', { eager: true });
const generatedMod = modules['./generated/fallback-examples.js'];

export const fallbackExamples = generatedMod?.examples ?? {};

export const fallbackTree = generatedMod?.tree ?? [
    {
        name: 'single_file',
        path: 'docs/examples/single_file',
        type: 'dir',
        children: []
    },
    {
        name: 'single_project',
        path: 'docs/examples/single_project',
        type: 'dir',
        children: null
    },
    {
        name: 'multi-project',
        path: 'docs/examples/multi-project',
        type: 'dir',
        children: null
    }
];
