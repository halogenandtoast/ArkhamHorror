#!/usr/bin/env node
//
// Generates a manifest of all image asset paths (relative to each app's public/).
// These manifests are committed to the repo so that contributors and Docker
// Compose users can download images from the CDN without needing AWS credentials.
//
// One manifest per frontend: frontend/ and the third edition's frontend-3ed/.
//
// Usage: node scripts/generate-manifest.cjs
//
const fs = require('fs');
const path = require('path');

const APPS = ['frontend', 'frontend-3ed'];

function walkDir(dir, excluded) {
  const results = [];
  if (!fs.existsSync(dir)) return results;

  const entries = fs.readdirSync(dir, { withFileTypes: true });
  for (const entry of entries) {
    if (entry.name === '.DS_Store') continue;
    const fullPath = path.join(dir, entry.name);
    if (excluded.has(fullPath)) continue;
    if (entry.isDirectory()) {
      results.push(...walkDir(fullPath, excluded));
    } else {
      results.push(fullPath);
    }
  }
  return results;
}

function generate(app) {
  const publicDir = path.join(__dirname, '..', app, 'public');
  const imgDir = path.join(publicDir, 'img');
  const outputFile = path.join(__dirname, '..', app, 'image-manifest.json');

  // Directories under img/ that are never synced to the CDN. img/custom holds
  // custom card art written by the dev server; it is local-only and gitignored.
  const excluded = new Set([path.join(imgDir, 'custom')]);

  // Walk all files under img/ and group by their immediate parent directory.
  // This covers both gitignored assets (cards, portraits, …) and git-tracked
  // UI assets (tokens, slots, icons, …) so the full set can be fetched from
  // the CDN without a git clone.
  const manifest = {};

  for (const absPath of walkDir(imgDir, excluded)) {
    const relPath = path.relative(publicDir, absPath);
    const dirKey = path.relative(publicDir, path.dirname(absPath));
    if (!manifest[dirKey]) manifest[dirKey] = [];
    manifest[dirKey].push(relPath);
  }

  // Stable sort: keys alphabetically, files within each key alphabetically
  const sortedManifest = {};
  let totalCount = 0;
  for (const key of Object.keys(manifest).sort()) {
    sortedManifest[key] = manifest[key].sort();
    totalCount += sortedManifest[key].length;
  }

  fs.writeFileSync(outputFile, JSON.stringify(sortedManifest, null, 2) + '\n');
  console.log(`Wrote ${totalCount} files across ${Object.keys(sortedManifest).length} directories to ${outputFile}`);
}

for (const app of APPS) generate(app);
