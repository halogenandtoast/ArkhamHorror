<script setup lang="ts">
import { fetchGameRaw, postGameRaw } from '@/arkham/api';

export interface Props {
  gameId: string
}
const props = defineProps<Props>()

const game = await fetchGameRaw(props.gameId);

function replaceUUIDsInJSON(json, excludePaths) {
  const uuidMap = new Map();
  const excludeUUIDs = new Set();

  function isUUID(str) {
    return /^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$|^[0-9a-fA-F]{32}$/.test(str);
  }

  function pathsMatch(path, excludePath) {
    if (path.length !== excludePath.length) return false;
    for (let i = 0; i < path.length; i++) {
      if (excludePath[i] === '*') {
        continue; // Wildcard matches any key at this position
      }
      if (path[i] !== excludePath[i]) {
        return false;
      }
    }
    return true;
  }

  // First, collect UUIDs at the exclude paths
  function collectExcludeUUIDs(value, path = []) {
    if (excludePaths.some(excludePath => pathsMatch(path, excludePath))) {
      // Exclude UUIDs at this path
      if (typeof value === 'string' && isUUID(value)) {
        excludeUUIDs.add(value);
      } else if (value !== null && typeof value === 'object') {
        // If the value is an object, check if any keys are UUIDs
        for (const key in value) {
          if (isUUID(key)) {
            excludeUUIDs.add(key);
          }
          const newPath = path.concat([key]);
          collectExcludeUUIDs(value[key], newPath);
        }
      }
      return; // Do not traverse further down this path
    }

    if (Array.isArray(value)) {
      value.forEach((item, index) => {
        collectExcludeUUIDs(item, path.concat([index]));
      });
    } else if (value !== null && typeof value === 'object') {
      for (const key in value) {
        if (isUUID(key)) {
          const keyPath = path.concat([key]);
          if (excludePaths.some(excludePath => pathsMatch(keyPath, excludePath))) {
            excludeUUIDs.add(key);
          }
        }
        collectExcludeUUIDs(value[key], path.concat([key]));
      }
    }
  }

  // Start collecting excluded UUIDs
  collectExcludeUUIDs(json);

  let currentInt = 1;

  function replaceUUIDs(value, path = []) {
    if (Array.isArray(value)) {
      return value.map((item, index) => replaceUUIDs(item, path.concat([index])));
    } else if (value !== null && typeof value === 'object') {
      const newObj = {};
      for (const key in value) {
        let newKey = key;
        const keyPath = path.concat([key]);

        // Check if the key is an excluded UUID
        if (isUUID(key)) {
          if (excludeUUIDs.has(key)) {
            newKey = key;
          } else {
            if (!uuidMap.has(key)) {
              uuidMap.set(key, currentInt++);
            }
            newKey = uuidMap.get(key).toString();
          }
        }

        const newVal = replaceUUIDs(value[key], keyPath);

        newObj[newKey] = newVal;
      }
      return newObj;
    } else if (typeof value === 'string' && isUUID(value)) {
      if (excludeUUIDs.has(value)) {
        return value;
      } else {
        if (!uuidMap.has(value)) {
          uuidMap.set(value, currentInt++);
        }
        return uuidMap.get(value);
      }
    } else {
      return value;
    }
  }

  let modifiedJson = replaceUUIDs(json);
  const finalInt = currentInt - 1;
  modifiedJson['currentData']['gameActionCanBeUndone'] = false;
  modifiedJson['currentData']['gameActionDiff'] = [];
  modifiedJson['currentData']['gameNextId'] = finalInt;
  return modifiedJson;
}

const excludePaths = [
  ['currentData', 'gameActivePlayerId'],
  ['currentData', 'gamePlayers', '*'],
  ["currentData", "gameEntities", "investigators", "*", "playerId"],
];

function go() {
  postGameRaw(props.gameId, replaceUUIDsInJSON(game, excludePaths).currentData);
}
</script>

<template>
  <button @click="go">Post Game</button>
  <pre>{{ replaceUUIDsInJSON(game, excludePaths) }}</pre>
</template>
