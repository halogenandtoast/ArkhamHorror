<script setup lang="ts">
import { ref } from 'vue'
import { fetchGamesIds, postGameRaw, fetchGameRaw } from '@/arkham/api'

const fetched = ref(false)
const started = ref(false)
const ids = ref<string[]>([])
const finished = ref(0)
const total = ref(0)
const failed = ref<string[]>([])
const done = ref(false)

function replaceUUIDsInJSON(json: any, excludePaths: string[][]) {
  const uuidMap = new Map();
  const excludeUUIDs = new Set();

  function isUUID(str) {
    return /^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$|^[0-9a-fA-F]{32}$/.test(str);
  }

  let excludedUUIDS = json['currentData']['gamePlayers'];

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

const fetch = async () => {
  fetched.value = true
  ids.value = await fetchGamesIds()
  total.value = ids.value.length
}

const start = async () => {
  started.value = true
  for (const id of ids.value) {
    const game = await fetchGameRaw(id)
    try {
      await postGameRaw(id, replaceUUIDsInJSON(game, excludePaths).currentData);
    } catch (e) {
      failed.value.push(id)
      console.error("Failed on game", id, e)
    }
    finished.value++
  }
}

</script>

<template>
<button v-if="!fetched" @click="fetch">Fetch Games</button>
<button v-else-if="!started" @click="start">Start</button>
<div v-else>
  <p>Finished: {{ finished }} / {{ total }}</p>

  <div v-if="finished">
    <p>Failed: {{ failed.length }}</p>
    <ul>
      <li v-for="id in failed" :key="id">{{ id }}</li>
    </ul>
  </div>
</div>
</template>
