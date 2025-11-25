<script setup lang="ts">

type KeyStatus = { tag: "KeyWithInvestigator", contents: string } | { tag: "KeyWithEnemy", contents: [string, string | null] }

const props = defineProps<{
  keys: Record<string, KeyStatus>
  toTitle: (cardCode: string) => string
}>()

const allScarletKeys = {
  c09519: "The Eye of Ravens",
  c09544: "The Last Blossom",
  c09659: "The Light of Pharos",
  c09634: "The Sable Glass",
  c09565: "The Weeping Lady",
  c09590: "The Twisted Antiprism",
  c09680: "The Shade Reaper",
  c09768: "The Mirroring Blade",
  c09769: "The Bale Engine",
  c09770: "The Ruinous Chime",
  c88045: "The Wellspring of Fortune"
}

const toBearer = (cardCode: string): string | null => {
  const status = props.keys[cardCode]
  if (!status) return null
  if (status.tag === "KeyWithInvestigator") return props.toTitle(status.contents)
  if (status.tag === "KeyWithEnemy") return props.toTitle(status.contents[0])
  return "Unknown"
}

</script>

<template>
  <div class="campaign-log column box keys-status">
    <header><h2>Keys</h2></header>
    <table>
      <thead><tr><th><span>Name of Paradimensional Artifact</span></th><th><span>Bearer</span></th></tr></thead>
      <tbody>
        <tr v-for="[cardCode, scarletKey] in Object.entries(allScarletKeys)" :key="scarletKey">
          <td><span class='checkbox'>{{props.keys[cardCode] ? "✓" : null}}</span>{{ scarletKey }}</td>
          <td>{{ toBearer(cardCode) }}</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<style scoped>
.keys-status {
  --ink-color: #903834;
  --line-color: #ccc;
  padding: 20px;
  font-family: "Wolgast", serif;
  font-size: 0.9em;
  font-weight: bold;
  background-color: #FFFDF5;
  color: var(--ink-color);
  box-shadow: 0 3px 6px rgba(0,0,0,0.23), 0 3px 6px rgba(0,0,0,0.53);
}

thead {
  width: 100%;
}

thead th {
  text-align: left;
  padding-left: 50px;
  box-sizing: border-box;
  border-bottom: 1px solid var(--line-color);
}

thead th span {
  position: relative;
  &::after {
    content: '';
    position: absolute;
    inset: 0;
    left: -20px;
    right: -20px;
    bottom: 10px;
    border-bottom: 2px solid var(--ink-color);
    background-color: transparent;
    transform: rotateZ(-0.477deg);
  }
}

tbody {
  counter-reset: row;
  tr {
    counter-increment: row;
  }
}

td {
  border-bottom: 1px solid var(--line-color);
}

header {
  border-bottom: 1px solid var(--line-color);
}

h2 {
  width: fit-content;
  margin: 0 auto;
  position: relative;
  &::after {
    content: '';
    position: absolute;
    inset: 0;
    left: -10px;
    right: -30px;
    bottom: 20px;
    border-bottom: 2px solid var(--ink-color);
    background-color: transparent;
    transform: rotateZ(-1.5deg);
  }
}

.checkbox {
  height: 1em;
  width: 1em;
  display: inline-flex;
  justify-content: center;
  align-items: center;
  border: 2px solid var(--ink-color);
  border-radius: 0.05em;
  margin-right: 0.5em;
  transform-origin: center;
  transform: rotateZ(0deg);
  position: relative;
}

tr:nth-child(6n + 1) .checkbox { transform: rotateZ(6deg); }
tr:nth-child(6n + 2) .checkbox { transform: rotateZ(2deg); }
tr:nth-child(6n + 3) .checkbox { transform: rotateZ(-2deg); }
tr:nth-child(6n + 4) .checkbox { transform: rotateZ(-4deg); }
tr:nth-child(6n + 5) .checkbox { transform: rotateZ(5deg); }
tr:nth-child(6n)     .checkbox { transform: rotateZ(6deg); }

tr:nth-child(4n + 1) .checkbox { width: 1.05em; }
tr:nth-child(4n + 2) .checkbox { width: 0.9em; }
tr:nth-child(4n + 3) .checkbox { width: 0.9em; }
tr:nth-child(4n) .checkbox { width: 1em; }

tr:nth-child(3n + 1) .checkbox { height: 1.05em; }
tr:nth-child(3n + 2) .checkbox { height: 0.9em; }
tr:nth-child(3n) .checkbox { height: 1em; }
</style>
