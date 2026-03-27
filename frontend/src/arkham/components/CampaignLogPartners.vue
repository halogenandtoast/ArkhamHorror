<script lang="ts" setup>
defineProps<{
  partners: Record<string, any>
  cardCodeToTitle: (code: string) => string
}>()
</script>

<template>
  <div class="log-section">
    <h3 class="section-title">Expedition Team</h3>
    <table>
      <thead>
        <tr>
          <th></th>
          <th>Damage</th>
          <th>Horror</th>
        </tr>
      </thead>
      <tbody>
        <tr
          v-for="[cCode, partner] in Object.entries(partners)"
          :key="cCode"
          class="partner"
          :class="{ [partner.status]: true }"
        >
          <td v-if="partner.status === 'TheEntity'" class="partner-name">
            <span class="name"><s>{{ cardCodeToTitle(cCode) }}</s></span>
            <span class="name">The Entity</span>
          </td>
          <td v-else class="partner-name">
            <span class="name">{{ cardCodeToTitle(cCode) }}</span>
            <span class="status-mia" v-if="partner.status === 'Mia'">MIA</span>
          </td>
          <td>{{ partner.damage }}</td>
          <td>{{ partner.horror }}</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<style scoped>
.log-section {
  background: var(--box-background);
  border: 1px solid rgba(255,255,255,0.07);
  border-radius: 8px;
  padding: 14px 16px;
}

.section-title {
  font-family: teutonic, sans-serif;
  font-size: 1.1em;
  font-weight: normal;
  color: rgba(255,255,255,0.5);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 0 0 12px;
  padding-bottom: 8px;
  border-bottom: 1px solid rgba(255,255,255,0.07);
}

table {
  width: 100%;
  display: grid;
  grid-template-columns: 1fr auto auto;
  gap: 4px;

  thead, tbody, tr { display: contents; }

  th {
    font-size: 0.72rem;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    color: rgba(255,255,255,0.35);
    padding: 4px 8px;
  }
}

.partner td {
  background: rgba(255,255,255,0.04);
  border-radius: 5px;
  padding: 8px 10px;
  color: var(--title);
  font-size: 0.9rem;
}

tr td:not(:first-child) { text-align: right; }

.Eliminated td {
  text-decoration: line-through;
  background: rgba(139,0,0,0.4);
  color: rgba(255,255,255,0.5);
}

.Mia td { background: rgba(184,134,11,0.3); }

.partner-name {
  display: flex;
  align-items: center;
  gap: 8px;

  .name { flex: 1; }
}

.status-mia {
  font-size: 0.7rem;
  font-weight: 700;
  letter-spacing: 0.06em;
  background: rgba(0,0,0,0.4);
  padding: 1px 6px;
  border-radius: 3px;
  color: #e8c84a;
}
</style>
