<script lang="ts" setup>
import { ref } from 'vue'
import { imgsrc } from '@/arkham/helpers'
import FormattedEntry from '@/arkham/components/FormattedEntry.vue'

defineProps<{ title: string; rules: { title?: string; bodyKey: string }[] }>()

const grunge = `url(${imgsrc('grunge.png')})`

const ruleEls = ref<(HTMLElement | null)[]>([])
const setRuleEl = (el: unknown, i: number) => {
  ruleEls.value[i] = el as HTMLElement | null
}
const scrollToRule = (i: number) => {
  ruleEls.value[i]?.scrollIntoView({ behavior: 'smooth', block: 'start' })
}
</script>

<template>
  <div class="log-section">
    <h3 class="section-title">{{ title }}</h3>
    <nav v-if="rules.filter((r) => r.title).length > 1" class="rule-jump">
      <template v-for="(rule, i) in rules" :key="i">
        <a
          v-if="rule.title"
          href="#"
          class="rule-jump-link"
          @click.prevent="scrollToRule(i)"
          v-html="rule.title"
        />
      </template>
    </nav>
    <div class="rules">
      <div
        class="rule intro-text"
        v-for="(rule, i) in rules"
        :key="i"
        :ref="(el) => setRuleEl(el, i)"
      >
        <h4 v-if="rule.title" class="rule-title" v-html="rule.title" />
        <div class="rule-body">
          <FormattedEntry :entry="{ tag: 'I18nEntry', key: rule.bodyKey, variables: {} }" />
        </div>
      </div>
    </div>
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
  color: rgba(255,255,255,0.75);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin: 0 0 10px;
  padding-bottom: 8px;
  border-bottom: 1px solid rgba(255,255,255,0.07);
}

.rule-jump {
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
  margin-bottom: 12px;
}

.rule-jump-link {
  font-family: teutonic, sans-serif;
  font-size: 0.85em;
  letter-spacing: 0.03em;
  color: rgba(255, 255, 255, 0.8);
  text-decoration: none;
  background: rgba(255, 255, 255, 0.06);
  border: 1px solid rgba(255, 255, 255, 0.1);
  border-radius: 999px;
  padding: 3px 12px;
  cursor: pointer;
  transition: background 0.15s, color 0.15s;
}

.rule-jump-link:hover {
  background: rgba(255, 255, 255, 0.14);
  color: #fff;
}

.rules {
  display: flex;
  flex-direction: column;
  gap: 12px;
}

.rule {
  scroll-margin-top: 12px;
}

.intro-text {
  color: var(--neutral-extra-dark);
  text-align: justify;
  background: linear-gradient(#DFDAD8, #c9c4c2);
  background-image: v-bind(grunge);
  background-size: cover;
  border-radius: 5px;
  padding: 16px 20px;
  letter-spacing: .03em;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-rendering: optimizelegibility;
}

.rule-title {
  font-family: teutonic, sans-serif;
  font-weight: normal;
  margin: 0 0 6px;
  font-size: 1.15em;
  text-align: left;
}

.rule-body {
  font-size: 1.05em;
  line-height: 1.5;
}

.rule-body :deep(p) {
  margin: 0 0 8px;
}

.rule-body :deep(p:last-child) {
  margin-bottom: 0;
}

.rule-body :deep(h3) {
  font-family: teutonic, sans-serif;
  font-weight: normal;
  font-size: 1.05em;
  margin: 12px 0 4px;
}

.rule-body :deep(ul) {
  margin: 8px 0 0;
  padding-left: 1.4em;
}

.rule-body :deep(li) {
  margin-bottom: 6px;
}

.rule-body :deep(li:last-child) {
  margin-bottom: 0;
}
</style>
