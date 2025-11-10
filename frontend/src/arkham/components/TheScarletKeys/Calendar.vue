<script lang="ts" setup>
defineProps<{ time: number, theta: number | null }>()

function symbolForDay(day: number): string | null {
  if (day == 7) return 'α'
  if (day == 10) return 'ε'
  if (day == 15) return 'β'
  if (day == 20) return 'ζ'
  if (day == 24) return 'γ'
  if (day == 35) return 'Ω'
  return null
}

</script>

<template>
  <div class="time">
    <h2>Time Passed</h2>
    <div class='calendar'>
      <!-- we need 35 cells each with an empty div a div with a number and another empty div -->
      <div class='day' v-for="n in 35" :key="n">
        <div class='checkbox' :class="{ checked: n <= time[1] }">{{n <= time[1] ? 'x' : null}}</div><div class='numeral'>{{n}}</div><div class='symbol'>{{symbolForDay(n)}}{{ n == theta ? 'Θ' : '' }}</div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.time {
  flex-shrink: 0;
  flex-grow: 0;
  position: relative;
  isolation: isolate;
  width: fit-content;
  height: fit-content;
  text-align: center;
  --time-bg: #FCF5E2;
  background-color: var(--time-bg);
  margin: 20px auto;
  font-family: "Arno", serif;
  border: 4px solid #333;

  &::before {
    content: '';
    position: absolute;
    inset: 0;
    z-index: -1;
    inset: -10px;
    border: 2px solid #333;
    background-color: transparent;
  }
  &::after {
    content: '';
    position: absolute;
    border-radius: 6px;
    inset: -20px;
    z-index: -2;
    border: 4px solid #333;
    background-color: var(--time-bg);
    box-shadow: 10px 10px 1px rgba(0,0,0,0.5);
  }
  h2 {
    text-transform: uppercase;
    font-family: "Albertus";
    font-size: 2em;
    padding-block: 10px;
    border: 4px solid #333;
    border-bottom: 0;
  }
  .calendar {
    display: grid;
    border: 4px solid #333;
    border-top-width: 2px;
    grid-template-columns: repeat(7, 1fr);
    width: fit-content;
    background-color: #333;
    gap: 2px;


    .day {
      background-color: var(--time-bg);
      width: 4em;
      height: 4em;
      grid-template-areas:
        "box box . . . num num"
        "box box . . . num num"
        ". . symbol symbol symbol . ."
        ". . symbol symbol symbol . ."
        ". . . . . . .";
      display: grid;
      .checkbox {
        grid-area: box;
        border-bottom: 1px solid #333;
        border-right: 1px solid #333;   
        width: 1em;
        height: 1em;
        justify-content: center;
        align-items: center;
        display: flex;
        line-height: 1em;
        font-weight: bold;
        &.checked {
          background-color: #333;
          color: var(--time-bg);
        }
      }
      .numeral {
        grid-area: num;
        width: 100%;
        height: 100%;
        text-align: right;
        justify-content: start;
        line-height: 1em;
        padding-right: 1px;
      }
      .symbol {
        grid-area: symbol;
        font-size: 2em;
        font-weight: bold;
      }
    }
  }
}

</style>
