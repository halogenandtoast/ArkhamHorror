<script lang="ts" setup>
import { computed } from 'vue'

export interface Props {
  prompt: string
  yes: () => void
  no: () => void
  cancel?: () => void
}

const props = defineProps<Props>()
const cancelFun = computed(() => typeof props.cancel === 'function' ? props.cancel : props.no)
</script>

<template>
  <div class="cd-popup" role="alert">
    <div class="cd-popup-container">
      <p>{{prompt}}</p>
      <ul class="cd-buttons">
         <li><a @click.prevent="yes" href="#yes">Yes</a></li>
         <li><a @click.prevent="no" href="#no">No</a></li>
      </ul>
      <a
        @click.prevent="cancelFun"
        href="#cancel"
        class="cd-popup-close img-replace">Close</a>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.cd-popup {
  position: fixed;
  left: 0;
  top: 0;
  height: 100%;
  width: 100%;
  background-color: rgba(94,110,141,.9);
  z-index: 999999999;
}

.cd-popup-container {
  position: relative;
  width: 90%;
  max-width: 400px;
  margin: 4em auto;
  background: #fff;
  border-radius: .25em .25em 0 0;
  text-align: center;
  box-shadow: 0 0 20px rgba(0,0,0,.2);

  p {
    padding: 3em 1em;
    margin: 0;
  }
}


.cd-popup-close {
  position: absolute;
  top: 8px;
  right: 8px;
  width: 30px;
  height: 30px;

  &::before {
    transform: rotate(45deg);
    left: 8px;
    content: '';
    position: absolute;
    top: 12px;
    width: 14px;
    height: 3px;
    background-color: #8f9cb5;
  }

  &::after {
    transform: rotate(135deg);
    left: 8px;
    content: '';
    position: absolute;
    top: 12px;
    width: 14px;
    height: 3px;
    background-color: #8f9cb5;
  }
}

.cd-buttons {
  list-style: none;
  padding: 0;
  margin: 0;
  a {
    display: block;
    height: 60px;
    line-height: 60px;
    text-transform: uppercase;
    text-decoration: none;
    color: #fff;
  }
  li:first-child a {
    background: #fc7169;
    border-radius: 0 0 0 .25em;
    &:hover {
        background-color: #fc8982;
    }
  }
  li:last-child a {
    background: #b6bece;
    border-radius: 0 0 .25em 0;
    &:hover {
        background-color: #c5ccd8;
    }
  }
  li {
    float: left;
    width: 50%;
  }
}

.img-replace {
  display: inline-block;
  overflow: hidden;
  text-indent: 100%;
  color: transparent;
  white-space: nowrap;
  text-wrap: pretty;
}
</style>
