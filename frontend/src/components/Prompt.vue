<script lang="ts" setup>
import { ref, computed, onMounted } from 'vue'

export interface Props {
  prompt: string
  yes: () => void
  no: () => void
  cancel?: () => void
}

const props = defineProps<Props>()
const cancelFun = computed(() => typeof props.cancel === 'function' ? props.cancel : props.no)

const dialogRef = ref<HTMLDialogElement>()

onMounted(() => {
  dialogRef.value?.showModal()
})

function handleYes() {
  dialogRef.value?.close()
  props.yes()
}

function handleNo() {
  dialogRef.value?.close()
  props.no()
}

function handleCancel() {
  dialogRef.value?.close()
  cancelFun.value()
}
</script>

<template>
  <dialog ref="dialogRef" @cancel.prevent="handleCancel">
    <button class="close-btn" @click.prevent="handleCancel" aria-label="Close">
      <font-awesome-icon icon="times" />
    </button>
    <p class="prompt-text">{{ prompt }}</p>
    <div class="prompt-actions">
      <button class="btn btn--confirm" @click.prevent="handleYes">Yes</button>
      <button class="btn btn--cancel" @click.prevent="handleNo">No</button>
    </div>
  </dialog>
</template>

<style scoped>
dialog {
  position: fixed;
  margin: auto;
  padding: 32px;
  width: 90%;
  max-width: 400px;
  background: #1e2030;
  border: 1px solid rgba(255, 255, 255, 0.1);
  border-radius: 10px;
  box-shadow: 0 20px 60px rgba(0, 0, 0, 0.6);
  color: #f0f0f0;
  opacity: 0;
  transform: scale(0.94) translateY(-10px);
  transition: opacity 0.2s ease, transform 0.2s ease,
              display 0.2s allow-discrete,
              overlay 0.2s allow-discrete;

  &[open] {
    opacity: 1;
    transform: scale(1) translateY(0);
  }

  @starting-style {
    &[open] {
      opacity: 0;
      transform: scale(0.94) translateY(-10px);
    }
  }
}

dialog::backdrop {
  background: rgba(0, 0, 0, 0);
  backdrop-filter: blur(0px);
  transition: background 0.2s ease, backdrop-filter 0.2s ease,
              display 0.2s allow-discrete,
              overlay 0.2s allow-discrete;
}

dialog[open]::backdrop {
  background: rgba(0, 0, 0, 0.65);
  backdrop-filter: blur(4px);
}

@starting-style {
  dialog[open]::backdrop {
    background: rgba(0, 0, 0, 0);
    backdrop-filter: blur(0px);
  }
}

.close-btn {
  position: absolute;
  top: 12px;
  right: 12px;
  background: transparent;
  border: none;
  color: #666;
  font-size: 1rem;
  cursor: pointer;
  padding: 4px 8px;
  border-radius: 4px;
  transition: color 0.15s;

  &:hover { color: #ccc; }
}

.prompt-text {
  margin: 0 0 28px;
  font-size: 1rem;
  line-height: 1.5;
  color: #d0d0d0;
  padding-right: 20px;
}

.prompt-actions {
  display: flex;
  gap: 10px;
  justify-content: flex-end;
}

.btn {
  padding: 8px 22px;
  border: none;
  border-radius: 6px;
  font-size: 0.88rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  cursor: pointer;
  transition: opacity 0.15s;

  &:hover { opacity: 0.85; }
}

.btn--confirm {
  background: #c0392b;
  color: #fff;
}

.btn--cancel {
  background: rgba(255, 255, 255, 0.1);
  color: #ccc;
}
</style>
