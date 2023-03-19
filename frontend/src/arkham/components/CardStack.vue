<template>
  <div class="card-row-cards">
    <div class="card-row-cards--inner">
      <slot></slot>
    </div>
  </div>
</template>

<style scoped lang="scss">
.card-row-cards {
  position: relative;
  width: calc($card-width + 20px);
  height: $card-width * 1.4;
  padding-bottom: 10px;
}

.card-row-cards--inner {
  display: grid;
  direction: rtl;
  grid-template-columns: repeat(auto-fit,  minmax(5px, max-content));
  &:hover {
    position: absolute;
    padding-right: 10px;
    padding-left: calc($card-width + 10px);
    background: rgba(0,0,0,0.01);
    display: flex;
    flex-direction: row;
    transform: translateX(-100%) translateX($card-width + 30px);
    &:deep(.card-container) {
      margin-left: 10px;
      width: $card-width !important;
      display: inline !important;
    }
  }

  &:deep(.card-container) {
    position: relative;
    display: none;
    &:nth-of-type(n) {
      width: 5px;
    }

    &:nth-last-of-type(1) {
      display:inline;
    }

    &:nth-last-of-type(2) {
      display:inline;
    }

    &:nth-last-of-type(3) {
      display:inline;
    }
  }
}

.skill--can-interact {
  border: 3px solid $select;
  cursor: pointer;
}
</style>
