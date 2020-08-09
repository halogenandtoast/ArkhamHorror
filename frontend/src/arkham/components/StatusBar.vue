<template>
  <section v-if="shouldShow">
    <div class="choices" v-for="(choice, index) in choices" :key="index">
      <div v-if="choice.tag === MessageType.AFTER_DISCOVER_CLUES">
        <span>You got some clues</span> <button @click="$emit('choose', index)">Continue</button>
      </div>

      <div v-if="choice.tag === MessageType.CONTINUE">
        <button @click="$emit('choose', index)">{{choice.contents}}</button>
      </div>

      <div v-if="choice.tag === MessageType.RUN && choice.contents[0].tag === MessageType.CONTINUE">
        <button @click="$emit('choose', index)">{{choice.contents[0].contents}}</button>
      </div>

      <a
        v-if="choice.tag === MessageType.BEGIN_SKILL_TEST_AFTER_FAST"
        class="button"
        @click="$emit('choose', index)"
      >
        Use <i :class="`icon${choice.contents[3]}`"></i>
      </a>
    </div>
  </section>
</template>

<script lang="ts">
import { Component, Prop, Vue } from 'vue-property-decorator';
import { choices, Game } from '@/arkham/types/Game';
import { Message, MessageType } from '@/arkham/types/Message';

@Component
export default class StatusBar extends Vue {
  @Prop(Object) readonly game!: Game

  MessageType: any = MessageType // eslint-disable-line

  get choices() {
    return choices(this.game);
  }

  get shouldShow() {
    return this.choices.some(this.isStatusBarMessage);
  }

  isStatusBarMessage(c: Message): boolean {
    const validMessageTags = [
      MessageType.CONTINUE,
      MessageType.AFTER_DISCOVER_CLUES,
      MessageType.BEGIN_SKILL_TEST_AFTER_FAST,
    ];

    switch (c.tag) {
      case MessageType.RUN:
        return this.isStatusBarMessage(c.contents[0]);
      default:
        return validMessageTags.includes(c.tag);
    }
  }
}
</script>

<style scoped lang="scss">
i {
  font-family: 'Arkham';
  speak: none;
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-transform: none;
  line-height: 1;
  -webkit-font-smoothing: antialiased;
  position: relative;

}

i.iconSkillWillpower {
  &:before {
    font-family: "Arkham";
    content: "\0041";
  }
}

i.iconSkillIntellect {
  &:before {
    font-family: "Arkham";
    content: "\0046";
  }
}

i.iconSkillCombat {
  &:before {
    font-family: "Arkham";
    content: "\0044";
  }
}

i.iconSkillAgility {
  &:before {
    font-family: "Arkham";
    content: "\0042";
  }
}

section {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 5px;
  background: #2D6153;
}

.button {
  display: inline-block;
  padding: 5px 10px;
  margin: 2px;
  background-color: #333;
  color: white;
  border: 1px solid #666;
  cursor: pointer;

  &:hover {
    background-color: #111;
  }

  &:active {
    background-color: #666;
    border-color: #111;
  }
}
</style>
