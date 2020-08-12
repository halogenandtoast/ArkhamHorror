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
        <div v-if="choice.contents[1].tag === MessageType.FLAVOR_TEXT" class="intro-text">
          <h1 v-if="choice.contents[1].contents[0]">{{choice.contents[1].contents[0]}}</h1>
          <p>{{choice.contents[1].contents[1]}}</p>
        </div>
        <button @click="$emit('choose', index)">{{choice.contents[0].contents}}</button>
      </div>

      <div v-if="choice.tag === MessageType.LABEL">
        <button @click="$emit('choose', index)">{{choice.contents[0]}}</button>
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
  @Prop(String) readonly investigatorId!: string

  MessageType: any = MessageType // eslint-disable-line

  get choices() {
    return choices(this.game, this.investigatorId);
  }

  get shouldShow() {
    return this.choices.some(this.isStatusBarMessage);
  }

  isStatusBarMessage(c: Message): boolean {
    const validMessageTags = [
      MessageType.CONTINUE,
      MessageType.AFTER_DISCOVER_CLUES,
      MessageType.BEGIN_SKILL_TEST_AFTER_FAST,
      MessageType.LABEL,
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

.intro-text {
  background-color: #DFDAD8;
  padding: 10px;
  margin: 10px;
  border-radius: 2px;
  box-sizing: border-box;
  h1 {
    font-family: "Teutonic";
    font-weight: 500;
    color: #38615F;
    margin: 0;
    padding-bottom: 2px;
    margin-bottom: 10px;
    border-bottom: 1px solid #38615f;
    &::after {
      display: block;
      content: " ";
      margin-top: 2px;
      border-bottom: 1px solid #38615f;
    }
  }

  p {
    margin: 0;
  }
}
</style>
