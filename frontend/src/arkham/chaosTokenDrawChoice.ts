import { MessageType, type Message } from '@/arkham/types/Message'

export function chaosTokenDrawChoiceIndex(choices: readonly Message[]): number {
  return choices.findIndex((choice) => choice.tag === MessageType.START_SKILL_TEST_BUTTON)
}
