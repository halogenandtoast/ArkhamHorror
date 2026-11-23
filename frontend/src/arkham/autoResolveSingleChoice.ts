import type { Question } from '@/arkham/types/Question'

const NON_ACTIONABLE_CHOICES = new Set<string>(['Info', 'InvalidLabel'])

export function onlyActionableChoiceIndex(question: Question | null | undefined): number | null {
  let current = question
  while (current?.tag === 'QuestionLabel' || current?.tag === 'QuestionWithSource') {
    current = current.question
  }

  if (current?.tag !== 'ChooseOne') return null

  const actionableChoices = current.choices.flatMap((choice, index) =>
    NON_ACTIONABLE_CHOICES.has(choice.tag) ? [] : [index],
  )

  return actionableChoices.length === 1 ? actionableChoices[0] : null
}
