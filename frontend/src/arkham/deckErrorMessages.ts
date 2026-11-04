export type DeckErrorTranslator = (key: string, named?: Record<string, unknown>) => string

// Older backends send English JSONError messages. Translate only known forms;
// keep unrecognized server diagnostics intact so failures remain actionable.
export function localizedDeckError(message: string, t: DeckErrorTranslator): string {
  const exact: Record<string, string> = {
    'This deck was not imported from a url, so there is nothing to sync from. Point it at one first.': 'noSyncUrl',
    'Enter the url of the deck to point this at': 'enterUrl',
    'Deck does not belong to this user': 'notOwned',
    'That investigator is not in this game': 'investigatorNotInGame',
  }
  const key = exact[message]
  if (key) return t(`deckUi.errors.${key}`)

  const fetchError = message.match(/^Could not fetch the deck at (.+)\. If it has been deleted, point this deck at one that still exists\.$/s)
  if (fetchError) return t('deckUi.errors.fetchFailed', { url: fetchError[1] })

  const readError = message.match(/^Could not read the deck at (.+?): (.*)$/s)
  if (readError) return t('deckUi.errors.readFailed', { url: readError[1], detail: readError[2] })

  const unimplemented = message.match(/^This deck contains cards that are not implemented yet: (.*)$/s)
  if (unimplemented) return t('deckUi.errors.unimplemented', { cards: unimplemented[1] })

  const upgradeError = message.match(/^Could not upgrade deck: (.*)$/s)
  if (upgradeError) return t('deckUi.errors.upgradeFailed', { detail: upgradeError[1] })

  return message
}
