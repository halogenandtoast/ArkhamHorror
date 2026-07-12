import { cardImg, imgsrc } from '@/arkham/helpers'
import type { Game } from '@/arkham/types/Game'
import type { Source } from '@/arkham/types/Source'

export function cardArt(cardCode: string, suffix: string = ''): string {
  return `${cardCode.replace(/^c/, '')}${suffix}`
}

export function cardImage(cardCode: string, suffix: string = ''): string {
  return cardImg(cardArt(cardCode, suffix))
}

export function portraitImage(cardCode: string, suffix: string = ''): string {
  return imgsrc(`portraits/${cardArt(cardCode, suffix)}.jpg`)
}

// YithianForm, HomunculusForm, and ShatteredForm portraits use the
// investigator's own id (so each player keeps a distinct portrait), while every
// other form uses the underlying card code.
export function investigatorPortrait(
  game: Game,
  investigatorId: string,
  suffix: string = ''
): string {
  const player = game.investigators[investigatorId]
  const code = (player?.form.tag === 'YithianForm' || player?.form.tag === 'HomunculusForm' || player?.form.tag === 'ShatteredForm')
    ? investigatorId
    : (player?.cardCode ?? investigatorId)
  return portraitImage(code, suffix)
}

// Resolves an in-game Source to the art code used for its card image.
// Returns null when the source has no associated card art or the referenced
// entity is no longer in play.
export function sourceCardCode(source: Source, game: Game): string | null {
  if (source.sourceTag === 'ProxySource') return sourceCardCode(source.source, game)

  if (source.tag === 'AbilitySource') {
    const [inner] = source.contents as unknown as [Source, number]
    return sourceCardCode(inner, game)
  }

  if (source.tag === 'LocationSource') {
    const location = game.locations[source.contents as string]
    if (!location) return null
    return cardArt(location.cardCode, location.revealed ? '' : 'b')
  }

  if (source.tag === 'AssetSource') {
    const asset = game.assets[source.contents as string]
    if (!asset) return null
    if (asset.flipped) {
      if (asset.cardCode === 'c90052') return '90052b'
      return null
    }
    const mutated = asset.mutated ? `_${asset.mutated}` : ''
    return cardArt(asset.cardCode, mutated)
  }

  if (source.tag === 'TreacherySource') {
    const treachery = game.treacheries[source.contents as string]
    return treachery ? cardArt(treachery.cardCode) : null
  }

  if (source.tag === 'EnemySource') {
    const enemy = game.enemies[source.contents as string]
    if (!enemy) return null
    return cardArt(enemy.cardCode, enemy.flipped ? 'b' : '')
  }

  if (source.tag === 'EventSource') {
    const event = game.events[source.contents as string]
    if (!event) return null
    const mutated = event.mutated ? `_${event.mutated}` : ''
    return cardArt(event.cardCode, mutated)
  }

  if (source.tag === 'InvestigatorSource') {
    return cardArt(source.contents as string)
  }

  if (source.tag === 'AgendaSource') {
    const id = source.contents as string
    const agenda = game.agendas[id]
    if (!agenda) return id.replace(/^c/, '')
    if (agenda.flipped) {
      if (['c03276a', 'c03279a'].includes(agenda.id)) {
        return `${agenda.id.replace(/^c/, '')}b`
      }
      return `${agenda.id.replace(/^c/, '').replace(/a$/, '')}b`
    }
    return agenda.id.replace(/^c/, '')
  }

  if (source.tag === 'ActSource') {
    return (source.contents as string).replace(/^c/, '')
  }

  return null
}
