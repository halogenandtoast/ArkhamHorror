import api from '@/api';
import { logRowDecoder, type LogRow } from '@/arkham/types/GameLog';
import { Game, GameDetails, GameDetailsEntry, gameDecoder, gameDetailsEntryDecoder } from '@/arkham/types/Game';
import { ArkhamDbDecklist, Deck, deckDecoder } from '@/arkham/types/Deck';
import { CardDef, cardDefDecoder } from '@/arkham/types/CardDef';
import { CardPortrait, CustomCard, customCardDecoder } from '@/arkham/customCards';
import { Difficulty } from '@/arkham/types/Difficulty';
import { Source } from '@/arkham/types/Source';
import { Token } from '@/arkham/types/Token';
import { DestinyDrawing } from '@/arkham/types/Question';
import { StandaloneSetting } from '@/arkham/types/StandaloneSetting';
import { CampaignLogSettings, Key, CampaignOption } from '@/arkham/types/CampaignSettings'
import { Achievement, achievementDecoder } from '@/arkham/types/Achievement'
import {
  CreateEventPost,
  EventDetails,
  EventListEntry,
  eventDetailsDecoder,
  eventListEntryDecoder,
} from '@/arkham/types/EpicEvent'
import * as NewGame from '@/arkham/types/NewGame'
import * as JsonDecoder from 'ts.data.json';
import { registerArtVariants } from '@/arkham/artVariants'

interface FetchData {
  playerId: string
  multiplayerMode: string
  // The Epic Multiplayer event this game is a group of, resolved server-side
  // regardless of how the player arrived (null for ordinary, non-event games).
  eventId: string | null
  game: Game
}

interface FetchReplay {
  totalSteps: number
  game: Game
}

export const fetchJoinGame = async (gameId: string): Promise<Game> => {
  const { data } = await api.get(`arkham/games/${gameId}/join`)
  return gameDecoder.decodePromise(data);
}

export const fetchGame = async (gameId: string, spectate = false): Promise<FetchData> => {
  const { data } = await api.get(`arkham/games/${gameId}${spectate ? '/spectate' : ''}`, {
    // Game GETs are also used to recover from missed websocket transitions.
    // A cache hit here can leave setup on an already-answered question.
    params: { _: Date.now() },
  })
  const { playerId, game, multiplayerMode, eventId } = data
  const gameData = await gameDecoder.decodePromise(game)
  return { playerId, game: gameData, multiplayerMode, eventId: eventId ?? null }
}

/* The "did anything happen?" probe: a single Int column, no game JSON, no lock.
 * fetchGame is the most expensive endpoint we have, so a poller asks this first
 * and only pays for the whole game when the step actually moved. */
export const fetchGameStep = async (gameId: string): Promise<number> => {
  const { data } = await api.get(`arkham/games/${gameId}/step`, { params: { _: Date.now() } })
  return data.step
}

export const fetchGameReplay = async (gameId: string, step: number): Promise<FetchReplay> => {
  const { data } = await api.get(`arkham/games/${gameId}/replay/${step}`)
  const { totalSteps, game } = data
  const gameData = await gameDecoder.decodePromise(game)
  return { game: gameData, totalSteps }
}

export interface AppNotification {
  id: number;
  body: string;
  createdAt: string;
}

export const fetchNotifications = async (): Promise<AppNotification[]> => {
  const { data } = await api.get('notifications')
  return data
}

/* --------------------------------------------------- admin: notifications ---

   The home-page banner everybody sees. There is no draft state on the server --
   the public read has no filter -- so creating one publishes it, and taking one
   down is a delete. Editing reaches whoever is still seeing it, but not anybody
   who already dismissed it: dismissal is per-browser and keyed by row id. */

export const fetchAdminNotifications = async (): Promise<AppNotification[]> => {
  const { data } = await api.get('admin/notifications')
  return data
}

/* `createdAt` is what the list is ordered by, so it is settable: without it there
 * is no way to put a new notification above one that is already up. */
export const createNotification = async (
  body: string,
  createdAt?: string,
): Promise<AppNotification> => {
  const { data } = await api.post('admin/notifications', { body, createdAt })
  return data
}

// Only what is sent is written, so re-dating one does not resend its body.
export const updateNotification = async (
  id: number,
  changes: { body?: string; createdAt?: string },
): Promise<AppNotification> => {
  const { data } = await api.put(`admin/notifications/${id}`, changes)
  return data
}

export const deleteNotification = async (id: number): Promise<void> => {
  await api.delete(`admin/notifications/${id}`)
}

/* ----------------------------------------------------------- admin: users ---

   Feature grants, for rolling one out a few accounts at a time. With no query
   the server answers with every account that has a grant on it, which is the
   list of who is already in; with one it matches username and email alike,
   which is how somebody new is found. */

export interface AdminUser {
  id: number
  username: string
  email: string
  admin: boolean
  features: string[]
}

export interface AdminUsers {
  allFeatures: string[]
  users: AdminUser[]
}

export const fetchAdminUsers = async (query?: string): Promise<AdminUsers> => {
  const { data } = await api.get('admin/users', { params: query ? { q: query } : {} })
  return data
}

/* The whole list, not a toggle: what is sent is what the account ends up with,
 * so a stale page cannot flip something it was not showing. */
export const updateUserFeatures = async (id: number, features: string[]): Promise<AdminUser> => {
  const { data } = await api.put(`admin/users/${id}/features`, { features })
  return data
}

/* What an account has been playing. Active or not is decided by the server from
 * the game's own state, not from whether a room is open for it: a room is who
 * is connected this second, and an abandoned game is still in progress.
 *
 * `total` is every game on the account, so a list capped at the server's limit
 * can say it is capped rather than reading as the whole of it. */
export interface AdminUserGame {
  active: boolean
  createdAt: string
  updatedAt: string
  game: GameDetailsEntry
}

export interface AdminUserGames {
  total: number
  games: AdminUserGame[]
}

const adminUserGameDecoder = JsonDecoder.object<AdminUserGame>(
  {
    active: JsonDecoder.boolean(),
    createdAt: JsonDecoder.string(),
    updatedAt: JsonDecoder.string(),
    game: gameDetailsEntryDecoder,
  },
  'AdminUserGame',
)

export const fetchAdminUserGames = async (id: number): Promise<AdminUserGames> => {
  const { data } = await api.get(`admin/users/${id}/games`)
  return JsonDecoder.object<AdminUserGames>(
    {
      total: JsonDecoder.number(),
      games: JsonDecoder.array(adminUserGameDecoder, 'AdminUserGame[]'),
    },
    'AdminUserGames',
  ).decodePromise(data)
}

export const findGame = async (playerId: string): Promise<GameDetailsEntry> => {
  const { data } = await api.get(`admin/games/find/${playerId}`)
  return gameDetailsEntryDecoder.decodePromise(data)
}

export const fetchGames = async (): Promise<GameDetailsEntry[]> => {
  const { data } = await api.get('arkham/games')
  return JsonDecoder.array(gameDetailsEntryDecoder, 'GameEntryDetails[]').decodePromise(data)
}

/* Games whose players have said they are happy to be watched. Already filtered
 * server-side to games in play that you have no seat in, so the home page shows
 * what comes back as-is. */
export const fetchSpectatableGames = async (): Promise<GameDetails[]> => {
  const { data } = await api.get('arkham/games/spectatable')
  const entries = await JsonDecoder.array(gameDetailsEntryDecoder, 'GameEntryDetails[]').decodePromise(data)
  return entries.filter((entry) => entry.tag === 'game') as GameDetails[]
}

/* The "has the list changed?" probe, for the same reason fetchGameStep exists:
 * the list above decodes a whole game per row, so the watch page asks for the
 * ids first and pays for the list only when the set of them moves. */
export const fetchSpectatableGameIds = async (): Promise<string[]> => {
  const { data } = await api.get('arkham/games/spectatable/digest', { params: { _: Date.now() } })
  return JsonDecoder.array(JsonDecoder.string(), 'ArkhamGameId[]').decodePromise(data)
}


export const fetchDecks = async (): Promise<Deck[]> => {
  const { data } = await api.get('arkham/decks')
  return JsonDecoder.array(deckDecoder, 'ArkhamDeck[]').decodePromise(data);
}

export const fetchDeck = async (deckId: string): Promise<Deck> => {
 const { data } = await api.get(`arkham/decks/${deckId}`)
 return deckDecoder.decodePromise(data)
}

export type CardPoolMode = 'player' | 'campaign' | 'both'

/* `allSides` keeps the back of a double-sided card, which the listing otherwise
 * drops in favour of its front. Only the debug card picker wants them: it names a
 * card by its def, and a back such as Gold Mining Revenues has no front that could
 * stand in for it. */
export const fetchCards = async (
  cardPool: CardPoolMode | boolean = 'player',
  allSides = false,
): Promise<CardDef[]> => {
  const mode: CardPoolMode = cardPool === true ? 'both' : cardPool === false ? 'player' : cardPool
  const params = [
    ...(mode === 'player' ? [] : ['includeEncounter', `cardPool=${mode}`]),
    ...(allSides ? ['allSides'] : []),
  ]
  const query = params.length === 0 ? '' : `?${params.join('&')}`
  const { data } = await api.get(`arkham/cards${query}`)
  const cards = await JsonDecoder.array(cardDefDecoder, 'ArkhamCardDef[]').decodePromise(data)
  registerArtVariants(cards)
  return cards
}

export const fetchHomebrewCards = async (): Promise<CardDef[]> => {
  const { data } = await api.get('arkham/homebrew/cards')
  const cards = await JsonDecoder.array(cardDefDecoder, 'ArkhamHomebrewCardDef[]').decodePromise(data)
  registerArtVariants(cards)
  return cards
}

export const setDeckOverlay = async (deckId: string, overlay: any): Promise<void> => {
  await api.put(`arkham/decks/${deckId}/overlay`, overlay)
}

export const removeDeckOverlay = async (deckId: string): Promise<void> => {
  await api.delete(`arkham/decks/${deckId}/overlay`)
}

export const fetchTraits = async (): Promise<[string, string][]> => {
  const { data } = await api.get('arkham/traits')
  return data
}

export type StoredCustomCard = {
  id: string
  setId: string
  cardCode: string
  def: any
  art: string | null
  updatedAt: string
}

/* A marker an investigator expansion brings with it: the token its cards place
 * on themselves. `slug` is what the engine stores and what a card's keyword
 * names; `name` is what a player reads; `image` is what it is drawn with, and a
 * token without one falls back to a plain marker. */
export type CustomSetToken = {
  slug: string
  name: string
  image?: string | null
}

/* One thing a keyword asks each card that prints it to fill in.
 *
 * `kind` only decides which editor the builder puts there; the engine just
 * substitutes whatever was filled in. `required` it does read: a card that
 * leaves a required slot empty gets nothing from the keyword, rather than a
 * template with a hole in it that quietly fails to decode. */
export type KeywordSlot = {
  name: string
  label: string
  kind: 'window' | 'criterion' | 'text' | 'steps' | 'value'
  required: boolean
}

/* What a keyword does, as templates in the shapes the engine already runs.
 *
 * `abilities`, `modifiers` and `handlers` are appended to the card's own, with
 * `$slot` replaced by what that card filled in. `activeWhen` gates the card's
 * OWN result -- the modifiers and abilities on its `_keywords` entry -- which
 * is how "that benefit remains active until the card leaves play" is said once
 * for every card that prints the keyword. */
export type KeywordBehavior = {
  abilities: any[]
  modifiers: any[]
  handlers: any[]
  activeWhen?: any
}

/* A keyword an investigator expansion prints on its own cards.
 *
 * `description` is the rules text as the box prints it. A keyword with no
 * `behavior` is rules text and nothing more -- the cards that print it say what
 * they do in the ordinary way, and the description is there so a player can
 * read what the word means. One WITH a behavior is run by the engine, which
 * knows nothing about the keyword beyond what is written here. */
export type CustomSetKeyword = {
  name: string
  description: string
  token?: string | null
  slots: KeywordSlot[]
  behavior?: KeywordBehavior | null
}

/* A set is what a card belongs to: the unit you build, export, and hand to
 * someone else. Every card has one. */
export type StoredCustomCardSet = {
  id: string
  name: string
  /* What the set is, in its author's words. Seeds the marketplace listing when
   * the set is published, and is the one thing a set says about itself beyond
   * its name. */
  description: string | null
  /* Where the set lives in the world: the post announcing it, the thread it is
   * discussed in. Seeds the listing alongside the description. Always an http
   * address -- the server refuses anything else, so this is safe to put in an
   * anchor. */
  url: string | null
  // The pack id an imported set came from, so re-importing that pack replaces
  // this set rather than making a second copy of it. Null for a set made here.
  sourceCode: string | null
  /* Whether the set is an investigator expansion: a box, rather than a handful
   * of cards. The three fields below belong to one and are only asked about
   * when this is set. */
  investigatorExpansion: boolean
  // The expansion symbol. Shown in the marketplace; nothing in a game reads it.
  icon: string | null
  /* The markers this set's cards place on themselves. Counters, not chaos
   * tokens -- a chaos token belongs to a scenario. */
  tokens: CustomSetToken[]
  // The keywords this set's cards print, and what each one means.
  keywords: CustomSetKeyword[]
  cardCount: number
  updatedAt: string
  /* Whether this set follows somebody else's — a marketplace listing, or a link
   * somebody sent you — and so cannot be edited. Its cards, its details and a
   * file imported over it are all refused; "Add to my editor" is the way out,
   * and makes it an ordinary set of yours. */
  locked: boolean
  /* The marketplace listing this set follows. All three are null on a copy that
   * was imported rather than subscribed, and on one since taken into the
   * editor: both are yours, and a listing has nothing more to say about either. */
  publishedCardSetId: string | null
  subscribedVersion: number | null
  latestVersion: number | null
  /* Your own marketplace listing for this set, if you have submitted it, and
   * where that submission stands. Distinct from `publishedCardSetId`, which is a
   * listing of somebody else's that this set is a copy of. */
  submittedCardSetId: string | null
  submittedVersion: number | null
  submissionStatus: ReviewStatus | null
  // Only ever set on a denial, and the one thing the author has to read.
  submissionReason: string | null
  // The version of this set that is in the marketplace now, which is not
  // necessarily the one last submitted.
  approvedVersion: number | null
  /* The secret in this set's share link, if it is being shared, and how many
   * people have opened it. Carried here so a row can say the set is shared and
   * hand over the link without a request per set. */
  shareToken: string | null
  shareCount: number
  /* The share link this set follows, for a set somebody handed you — the other
   * direction from `shareToken`, and the way back to the page that can take the
   * author's newest cards. Null on a copy that was imported rather than
   * subscribed, and once they stop sharing or remove you, because there is then
   * nothing at the other end of it. */
  sharedToken: string | null
  // Who shared it — the one thing such a copy has to say for itself that the
  // rest of the row cannot.
  sharedBy: string | null
  // The author has changed that set since you took it.
  sharedBehind: boolean
}

/* Where a submission stands. A set is listed only once a version of it is
 * approved, and every version is reviewed on its own. */
export type ReviewStatus = 'pending' | 'approved' | 'denied' | 'none'

export const fetchCustomCardSets = async (): Promise<StoredCustomCardSet[]> => {
  const { data } = await api.get('arkham/custom-card-sets')
  return data
}

export const createCustomCardSet = async (
  name: string,
  description?: string | null,
  url?: string | null,
): Promise<StoredCustomCardSet> => {
  const { data } = await api.post('arkham/custom-card-sets', { name, description, url })
  return data
}

/* Everything about a set in one call, because it is one form.
 *
 * Every field but the name is three-valued on the wire: leaving the key out
 * keeps whatever is stored, `null` clears it. So a caller that only means to
 * rename passes nothing else rather than passing null, which would silently
 * wipe the rest.
 *
 * Renaming, or changing what the set declares, stops it following a published
 * one -- the cards carry copies, so those are the cards changing. Changing only
 * the description, the link or the icon does not. */
export const updateCustomCardSet = async (
  id: string,
  changes: {
    name: string
    description?: string | null
    url?: string | null
    investigatorExpansion?: boolean
    icon?: string | null
    tokens?: CustomSetToken[]
    keywords?: CustomSetKeyword[]
  },
): Promise<StoredCustomCardSet> => {
  const { data } = await api.put(`arkham/custom-card-sets/${id}`, changes)
  return data
}

// Takes the set's cards with it.
export const deleteCustomCardSet = async (id: string): Promise<void> => {
  await api.delete(`arkham/custom-card-sets/${id}`)
}

/* Take a followed set into your editor: stop following it, keep the cards.
 *
 * The way out of a locked copy of a published set. Nothing is fetched — the
 * cards are already here — and what changes is only that this copy stops claiming
 * to be what was published, so it can be edited, published and shared like any
 * set of yours. Subscribing again from the listing puts it back.
 *
 * Refused for a copy taken from a share link: that set is unpublished and stays
 * the author's. */
export const unlockCustomCardSet = async (id: string): Promise<StoredCustomCardSet> => {
  const { data } = await api.post(`arkham/custom-card-sets/${id}/unlock`, {})
  return data
}

/* One call, one set: whatever the set held before is replaced by exactly these
 * cards, so importing a corrected pack cannot leave the cards it dropped
 * behind. Matched to an existing set by `sourceCode` when there is one, by
 * name otherwise. */
export const importCustomCardSet = async (payload: {
  name: string
  // A file written before descriptions existed carries none, and the server
  // leaves the set's own alone rather than blanking it.
  description?: string | null
  url?: string | null
  sourceCode: string | null
  /* What the set declares. A file written before expansions existed carries
   * none of it, and the server falls back to the copies stamped on the cards. */
  investigatorExpansion?: boolean
  icon?: string | null
  tokens?: CustomSetToken[]
  keywords?: CustomSetKeyword[]
  cards: { def: any; art: string | null }[]
}): Promise<{ set: StoredCustomCardSet; cards: StoredCustomCard[] }> => {
  const { data } = await api.post('arkham/custom-card-sets/import', payload)
  return { set: data.set, cards: data.cards.map(toStoredCustomCard) }
}

/* ----------------------------------------------------------------- shares ---

   A link that hands an unpublished set to a few people. The marketplace is the
   other way to do this and the wrong one for a set that is not finished: it is
   public, every version is reviewed before anyone can take it, and what
   playtesting wants is for the people trying the cards to have whatever changed
   this afternoon. Nothing here is versioned -- a share hands out the set as it
   stands, and taking it again is how a friend catches up. */

export type CardSetShare = {
  setId: string
  // The secret in the url. `shareUrl` is what turns it into a link to send.
  token: string
  createdAt: string
  // When the link was last minted, so rotating it can be seen to have worked.
  updatedAt: string
  people: CardSetShareAccess[]
}

/* One person the link reached. A share is a url, so there is no invitation
 * addressed to anybody: who has opened it is the whole of who has access. */
export type CardSetShareAccess = {
  id: string
  name: string
  // When they last took a copy. Null for somebody who has only looked.
  takenAt: string | null
  // Whether you have changed the set since they took it.
  behind: boolean
  revoked: boolean
  firstSeenAt: string
  lastSeenAt: string
}

// A shared set as the person holding the link sees it: the whole set, and what
// they have of it.
export type SharedCardSet = {
  name: string
  description: string | null
  url: string | null
  icon: string | null
  investigatorExpansion: boolean
  author: string
  // You following your own link, which has nothing to take.
  mine: boolean
  cards: CustomCard[]
  // When the author last changed the set, which is what "new" is measured from.
  updatedAt: string
  // Your own copy, if you have taken one.
  setId: string | null
  /* The marketplace listing for this set, once the author has published it and a
   * version has been approved. It changes what you may do: a shared set stays
   * the author's, but a published one is public, so subscribing to the listing
   * gives an ordinary marketplace copy — editable via "Add to my editor", and
   * exportable. Subscribing there adopts the copy you already have. */
  listing: string | null
  /* Whether that copy follows this set, and so is locked, rather than being an
   * ordinary set of yours imported from it. */
  subscribed: boolean
  /* The author has changed the set since you took it — the whole of whether
   * there is anything to do here. Subscribing again is how it catches up, and
   * when it is level there is nothing to take and no button for it. */
  behind: boolean
}

// The link to send. Hash history, so the route lives after the `#`.
export const shareUrl = (token: string) =>
  `${window.location.origin}${window.location.pathname}#/card-builder/shared/${token}`

// 404 for a set that is not being shared, which is how the panel knows to offer
// to start.
export const fetchCardSetShare = async (setId: string): Promise<CardSetShare> => {
  const { data } = await api.get(`arkham/custom-card-sets/${setId}/share`)
  return data
}

/* Start sharing, or -- with `rotate` -- mint a new link for a set already
 * shared, which makes a link that got out worthless. Asking without `rotate`
 * hands back the link that is there rather than making a second one. Rotating
 * keeps the people: they are the record of who took the set, which is still
 * true of them. */
export const shareCardSet = async (setId: string, rotate = false): Promise<CardSetShare> => {
  const { data } = await api.post(`arkham/custom-card-sets/${setId}/share`, { rotate })
  return data
}

/* Stop sharing: the link stops working and the list of who had it goes with it.
 * What anyone already took stays in their library -- those are their cards now,
 * possibly in a deck or a game. */
export const unshareCardSet = async (setId: string): Promise<void> => {
  await api.delete(`arkham/custom-card-sets/${setId}/share`)
}

/* Cut one person off, or let them back in, without disturbing the link everyone
 * else is holding. Takes nothing away from them: all it stops is reading the set
 * and taking it again. */
export const setCardSetShareAccess = async (
  setId: string,
  accessId: string,
  revoked: boolean,
): Promise<CardSetShare> => {
  const { data } = await api.put(`arkham/custom-card-sets/${setId}/share/${accessId}`, {
    revoked,
  })
  return data
}

/* The set behind a link. Reading it is what grants access -- there is no
 * invitation to accept -- so this is also what puts you on the author's list. */
export const fetchSharedCardSet = async (token: string): Promise<SharedCardSet> => {
  const { data } = await api.get(`arkham/shared-card-sets/${token}`)
  return data
}

/* The one way to take a shared set: subscribe. The copy is locked, so it stays
 * what the author shared, and subscribing again is how it catches up.
 *
 * The marketplace's other way — importing a copy of your own, to edit and keep —
 * is not offered for a shared set, because that set is unpublished. It was handed
 * to a few people to try, not given away, and a copy somebody can edit is a copy
 * they can export and pass on. */
export const subscribeToSharedCardSet = async (token: string): Promise<SharedCardSet> => {
  const { data } = await api.post(`arkham/shared-card-sets/${token}/subscribe`, {})
  return data
}

/* ------------------------------------------------------------ marketplace ---

   Published sets: a set its author has put up, and other people's copies
   following it. A copy stops following as soon as it is edited, so "subscribed"
   is a fact about the server's records rather than a switch anyone sets. */

export type PublishedCardSet = {
  id: string
  name: string
  /* What the set is. The listing's own copy, not the version's: the author can
   * rewrite it at any time without republishing and without it being reviewed
   * again, because the cards have not changed. */
  description: string | null
  /* Where the set lives in the world, for the one thing a listing cannot say
   * for itself. The listing's own, like the description. */
  url: string | null
  /* The expansion symbol, copied onto the listing when the set was published,
   * so a symbol can be drawn beside a name without loading a card. */
  icon: string | null
  // Whether this is an investigator expansion rather than a handful of cards.
  investigatorExpansion: boolean
  author: string
  mine: boolean
  /* Whether the author is an admin, which is what the marketplace calls
   * officially supported. Read off the account rather than stamped on the
   * listing, so a set stops claiming support when its author stops being able to
   * give it. */
  official: boolean
  // The newest approved version: the one described below, and the one an import
  // gets. Zero for a set of your own that has never passed review, which is the
  // only way an unapproved set is listed at all.
  latestVersion: number
  // Where the newest submission stands. Only ever anything but 'approved' on
  // your own listing, since nobody else's is shown until it has passed.
  reviewStatus: ReviewStatus
  pendingVersion: number | null
  denialReason: string | null
  cardCount: number
  // What the author said about the newest version, if they said anything.
  note: string | null
  // The first few cards, so a listing can show what is in the set.
  preview: { def: any; art: string | null }[]
  likes: number
  liked: boolean
  updatedAt: string
  // The version your own copy is on, if you have one that still follows this.
  subscribedVersion: number | null
  /* Every version of your own listing, newest first. Empty on anybody else's:
   * what someone submitted and what was turned down is theirs to see. */
  versions: PublishedCardSetVersionSummary[]
}

/* One version in an author's history of their own listing: what they said about
 * it and what came of it. No cards -- those are a request of their own. */
export type PublishedCardSetVersionSummary = {
  version: number
  note: string | null
  status: ReviewStatus
  reason: string | null
  // Whether this is the version the marketplace is handing out.
  live: boolean
  createdAt: string
}

/* One set in full: its listing plus every card in the version. The listing comes
 * along so a page showing one set needs one request. */
export type PublishedCardSetVersion = {
  listing: PublishedCardSet
  version: number
  name: string
  note: string | null
  cards: { def: any; art: string | null }[]
  createdAt: string
}

/* The marketplace. `mine` narrows it to your own listings, which is a page of
 * its own: a listing outlives the set it was published from, so one whose set
 * has since been deleted is only reachable that way. */
export const fetchPublishedCardSets = async (mine = false): Promise<PublishedCardSet[]> => {
  const { data } = await api.get(`arkham/published-card-sets${mine ? '?mine=true' : ''}`)
  return data
}

/* Rewrite what your listing says about itself. Only the description and the
 * link: the name and the cards belong to the version, and changing those is
 * publishing a new one. Nothing is re-reviewed, and the server writes the same
 * text back onto your own copy of the set so the two places you can edit it
 * agree.
 *
 * Both are three-valued here too -- a key left out keeps what is stored -- so
 * this takes a patch rather than two positional arguments that would have to
 * be spelled out to change one. */
export const updatePublishedCardSet = async (
  id: string,
  changes: { description?: string | null; url?: string | null },
): Promise<PublishedCardSet> => {
  const { data } = await api.put(`arkham/published-card-sets/${id}`, changes)
  return data
}

export const fetchPublishedCardSet = async (
  id: string,
  version?: number,
): Promise<PublishedCardSetVersion> => {
  const query = version === undefined ? '' : `?version=${version}`
  const { data } = await api.get(`arkham/published-card-sets/${id}${query}`)
  return data
}

/* Submit the set as it stands. Each call is a new version, kept whole, so what a
 * reviewer looked at is what anyone importing it gets. Nothing is listed by this:
 * the version goes into the review queue, and the marketplace keeps showing
 * whichever version was last approved.
 *
 * Unless the caller is an admin -- then the server records the version as
 * approved by them and lists it at once, and the listing comes back with
 * `reviewStatus: 'approved'` and the new `latestVersion`. The server decides
 * that from the account, not from anything sent here.
 *
 * `notify` is whether to email the author the decision, and is ignored for a
 * publish that is already decided. Submitting again while something is still
 * waiting replaces it rather than queueing a second thing. */
/* The listing's blurb and link are not sent: they are the set's, and the server
 * reads them off it. Only what is about this one submission goes here. */
export const publishCustomCardSet = async (
  id: string,
  note: string | null,
  notify: boolean,
): Promise<PublishedCardSet> => {
  const { data } = await api.post(`arkham/custom-card-sets/${id}/publish`, { note, notify })
  return data
}

export const unpublishCardSet = async (id: string): Promise<void> => {
  await api.delete(`arkham/published-card-sets/${id}`)
}

/* Take a published set into your collection and follow it from then on: the copy
 * is locked, so it stays what was published, and subscribing again is how it
 * catches up. The import happens on the server, so the cards never round-trip
 * through here. */
export const subscribeToCardSet = async (
  id: string,
  version?: number,
): Promise<PublishedCardSet> => {
  const { data } = await api.post(`arkham/published-card-sets/${id}/subscribe`, {
    version: version ?? null,
  })
  return data
}

/* Take it as your own instead: the same cards, editable, frozen at this version
 * and hearing nothing more from the listing. One replaces the other — a card
 * code is unique per account, so nobody holds both. */
export const importCardSet = async (
  id: string,
  version?: number,
): Promise<PublishedCardSet> => {
  const { data } = await api.post(`arkham/published-card-sets/${id}/import`, {
    version: version ?? null,
  })
  return data
}

/* Liking is a row existing, so these are idempotent: liking twice is liking, and
 * unliking something you never liked is already true. Both hand back the listing
 * with its new count. */
export const likeCardSet = async (id: string): Promise<PublishedCardSet> => {
  const { data } = await api.post(`arkham/published-card-sets/${id}/like`, {})
  return data
}

export const unlikeCardSet = async (id: string): Promise<PublishedCardSet> => {
  const { data } = await api.delete(`arkham/published-card-sets/${id}/like`)
  return data
}

// Bring a subscribed set up to the newest approved version.
export const syncCustomCardSet = async (id: string): Promise<PublishedCardSet> => {
  const { data } = await api.post(`arkham/custom-card-sets/${id}/sync`, {})
  return data
}

/* ------------------------------------------------- reviewing submissions ---

   Admin only, and gated server side: everything under /admin requires the admin
   flag, so these 403 for anybody else rather than relying on the page being
   hidden. */

export type CardSetSubmission = {
  id: string
  publishedCardSetId: string
  setName: string
  // What the author says the set is, so a reviewer reads the same blurb a
  // browser would.
  setDescription: string | null
  // Where they say it lives, which is often the only way to check a set is
  // theirs to publish.
  setUrl: string | null
  author: string
  // So a reviewer can reach the author about something the form cannot say.
  authorEmail: string
  version: number
  status: ReviewStatus
  // What the author said about this version when they submitted it.
  note: string | null
  // Whether they asked to be emailed the decision.
  notify: boolean
  reason: string | null
  reviewedBy: string | null
  reviewedAt: string | null
  submittedAt: string
  cardCount: number
  preview: { def: any; art: string | null }[]
  // What is in the marketplace now, so an update reads as an update.
  approvedVersion: number | null
}

export type CardSetSubmissionDetail = {
  submission: CardSetSubmission
  cards: { def: any; art: string | null }[]
}

/* The review queue. No status means the pending ones, oldest first, which is what
 * there is to do. */
export const fetchCardSetSubmissions = async (
  status?: 'pending' | 'approved' | 'denied' | 'all',
): Promise<CardSetSubmission[]> => {
  const query = status === undefined ? '' : `?status=${status}`
  const { data } = await api.get(`admin/card-set-submissions${query}`)
  return data
}

// One submission with every card in it, which is what reviewing it takes.
export const fetchCardSetSubmission = async (id: string): Promise<CardSetSubmissionDetail> => {
  const { data } = await api.get(`admin/card-set-submissions/${id}`)
  return data
}

export const approveCardSetSubmission = async (id: string): Promise<CardSetSubmission> => {
  const { data } = await api.post(`admin/card-set-submissions/${id}/approve`, {})
  return data
}

/* --------------------------------------------------------------- game stats ---

   Admin only. Read off a materialized view rather than computed per request:
   every fact here lives inside a 28kB jsonb blob per game, which costs ~0.7ms
   each to extract, so the panel reads pre-extracted columns and shows how stale
   they are. `populated` is false until the view has been built for the first
   time. */

export type StatTotals = {
  games: number
  campaignGames: number
  standaloneGames: number
  finished: number
  inProgress: number
  // Never got past deck selection: games nobody actually sat down to.
  neverStarted: number
  players: number
  // Finished games whose outcome is known, and those that ended before outcomes
  // were tracked and have not been reconstructed yet.
  won: number
  // Of the wins, how many the Campaign Guide prints as "The investigators win
  // the campaign…?" rather than stating outright.
  wonUncertain: number
  lost: number
  outcomeUnknown: number
}

export type CampaignStat = {
  id: string
  games: number
  finished: number
  inProgress: number
  neverStarted: number
  // Summed over every run of the campaign, which is what makes a success rate
  // mean something for a campaign nobody has finished yet.
  scenariosPlayed: number
  scenariosWon: number
  // Runs that reached a winning ending, a losing one, and ones that finished
  // with no outcome on record.
  won: number
  lost: number
  outcomeUnknown: number
  players1: number
  players2: number
  players3: number
  players4: number
  easy: number
  standard: number
  hard: number
  expert: number
}

export type StandaloneStat = {
  id: string
  games: number
  finished: number
  won: number
  lost: number
  isSideStory: boolean
}

export type CountStat = { key: string; count: number }

export type SideStoryInCampaign = {
  campaignId: string
  scenarioId: string
  count: number
}

export type ScenarioOutcome = { id: string; played: number; won: number }

export type CampaignProgress = {
  campaignId: string
  // How many scenarios these runs have completed. The fall from one to the next
  // is where people stop.
  scenarios: number
  games: number
}

/* Which ending a campaign's runs stopped on. `resolution` is null for a scenario
 * lost or resigned out of, the one ending with no number. Only games with a known
 * outcome appear. */
export type CampaignEnding = {
  campaignId: string
  scenarioId: string
  resolution: number | null
  result: 'Won' | 'Lost'
  certainty: 'Certain' | 'Uncertain'
  /** i18n key for the clause the guide attaches to the verdict, if any. */
  note: string | null
  count: number
}

export type AchievementStat = { id: string; earned: number; inProgress: number }

export type GameStats = {
  populated: boolean
  refreshedAt: string | null
  refreshDurationMs: number | null
  totals: StatTotals
  campaigns: CampaignStat[]
  standalones: StandaloneStat[]
  playerCounts: CountStat[]
  difficulties: CountStat[]
  variants: CountStat[]
  sideStoriesInCampaigns: SideStoryInCampaign[]
  scenarioOutcomes: ScenarioOutcome[]
  campaignProgress: CampaignProgress[]
  campaignEndings: CampaignEnding[]
  achievements: AchievementStat[]
  // Users who have earned at least one achievement: the denominator that does
  // not count people who never played as having failed to earn it.
  achievementUsers: number
  monthly: CountStat[]
  // Campaign and scenario code to title, so the panel names things without
  // loading the card catalogue.
  names: Record<string, string>
}

export const fetchGameStats = async (): Promise<GameStats> => {
  const { data } = await api.get('admin/game-stats')
  return data
}

// Rebuilds the view and returns the fresh numbers in the same response.
export const refreshGameStats = async (): Promise<GameStats> => {
  const { data } = await api.post('admin/game-stats/refresh', {})
  return data
}

/* One pass of the outcomes backfill, for games that finished before the engine
 * recorded how they ended. Bounded per call, so the panel repeats it until
 * `remaining` stops falling. `undecided` is games it looked at and would not
 * guess about — a campaign abandoned between scenarios with nothing resolved. */
export type OutcomesBackfill = {
  computed: number
  undecided: number
  remaining: number
  durationMs: number
}

export const backfillGameOutcomes = async (): Promise<OutcomesBackfill> => {
  const { data } = await api.post('admin/game-stats/outcomes', {})
  return data
}

/* What the nightly archive sweep has reclaimed, for the admin panel.
 *
 * `candidates` is capped at `candidateCap`: the backlog is a predicate over the
 * whole games table, and the page only needs to know whether it is large. */
export interface ArchiveStatsRow {
  gameId: string
  gameName: string
  state: GameArchiveState
  steps: number
  bytes: number
  archivedAt: string
  gameUpdatedAt: string
  restoredAt: string | null
  error: string | null
}

export interface ArchiveStats {
  enabled: boolean
  /* Whether the nightly job runs. Separate from `enabled`: a configured bucket
   * makes archiving possible, this is what lets a job pick games itself. */
  sweepEnabled: boolean
  bucket: string | null
  afterMonths: number
  batchSize: number
  pauseSeconds: number
  archived: number
  restoring: number
  restored: number
  failed: number
  stepsOffloaded: number
  stepsEverArchived: number
  bytesStored: number
  archivedLastDay: number
  restoredLastWeek: number
  candidates: number
  candidateCap: number
  lastSweepAt: string | null
  lastSweepMs: number | null
  recent: ArchiveStatsRow[]
}

/* Archive one game, chosen by hand -- the way to do it the first time, since a
 * sweep picks its own games by age.
 *
 * Synchronous on the server: the response carries what was stored, or why it
 * could not be. `force` is needed for a game played in the last day, which is a
 * guard against a mistyped id rather than against the operation. */
export const archiveGameNow = async (
  gameId: string,
  force = false,
): Promise<{ game: string; steps: number; bytes: number; key: string }> => {
  const { data } = await api.post(
    `admin/archives/${gameId}${force ? '?force=true' : ''}`,
    {},
    // A big game is minutes of export, upload and read-back.
    { timeout: 600000 },
  )
  return data
}

export const fetchArchiveStats = async (): Promise<ArchiveStats> => {
  const { data } = await api.get('admin/archives')
  return data
}

/* Start a sweep now.
 *
 * Rarely needed: the server claims a run a minute after every replica starts,
 * so a deploy kicks one off by itself. Answers as soon as it has started -- a
 * run drains the whole backlog, which is hours -- and 409s if one already ran in
 * the last hour, which `force` overrides. */
export const runArchiveSweep = async (force = false): Promise<void> => {
  await api.post(`admin/archives/run${force ? '?force=true' : ''}`, {})
}

// The reason is required: a denial the author cannot act on is worse than silence.
export const denyCardSetSubmission = async (
  id: string,
  reason: string,
): Promise<CardSetSubmission> => {
  const { data } = await api.post(`admin/card-set-submissions/${id}/deny`, { reason })
  return data
}

/* ------------------------------------------------- everyone's card sets ---

   Admin only. The other half of the review queue: every set on the instance,
   not just the ones somebody chose to publish. Paginated, because this grows
   with every account, and scoped, because most sets are copies -- a
   subscription or a share link leaves a row per subscriber. */

export type AdminCardSetScope = 'originals' | 'published' | 'copies' | 'all'

/* What a copy follows. Absent on a set its owner actually made, which is what
 * the `originals` scope selects. */
export type AdminCardSetFollows = 'marketplace' | 'link'

export type AdminCardSet = {
  id: string
  name: string
  description: string | null
  url: string | null
  icon: string | null
  investigatorExpansion: boolean
  // Set when the set came from an arkham.build pack rather than being built here.
  sourceCode: string | null
  userId: string
  author: string
  // Shown because it is searched: a row that matched on email has to say so.
  authorEmail: string
  cardCount: number
  createdAt: string
  updatedAt: string
  follows: AdminCardSetFollows | null
  // This set's own marketplace listing, if its author has published it.
  publishedCardSetId: string | null
  approvedVersion: number | null
  submissionVersion: number | null
  submissionStatus: ReviewStatus | null
  preview: { def: any; art: string | null }[]
}

export type AdminCardSetPage = {
  sets: AdminCardSet[]
  // Under the same filters as the rows, so the page count works out reachable.
  total: number
  page: number
  perPage: number
}

export const fetchAdminCustomCardSets = async (params: {
  page?: number
  scope?: AdminCardSetScope
  q?: string
}): Promise<AdminCardSetPage> => {
  const query = new URLSearchParams()
  if (params.page !== undefined) query.set('page', String(params.page))
  if (params.scope !== undefined) query.set('scope', params.scope)
  if (params.q) query.set('q', params.q)
  const suffix = query.size === 0 ? '' : `?${query}`
  const { data } = await api.get(`admin/custom-card-sets${suffix}`)
  return data
}

// One set with every card in it, which is what actually looking at one takes.
export const fetchAdminCustomCardSet = async (
  id: string,
): Promise<{ set: AdminCardSet; cards: { def: any; art: string | null }[] }> => {
  const { data } = await api.get(`admin/custom-card-sets/${id}`)
  return data
}

/* A card row names its set the way the entity spells the field; `setId` is what
 * it is called here. */
const toStoredCustomCard = (row: any): StoredCustomCard => ({
  id: row.id,
  setId: row.customCardSetId,
  cardCode: row.cardCode,
  def: row.def,
  art: row.art,
  updatedAt: row.updatedAt,
})

export const fetchCustomCardLibrary = async (): Promise<StoredCustomCard[]> => {
  const { data } = await api.get('arkham/custom-cards')
  return data.map(toStoredCustomCard)
}

export const saveCustomCard = async (card: {
  setId: string
  def: any
  art: string | null
}): Promise<StoredCustomCard> => {
  const { data } = await api.post('arkham/custom-cards', card)
  return toStoredCustomCard(data)
}

export const deleteCustomCard = async (id: string): Promise<void> => {
  await api.delete(`arkham/custom-cards/${id}`)
}

/* Art is uploaded rather than inlined: a data URI would ride in the def, and
 * from there into every game that uses the card. */
export const uploadCustomCardArt = async (file: File | Blob, filename = 'art.webp'): Promise<string> => {
  const body = new FormData()
  body.append('file', file, filename)
  /* The client defaults to application/json; a multipart body has to carry its
   * own boundary, which the browser only adds when the header is left unset. */
  const { data } = await api.post('arkham/custom-cards/art', body, {
    headers: { 'Content-Type': undefined },
  })
  return data
}

export const fetchCustomCards = async (gameId: string): Promise<CustomCard[]> => {
  const { data } = await api.get(`arkham/games/${gameId}/custom-cards`)
  return JsonDecoder.array(customCardDecoder, 'ArkhamCustomCard[]').decodePromise(data)
}

/* What a game row needs of a custom investigator: its portrait, and nothing
 * else. A row draws a face and a name, and it used to buy every card in your
 * library -- script included -- once per row to do it. */
const cardPortraitDecoder = JsonDecoder.object<CardPortrait>(
  {
    cardCode: JsonDecoder.string(),
    art: JsonDecoder.nullable(JsonDecoder.string()),
    portrait: JsonDecoder.nullable(JsonDecoder.string()),
    portraitBack: JsonDecoder.nullable(JsonDecoder.string()),
  },
  'CardPortrait',
)

export const fetchCustomCardPortraits = async (codes: string[]): Promise<CardPortrait[]> => {
  if (codes.length === 0) return []
  const { data } = await api.get('arkham/custom-cards/portraits', { params: { codes: codes.join(',') } })
  return JsonDecoder.array(cardPortraitDecoder, 'CardPortrait[]').decodePromise(data)
}

export const fetchCard = async (cardCode: string): Promise<CardDef> => {
  const { data } = await api.get(`arkham/card/${cardCode}`)
  return cardDefDecoder.decodePromise(data)
}

export const fetchInvestigators = async (): Promise<string[]> => {
  const { data } = await api.get('arkham/investigators')
  return JsonDecoder.array(JsonDecoder.string(), 'string[]').decodePromise(data)
}

export const newDeck = async (
  deckId: string,
  deckName: string,
  deckUrl: string | null,
  deckList: ArkhamDbDecklist | null,
): Promise<Deck> => {
  const { data } = await api .post('arkham/decks', { deckId, deckName, deckUrl, deckList })
  return deckDecoder.decodePromise(data)
}

/* Validation reads nothing and writes nothing, so a request that produced no
 * response at all is safe to send again. Worth doing because a browser that
 * loses a request mid-flight will not retry a POST on its own -- Firefox on
 * HTTP/3 hangs here rather than falling back (see terraform's http3_enabled). */
export const validateDeck = async (deckList: ArkhamDbDecklist): Promise<void> => {
  try {
    await api.post('arkham/decks/validate', deckList, { timeout: 15000 })
  } catch (err) {
    if ((err as { response?: unknown }).response) throw err
    await api.post('arkham/decks/validate', deckList, { timeout: 15000 })
  }
}

export interface ArkhamBuildConnection {
  accountId: string
  username: string
  scopes: string[]
  missingScopes: string[]
  connectedAt: string
  refreshedAt: string
  expiresAt: string
}

export interface ArkhamBuildStatus {
  configured: boolean
  scopes: string[]
  connection: ArkhamBuildConnection | null
}

export const fetchArkhamBuildConnection = async (): Promise<ArkhamBuildStatus> => {
  const { data } = await api.get('arkham-build/connection')
  return data
}

export interface ArkhamBuildDeck {
  source: 'account' | 'arkhamdb'
  id: string | number
  updatedAt: string
  decklist: ArkhamDbDecklist
}

export interface ArkhamBuildDecks {
  decks: ArkhamBuildDeck[]
  providers: { account: boolean; arkhamdb: boolean }
}

/* The decks on the player's connected arkham.build account. Reading them is two
 * round trips upstream, so this is slower than the rest of the deck API. */
export const fetchArkhamBuildDecks = async (): Promise<ArkhamBuildDecks> => {
  const { data } = await api.get('arkham-build/decks', { timeout: 30000 })
  return data
}

export const fetchDeckList = async (url: string): Promise<ArkhamDbDecklist> => {
  const { data } = await api.post('arkham/decks/fetch', { url })
  return data
}

export const deleteDeck = (deckId: string): Promise<void> =>
  api.delete(`arkham/decks/${deckId}`);

export const syncDeck = async (deckId: string): Promise<Deck> => {
  const { data } = await api.post(`arkham/decks/${deckId}/sync`)
  return deckDecoder.decodePromise(data)
}

/* Point a saved deck at a different url. The list comes back refetched from
 * there, so the caller replaces the whole deck rather than just its url. */
export const setDeckUrl = async (deckId: string, url: string): Promise<Deck> => {
  const { data } = await api.put(`arkham/decks/${deckId}/url`, { url })
  return deckDecoder.decodePromise(data)
}

/* The API reports a rejected deck request as a JSONError (Api.Handler.Arkham.Decks). */
export const deckErrorMessage = (e: unknown): string | null => {
  const msg = (e as { response?: { data?: { errorMsg?: string } } })?.response?.data?.errorMsg
  return typeof msg === 'string' && msg.length > 0 ? msg : null
}

export const fileBug = (gameId: string): Promise<{ data: string }> =>
  api.post(`arkham/games/${gameId}/file-bug`)

export const updateGame = (gameId: string, choice: number, investigatorId: string | null): Promise<void> =>
  api.put(`arkham/games/${gameId}`,  {tag: 'Answer', contents: { choice, investigatorId }})

export const upgradeDeck = (gameId: string, investigatorId: string, deckUrl?: string, deckList?: ArkhamDbDecklist | null): Promise<void> =>
  api.put(`arkham/games/${gameId}/decks`, { deckUrl, investigatorId, deckList });

export const updateStandaloneSettings = (gameId: string, settings: StandaloneSetting[]): Promise<void> =>
  api.put(`arkham/games/${gameId}`, {tag: 'StandaloneSettingsAnswer', contents: settings })

export const updateCampaignSettings = (gameId: string, campaignLog: CampaignLogSettings): Promise<void> =>
  api.put(`arkham/games/${gameId}`, {
    tag: 'CampaignSettingsAnswer',
    contents: {
      counts: Object.entries(campaignLog.counts),
      sets: Object.entries(campaignLog.sets),
      options: campaignLog.options.flatMap((o: CampaignOption) => o.ckey ? [o.ckey] : []),
      keys: campaignLog.keys.map((o: Key) => o.key)
    }
  })

export const exchangeTokens = (gameId: string, source: Source, fromInvestigator: string, toInvestigator: string, token: Token, amount: number): Promise<void> =>
  api.put(`arkham/games/${gameId}`, { tag: 'ExchangeAmountsAnswer', source, fromInvestigator, toInvestigator, token, amount })

export const retireInvestigator = (gameId: string, investigatorId: string): Promise<void> =>
  api.put(`arkham/games/${gameId}`, { tag: 'RetireInvestigatorAnswer', investigatorId })

export const rejoinInvestigator = (gameId: string, investigatorId: string): Promise<void> =>
  api.put(`arkham/games/${gameId}`, { tag: 'RejoinInvestigatorAnswer', investigatorId })

// Lays custom cards over an investigator's campaign deck between scenarios.
export const applyInvestigatorOverlay = (
  gameId: string,
  investigatorId: string,
  overlay: unknown,
): Promise<void> =>
  api.put(`arkham/games/${gameId}`, { tag: 'ApplyOverlayAnswer', investigatorId, overlay })

export const joinCampaign = (gameId: string): Promise<void> =>
  api.put(`arkham/games/${gameId}`, { tag: 'JoinCampaignAnswer' })

export const setDestiny = (gameId: string, drawings: DestinyDrawing[]): Promise<void> =>
  api.put(`arkham/games/${gameId}`, { tag: 'PickDestinyAnswer', contents: drawings })

export const deleteGame = (gameId: string): Promise<void> =>
  api.delete(`arkham/games/${gameId}`)

export const updateGameRaw = (gameId: string, gameMessage: any): Promise<void> =>
  api.put(`arkham/games/${gameId}/raw`, { gameMessage })

export const setLocationOffset = (gameId: string, locationId: string, x: number, y: number): Promise<void> =>
  updateGameRaw(gameId, { tag: 'SetLocationOffset', contents: [locationId, x, y] })

export const resetLocationOffsets = (gameId: string): Promise<void> =>
  updateGameRaw(gameId, { tag: 'ResetLocationOffsets' })

export const setCardOption = (
  gameId: string,
  investigatorId: string,
  cardCode: string,
  key: string,
  value: boolean | string,
): Promise<void> =>
  updateGameRaw(gameId, { tag: 'SetCardOption', contents: [investigatorId, cardCode, key, value] })

export const setCardSilenced = (
  gameId: string,
  investigatorId: string,
  cardCode: string,
  silenced: boolean,
): Promise<void> =>
  updateGameRaw(gameId, { tag: 'SetCardSilenced', contents: [investigatorId, cardCode, silenced] })

export interface PlayabilityResponse {
  cardId: string
  cardCode: string
  checks: [string, string | null][]
}

export const fetchPlayability = async (gameId: string, investigatorId: string, cardId: string): Promise<PlayabilityResponse> => {
  const { data } = await api.post(`arkham/games/${gameId}/playability`, { investigatorId, cardId })
  return data
}

export const newGame = async (
  deckIds: (string | null)[],
  playerCount: number,
  campaignId: string | null,
  scenarioId: string | null,
  difficulty: Difficulty,
  campaignName: string,
  multiplayerVariant: string,
  includeTarotReadings: boolean,
  options: NewGame.CampaignOption[],
  strictAsIfAt?: boolean,
  // Ultimatums and Boons variant tags (e.g. "BoonOfHades"). Omitted = none.
  ultimatumsAndBoons?: string[],
  // Achievement tracking (only meaningful for campaigns with an achievement
  // catalog); backend defaults to true when omitted.
  achievementsEnabled = true,
  // List the game on the home page for anyone to watch. Backend defaults to
  // false when omitted.
  allowSpectators = false
): Promise<Game> => {
  const { data } = await api.post('arkham/games', {
    deckIds,
    playerCount,
    campaignId,
    scenarioId,
    difficulty,
    campaignName,
    multiplayerVariant,
    includeTarotReadings,
    options,
    strictAsIfAt,
    asIfRuling: strictAsIfAt == null ? undefined : strictAsIfAt ? 'chapter2' : 'chapter1',
    ultimatumsAndBoons,
    achievementsEnabled,
    allowSpectators
  })
  return gameDecoder.decodePromise(data)
}

export const fetchAchievements = async (): Promise<Achievement[]> => {
  const { data } = await api.get('arkham/achievements')
  return JsonDecoder.array(achievementDecoder, 'Achievement[]').decodePromise(data)
}

export const fetchGameAchievements = async (gameId: string): Promise<Achievement[]> => {
  const { data } = await api.get(`arkham/games/${gameId}/achievements`)
  return JsonDecoder.array(achievementDecoder, 'Achievement[]').decodePromise(data)
}

export type ClearAchievementsScope =
  | { scope: 'all' }
  | { scope: 'campaign', campaign: string }
  | { scope: 'achievement', achievement: string }

// Clears EARNED achievements only; in-progress rows keep accruing.
export const clearAchievements = (scope: ClearAchievementsScope): Promise<void> =>
  api.delete('arkham/achievements', { data: scope })

export const joinGame = async (gameId: string): Promise<Game> => {
  const { data } = await api.put(`arkham/games/${gameId}/join`)
  return gameDecoder.decodePromise(data)
}

// The axios instance has no default timeout, so a request that never answers
// never settles either. Undo holds a client-side lock for its round trip, and a
// promise that never settles leaves that lock -- and the Undo button -- stuck for
// the life of the page. Bound it: a rejected undo is recoverable, a hung one is
// not. Multi-step undos fold N patches, so they get more room than a single step.
const UNDO_TIMEOUT_MS = 30000
const UNDO_MULTI_TIMEOUT_MS = 60000

const undoRequest = (path: string, timeout: number): Promise<void> =>
  api.put(path, null, { timeout })

export const undoChoice = (gameId: string, debug: boolean): Promise<void> =>
  undoRequest(`arkham/games/${gameId}/undo${debug ? '?debug' : ''}`, UNDO_TIMEOUT_MS)

export const undoScenarioChoice = (gameId: string): Promise<void> =>
  undoRequest(`arkham/games/${gameId}/undo/scenario`, UNDO_MULTI_TIMEOUT_MS)

export const undoAction = (gameId: string): Promise<void> =>
  undoRequest(`arkham/games/${gameId}/undo/action`, UNDO_MULTI_TIMEOUT_MS)

export const undoTurn = (gameId: string): Promise<void> =>
  undoRequest(`arkham/games/${gameId}/undo/turn`, UNDO_MULTI_TIMEOUT_MS)

export const undoPhase = (gameId: string): Promise<void> =>
  undoRequest(`arkham/games/${gameId}/undo/phase`, UNDO_MULTI_TIMEOUT_MS)

export const undoRound = (gameId: string): Promise<void> =>
  undoRequest(`arkham/games/${gameId}/undo/round`, UNDO_MULTI_TIMEOUT_MS)

/* Undo back to the game step a log entry was written under: everything from
 * that entry onwards is reverted. The server refuses a step that is not in the
 * past, so a stale panel cannot roll a game forwards. */
export const undoToStep = (gameId: string, step: number): Promise<void> =>
  undoRequest(`arkham/games/${gameId}/undo/step/${step}`, UNDO_MULTI_TIMEOUT_MS)

/* Where a game's undo history is.
 *
 * A game nobody has touched in six months has its steps moved to object
 * storage by a nightly job (see Api.Arkham.Archive); the game plays normally,
 * but undo has nothing to step back through until they are fetched. The undo
 * endpoints refuse with 409 and one of these states attached, which is how the
 * client finds out -- nothing is checked in advance. */
export type GameArchiveState = 'archived' | 'restoring' | 'restored' | 'failed'

export interface GameArchive {
  state: GameArchiveState
  steps: number
  bytes: number
  archivedAt: string
  requestedAt: string | null
  restoredAt: string | null
  error: string | null
}

/* Ask for an archived undo history back.
 *
 * Returns as soon as the download is claimed, not when it has finished: the
 * outcome arrives on the game's websocket as GameUnarchived or
 * GameUnarchiveFailed. */
export const unarchiveGame = async (gameId: string): Promise<{ state: GameArchiveState }> => {
  const { data } = await api.put(`arkham/games/${gameId}/unarchive`)
  return data
}

/* Say something in the log. A real engine message rather than a side channel,
 * so it is persisted with a step, reaches the room like any other update, and
 * the rules that care what you typed -- Carcosa's HASTUR recorder -- can see
 * it. */
/* A page of scrollback: the entries immediately older than `beforeSeq`.
 *
 * The game payload only ever carries the newest 40, so this is how the rest of
 * a game's history is reached. */
export const fetchLogBefore = async (gameId: string, beforeSeq: number): Promise<LogRow[]> => {
  const { data } = await api.get(`arkham/games/${gameId}/log/before/${beforeSeq}`)
  return JsonDecoder.array(logRowDecoder, 'LogRow[]').decodePromise(data)
}

/* The scenarios a game's log covers, as step ranges, for the export dialog's
 * scope picker. Empty for a game whose log predates the structured overhaul:
 * there is no scenario banner row to find, so only the whole log can be
 * exported. */
export type LogExportSection = {
  index: number
  /* Absent when the banner row's payload would not decode: the client names the
   * section from `cardCode`, or says it cannot. */
  name: string | null
  cardCode: string | null
  fromStep: number
  untilStep: number | null
}

const logExportSectionDecoder = JsonDecoder.object<LogExportSection>(
  {
    index: JsonDecoder.number(),
    name: JsonDecoder.nullable(JsonDecoder.string()),
    cardCode: JsonDecoder.nullable(JsonDecoder.string()),
    fromStep: JsonDecoder.number(),
    untilStep: JsonDecoder.nullable(JsonDecoder.number()),
  },
  'LogExportSection',
)

export const fetchLogSections = async (gameId: string): Promise<LogExportSection[]> => {
  const { data } = await api.get(`arkham/games/${gameId}/log/sections`)
  return JsonDecoder.array(logExportSectionDecoder, 'LogExportSection[]').decodePromise(data)
}

/* Which slice of the log to export. Both bounds absent is the whole game;
 * `scenario` only names the slice in the exported file. */
export type LogExportRange = {
  fromStep?: number
  untilStep?: number
  scenario?: string
}

export type LogExport = {
  gameName: string
  scenario: string | null
  rows: LogRow[]
}

/* The rows themselves, which the readable transcript is rendered from -- see
 * logTranscript.ts for why that happens on this side. */
export const fetchLogExport = async (gameId: string, range: LogExportRange): Promise<LogExport> => {
  const { data } = await api.get(`arkham/games/${gameId}/log/export`, { params: range })
  const rows = await JsonDecoder.array(logRowDecoder, 'LogRow[]').decodePromise(data.rows)
  return { gameName: data.gameName, scenario: data.scenario ?? null, rows }
}

/* The same endpoint as a file to save, for the formats nothing has to render:
 * the raw rows, or the server's own trace text. */
export const fetchLogExportFile = async (
  gameId: string,
  range: LogExportRange,
  format: 'json' | 'trace',
): Promise<Blob> => {
  const { data } = await api.get(`arkham/games/${gameId}/log/export`, {
    params: { ...range, format },
    responseType: 'blob',
  })
  return data
}

export const sayInLog = (gameId: string, investigatorId: string, text: string): Promise<void> =>
  /* The null is the speaker's name: the API fills it from the authenticated
   * session, so sending one here would be ignored anyway. */
  updateGameRaw(gameId, { tag: 'ChatMessage', contents: [investigatorId, null, text] })

export const importGame = async (formData: FormData, multiplayerVariant: string): Promise<Game> => {
  const { data } = await api.post(`arkham/games/import?multiplayerVariant=${multiplayerVariant}`, formData, { headers: { 'Content-Type': 'multipart/form-data' } })
  return gameDecoder.decodePromise(data)
}

export const fetchOpenSeats = async (gameId: string): Promise<string[]> => {
  const { data } = await api.get(`arkham/games/${gameId}/open-seats`)
  return data as string[]
}

export const claimSeat = async (gameId: string, investigatorId: string): Promise<void> => {
  await api.post(`arkham/games/${gameId}/claim-seat`, { investigatorId })
}

// "Epic Multiplayer" events ---------------------------------------------------

export const fetchEvents = async (): Promise<EventListEntry[]> => {
  const { data } = await api.get('arkham/events')
  return JsonDecoder.array(eventListEntryDecoder, 'EventListEntry[]').decodePromise(data)
}

export const fetchEvent = async (eventId: string): Promise<EventDetails> => {
  const { data } = await api.get(`arkham/events/${eventId}`)
  return eventDetailsDecoder.decodePromise(data)
}

export const createEvent = async (payload: CreateEventPost): Promise<EventDetails> => {
  const { data } = await api.post('arkham/events', payload)
  return eventDetailsDecoder.decodePromise(data)
}

export const adjustEventCounter = (eventId: string, key: string, amount: number): Promise<void> =>
  api.post(`arkham/events/${eventId}/counter`, { key, amount })

// Mark the caller's group ready at the start barrier. Idempotent server-side; the
// countdown begins once every group has been marked ready.
export const markEventReady = async (eventId: string): Promise<void> => {
  await api.post(`arkham/events/${eventId}/ready`)
}

// Force all still-playing groups to agenda 3b when the clock runs out. Idempotent
// server-side, so it's safe for more than one client to fire it.
export const eventTimeUp = async (eventId: string): Promise<void> => {
  await api.post(`arkham/events/${eventId}/time-up`)
}

// Organizer resolves an over-threshold shared act advance for `stage`, choosing how
// many clues each group spends. `allocation` entries are { ordinal, spend }. The
// backend clears the awaiting-organizer gate + resets the pool, then broadcasts the
// updated shared state over the event ws.
export const resolveEventAdvance = async (
  eventId: string,
  stage: number,
  allocation: { ordinal: number; spend: number }[],
): Promise<void> => {
  await api.post(`arkham/events/${eventId}/resolve-advance`, { stage, allocation })
}

export const swapMainStreetInvestigators = async (
  eventId: string,
  firstGroupOrdinal: number,
  secondGroupOrdinal: number,
): Promise<void> => {
  await api.post(`arkham/events/${eventId}/swap-main-street`, { firstGroupOrdinal, secondGroupOrdinal })
}

export const replicateAberration = async (
  eventId: string,
  groupOrdinal: number,
  cardCode: string,
  target: unknown,
): Promise<void> => {
  await api.post(`arkham/events/${eventId}/replicate`, { groupOrdinal, cardCode, target })
}

export const deleteEvent = async (eventId: string): Promise<void> => {
  await api.delete(`arkham/events/${eventId}`)
}

// Builds a websocket URL for an /api/v1 path: same origin as the page, http(s) ->
// ws(s) rewrite, and `?token=` auth appended (the GET upgrades to a websocket).
// Shared by the event socket here and the game socket in views/Game.vue.
export const buildWebsocketUrl = (path: string, token?: string | null): string => {
  const baseURL = `${window.location.protocol}//${window.location.hostname}${window.location.port ? `:${window.location.port}` : ''}`
  return `${baseURL}${path}?token=${token}`
    .replace(/https/, 'wss')
    .replace(/http/, 'ws')
}

export const eventWebsocketUrl = (eventId: string, token: string | null): string =>
  buildWebsocketUrl(`/api/v1/arkham/events/${eventId}`, token)
