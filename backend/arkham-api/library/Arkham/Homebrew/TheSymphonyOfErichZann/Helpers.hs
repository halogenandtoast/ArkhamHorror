{- | Shared rules for The Symphony of Erich Zann.

The scenario's spine is the row of [[Music]] treacheries next to the agenda
deck. A Music treachery is never discarded on resolution: it is put into play
next to the agenda deck, and the current agenda prints how many may sit there at
once. When placing one would exceed that maximum, the *earliest* one placed is
discarded instead -- so the row never grows, it rotates.

"Earliest" is insertion order, which the engine does not record anywhere, so the
scenario keeps its own ordered list of treachery ids in @scenarioMeta@ under
'musicOrderKey'. Turnaround reshuffles that list, which is the whole of its
effect.

If the current agenda prints no maximum (Coda Ultimatum does not), Music
treacheries cannot be discarded by this effect at all.
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.Helpers where

import Arkham.Agenda.Sequence (agendaStep, unAgendaStep)
import Arkham.Agenda.Types (Field (..))
import Arkham.Classes.HasGame
import Arkham.Card.CardDef (CardDef)
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Helpers.Scenario (getScenarioMetaKeyDefault, scenarioField, setScenarioMeta)
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits (pattern Music)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message (Message (PlaceTreachery), ShuffleIn (..))
import Arkham.Placement (Placement (InPlayArea, NextToAgenda))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioMeta))
import Arkham.Source (toSource)
import Arkham.Trait (Trait)
import Arkham.Treachery.Types (TreacheryAttrs)
import Data.Aeson.KeyMap qualified as KeyMap

-- | Where the placement order of the Music row lives inside @scenarioMeta@.
musicOrderKey :: Key
musicOrderKey = "musicOrder"

-- | Every [[Music]] treachery currently next to the agenda deck.
musicTreacheriesInPlay :: HasGame m => m [TreacheryId]
musicTreacheriesInPlay = select $ TreacheryWithTrait Music <> TreacheryWithPlacement NextToAgenda

-- | How many [[Music]] treacheries may sit next to the agenda deck right now.
musicMaximum :: HasGame m => m (Maybe Int)
musicMaximum =
  selectOne AnyAgenda >>= \case
    Nothing -> pure Nothing
    Just aid -> do
      stage <- field AgendaSequence aid
      -- Agendas 1a/2a/3a print 1, 2 and 3. Coda Ultimatum prints no maximum.
      pure $ case unAgendaStep (agendaStep stage) of
        1 -> Just 1
        2 -> Just 2
        3 -> Just 3
        _ -> Nothing

-- | The recorded placement order, filtered to what is still in play.
getMusicOrder :: HasGame m => m [TreacheryId]
getMusicOrder = do
  recorded <- getScenarioMetaKeyDefault musicOrderKey []
  inPlay <- musicTreacheriesInPlay
  -- Anything in play but unrecorded (a treachery placed by some other effect)
  -- sorts after everything that was recorded, so it is discarded last.
  pure $ filter (`elem` inPlay) recorded <> filter (`notElem` recorded) inPlay

-- | Rewrite the placement order, leaving the rest of @scenarioMeta@ alone.
setMusicOrder :: ReverseQueue m => [TreacheryId] -> m ()
setMusicOrder order = do
  meta <- scenarioField ScenarioMeta
  let object' = case meta of
        Object o -> o
        _ -> KeyMap.empty
  setScenarioMeta $ Object $ KeyMap.insert musicOrderKey (toJSON order) object'

{- | Put a [[Music]] treachery into play next to the agenda deck.

"If placing a Music treachery next to the agenda would exceed the maximum amount
written on the agenda, discard the earliest Music treachery that was put into
play." If the agenda prints no maximum, nothing is ever discarded.
-}
placeMusicTreachery :: ReverseQueue m => TreacheryAttrs -> m ()
placeMusicTreachery attrs = do
  push $ PlaceTreachery attrs.id NextToAgenda
  order <- getMusicOrder
  let placed = order <> [attrs.id]
  musicMaximum >>= \case
    Just n | length placed > n -> case placed of
      (earliest : rest) -> do
        toDiscard (toSource attrs) earliest
        setMusicOrder rest
      [] -> setMusicOrder placed
    _ -> setMusicOrder placed

{- | A [[Musician]] can be neither parleyed with nor damaged while no treachery
of its instrument is in play. Each of the four names a different instrument
trait, and the matching [[Music]] treachery is what unlocks them.
-}
instrumentInPlay :: HasGame m => Trait -> m Bool
instrumentInPlay t = selectAny (TreacheryWithTrait t <> InPlayTreachery)

{- | What each Muse pays out.

"Add <Musician> to the victory display. You may choose to put the set aside
<instrument> into play in any investigator's play area. That investigator has
earned it and may choose to add it to his or her deck. This card does not count
toward that investigator's deck size."
-}
musePayoff :: (HasI18n, ReverseQueue m) => InvestigatorId -> CardDef -> CardDef -> m ()
musePayoff iid musician instrument = do
  selectEach (enemyIs musician) (addToVictory iid)
  offerInstrument iid instrument

{- | The reward half on its own, for The Piano -- which pays out La Fratta's
Piano Key the same way, but banks itself rather than a Musician.
-}
offerInstrument :: (HasI18n, ReverseQueue m) => InvestigatorId -> CardDef -> m ()
offerInstrument iid instrument = chooseOneM iid do
  labeled "takeInstrument" do
    investigators <- getInvestigators
    chooseOrRunOneM iid $ targets investigators \owner -> do
      createAssetAt_ instrument (InPlayArea owner)
      addCampaignCardToDeck owner DoNotShuffleIn instrument
  labeled "leaveInstrument" nothing

-- | The campaign's own i18n scope; the folder name camelCased.
campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theSymphonyOfErichZann" a

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "scenario" a
