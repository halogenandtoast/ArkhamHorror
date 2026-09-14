module Arkham.Scenarios.TheFeastOfHemlockVale.FateOfTheVale.Helpers where

import Arkham.Act.CardDefs.TheFeastOfHemlockVale.FateOfTheVale qualified as Acts
import Arkham.Act.Types (Field (ActCard))
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.Classes.HasQueue (push)
import Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.Residents qualified as Enemies
import Arkham.Helpers.Act (getCurrentAct)
import Arkham.Helpers.Scenario (getScenarioDeck)
import Arkham.Helpers.Window (wouldDo)
import Arkham.I18n
import Arkham.Id (InvestigatorId)
import Arkham.Investigator.Types (Field (InvestigatorDoom))
import Arkham.Message (Message (Run, ScenarioSpecific))
import Arkham.Message.Lifted
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Source
import Arkham.Window qualified as Window

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "fateOfTheVale" a

residentEnemyDef :: Resident -> CardDef
residentEnemyDef = \case
  WilliamHemlock -> Enemies.williamHemlock
  RiverHawthorne -> Enemies.riverHawthorne
  MotherRachel -> Enemies.motherRachelStarbornHerald
  SimeonAtwood -> Enemies.simeonAtwood
  LeahAtwood -> Enemies.leahAtwood
  TheoPeters -> Enemies.theoPeters
  GideonMizrah -> Enemies.gideonMizrah
  JudithPark -> Enemies.judithPark

whenFateOfTheValeV4 :: ReverseQueue m => m () -> m ()
whenFateOfTheValeV4 body = do
  act <- getCurrentAct
  actCard <- field ActCard act
  when (toCardCode actCard == toCardCode Acts.fateOfTheValeV4) body

{- | Resolve a "true" investigator card leaving The Abyss: its owner returns to
their true self, healed, with Old Memory in their play area.
-}
resolveTrueSelf :: ReverseQueue m => Source -> InvestigatorId -> Card -> m ()
resolveTrueSelf source fallback card = do
  let owner = fromMaybe fallback $ toCardOwner card
  void $ setOwner owner card
  healAllDamageAndHorror source owner
  -- Doom placed on the Shattered Self card remains on it as the card flips to
  -- its Old Memory side (per FFG ruling, issue #5184). returnFromShatteredSelf
  -- carries the doom back onto the true self, so move it onto Old Memory here.
  doom <- field InvestigatorDoom owner
  oldMemory <- setOwner owner =<< genCard Assets.oldMemory
  oldMemoryId <- createAssetAt oldMemory (InPlayArea owner)
  scenarioSpecific "returnFromShatteredSelf" owner
  when (doom > 0) do
    removeAllDoom source owner
    placeDoom source oldMemoryId doom

{- | Route a card being drawn or revealed from The Abyss through a cancellable
window so cards such as Old Memory can react before it resolves. @resolveMsg@ is
performed unless the batch is cancelled; because the generic @DoBatch@ runner
only unwraps 'Run' and message-specific handlers, @resolveMsg@ must be a 'Run'
or a message with a matching @DoBatch _ msg@ handler. @tag@ distinguishes the
draw context (e.g. "mythos" vs "scenario") so reactors can resolve a redirect
the right way.
-}
abyssDrawWindow :: ReverseQueue m => Text -> InvestigatorId -> Card -> Message -> m ()
abyssDrawWindow tag iid card resolveMsg = do
  -- Reactors such as Old Memory must not trigger on signature cards drawn from
  -- The Abyss, so flag those windows with a ":signature" suffix they ignore.
  let key prefix = prefix <> ":" <> tag <> (if isSignature card then ":signature" else "")
  highlightCards [card]
  wouldDo
    resolveMsg
    (Window.ScenarioEvent (key "wouldDrawFromAbyss") (Just iid) (toJSON card))
    (Window.ScenarioEvent (key "drewFromAbyss") (Just iid) (toJSON card))
  highlightCards ([] :: [Card])

{- | Reveal cards from The Abyss one at a time, giving reactions such as Old
Memory a window for every card before continuing. The cards stay focused until
@resolveMsg@ has finished asking any follow-up question.
-}
revealCardsFromAbyss :: ReverseQueue m => InvestigatorId -> [Card] -> Message -> m ()
revealCardsFromAbyss iid cards resolveMsg = focusCards cards do
  for_ cards \card -> do
    let suffix = if isSignature card then ":signature" else ""
    highlightCards [card]
    wouldDo
      (Run [])
      (Window.ScenarioEvent ("wouldRevealFromAbyss" <> suffix) (Just iid) (toJSON card))
      (Window.ScenarioEvent ("revealedFromAbyss" <> suffix) (Just iid) (toJSON card))
    highlightCards ([] :: [Card])
  push resolveMsg

{- | Reveal cards from the top of The Abyss until an encounter-backed card is
found and draw it, replacing the mythos encounter draw. Shared by every Fate of
the Vale agenda (The Silence, The Miasma, The Spiral), each of which carries the
same forced ability.
-}
revealEncounterCardFromAbyss :: ReverseQueue m => Source -> InvestigatorId -> m ()
revealEncounterCardFromAbyss source iid = do
  abyss <- getScenarioDeck AbyssDeck
  let
    hasEncounterBack = \case
      EncounterCard c -> not (cdDoubleSided $ toCardDef c)
      _ -> False
    (nonEncounter, rest) = break hasEncounterBack (reverse abyss)
  let encounter = case rest of
        EncounterCard ec : _ -> Just (EncounterCard ec)
        _ -> Nothing
      revealed = nonEncounter <> maybeToList encounter
  unless (null revealed)
    $ revealCardsFromAbyss iid revealed
    $ ScenarioSpecific "resolveEncounterCardsRevealedFromAbyss"
    $ toJSON (source, iid, nonEncounter, encounter)
