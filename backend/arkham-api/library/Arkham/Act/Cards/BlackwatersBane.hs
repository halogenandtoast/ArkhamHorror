module Arkham.Act.Cards.BlackwatersBane (blackwatersBane) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Story
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Placement
import Arkham.Trait (Trait (Ooze, Oozified))

newtype BlackwatersBane = BlackwatersBane ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackwatersBane :: ActCard BlackwatersBane
blackwatersBane = act (3, A) BlackwatersBane Cards.blackwatersBane Nothing

instance HasModifiersFor BlackwatersBane where
  getModifiersFor (BlackwatersBane a) =
    modifySelect a (EnemyWithTrait Ooze) [AddKeyword Keyword.Retaliate, ScenarioModifier "noBlob"]

instance HasAbilities BlackwatersBane where
  getAbilities (BlackwatersBane a) = [mkAbility a 1 $ Objective $ forced $ RoundEnds #when]

instance RunMessage BlackwatersBane where
  runMessage msg a@(BlackwatersBane attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- Only on the first advance: shuffle the set-aside Mi-Go Drones into the
      -- encounter deck along with the encounter discard pile.
      drones <- getSetAsideCardsMatching (cardIs Enemies.miGoDrone)
      unless (null drones) do
        shuffleSetAsideIntoEncounterDeck $ cardIs Enemies.miGoDrone
        shuffleEncounterDiscardBackIn
      n <- perPlayer 1
      selectEach (RevealedLocation <> LocationWithTrait Oozified <> LocationNotAtClueLimit) \loc -> do
        push $ PlaceCluesUpToClueValue loc (toSource attrs) n

      stories <- getSetAsideCardsMatching (CardWithType StoryType)
      for_ (nonEmpty stories) \xs -> do
        lead <- getLead
        x <- sample xs
        readStoryWithPlacement_ lead (toCardDef x) Global
      push $ ResetActDeckToStage 1
      pure a
    _ -> BlackwatersBane <$> liftRunMessage msg attrs
