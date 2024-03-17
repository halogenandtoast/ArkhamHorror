module Arkham.Act.Cards.EnteringTheUnderworldV1 (EnteringTheUnderworldV1 (..), enteringTheUnderworldV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet (EncounterSet (Ghouls, Nightgaunts, StrikingFear, TerrorOfTheVale))
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Source
import Arkham.Trait (Trait (Vale))

newtype EnteringTheUnderworldV1 = EnteringTheUnderworldV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enteringTheUnderworldV1 :: ActCard EnteringTheUnderworldV1
enteringTheUnderworldV1 = act (1, A) EnteringTheUnderworldV1 Cards.enteringTheUnderworldV1 Nothing

instance HasAbilities EnteringTheUnderworldV1 where
  getAbilities (EnteringTheUnderworldV1 x) =
    [mkAbility x 1 $ forced $ EnemyDefeated #after Anyone ByAny $ EnemyWithClues $ atLeast 1]

instance RunMessage EnteringTheUnderworldV1 where
  runMessage msg a@(EnteringTheUnderworldV1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ GainClues iid (attrs.ability 1) 1
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      traverse_ placeLocation_ =<< getSetAsideCardsMatching (#location <> withTrait Vale)
      push $ RemoveAllCopiesOfEncounterCardFromGame (fromSets [Ghouls, StrikingFear])
      encounterSets <- getSetAsideCardsMatching (fromSets [TerrorOfTheVale, Nightgaunts])
      pushAll [ShuffleCardsIntoDeck Deck.EncounterDeck encounterSets, ShuffleEncounterDiscardBackIn]
      advanceActDeck attrs
      pure a
    _ -> EnteringTheUnderworldV1 <$> lift (runMessage msg attrs)
