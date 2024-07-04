module Arkham.Act.Cards.TheDescent (TheDescent (..), theDescent) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet (EncounterSet (AgentsOfAtlachNacha, DescentIntoThePitch))
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Matcher
import Arkham.Trait (Trait (Depths))

newtype TheDescent = TheDescent ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDescent :: ActCard TheDescent
theDescent = act (2, A) TheDescent Cards.theDescent Nothing

instance HasAbilities TheDescent where
  getAbilities (TheDescent x) =
    [mkAbility x 1 $ forced $ EnemyDefeated #after Anyone ByAny $ EnemyWithClues $ atLeast 1]

instance RunMessage TheDescent where
  runMessage msg a@(TheDescent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ GainClues iid (attrs.ability 1) 1
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      placeRandomLocationGroup "seaOfPitch"
        =<< getSetAsideCardsMatching (#location <> withTrait Depths)
      encounterSets <- getSetAsideCardsMatching (fromSets [DescentIntoThePitch, AgentsOfAtlachNacha])
      pushAll [ShuffleCardsIntoDeck Deck.EncounterDeck encounterSets, ShuffleEncounterDiscardBackIn]
      advanceActDeck attrs
      pure a
    _ -> TheDescent <$> liftRunMessage msg attrs
