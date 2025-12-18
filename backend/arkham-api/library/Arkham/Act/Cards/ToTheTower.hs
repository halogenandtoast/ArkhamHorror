module Arkham.Act.Cards.ToTheTower (toTheTower) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenario.Deck
import Arkham.Trait (Trait (Tower))

newtype ToTheTower = ToTheTower ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toTheTower :: ActCard ToTheTower
toTheTower = act (2, A) ToTheTower Cards.toTheTower Nothing

instance HasModifiersFor ToTheTower where
  getModifiersFor (ToTheTower a) = do
    when (onSide A a)
      $ modifySelect a (LocationWithTrait Tower) [ScenarioModifier "noCityOfRemnants"]

instance HasAbilities ToTheTower where
  getAbilities = actAbilities1 \a ->
    mkAbility a 1 $ Objective $ forced $ Enters #after Anyone "Gravity-Defying Climb"

instance RunMessage ToTheTower where
  runMessage msg a@(ToTheTower attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach ConcealedCardAny removeFromGame
      selectEach (LocationWithPlacement InTheShadows) removeLocation
      removeScenarioDeck OtherworldDeck
      decoys <- flip replicate Decoy <$> perPlayer 1
      cards <-
        shuffle =<< traverse mkConcealedCard ([CityOfRemnantsL, CityOfRemnantsM, CityOfRemnantsR] <> decoys)
      lead <- getLead
      let positions = [Pos 1 1, Pos (-1) 1]
      for_ cards (push . CreateConcealedCard)
      scenarioSpecific "distributeConcealedLocations" (lead, cards, positions, positions)
      advanceActDeck attrs
      pure a
    _ -> ToTheTower <$> liftRunMessage msg attrs
