module Arkham.Location.Cards.OuterFieldsBlightedCornfields (outerFieldsBlightedCornfields) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey

newtype OuterFieldsBlightedCornfields = OuterFieldsBlightedCornfields LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outerFieldsBlightedCornfields :: LocationCard OuterFieldsBlightedCornfields
outerFieldsBlightedCornfields =
  symbolLabel
    $ locationWith
      OuterFieldsBlightedCornfields
      Cards.outerFieldsBlightedCornfields
      3
      (Static 2)
      connectsToAdjacent

instance HasAbilities OuterFieldsBlightedCornfields where
  getAbilities (OuterFieldsBlightedCornfields a) =
    extendRevealed
      a
      [ mkAbility a 1 $ freeReaction $ DiscoveringLastClue #after You (be a)
      , playerLimit PerTurn
          $ restricted a 2 (DuringTurn You <> exists LocationWithAdjacentBarrier)
          $ FastAbility Free
      ]

instance RunMessage OuterFieldsBlightedCornfields where
  runMessage msg l@(OuterFieldsBlightedCornfields attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connected <- select $ connectedTo (be attrs)
      chooseTargetM iid connected \toLid ->
        push $ ScenarioCountIncrementBy (Barriers attrs.id toLid) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      barrieredLocations <- select LocationWithAdjacentBarrier
      chooseTargetM iid barrieredLocations $ handleTarget iid (attrs.ability 2)
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (LocationTarget srcLid) -> do
      mods <- getModifiers srcLid
      let barricaded = concat [ls | Barricades ls <- mods]
      chooseTargetM iid barricaded \toLid -> do
        push $ ScenarioCountDecrementBy (Barriers srcLid toLid) 1
        doStep 1 msg
      pure l
    DoStep 1 msg'@(HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (LocationTarget _srcLid)) -> do
      allLocations <- select Anywhere
      chooseTargetM iid allLocations (`forTarget` msg')
      pure l
    ForTarget
      (LocationTarget newLid)
      (HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (LocationTarget _srcLid)) -> do
      connected <- select $ connectedTo (LocationWithId newLid)
      chooseTargetM iid connected \toLid ->
        push $ ScenarioCountIncrementBy (Barriers newLid toLid) 1
      pure l
    _ -> OuterFieldsBlightedCornfields <$> liftRunMessage msg attrs
