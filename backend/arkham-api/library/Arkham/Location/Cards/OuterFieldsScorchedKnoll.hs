module Arkham.Location.Cards.OuterFieldsScorchedKnoll (outerFieldsScorchedKnoll) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheLongestNight.Helpers
import Arkham.Token

newtype OuterFieldsScorchedKnoll = OuterFieldsScorchedKnoll LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outerFieldsScorchedKnoll :: LocationCard OuterFieldsScorchedKnoll
outerFieldsScorchedKnoll =
  symbolLabel
    $ locationWith
      OuterFieldsScorchedKnoll
      Cards.outerFieldsScorchedKnoll
      3
      (PerPlayer 2)
      connectsToAdjacent

instance HasAbilities OuterFieldsScorchedKnoll where
  getAbilities (OuterFieldsScorchedKnoll a) =
    extendRevealed
      a
      [ mkAbility a 1 $ freeReaction $ DiscoveringLastClue #after You (be a)
      , playerLimit PerTurn
          $ restricted a 2 (Here <> DuringTurn You <> exists (LocationWithDamage (atLeast 1)))
          $ FastAbility Free
      ]

instance RunMessage OuterFieldsScorchedKnoll where
  runMessage msg l@(OuterFieldsScorchedKnoll attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTrap attrs attrs.id
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      trapLocations <- select $ LocationWithDamage (atLeast 1)
      chooseTargetM iid trapLocations \fromLid -> do
        destinations <- select $ LocationWithoutModifier CannotHaveTraps <> not_ (LocationWithId fromLid)
        chooseTargetM iid destinations \toLid ->
          moveTokens (attrs.ability 2) fromLid toLid Damage 1
      pure l
    _ -> OuterFieldsScorchedKnoll <$> liftRunMessage msg attrs
