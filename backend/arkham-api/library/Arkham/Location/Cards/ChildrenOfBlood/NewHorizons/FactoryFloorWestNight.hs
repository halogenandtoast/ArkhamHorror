module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.FactoryFloorWestNight (factoryFloorWestNight) where

import Arkham.Ability
import Arkham.Helpers.ChaosBag (canAddChaosTokenFace)
import Arkham.I18n
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype FactoryFloorWestNight = FactoryFloorWestNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

factoryFloorWestNight :: LocationCard FactoryFloorWestNight
factoryFloorWestNight = symbolLabel $ location FactoryFloorWestNight Cards.factoryFloorWestNight 2 (PerPlayer 1)

instance HasAbilities FactoryFloorWestNight where
  getAbilities (FactoryFloorWestNight a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoveringLastClue #after You (be a)

instance RunMessage FactoryFloorWestNight where
  runMessage msg l@(FactoryFloorWestNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canBlood <- canAddChaosTokenFace #blood
      canDoom <- selectAny $ not_ (AgendaWithModifier CannotPlaceDoomOnThis)
      chooseOneM iid $ withI18n do
        labeledValidate' canBlood "addBloodToken" $ addChaosToken #blood
        countVar 1
          $ labeledValidate' canDoom "placeAgendaDoom"
          $ placeDoomOnAgendaBy (attrs.ability 1) 1
      pure l
    _ -> FactoryFloorWestNight <$> liftRunMessage msg attrs
