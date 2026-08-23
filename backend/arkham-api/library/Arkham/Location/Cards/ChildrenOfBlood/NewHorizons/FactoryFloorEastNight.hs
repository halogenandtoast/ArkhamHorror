module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.FactoryFloorEastNight (factoryFloorEastNight) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Helpers (unDeck)
import Arkham.Helpers.ChaosBag (canAddChaosTokenFace)
import Arkham.Helpers.Scenario (getEncounterDeck, getEncounterDiscard)
import Arkham.I18n
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck

newtype FactoryFloorEastNight = FactoryFloorEastNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

factoryFloorEastNight :: LocationCard FactoryFloorEastNight
factoryFloorEastNight = symbolLabel $ location FactoryFloorEastNight Cards.factoryFloorEastNight 2 (PerPlayer 1)

instance HasAbilities FactoryFloorEastNight where
  getAbilities (FactoryFloorEastNight a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoveringLastClue #after You (be a)

instance RunMessage FactoryFloorEastNight where
  runMessage msg l@(FactoryFloorEastNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canBlood <- canAddChaosTokenFace #blood
      canDraw <-
        andM
          [ can.target.encounterDeck iid
          , orM [notNull . unDeck <$> getEncounterDeck, notNull <$> getEncounterDiscard RegularEncounterDeck]
          ]
      chooseOneM iid $ withI18n do
        labeledValidate' canBlood "addBloodToken" $ addChaosToken #blood
        labeledValidate' canDraw "drawEncounterCard" $ drawEncounterCard iid (attrs.ability 1)
      pure l
    _ -> FactoryFloorEastNight <$> liftRunMessage msg attrs
