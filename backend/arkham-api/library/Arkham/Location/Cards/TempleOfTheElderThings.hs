module Arkham.Location.Cards.TempleOfTheElderThings (templeOfTheElderThings) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype TempleOfTheElderThings = TempleOfTheElderThings LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeOfTheElderThings :: LocationCard TempleOfTheElderThings
templeOfTheElderThings =
  locationWith TempleOfTheElderThings Cards.templeOfTheElderThings 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities TempleOfTheElderThings where
  getAbilities (TempleOfTheElderThings a) =
    extendRevealed
      a
      [ restricted a 1 (DuringSkillTest $ SkillTestAt (be a)) $ forced $ RevealChaosToken #after You #frost
      , groupLimit PerGame
          $ restricted
            a
            2
            (Here <> thisExists a LocationWithoutClues <> LocationCount 2 LocationWithAnyKeys)
            actionAbility
      ]

instance RunMessage TempleOfTheElderThings where
  runMessage msg l@(TempleOfTheElderThings attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      ls <- select LocationWithAnyKeys
      chooseTargetM iid ls (handleTarget iid (attrs.ability 2))
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (LocationTarget lid) -> do
      ls <- select $ LocationWithAnyKeys <> not_ (LocationWithId lid)
      ks <- field LocationKeys lid
      chooseTargetM iid ls \lid' -> do
        ks' <- field LocationKeys lid'
        for_ ks (placeKey lid')
        for_ ks' (placeKey lid)
      pure l
    _ -> TempleOfTheElderThings <$> liftRunMessage msg attrs
