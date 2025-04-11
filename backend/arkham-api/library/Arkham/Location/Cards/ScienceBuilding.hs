module Arkham.Location.Cards.ScienceBuilding (scienceBuilding) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ScienceBuilding = ScienceBuilding LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scienceBuilding :: LocationCard ScienceBuilding
scienceBuilding = symbolLabel $ location ScienceBuilding Cards.scienceBuilding 2 (PerPlayer 1)

instance HasAbilities ScienceBuilding where
  getAbilities (ScienceBuilding x) =
    extendRevealed
      x
      [ restricted x 1 (Here <> not_ (exists $ LocationWithTitle "Alchemy Labs"))
          $ forced
          $ RevealLocation #after You (be x)
      , restricted x 2 Here
          $ forced
          $ SkillTestResult #when You (SkillTestWithSkillType #willpower) #failure
      ]

instance RunMessage ScienceBuilding where
  runMessage msg l@(ScienceBuilding attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocation_ Cards.alchemyLabs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignDamage iid (attrs.ability 2) 1
      pure l
    _ -> ScienceBuilding <$> liftRunMessage msg attrs
