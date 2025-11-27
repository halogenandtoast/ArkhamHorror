module Arkham.Location.Cards.MountainStream (mountainStream) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MountainStream = MountainStream LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mountainStream :: LocationCard MountainStream
mountainStream = location MountainStream Cards.mountainStream 1 (PerPlayer 1)

instance HasAbilities MountainStream where
  getAbilities (MountainStream a) =
    extendRevealed
      a
      [ forcedAbility a 1 $ Enters #after You (be a)
      , forcedAbility a 2 $ DiscoveringLastClue #after Anyone (be a)
      ]

instance RunMessage MountainStream where
  runMessage msg l@(MountainStream attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#intellect, #agility] (Fixed 1)
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      placeRandomLocationGroupCards
        "outerWilderness"
        [Cards.forgottenOutpost, Cards.huntersLodge, Cards.condemnedGoldMine]
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> MountainStream <$> liftRunMessage msg attrs
