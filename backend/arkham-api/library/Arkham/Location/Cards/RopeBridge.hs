module Arkham.Location.Cards.RopeBridge (ropeBridge, RopeBridge (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Name

newtype RopeBridge = RopeBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ropeBridge :: LocationCard RopeBridge
ropeBridge = location RopeBridge Cards.ropeBridge 2 (PerPlayer 1)

instance HasAbilities RopeBridge where
  getAbilities (RopeBridge attrs) =
    extendRevealed
      attrs
      [skillTestAbility $ restrictedAbility attrs 1 Here $ forced $ AttemptExplore #when You]

instance RunMessage RopeBridge where
  runMessage msg l@(RopeBridge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      let source = attrs.ability 1
      cancelExplore source
      assignDamage iid source 2
      push $ SetActions iid source 0
      endYourTurn iid
      mRiverCanyon <- find ((== "River Canyon") . toTitle) <$> getExplorationDeck
      place iid =<< maybe (selectJust $ location_ "River Canyon") placeLocation mRiverCanyon
      pure l
    _ -> RopeBridge <$> liftRunMessage msg attrs
