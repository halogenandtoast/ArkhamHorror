module Arkham.Location.Cards.PathOfThorns (pathOfThorns) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype PathOfThorns = PathOfThorns LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pathOfThorns :: LocationCard PathOfThorns
pathOfThorns = symbolLabel $ location PathOfThorns Cards.pathOfThorns 3 (PerPlayer 1)

instance HasAbilities PathOfThorns where
  getAbilities (PathOfThorns a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ SkillTestResult #after You (WhileInvestigating $ be a) #failure
      , mkAbility a 2 $ forced $ Explored #after You (be a) $ FailedExplore AnyCard
      ]

instance RunMessage PathOfThorns where
  runMessage msg l@(PathOfThorns attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignDamage iid (attrs.ability 2) 1
      pure l
    _ -> PathOfThorns <$> liftRunMessage msg attrs
