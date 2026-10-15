module Arkham.Homebrew.ThePhantomOfTheOpera.Enemies.StifledLaughter (stifledLaughter) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (ExposeRoute (..), afterExposedViaFor)
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype StifledLaughter = StifledLaughter EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stifledLaughter :: EnemyCard StifledLaughter
stifledLaughter = enemy StifledLaughter Cards.stifledLaughter

instance HasAbilities StifledLaughter where
  getAbilities (StifledLaughter a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ afterExposedViaFor (Just You) ExposedByEvade a

instance RunMessage StifledLaughter where
  runMessage msg e@(StifledLaughter attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> StifledLaughter <$> liftRunMessage msg attrs
