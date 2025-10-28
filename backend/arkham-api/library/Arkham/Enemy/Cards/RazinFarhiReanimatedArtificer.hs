module Arkham.Enemy.Cards.RazinFarhiReanimatedArtificer (razinFarhiReanimatedArtificer) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher hiding (EnemyAttacks)

newtype RazinFarhiReanimatedArtificer = RazinFarhiReanimatedArtificer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

razinFarhiReanimatedArtificer :: EnemyCard RazinFarhiReanimatedArtificer
razinFarhiReanimatedArtificer =
  enemy RazinFarhiReanimatedArtificer Cards.razinFarhiReanimatedArtificer (4, PerPlayer 4, 3) (1, 1)

instance HasAbilities RazinFarhiReanimatedArtificer where
  getAbilities (RazinFarhiReanimatedArtificer a) = extend1 a $ mkAbility a 1 $ forced $ CampaignEvent #after Nothing "shiftKey"

instance RunMessage RazinFarhiReanimatedArtificer where
  runMessage msg e@(RazinFarhiReanimatedArtificer attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      if attrs.exhausted
        then readyThis attrs
        else push $ SendMessage (toTarget attrs) EnemiesAttack
      pure e
    _ -> RazinFarhiReanimatedArtificer <$> liftRunMessage msg attrs
