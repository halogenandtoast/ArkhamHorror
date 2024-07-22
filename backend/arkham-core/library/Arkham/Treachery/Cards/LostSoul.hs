module Arkham.Treachery.Cards.LostSoul (lostSoul, LostSoul (..)) where

import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LostSoul = LostSoul TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostSoul :: TreacheryCard LostSoul
lostSoul = treachery LostSoul Cards.lostSoul

instance RunMessage LostSoul where
  runMessage msg t@(LostSoul attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      moreConvictionThanDoubt <- getMoreConvictionThanDoubt
      sid <- getRandom
      push
        $ if moreConvictionThanDoubt
          then
            revelationSkillTest sid iid attrs #willpower
              $ InvestigatorFieldCalculation iid InvestigatorIntellect
          else
            revelationSkillTest sid iid attrs #intellect
              $ InvestigatorFieldCalculation iid InvestigatorWillpower
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0
      pure t
    _ -> LostSoul <$> runMessage msg attrs
