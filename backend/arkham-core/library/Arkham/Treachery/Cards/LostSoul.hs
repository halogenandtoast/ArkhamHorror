module Arkham.Treachery.Cards.LostSoul (
  lostSoul,
  LostSoul (..),
) where

import Arkham.Prelude

import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LostSoul = LostSoul TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostSoul :: TreacheryCard LostSoul
lostSoul = treachery LostSoul Cards.lostSoul

instance RunMessage LostSoul where
  runMessage msg t@(LostSoul attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      moreConvictionThanDoubt <- getMoreConvictionThanDoubt
      if moreConvictionThanDoubt
        then do
          push
            $ RevelationSkillTest
              iid
              source
              SkillWillpower
              (InvestigatorFieldDifficulty iid InvestigatorIntellect)
        else do
          push
            $ RevelationSkillTest
              iid
              source
              SkillIntellect
              (InvestigatorFieldDifficulty iid InvestigatorWillpower)
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0
        pure t
    _ -> LostSoul <$> runMessage msg attrs
