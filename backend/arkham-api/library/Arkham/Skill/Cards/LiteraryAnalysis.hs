module Arkham.Skill.Cards.LiteraryAnalysis (literaryAnalysis) where

import Arkham.Asset.Uses qualified as Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Trait (Trait (Tome))

newtype LiteraryAnalysis = LiteraryAnalysis SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

literaryAnalysis :: SkillCard LiteraryAnalysis
literaryAnalysis = skill LiteraryAnalysis Cards.literaryAnalysis

instance RunMessage LiteraryAnalysis where
  runMessage msg s@(LiteraryAnalysis attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ (isTarget attrs -> True) _ _ -> do
      tomes <-
        select
          $ withTrait Tome
          <> assetAtLocationWith iid
          <> AssetCanHaveUses Uses.Secret
          <> AssetNotAtUsesX
      chooseOrRunOneM iid $ targets tomes $ addUsesOn attrs Uses.Secret 1
      pure s
    _ -> LiteraryAnalysis <$> liftRunMessage msg attrs
