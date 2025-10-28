module Arkham.Skill.Cards.TorrentOfPower (torrentOfPower) where

import Arkham.Asset.Uses
import Arkham.Calculation
import Arkham.Cost
import Arkham.Helpers.SkillTest (duringSkillTest)
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype TorrentOfPower = TorrentOfPower SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

torrentOfPower :: SkillCard TorrentOfPower
torrentOfPower =
  skillWith TorrentOfPower Cards.torrentOfPower
    $ additionalCostL
    ?~ UpTo (Fixed 3) (UseCost (AssetControlledBy You) Charge 1)

instance HasModifiersFor TorrentOfPower where
  getModifiersFor (TorrentOfPower attrs) = do
    duringSkillTest do
      let n = maybe 0 totalUsesPayment attrs.additionalPayment
      addSkillIcons attrs $ cycleN n [#willpower, #wild]

instance RunMessage TorrentOfPower where
  runMessage msg (TorrentOfPower attrs) = TorrentOfPower <$> runMessage msg attrs
