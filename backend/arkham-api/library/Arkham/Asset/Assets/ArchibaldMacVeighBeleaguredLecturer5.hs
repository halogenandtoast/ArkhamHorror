module Arkham.Asset.Assets.ArchibaldMacVeighBeleaguredLecturer5 (archibaldMacVeighBeleaguredLecturer5) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype ArchibaldMacVeighBeleaguredLecturer5 = ArchibaldMacVeighBeleaguredLecturer5 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

archibaldMacVeighBeleaguredLecturer5 :: AssetCard ArchibaldMacVeighBeleaguredLecturer5
archibaldMacVeighBeleaguredLecturer5 = ally ArchibaldMacVeighBeleaguredLecturer5 Cards.archibaldMacVeighBeleaguredLecturer5 (2, 3)

instance HasModifiersFor ArchibaldMacVeighBeleaguredLecturer5 where
  getModifiersFor (ArchibaldMacVeighBeleaguredLecturer5 a) = for_ a.controller \iid -> do
    assets <- select $ assetControlledBy iid <> AssetWithUses Secret
    controllerGets a $ SkillModifier #intellect 1
      : [ CanSpendUsesAsResourceOnCardFromInvestigator
            aid
            Secret
            (InvestigatorWithId iid)
            (#event <> #insight)
        | aid <- assets
        ]

instance RunMessage ArchibaldMacVeighBeleaguredLecturer5 where
  runMessage msg (ArchibaldMacVeighBeleaguredLecturer5 attrs) = ArchibaldMacVeighBeleaguredLecturer5 <$> runMessage msg attrs
