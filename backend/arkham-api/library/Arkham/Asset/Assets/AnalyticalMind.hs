module Arkham.Asset.Assets.AnalyticalMind (analyticalMind) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype AnalyticalMind = AnalyticalMind AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

analyticalMind :: AssetCard AnalyticalMind
analyticalMind = asset AnalyticalMind Cards.analyticalMind

instance HasAbilities AnalyticalMind where
  getAbilities (AnalyticalMind attrs) =
    [ restricted attrs 1 ControlsThis
        $ triggered (CommittedCards #after You $ LengthIs $ EqualTo $ Static 1) (exhaust attrs)
    ]

instance HasModifiersFor AnalyticalMind where
  getModifiersFor (AnalyticalMind a) = for_ a.controller \iid -> do
    modified_ a iid [CanCommitToSkillTestPerformedByAnInvestigatorAt Anywhere]

instance RunMessage AnalyticalMind where
  runMessage msg a@(AnalyticalMind attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure a
    _ -> AnalyticalMind <$> liftRunMessage msg attrs
