module Arkham.Asset.Assets.MortarAndPestle (mortarAndPestle) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (DiscoverClues)
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype MortarAndPestle = MortarAndPestle AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mortarAndPestle :: AssetCard MortarAndPestle
mortarAndPestle = asset MortarAndPestle Cards.mortarAndPestle

instance HasModifiersFor MortarAndPestle where
  getModifiersFor (MortarAndPestle a) = for_ a.controller \iid -> do
    controllerGets
      a
      [CanSpendUsesAsResourceOnCardFromInvestigator a.id #resource (InvestigatorWithId iid) #spell]

instance HasAbilities MortarAndPestle where
  getAbilities (MortarAndPestle a) =
    [ restricted a 1 ControlsThis $ triggered (DiscoverClues #after You Anywhere (atLeast 1)) (exhaust a)
    ]

instance RunMessage MortarAndPestle where
  runMessage msg a@(MortarAndPestle attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      addUses (attrs.ability 1) attrs Resource 1
      pure a
    _ -> MortarAndPestle <$> liftRunMessage msg attrs
