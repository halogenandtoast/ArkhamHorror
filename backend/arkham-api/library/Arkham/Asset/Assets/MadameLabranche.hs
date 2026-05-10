module Arkham.Asset.Assets.MadameLabranche (madameLabranche) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.GameValue
import Arkham.I18n
import Arkham.Matcher

newtype MadameLabranche = MadameLabranche AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madameLabranche :: AssetCard MadameLabranche
madameLabranche = ally MadameLabranche Cards.madameLabranche (2, 2)

instance HasAbilities MadameLabranche where
  getAbilities (MadameLabranche attrs) =
    [ (cardI18n $ withI18nTooltip "madameLabranche.fastIfYouHaveNoCardsInYourHandExhaustMadameLabrancheDraw1Car")
        $ controlled attrs 1 (youExist $ HandWith (LengthIs $ EqualTo $ Static 0))
        $ FastAbility (exhaust attrs)
    , (cardI18n $ withI18nTooltip "madameLabranche.fastIfYouHaveNoResourcesExhaustMadameLabrancheGain1Resource")
        $ controlled attrs 2 (youExist $ InvestigatorWithResources (EqualTo $ Static 0))
        $ FastAbility (exhaust attrs)
    ]

instance RunMessage MadameLabranche where
  runMessage msg a@(MadameLabranche attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      gainResources iid (attrs.ability 2) 1
      pure a
    _ -> MadameLabranche <$> liftRunMessage msg attrs
