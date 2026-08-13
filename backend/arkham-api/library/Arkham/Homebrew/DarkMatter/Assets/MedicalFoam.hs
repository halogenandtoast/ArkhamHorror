module Arkham.Homebrew.DarkMatter.Assets.MedicalFoam (medicalFoam) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Window (getDamaged)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n, shuffleIntoScanningDeck)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MedicalFoam = MedicalFoam AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medicalFoam :: AssetCard MedicalFoam
medicalFoam = asset MedicalFoam Cards.medicalFoam

instance HasAbilities MedicalFoam where
  getAbilities (MedicalFoam a) =
    [ controlled a 1 (thisExists a $ AssetWithUses Supply)
        $ triggered
          ( InvestigatorTakeDamage
              #after
              (HealableInvestigator (toSource a) #damage $ colocatedWithMatch You)
              AnySource
          )
          (exhaust a <> assetUseCost a Supply 1)
    ]

instance RunMessage MedicalFoam where
  runMessage msg a@(MedicalFoam attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select Anyone
      chooseOneM iid $ campaignI18n do
        targets investigators (`putCardIntoPlay` attrs)
        labeled' "medicalFoam.doNotPutIntoPlay" $ shuffleIntoScanningDeck [attrs]
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 (getDamaged -> damaged) _ -> do
      for_ (take 1 damaged) \(target, n) -> healDamage target (attrs.ability 1) n
      pure a
    _ -> MedicalFoam <$> liftRunMessage msg attrs
