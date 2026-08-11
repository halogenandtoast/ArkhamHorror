module Arkham.Homebrew.DarkMatter.Assets.MedicalFoam (medicalFoam) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n, shuffleIntoScanningDeck)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

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

-- The damaged investigator and the amount of damage they just took.
getDamaged :: [Window] -> [(Target, Int)]
getDamaged = \case
  (windowType -> Window.TakeDamage _ _ target n) : rest -> (target, n) : getDamaged rest
  _ : rest -> getDamaged rest
  [] -> []

instance RunMessage MedicalFoam where
  runMessage msg a@(MedicalFoam attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select Anyone
      chooseOneM iid $ campaignI18n do
        targets investigators \iid' -> putCardIntoPlay iid' attrs
        labeled' "medicalFoam.doNotPutIntoPlay" $ shuffleIntoScanningDeck [attrs]
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 (getDamaged -> damaged) _ -> do
      for_ (take 1 damaged) \(target, n) -> healDamage target (attrs.ability 1) n
      pure a
    _ -> MedicalFoam <$> liftRunMessage msg attrs
