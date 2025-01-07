module Arkham.Asset.Assets.PowderOfIbnGhazi (powderOfIbnGhazi) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.Helpers.Log (getHasRecord)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype PowderOfIbnGhazi = PowderOfIbnGhazi AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

powderOfIbnGhazi :: AssetCard PowderOfIbnGhazi
powderOfIbnGhazi = asset PowderOfIbnGhazi Cards.powderOfIbnGhazi

instance HasAbilities PowderOfIbnGhazi where
  getAbilities (PowderOfIbnGhazi x) =
    [ controlled
        x
        1
        (AnyCluesOnThis <> exists (enemy_ $ #exhausted <> at_ YourLocation <> "Brood of Yog-Sothoth"))
        (FastAbility Free)
    ]

instance RunMessage PowderOfIbnGhazi where
  runMessage msg (PowderOfIbnGhazi attrs) = runQueueT $ case msg of
    InvestigatorPlayAsset _ (is attrs -> True) -> do
      survivedCount <-
        countM
          getHasRecord
          [ DrHenryArmitageSurvivedTheDunwichLegacy
          , ProfessorWarrenRiceSurvivedTheDunwichLegacy
          , DrFrancisMorganSurvivedTheDunwichLegacy
          , ZebulonWhateleySurvivedTheDunwichLegacy
          , EarlSawyerSurvivedTheDunwichLegacy
          ]
      attrs' <- liftRunMessage msg attrs
      pure . PowderOfIbnGhazi $ attrs' & tokensL %~ setTokens Clue survivedCount
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      xs <- select $ enemy_ $ "Brood of Yog-Sothoth" <> at_ (locationWithInvestigator iid) <> #exhausted
      chooseTargetM iid xs \x -> placeClues (attrs.ability 1) x 1
      pure . PowderOfIbnGhazi $ attrs & tokensL %~ decrementTokens Clue
    _ -> PowderOfIbnGhazi <$> liftRunMessage msg attrs
