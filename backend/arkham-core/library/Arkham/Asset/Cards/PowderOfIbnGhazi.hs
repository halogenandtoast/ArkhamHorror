module Arkham.Asset.Cards.PowderOfIbnGhazi
  ( powderOfIbnGhazi
  , PowderOfIbnGhazi(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import Arkham.Cost
import Arkham.Criteria
import Arkham.Exception
import Arkham.GameValue
import Arkham.Matcher

newtype PowderOfIbnGhazi = PowderOfIbnGhazi AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

powderOfIbnGhazi :: AssetCard PowderOfIbnGhazi
powderOfIbnGhazi = asset PowderOfIbnGhazi Cards.powderOfIbnGhazi

instance HasAbilities PowderOfIbnGhazi where
  getAbilities (PowderOfIbnGhazi x) =
    [ restrictedAbility
        x
        1
        (ControlsThis <> CluesOnThis (GreaterThan $ Static 0) <> EnemyCriteria
          (EnemyExists
          $ ExhaustedEnemy
          <> EnemyAt YourLocation
          <> EnemyWithTitle "Brood of Yog-Sothoth"
          )
        )
        (FastAbility Free)
    ]

instance RunMessage PowderOfIbnGhazi where
  runMessage msg (PowderOfIbnGhazi attrs) = case msg of
    InvestigatorPlayAsset _ aid | aid == toId attrs -> do
      survivedCount <- countM
        getHasRecord
        [ DrHenryArmitageSurvivedTheDunwichLegacy
        , ProfessorWarrenRiceSurvivedTheDunwichLegacy
        , DrFrancisMorganSurvivedTheDunwichLegacy
        , ZebulonWhateleySurvivedTheDunwichLegacy
        , EarlSawyerSurvivedTheDunwichLegacy
        ]
      PowderOfIbnGhazi <$> runMessage msg (attrs & cluesL .~ survivedCount)
    UseCardAbility you source 1 _ _ | isSource attrs source -> do
      targets <-
        selectListMap EnemyTarget
        $ EnemyWithTitle "Brood of Yog-Sothoth"
        <> EnemyAt (LocationWithInvestigator $ InvestigatorWithId you)
        <> ExhaustedEnemy
      case targets of
        [] -> throwIO $ InvalidState "missing brood of yog sothoth"
        [x] -> push (PlaceClues x 1)
        xs -> push (chooseOne you [ TargetLabel x [PlaceClues x 1] | x <- xs ])
      pure . PowderOfIbnGhazi $ attrs & cluesL %~ max 0 . subtract 1
    _ -> PowderOfIbnGhazi <$> runMessage msg attrs

