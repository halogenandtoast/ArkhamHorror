module Arkham.Types.Asset.Cards.PowderOfIbnGhazi
  ( powderOfIbnGhazi
  , PowderOfIbnGhazi(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

newtype PowderOfIbnGhazi = PowderOfIbnGhazi AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

powderOfIbnGhazi :: AssetCard PowderOfIbnGhazi
powderOfIbnGhazi = asset PowderOfIbnGhazi Cards.powderOfIbnGhazi

instance HasAbilities PowderOfIbnGhazi where
  getAbilities (PowderOfIbnGhazi x) =
    [ restrictedAbility
        x
        1
        (OwnsThis <> CluesOnThis (GreaterThan $ Static 0) <> EnemyCriteria
          (EnemyExists
          $ ExhaustedEnemy
          <> EnemyAt YourLocation
          <> EnemyWithTitle "Brood of Yog-Sothoth"
          )
        )
        (FastAbility Free)
    ]

instance AssetRunner env => RunMessage env PowderOfIbnGhazi where
  runMessage msg (PowderOfIbnGhazi attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == toId attrs -> do
      survivedCount <- countM
        getHasRecord
        [ DrHenryArmitageSurvivedTheDunwichLegacy
        , ProfessorWarrenRiceSurvivedTheDunwichLegacy
        , DrFrancisMorganSurvivedTheDunwichLegacy
        , ZebulonWhateleySurvivedTheDunwichLegacy
        , EarlSawyerSurvivedTheDunwichLegacy
        ]
      PowderOfIbnGhazi <$> runMessage msg (attrs & cluesL .~ survivedCount)
    UseCardAbility you source _ 1 _ | isSource attrs source -> do
      yourLocation <- LocationWithId <$> getId you
      targets <-
        selectListMap EnemyTarget
        $ EnemyWithTitle "Brood of Yog-Sothoth"
        <> EnemyAt yourLocation
        <> ExhaustedEnemy
      case targets of
        [] -> throwIO $ InvalidState "missing brood of yog sothoth"
        [x] -> push (PlaceClues x 1)
        xs -> push (chooseOne you [ TargetLabel x [PlaceClues x 1] | x <- xs ])
      pure . PowderOfIbnGhazi $ attrs & cluesL %~ max 0 . subtract 1
    _ -> PowderOfIbnGhazi <$> runMessage msg attrs

