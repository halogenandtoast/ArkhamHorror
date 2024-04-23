module Arkham.Asset.Cards.PowderOfIbnGhazi (
  powderOfIbnGhazi,
  PowderOfIbnGhazi (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import Arkham.Exception
import Arkham.Matcher
import Arkham.Token

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
        ( ControlsThis
            <> CluesOnThis (GreaterThan $ Static 0)
            <> EnemyCriteria
              ( EnemyExists
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
      survivedCount <-
        countM
          getHasRecord
          [ DrHenryArmitageSurvivedTheDunwichLegacy
          , ProfessorWarrenRiceSurvivedTheDunwichLegacy
          , DrFrancisMorganSurvivedTheDunwichLegacy
          , ZebulonWhateleySurvivedTheDunwichLegacy
          , EarlSawyerSurvivedTheDunwichLegacy
          ]
      attrs' <- runMessage msg attrs
      pure . PowderOfIbnGhazi $ attrs' & tokensL %~ setTokens Clue survivedCount
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      player <- getPlayer iid
      targets <-
        selectMap EnemyTarget
          $ EnemyWithTitle "Brood of Yog-Sothoth"
          <> EnemyAt (LocationWithInvestigator $ InvestigatorWithId iid)
          <> ExhaustedEnemy
      case targets of
        [] -> throwIO $ InvalidState "missing brood of yog sothoth"
        [x] -> push (PlaceClues (toAbilitySource attrs 1) x 1)
        xs -> push (chooseOne player [TargetLabel x [PlaceClues (toAbilitySource attrs 1) x 1] | x <- xs])
      pure . PowderOfIbnGhazi $ attrs & tokensL %~ decrementTokens Clue
    _ -> PowderOfIbnGhazi <$> runMessage msg attrs
