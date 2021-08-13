module Arkham.Types.Asset.Cards.PowderOfIbnGhazi
  ( powderOfIbnGhazi
  , PowderOfIbnGhazi(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype PowderOfIbnGhazi = PowderOfIbnGhazi AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

powderOfIbnGhazi :: AssetCard PowderOfIbnGhazi
powderOfIbnGhazi = asset PowderOfIbnGhazi Cards.powderOfIbnGhazi

instance HasActions PowderOfIbnGhazi where
  getActions (PowderOfIbnGhazi x) =
    [ restrictedAbility
        x
        1
        (OwnsThis <> CluesOnThis (GreaterThan $ Static 0) <> EnemyExists
          (ExhaustedEnemy <> EnemyAt YourLocation <> EnemyWithTitle
            "Brood of Yog-Sothoth"
          )
        )
        (FastAbility Free)
    ]

instance HasModifiersFor env PowderOfIbnGhazi

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasSet ExhaustedEnemyId env LocationId
  , HasId LocationId env InvestigatorId
  , HasSet StoryEnemyId env CardCode
  , HasRecord env
  )
  => RunMessage env PowderOfIbnGhazi where
  runMessage msg (PowderOfIbnGhazi attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs -> do
      survivedCount <- countM
        getHasRecord
        [ DrHenryArmitageSurvivedTheDunwichLegacy
        , ProfessorWarrenRiceSurvivedTheDunwichLegacy
        , DrFrancisMorganSurvivedTheDunwichLegacy
        , ZebulonWhateleySurvivedTheDunwichLegacy
        , EarlSawyerSurvivedTheDunwichLegacy
        ]
      PowderOfIbnGhazi <$> runMessage msg (attrs & cluesL .~ survivedCount)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      broodOfYogSothoth <- mapSet unStoryEnemyId <$> getSet (CardCode "02255")
      lid <- getId @LocationId iid
      exhaustedBroodOfYogSothothAtLocation <-
        intersection broodOfYogSothoth
        . mapSet unExhaustedEnemyId
        <$> getSet lid
      case setToList exhaustedBroodOfYogSothothAtLocation of
        [] -> throwIO $ InvalidState "missing brood of yog sothoth"
        [x] -> push (PlaceClues (EnemyTarget x) 1)
        xs -> push
          (chooseOne
            iid
            [ TargetLabel x [PlaceClues x 1] | x <- map EnemyTarget xs ]
          )
      pure . PowderOfIbnGhazi $ attrs & cluesL %~ max 0 . subtract 1
    _ -> PowderOfIbnGhazi <$> runMessage msg attrs

