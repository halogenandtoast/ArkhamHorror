module Arkham.Types.Asset.Cards.PowderOfIbnGhazi
  ( powderOfIbnGhazi
  , PowderOfIbnGhazi(..)
  )
where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.CampaignLogKey

newtype PowderOfIbnGhazi = PowderOfIbnGhazi AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

powderOfIbnGhazi :: AssetId -> PowderOfIbnGhazi
powderOfIbnGhazi uuid = PowderOfIbnGhazi $ baseAttrs uuid "02219"

instance
  ( HasId LocationId env InvestigatorId
  , HasSet ExhaustedEnemyId env LocationId
  , HasSet StoryEnemyId env CardCode
  )
  => HasActions env PowderOfIbnGhazi where
  getActions iid FastPlayerWindow (PowderOfIbnGhazi attrs) =
    withBaseActions iid FastPlayerWindow attrs $ do
      broodOfYogSothoth <- mapSet unStoryEnemyId <$> getSet (CardCode "02255")
      lid <- getId @LocationId iid
      exhaustedBroodOfYogSothothAtLocation <-
        intersection broodOfYogSothoth
        . mapSet unExhaustedEnemyId
        <$> getSet lid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource attrs) 1 (ReactionAbility Free))
        | ownedBy attrs iid
          && not (null exhaustedBroodOfYogSothothAtLocation)
          && (assetClues attrs > 0)
        ]
  getActions iid window (PowderOfIbnGhazi attrs) = getActions iid window attrs

instance HasModifiersFor env PowderOfIbnGhazi where
  getModifiersFor = noModifiersFor

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
        , ZebulonWhatelySurvivedTheDunwichLegacy
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
        [x] -> unshiftMessage (PlaceClues (EnemyTarget x) 1)
        xs -> unshiftMessage
          (chooseOne
            iid
            [ TargetLabel x [PlaceClues x 1] | x <- map EnemyTarget xs ]
          )
      pure . PowderOfIbnGhazi $ attrs & cluesL %~ max 0 . subtract 1
    _ -> PowderOfIbnGhazi <$> runMessage msg attrs

