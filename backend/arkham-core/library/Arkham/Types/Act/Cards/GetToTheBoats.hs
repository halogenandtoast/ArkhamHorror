module Arkham.Types.Act.Cards.GetToTheBoats
  ( GetToTheBoats(..)
  , getToTheBoats
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.AssetMatcher
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Phase
import Arkham.Types.Target
import Arkham.Types.Window

newtype GetToTheBoats = GetToTheBoats ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

getToTheBoats :: GetToTheBoats
getToTheBoats =
  GetToTheBoats $ baseAttrs "82006" "Get to the Boats!" (Act 2 A) Nothing

ability :: ActAttrs -> Ability
ability a = mkAbility a 1 ForcedAbility

instance ActionRunner env => HasActions env GetToTheBoats where
  getActions iid (PhaseBegins MythosPhase) (GetToTheBoats x) = do
    leadInvestigatorId <- getLeadInvestigatorId
    pure [ UseAbility iid (ability x) | iid == leadInvestigatorId ]
  getActions iid window (GetToTheBoats x) = getActions iid window x

instance
  ( HasSet AssetId env AssetMatcher
  , HasId LocationId env InvestigatorId
  , HasName env LocationId
  , ActRunner env
  )
  => RunMessage env GetToTheBoats where
  runMessage msg a@(GetToTheBoats attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      gondola <- genEncounterCard Locations.gondola
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ pushAll
        [ InvestigatorDrewEncounterCard leadInvestigatorId gondola
        , NextAct actId "82006"
        ]
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      maskedCarnevaleGoers <- getSetList @AssetId
        (AssetWithTitle "Masked Carnevale-Goer")
      case maskedCarnevaleGoers of
        [] -> pure a
        xs -> a <$ pushAll
          [chooseOne iid [ Flip (toSource attrs) (AssetTarget x) | x <- xs ]]
    WhenEnterLocation _ lid -> do
      name <- getName lid
      if nameTitle name == "Canal-side"
        then do
          investigatorIds <- getInvestigatorIds
          locationIds <- traverse (getId @LocationId) investigatorIds
          a <$ when
            (all (== lid) locationIds)
            (push $ AdvanceAct actId (toSource attrs))
        else pure a
    _ -> GetToTheBoats <$> runMessage msg attrs
