module Arkham.Types.Act.Cards.TheChamberOfTheBeast
  ( TheChamberOfTheBeast(..)
  , theChamberOfTheBeast
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher hiding (EnemyDefeated)
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution

newtype TheChamberOfTheBeast = TheChamberOfTheBeast ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

theChamberOfTheBeast :: ActCard TheChamberOfTheBeast
theChamberOfTheBeast =
  act (2, A) TheChamberOfTheBeast Cards.theChamberOfTheBeast Nothing

instance ActionRunner env => HasAbilities env TheChamberOfTheBeast where
  getAbilities i window (TheChamberOfTheBeast x) = do
    mHiddenChamberId <- getId @(Maybe LocationId)
      (LocationWithTitle "The Hidden Chamber")
    clueCount <- maybe (pure 0) (fmap unClueCount . getCount) mHiddenChamberId
    leadInvestigatorId <- getLeadInvestigatorId
    if clueCount == 0
      then pure
        [ mkAbility (toSource x) 1 LegacyForcedAbility | i == leadInvestigatorId ]
      else getAbilities i window x

instance ActRunner env => RunMessage env TheChamberOfTheBeast where
  runMessage msg a@(TheChamberOfTheBeast attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      resolution <- maybe 3 (const 2)
        <$> selectOne (assetIs Cards.theNecronomiconOlausWormiusTranslation)
      a <$ push
        (chooseOne
          leadInvestigatorId
          [ Label
              ("Resolution " <> tshow resolution)
              [ScenarioResolution $ Resolution resolution]
          ]
        )
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct actId source)
    EnemyDefeated _ _ _ "02216" _ _ ->
      a <$ push (ScenarioResolution $ Resolution 1)
    _ -> TheChamberOfTheBeast <$> runMessage msg attrs
