module Arkham.Types.Act.Cards.NightAtTheMuseum
  ( NightAtTheMuseum(..)
  , nightAtTheMuseum
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype NightAtTheMuseum = NightAtTheMuseum ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightAtTheMuseum :: ActCard NightAtTheMuseum
nightAtTheMuseum = act (2, A) NightAtTheMuseum Cards.nightAtTheMuseum Nothing

instance HasAbilities env NightAtTheMuseum where
  getAbilities _ _ (NightAtTheMuseum x) = pure
    [ mkAbility x 1 $ ForcedAbility $ Enters Timing.When You $ locationIs
        Cards.exhibitHallRestrictedHall
    ]

instance ActRunner env => RunMessage env NightAtTheMuseum where
  runMessage msg a@(NightAtTheMuseum attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      mHuntingHorror <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      case mHuntingHorror of
        Just eid -> do
          lid <- fromJustNote "Exhibit Hall (Restricted Hall) missing"
            <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
          a <$ pushAll
            [ EnemySpawn Nothing lid eid
            , Ready (EnemyTarget eid)
            , NextAct (toId attrs) "02125"
            ]
        Nothing -> a <$ push
          (FindEncounterCard
            leadInvestigatorId
            (toTarget attrs)
            (CardWithCardCode "02141")
          )
    FoundEnemyInVoid _ target eid | isTarget attrs target -> do
      lid <- getJustLocationIdByName
        (mkFullName "Exhibit Hall" "Restricted Hall")
      a <$ pushAll
        [EnemySpawnFromVoid Nothing lid eid, NextAct (toId attrs) "02125"]
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      lid <- getJustLocationIdByName
        (mkFullName "Exhibit Hall" "Restricted Hall")
      a <$ pushAll
        [SpawnEnemyAt (EncounterCard ec) lid, NextAct (toId attrs) "02125"]
    _ -> NightAtTheMuseum <$> runMessage msg attrs
