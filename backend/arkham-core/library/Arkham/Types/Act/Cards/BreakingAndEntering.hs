module Arkham.Types.Act.Cards.BreakingAndEntering
  ( BreakingAndEntering(..)
  , breakingAndEntering
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype BreakingAndEntering = BreakingAndEntering ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingAndEntering :: ActCard BreakingAndEntering
breakingAndEntering =
  act (2, A) BreakingAndEntering Cards.breakingAndEntering Nothing

instance HasAbilities env BreakingAndEntering where
  getAbilities _ _ (BreakingAndEntering x) = pure
    [ mkAbility x 1 $ ForcedAbility $ Enters Timing.When You $ locationIs
        Cards.exhibitHallRestrictedHall
    ]

instance ActRunner env => RunMessage env BreakingAndEntering where
  runMessage msg a@(BreakingAndEntering attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      mHuntingHorror <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      haroldWalsted <- EncounterCard <$> genEncounterCard Assets.haroldWalsted
      case mHuntingHorror of
        Just eid -> do
          lid <- fromJustNote "Exhibit Hall (Restricted Hall) missing"
            <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
          a <$ pushAll
            [ chooseOne
              leadInvestigatorId
              [ TargetLabel
                  (InvestigatorTarget iid)
                  [AddCampaignCardToDeck iid Assets.haroldWalsted]
              | iid <- investigatorIds
              ]
            , EnemySpawn Nothing lid eid
            , Ready (EnemyTarget eid)
            , NextAct (toId attrs) "02125"
            ]
        Nothing -> a <$ pushAll
          [ chooseOne
            leadInvestigatorId
            [ TargetLabel
                (InvestigatorTarget iid)
                [TakeControlOfSetAsideAsset iid haroldWalsted]
            | iid <- investigatorIds
            ]
          , FindEncounterCard
            leadInvestigatorId
            (toTarget attrs)
            (CardWithCardCode "02141")
          ]
    FoundEnemyInVoid _ target eid | isTarget attrs target -> do
      lid <- fromJustNote "Exhibit Hall (Restricted Hall) missing"
        <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
      a <$ pushAll
        [EnemySpawnFromVoid Nothing lid eid, NextAct (toId attrs) "02125"]
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      lid <- fromJustNote "Exhibit Hall (Restricted Hall) missing"
        <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
      a <$ pushAll
        [SpawnEnemyAt (EncounterCard ec) lid, NextAct (toId attrs) "02125"]
    _ -> BreakingAndEntering <$> runMessage msg attrs
