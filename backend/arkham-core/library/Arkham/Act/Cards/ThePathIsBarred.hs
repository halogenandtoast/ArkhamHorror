module Arkham.Act.Cards.ThePathIsBarred (
  ThePathIsBarred (..),
  thePathIsBarred,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Card
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ThePathIsBarred = ThePathIsBarred ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

thePathIsBarred :: ActCard ThePathIsBarred
thePathIsBarred =
  act
    (2, A)
    ThePathIsBarred
    Cards.thePathIsBarred
    (Just $ GroupClueCost (PerPlayer 2) (locationIs Locations.tombOfShadows))

instance HasAbilities ThePathIsBarred where
  getAbilities (ThePathIsBarred a) =
    withBaseAbilities
      a
      [ mkAbility a 1
          $ Objective
          $ ForcedAbility
          $ EnemyDefeated Timing.When Anyone ByAny
          $ enemyIs Enemies.theManInThePallidMask
      ]

instance RunMessage ThePathIsBarred where
  runMessage msg a@(ThePathIsBarred attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ advanceMode | aid == actId attrs && onSide B attrs -> do
      let
        (convictionOrDoubt, nextAct) = case advanceMode of
          AdvancedWithOther -> (Conviction, Cards.theWayOut)
          AdvancedWithClues -> (Doubt, Cards.leadingTheWay)
      enemy <- getCampaignStoryCard Enemies.theManInThePallidMask
      mTheManInThePallidMaskId <-
        selectOne
          $ enemyIs Enemies.theManInThePallidMask
      convictionOrDoubtCount <- getRecordCount convictionOrDoubt
      pushAll
        ( [ RecordCount convictionOrDoubt (convictionOrDoubtCount + 2)
          , RemoveFromBearersDeckOrDiscard enemy
          ]
            <> [ RemoveEnemy enemyId
               | enemyId <- maybeToList mTheManInThePallidMaskId
               ]
            <> [AdvanceToAct (actDeckId attrs) nextAct A (toSource attrs)]
        )
      pure a
    _ -> ThePathIsBarred <$> runMessage msg attrs
