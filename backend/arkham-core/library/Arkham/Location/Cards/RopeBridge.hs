module Arkham.Location.Cards.RopeBridge (
  ropeBridge,
  RopeBridge (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Name
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype RopeBridge = RopeBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ropeBridge :: LocationCard RopeBridge
ropeBridge = location RopeBridge Cards.ropeBridge 2 (PerPlayer 1)

instance HasAbilities RopeBridge where
  getAbilities (RopeBridge attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 Here
          $ ForcedAbility
          $ AttemptExplore
            Timing.When
            You
      ]

instance RunMessage RopeBridge where
  runMessage msg l@(RopeBridge attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ beginSkillTest iid source (toTarget attrs) SkillAgility 2
      pure l
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          mRiverCanyon <-
            find ((== "River Canyon") . nameTitle . cdName . toCardDef)
              <$> getExplorationDeck
          (riverCanyonId, mPlacement) <- case mRiverCanyon of
            Just riverCanyon -> second Just <$> placeLocation riverCanyon
            Nothing -> (,Nothing) <$> selectJust (LocationWithTitle "River Canyon")
          pushAll
            $ [ CancelNext (toSource attrs) ExploreMessage
              , InvestigatorAssignDamage iid source DamageAny 2 0
              , SetActions iid source 0
              , ChooseEndTurn iid
              ]
            <> maybeToList mPlacement
            <> [MoveTo $ move source iid riverCanyonId]
          pure l
    _ -> RopeBridge <$> runMessage msg attrs
