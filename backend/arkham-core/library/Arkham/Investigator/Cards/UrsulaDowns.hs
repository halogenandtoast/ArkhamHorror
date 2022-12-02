module Arkham.Investigator.Cards.UrsulaDowns
  ( ursulaDowns
  , UrsulaDowns(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria hiding ( DuringTurn )
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype Metadata = Metadata { moveAfterTest :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype UrsulaDowns = UrsulaDowns (InvestigatorAttrs `With` Metadata)
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ursulaDowns :: InvestigatorCard UrsulaDowns
ursulaDowns = investigator
  (UrsulaDowns . (`with` Metadata False))
  Cards.ursulaDowns
  Stats
    { health = 7
    , sanity = 7
    , willpower = 3
    , intellect = 4
    , combat = 1
    , agility = 4
    }

instance HasAbilities UrsulaDowns where
  getAbilities (UrsulaDowns (attrs `With` _)) =
    [ limitedAbility (PlayerLimit PerRound 1)
        $ restrictedAbility attrs 1 Self
        $ ReactionAbility (Moves Timing.After You Anywhere Anywhere) Free
    ]

instance HasTokenValue UrsulaDowns where
  getTokenValue iid ElderSign (UrsulaDowns (attrs `With` _))
    | iid == toId attrs = do
      pure $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage UrsulaDowns where
  runMessage msg i@(UrsulaDowns (attrs `With` metadata)) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let
        windows' =
          [ Window Timing.When (Window.DuringTurn iid)
          , Window Timing.When Window.FastPlayerWindow
          , Window Timing.When Window.NonFast
          ]
      let decreaseCost = flip applyAbilityModifiers [ActionCostModifier (-1)]
      actions <-
        nub <$> concatMapM (\w -> getActionsWith iid w decreaseCost) windows'
      handCards <- field InvestigatorHand iid
      let
        investigateCards =
          filter (elem Action.Investigate . cdActions . toCardDef) handCards
      playableCards <- filterM
        (getIsPlayable iid (toSource attrs) UnpaidCost windows')
        investigateCards
      push
        $ AskPlayer
        $ chooseOne iid
        $ map
            ((\f -> f windows' []) . AbilityLabel iid)
            (filter (`abilityIs` Action.Investigate) actions)
        <> [ TargetLabel
               (CardIdTarget $ toCardId item)
               [PayCardCost iid item windows']
           | item <- playableCards
           ]
      pure i
    ResolveToken _drawnToken ElderSign iid | iid == toId attrs -> do
      pure $ UrsulaDowns $ attrs `with` Metadata True
    SkillTestEnds _ _ | moveAfterTest metadata -> do
      targets <- selectList $ accessibleFrom $ investigatorLocation attrs
      when (notNull targets) $ push $ chooseOne
        (toId attrs)
        [ Label "Do not move to a connecting location" []
        , Label
          "Move to a connecting location"
          [ chooseOne
              (toId attrs)
              [ targetLabel lid' [Move (toSource attrs) (toId attrs) lid']
              | lid' <- targets
              ]
          ]
        ]
      pure $ UrsulaDowns $ attrs `with` Metadata False
    _ -> UrsulaDowns . (`with` metadata) <$> runMessage msg attrs
