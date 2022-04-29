module Arkham.Investigator.Cards.WendyAdams
  ( WendyAdams(..)
  , wendyAdams
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.AssetId
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message hiding (RevealToken)
import Arkham.Source
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype WendyAdams = WendyAdams InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wendyAdams :: InvestigatorCard WendyAdams
wendyAdams = investigator
  WendyAdams
  Cards.wendyAdams
  Stats
    { health = 7
    , sanity = 7
    , willpower = 4
    , intellect = 3
    , combat = 1
    , agility = 4
    }

instance HasTokenValue env WendyAdams where
  getTokenValue (WendyAdams attrs) iid ElderSign | iid == investigatorId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 0)
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance HasAbilities WendyAdams where
  getAbilities (WendyAdams attrs) =
    [ restrictedAbility
          attrs
          1
          Self
          (ReactionAbility (RevealChaosToken Timing.When You AnyToken)
          $ HandDiscardCost 1 AnyCard
          )
        & (abilityLimitL .~ PlayerLimit PerTestOrAbility 1)
    ]

instance (InvestigatorRunner env) => RunMessage env WendyAdams where
  runMessage msg i@(WendyAdams attrs@InvestigatorAttrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) [Window _ (Window.RevealToken _ token)] 1 _
      | iid == investigatorId
      -> do
        cancelToken token
        i <$ pushAll
          [ CancelNext RunWindowMessage
          , CancelNext DrawTokenMessage
          , CancelNext RevealTokenMessage
          , ReturnTokens [token]
          , UnfocusTokens
          , DrawAnotherToken iid
          ]
    -- When (DrawToken iid token) | iid == investigatorId -> i <$ pushAll
    --   [ FocusTokens [token]
    --   , CheckWindow
    --     investigatorId
    --     [Window Timing.When (Window.DrawToken investigatorId token)]
    --   , UnfocusTokens
    --   ]
    ResolveToken _drawnToken ElderSign iid | iid == investigatorId -> do
      maid <- getId @(Maybe AssetId) (CardCode "01014")
      i <$ when (isJust maid) (push PassSkillTest)
    _ -> WendyAdams <$> runMessage msg attrs
