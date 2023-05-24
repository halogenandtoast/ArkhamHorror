module Arkham.Investigator.Cards.WendyAdams (
  WendyAdams (..),
  wendyAdams,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message hiding (RevealToken)
import Arkham.Timing qualified as Timing
import Arkham.Window qualified as Window

newtype WendyAdams = WendyAdams InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wendyAdams :: InvestigatorCard WendyAdams
wendyAdams =
  investigator
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

instance HasTokenValue WendyAdams where
  getTokenValue iid ElderSign (WendyAdams attrs) | iid == toId attrs = do
    pure $ TokenValue ElderSign $ PositiveModifier 0
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance HasAbilities WendyAdams where
  getAbilities (WendyAdams attrs) =
    [ limitedAbility (PlayerLimit PerTestOrAbility 1) $
        restrictedAbility attrs 1 Self $
          ReactionAbility (RevealChaosToken Timing.When You AnyToken) $
            HandDiscardCost 1 AnyCard
    ]

instance RunMessage WendyAdams where
  runMessage msg i@(WendyAdams attrs@InvestigatorAttrs {..}) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (Window.revealedTokens -> [token]) _ -> do
      cancelToken token
      pushAll
        [ CancelEachNext
            (toSource attrs)
            [RunWindowMessage, DrawTokenMessage, RevealTokenMessage]
        , ReturnTokens [token]
        , UnfocusTokens
        , DrawAnotherToken (toId attrs)
        ]
      pure i
    ResolveToken _ ElderSign iid | iid == investigatorId -> do
      maid <- selectOne $ assetIs Assets.wendysAmulet
      when (isJust maid) (push PassSkillTest)
      pure i
    _ -> WendyAdams <$> runMessage msg attrs
