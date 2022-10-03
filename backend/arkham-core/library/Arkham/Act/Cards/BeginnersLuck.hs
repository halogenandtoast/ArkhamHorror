module Arkham.Act.Cards.BeginnersLuck
  ( BeginnersLuck(..)
  , beginnersLuck
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.EffectMetadata
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Helpers.ChaosBag
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message hiding ( RevealToken )
import Arkham.ScenarioLogKey
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait
import Arkham.Window

newtype BeginnersLuck = BeginnersLuck ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Advancement is forced
beginnersLuck :: ActCard BeginnersLuck
beginnersLuck = act (1, A) BeginnersLuck Cards.beginnersLuck Nothing

instance HasAbilities BeginnersLuck where
  getAbilities (BeginnersLuck x) = withBaseAbilities x $ if onSide A x
    then
      [ limitedAbility (GroupLimit PerRound 1) $ mkAbility
        x
        1
        (ReactionAbility (RevealChaosToken Timing.When Anyone AnyToken) Free)
      , mkAbility x 2 $ Objective $ ForcedAbilityWithCost
        AnyWindow
        (GroupClueCost (PerPlayer 4) Anywhere)
      ]
    else []

instance RunMessage BeginnersLuck where
  runMessage msg a@(BeginnersLuck attrs) = case msg of
    UseCardAbility iid source 1 [Window Timing.When (RevealToken _ token)] _
      | isSource attrs source -> do
        tokensInBag <- getTokensInBag
        pushAll
          [ FocusTokens tokensInBag
          , chooseOne
            iid
            [ TargetLabel
                (TokenFaceTarget $ tokenFace token')
                [ CreateTokenEffect
                  (EffectModifiers
                  $ toModifiers attrs [TokenFaceModifier [tokenFace token']]
                  )
                  source
                  token
                , UnfocusTokens
                , FocusTokens [token']
                ]
            | token' <- tokensInBag
            ]
          , Remember Cheated
          ]
        pure a
    UseCardAbility _ source 2 _ _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId a) source AdvancedWithClues)
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      darkenedHall <- getSetAsideCard Locations.darkenedHall
      a <$ pushAll
        [ PlaceLocation darkenedHall
        , DiscardEncounterUntilFirst
          (toSource attrs)
          (CardWithType EnemyType <> CardWithTrait Criminal)
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
    RequestedEncounterCard source (Just ec) | isSource attrs source -> do
      darkenedHallId <- fromJustNote "missing darkened hall"
        <$> selectOne (LocationWithTitle "Darkened Hall")
      a <$ push (SpawnEnemyAt (EncounterCard ec) darkenedHallId)
    _ -> BeginnersLuck <$> runMessage msg attrs
