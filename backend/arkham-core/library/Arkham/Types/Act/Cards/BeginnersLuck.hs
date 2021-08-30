module Arkham.Types.Act.Cards.BeginnersLuck
  ( BeginnersLuck(..)
  , beginnersLuck
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EffectMetadata
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealToken)
import Arkham.Types.Modifier
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window

newtype BeginnersLuck = BeginnersLuck ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Advancement is forced
beginnersLuck :: ActCard BeginnersLuck
beginnersLuck = act (1, A) BeginnersLuck Cards.beginnersLuck Nothing

instance HasAbilities env BeginnersLuck where
  getAbilities iid window (BeginnersLuck x) =
    withBaseAbilities iid window x $ pure $ if onSide A x
      then
        [ mkAbility
            x
            1
            (ReactionAbility (RevealChaosToken Timing.When Anyone AnyToken) Free
            )
          & (abilityLimitL .~ GroupLimit PerRound 1)
        , mkAbility x 2 $ Objective $ ForcedAbilityWithCost
          AnyWindow
          (GroupClueCost (PerPlayer 4) Anywhere)
        ]
      else []

instance ActRunner env => RunMessage env BeginnersLuck where
  runMessage msg a@(BeginnersLuck attrs) = case msg of
    UseCardAbility iid source [Window Timing.When (RevealToken _ token)] 1 _
      | isSource attrs source -> do
        tokensInBag <- getList @Token ()
        a <$ pushAll
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
          , Remember $ Cheated iid
          ]
    UseCardAbility _ source _ 2 _ -> a <$ push (AdvanceAct (toId a) source)
    AdvanceAct aid _ | aid == toId a && onSide B attrs -> do
      lid <- getRandom
      a <$ pushAll
        [ PlaceLocation lid Locations.darkenedHall
        , DiscardEncounterUntilFirst
          (toSource attrs)
          (CardWithType EnemyType <> CardWithTrait Criminal)
        , NextAct aid "02067"
        ]
    RequestedEncounterCard source (Just ec) | isSource attrs source -> do
      darkenedHallId <- fromJustNote "missing darkened hall"
        <$> selectOne (LocationWithTitle "Darkened Hall")
      a <$ push (SpawnEnemyAt (EncounterCard ec) darkenedHallId)
    _ -> BeginnersLuck <$> runMessage msg attrs
