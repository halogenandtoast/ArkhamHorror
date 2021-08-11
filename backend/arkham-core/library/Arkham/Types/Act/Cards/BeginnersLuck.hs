module Arkham.Types.Act.Cards.BeginnersLuck
  ( BeginnersLuck(..)
  , beginnersLuck
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EffectMetadata
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import qualified Arkham.Types.Window as W

newtype BeginnersLuck = BeginnersLuck ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

beginnersLuck :: ActCard BeginnersLuck
beginnersLuck = act
  (1, A)
  BeginnersLuck
  Cards.beginnersLuck
  (Just $ GroupClueCost (PerPlayer 4) Nothing)

instance HasActions BeginnersLuck where
  getActions (BeginnersLuck x) =
    [ (mkAbility x 1
      $ ReactionAbility (RevealChaosToken W.When You AnyToken) Free
      )
        { abilityLimit = GroupLimit PerRound 1
        }
      , restrictedAbility
        x
        2
        (InvestigatorsHaveSpendableClues $ AtLeast (PerPlayer 4))
        (Objective $ ForcedAbility AnyWindow)
      ]
      <> getActions x

instance
  ( ActAttrsRunner env
  , HasList Token env ()
  )
  => RunMessage env BeginnersLuck where
  runMessage msg a@(BeginnersLuck attrs@ActAttrs {..}) = case msg of
    UseCardAbility iid source [W.Window W.When (W.RevealToken _ token)] 1 _
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
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId attrs) (toSource attrs))
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
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
        <$> getId (LocationWithTitle "Darkened Hall")
      a <$ push (SpawnEnemyAt (EncounterCard ec) darkenedHallId)
    _ -> BeginnersLuck <$> runMessage msg attrs
