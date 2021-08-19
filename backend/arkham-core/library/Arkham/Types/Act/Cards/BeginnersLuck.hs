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
import Arkham.Types.Message hiding (RevealToken)
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window

newtype BeginnersLuck = BeginnersLuck ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

beginnersLuck :: ActCard BeginnersLuck
beginnersLuck = act
  (1, A)
  BeginnersLuck
  Cards.beginnersLuck
  (Just $ GroupClueCost (PerPlayer 4) Nothing)

ability :: Token -> ActAttrs -> Ability
ability token attrs =
  (mkAbility (toSource attrs) 1 (LegacyReactionAbility Free))
    { abilityLimit = GroupLimit PerRound 1
    , abilityMetadata = Just (TargetMetadata $ TokenTarget token)
    }

instance ActionRunner env => HasAbilities env BeginnersLuck where
  getAbilities iid (Window Timing.When (RevealToken who token)) (BeginnersLuck x)
    | iid == who
    = pure [ability token x]
  getAbilities iid window (BeginnersLuck x) = getAbilities iid window x

instance
  ( ActAttrsRunner env
  , HasList Token env ()
  , HasCount SpendableClueCount env ()
  )
  => RunMessage env BeginnersLuck where
  runMessage msg a@(BeginnersLuck attrs@ActAttrs {..}) = case msg of
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
    After (GainClues _ _) -> do
      totalClues <- unSpendableClueCount <$> getCount ()
      requiredClues <- getPlayerCountValue (PerPlayer 4)
      a <$ when
        (totalClues >= requiredClues)
        (push $ AdvanceAct actId (toSource attrs))
    _ -> BeginnersLuck <$> runMessage msg attrs
