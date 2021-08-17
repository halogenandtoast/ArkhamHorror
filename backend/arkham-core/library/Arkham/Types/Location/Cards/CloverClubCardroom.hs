module Arkham.Types.Location.Cards.CloverClubCardroom
  ( cloverClubCardroom
  , CloverClubCardroom(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (cloverClubCardroom)
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.RequestedTokenStrategy
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Arkham.Types.Window

newtype CloverClubCardroom = CloverClubCardroom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubCardroom :: LocationCard CloverClubCardroom
cloverClubCardroom = location
  CloverClubCardroom
  Cards.cloverClubCardroom
  3
  (Static 0)
  Triangle
  [Circle, Square, Diamond]

instance HasModifiersFor env CloverClubCardroom

ability :: LocationAttrs -> Ability
ability attrs = base { abilityLimit = GroupLimit PerRound 1 }
 where
  base = mkAbility
    (toSource attrs)
    1
    (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 2])

instance ActionRunner env => HasAbilities env CloverClubCardroom where
  getAbilities iid window@(Window Timing.When NonFast) (CloverClubCardroom attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseActions iid window attrs $ do
      step <- unActStep <$> getStep
      pure [ locationAbility (ability attrs) | step == 1 ]
  getAbilities iid window (CloverClubCardroom attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env CloverClubCardroom where
  runMessage msg l@(CloverClubCardroom attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _
      | isSource attrs source && locationRevealed -> l
      <$ push (RequestTokens source (Just iid) 1 SetAside)
    RequestedTokens source (Just iid) tokens | isSource attrs source -> do
      tokenFaces <- getModifiedTokenFaces source tokens
      let
        msgs = concatMap
          (\case
            ElderSign -> [GainClues iid 2, TakeResources iid 2 False]
            PlusOne -> []
            Zero -> [GainClues iid 2]
            MinusOne -> []
            MinusTwo -> [GainClues iid 2]
            MinusThree -> []
            MinusFour -> [GainClues iid 2]
            MinusFive -> []
            MinusSix -> [GainClues iid 2]
            MinusSeven -> []
            MinusEight -> [GainClues iid 2]
            Skull -> []
            Cultist -> []
            Tablet -> []
            ElderThing -> []
            AutoFail -> []
          )
          tokenFaces
      l <$ pushAll
        ([chooseOne iid [Continue "Apply results"]]
        <> msgs
        <> [ResetTokens source]
        )
    _ -> CloverClubCardroom <$> runMessage msg attrs
