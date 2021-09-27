module Arkham.Types.Location.Cards.CloverClubCardroom
  ( cloverClubCardroom
  , CloverClubCardroom(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (cloverClubCardroom)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Token

newtype CloverClubCardroom = CloverClubCardroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubCardroom :: LocationCard CloverClubCardroom
cloverClubCardroom = location
  CloverClubCardroom
  Cards.cloverClubCardroom
  3
  (Static 0)
  Triangle
  [Circle, Square, Diamond]

instance HasAbilities CloverClubCardroom where
  getAbilities (CloverClubCardroom attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
          attrs
          1
          (OnAct 1)
          (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 2])
        & (abilityLimitL .~ GroupLimit PerRound 1)
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env CloverClubCardroom where
  runMessage msg l@(CloverClubCardroom attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (RequestTokens source (Just iid) 1 SetAside)
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
