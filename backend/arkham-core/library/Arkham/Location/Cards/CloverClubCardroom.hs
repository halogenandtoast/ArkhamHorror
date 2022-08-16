module Arkham.Location.Cards.CloverClubCardroom
  ( cloverClubCardroom
  , CloverClubCardroom(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( cloverClubCardroom )
import Arkham.Location.Runner
import Arkham.Message
import Arkham.RequestedTokenStrategy
import Arkham.Token

newtype CloverClubCardroom = CloverClubCardroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubCardroom :: LocationCard CloverClubCardroom
cloverClubCardroom =
  location CloverClubCardroom Cards.cloverClubCardroom 3 (Static 0)

instance HasAbilities CloverClubCardroom where
  getAbilities (CloverClubCardroom attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 (OnAct 1 <> Here)
      $ ActionAbility Nothing
      $ Costs [ActionCost 1, ResourceCost 2]
    | locationRevealed attrs
    ]

instance RunMessage CloverClubCardroom where
  runMessage msg l@(CloverClubCardroom attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (RequestTokens source (Just iid) (Reveal 1) SetAside)
    RequestedTokens source (Just iid) tokens | isSource attrs source -> do
      tokenFaces <- getModifiedTokenFaces tokens
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
      pushAll
        $ [chooseOne iid [Label "Apply results" msgs]]
        <> [ResetTokens source]
      pure l
    _ -> CloverClubCardroom <$> runMessage msg attrs
