module Arkham.Types.Location.Cards.CloverClubCardroom
  ( cloverClubCardroom
  , CloverClubCardroom(..)
  )
where

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
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Token
import Arkham.Types.Window

newtype CloverClubCardroom = CloverClubCardroom LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubCardroom :: LocationId -> CloverClubCardroom
cloverClubCardroom = CloverClubCardroom . baseAttrs
  Cards.cloverClubCardroom
  3
  (Static 0)
  Triangle
  [Circle, Square, Diamond]

instance HasModifiersFor env CloverClubCardroom where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs = base { abilityLimit = GroupLimit PerRound 1 }
 where
  base = mkAbility
    (toSource attrs)
    1
    (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 2])

instance ActionRunner env => HasActions env CloverClubCardroom where
  getActions iid NonFast (CloverClubCardroom attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ do
      step <- unActStep <$> getStep
      pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | iid `member` locationInvestigators && step == 1
        ]
  getActions iid window (CloverClubCardroom attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env CloverClubCardroom where
  runMessage msg l@(CloverClubCardroom attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _
      | isSource attrs source && locationRevealed -> l
      <$ unshiftMessage (RequestTokens source (Just iid) 1 SetAside)
    RequestedTokens source (Just iid) tokens | isSource attrs source -> do
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
          tokens
      l <$ unshiftMessages
        ([chooseOne iid [Continue "Apply results"]]
        <> msgs
        <> [ResetTokens source]
        )
    _ -> CloverClubCardroom <$> runMessage msg attrs
