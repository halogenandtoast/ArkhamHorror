module Arkham.Location.Cards.SecondFloorHall (
  secondFloorHall,
  SecondFloorHall (..),
)
where

import Arkham.Prelude

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.RequestedChaosTokenStrategy

newtype SecondFloorHall = SecondFloorHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secondFloorHall :: LocationCard SecondFloorHall
secondFloorHall = location SecondFloorHall Cards.secondFloorHall 2 (PerPlayer 1)

instance HasAbilities SecondFloorHall where
  getAbilities (SecondFloorHall attrs) =
    withRevealedAbilities
      attrs
      [mkAbility attrs 1 $ ReactionAbility (Enters #after You $ LocationWithId $ toId attrs) Free]

instance RunMessage SecondFloorHall where
  runMessage msg l@(SecondFloorHall attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      accessibleLocationIds <- accessibleLocations iid
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel
            lid
            [ Move $ move attrs iid lid
            , RequestChaosTokens (toAbilitySource attrs 1) (Just iid) (Reveal 1) SetAside
            ]
          | lid <- accessibleLocationIds
          ]
      pure l
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      push $ ResetChaosTokens (toAbilitySource attrs 1)
      pushWhen
        ( any
            ((`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) . chaosTokenFace)
            tokens
        )
        $ InvestigatorDrawEncounterCard iid
      pure l
    _ -> SecondFloorHall <$> runMessage msg attrs
