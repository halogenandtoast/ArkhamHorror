module Arkham.Location.Cards.AlienFrontierA (alienFrontierA) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenario.Deck
import Arkham.Scenarios.WithoutATrace.Helpers

newtype AlienFrontierA = AlienFrontierA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienFrontierA :: LocationCard AlienFrontierA
alienFrontierA = symbolLabel $ locationWith AlienFrontierA Cards.alienFrontierA 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities AlienFrontierA where
  getAbilities (AlienFrontierA a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ Moves #when You AnySource (be a) leftOrRight
      , groupLimit PerGame
          $ restricted
            a
            2
            (Here <> oneOf [exists $ LocationWithPlacement InTheShadows, ScenarioDeckWithCard OtherworldDeck])
          $ actionAbilityWithCost (HorrorCost (a.ability 2) YouTarget 1)
      ]
   where
    leftOrRight = case a.position of
      Just (Pos x y) -> mapOneOf LocationInPosition [Pos (x - 1) y, Pos (x + 1) y]
      Nothing -> Nowhere

instance RunMessage AlienFrontierA where
  runMessage msg l@(AlienFrontierA attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnants]" v -> do
      let (iid, dir, lid) = toResult v
      when (lid == attrs.id) do
        case dir of
          LeftPosition -> exposedInShadows iid attrs $ assignDamage iid (attrs.ability (-1)) 1
          MiddlePosition -> pure ()
          RightPosition -> exposedInShadows iid attrs $ assignHorror iid (attrs.ability (-1)) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 5)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 2
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      ls <- select $ LocationWithPlacement InTheShadows
      chooseOneM iid do
        targeting OtherworldDeck $ lookAtTopOfDeck iid OtherworldDeck
        targets ls $ lookAtRevealed iid (toSource attrs)
      pure l
    _ -> AlienFrontierA <$> liftRunMessage msg attrs
