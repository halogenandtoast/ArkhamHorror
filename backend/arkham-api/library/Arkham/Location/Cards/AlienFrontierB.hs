module Arkham.Location.Cards.AlienFrontierB (alienFrontierB) where

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

newtype AlienFrontierB = AlienFrontierB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienFrontierB :: LocationCard AlienFrontierB
alienFrontierB = symbolLabel $ locationWith AlienFrontierB Cards.alienFrontierB 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities AlienFrontierB where
  getAbilities (AlienFrontierB a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ Moves #when You AnySource (be a) aboveOrBelow
      , groupLimit PerGame
          $ restricted
            a
            2
            (Here <> oneOf [exists $ LocationWithPlacement InTheShadows, ScenarioDeckWithCard OtherworldDeck])
          $ actionAbilityWithCost (HorrorCost (a.ability 2) YouTarget 1)
      ]
   where
    aboveOrBelow = case a.position of
      Just (Pos x y) -> mapOneOf LocationInPosition [Pos x (y - 1), Pos x (y + 1)]
      Nothing -> Nowhere

instance RunMessage AlienFrontierB where
  runMessage msg l@(AlienFrontierB attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnants]" v -> do
      let (iid, dir, lid) = toResult v
      when (lid == attrs.id) do
        case dir of
          LeftPosition -> do
            whenMatch iid InvestigatorWithAnyResources do
              exposedInShadows iid attrs $ loseResources iid (attrs.ability (-1)) 2
          MiddlePosition -> do
            whenMatch iid InvestigatorWithAnyActionsRemaining do
              exposedInShadows iid attrs $ loseActions iid (attrs.ability (-1)) 1
          RightPosition -> pure ()
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
    _ -> AlienFrontierB <$> liftRunMessage msg attrs
