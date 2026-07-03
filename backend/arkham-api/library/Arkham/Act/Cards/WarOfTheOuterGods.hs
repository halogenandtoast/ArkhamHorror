module Arkham.Act.Cards.WarOfTheOuterGods (warOfTheOuterGods) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype WarOfTheOuterGods = WarOfTheOuterGods ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

warOfTheOuterGods :: ActCard WarOfTheOuterGods
warOfTheOuterGods = act (1, A) WarOfTheOuterGods Cards.warOfTheOuterGods Nothing

instance HasAbilities WarOfTheOuterGods where
  getAbilities (WarOfTheOuterGods a) =
    [ mkAbility a 1 $ forced $ PlacedDoomCounter #after AnySource (TargetControlledBy Anyone)
    , restricted a 2 (notExists $ enemy_ #cultist)
        $ Objective
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 3) Anywhere)
    ]

instance RunMessage WarOfTheOuterGods where
  runMessage msg a@(WarOfTheOuterGods attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getPlacedDoomAmount -> n) _ -> do
      lead <- getLead
      chooseSelectM lead AnyAgenda \agenda -> placeDoom (attrs.ability 1) agenda n
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      placeSetAsideLocation_ Locations.hubDimension
      advanceActDeck attrs
      pure a
    _ -> WarOfTheOuterGods <$> liftRunMessage msg attrs
