module Arkham.Act.Cards.OpeningTheMaw (openingTheMaw) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Token

newtype OpeningTheMaw = OpeningTheMaw ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openingTheMaw :: ActCard OpeningTheMaw
openingTheMaw = act (2, A) OpeningTheMaw Cards.openingTheMaw Nothing

instance HasAbilities OpeningTheMaw where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ actionAbilityWithCost
        $ GroupClueCost (PerPlayer 1) (locationIs Locations.mouthOfKnYanTheCavernsMaw)
    , restricted
        a
        2
        (TokensOnLocation (locationIs Locations.mouthOfKnYanTheCavernsMaw) Pillar (atLeast 6))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage OpeningTheMaw where
  runMessage msg a@(OpeningTheMaw attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mouthOfKnYan <- selectJust $ locationIs Locations.mouthOfKnYanTheCavernsMaw
      placeTokens (attrs.ability 1) mouthOfKnYan Pillar 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> OpeningTheMaw <$> liftRunMessage msg attrs
