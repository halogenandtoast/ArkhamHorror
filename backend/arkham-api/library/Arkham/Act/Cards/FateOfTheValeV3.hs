module Arkham.Act.Cards.FateOfTheValeV3 (fateOfTheValeV3) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Investigator.Types (Field (InvestigatorLocation))
import Arkham.Location.Types (Field (LocationResources, LocationShroud))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Projection
import Arkham.Token (Token (Resource))
import Arkham.Treachery.Cards qualified as Treacheries

newtype FateOfTheValeV3 = FateOfTheValeV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfTheValeV3 :: ActCard FateOfTheValeV3
fateOfTheValeV3 = act (3, A) FateOfTheValeV3 Cards.fateOfTheValeV3 Nothing

instance HasAbilities FateOfTheValeV3 where
  getAbilities (FateOfTheValeV3 a) =
    extend
      a
      [ restricted a 1 (DuringTurn You) $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) YourLocation)
      , restricted a 2 (DuringTurn You) actionAbility
      , onlyOnce $ mkAbility a 3 $ Objective $ forced $ RoundEnds #when
      ]

instance RunMessage FateOfTheValeV3 where
  runMessage msg a@(FateOfTheValeV3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      field InvestigatorLocation iid >>= traverse_ \lid -> placeTokens (attrs.ability 1) lid Resource 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      field InvestigatorLocation iid >>= traverse_ \lid -> do
        resources <- field LocationResources lid
        shroud <- fieldWithDefault 0 LocationShroud lid
        when (resources >= shroud) do
          removeTokens (attrs.ability 2) lid Resource resources
          drawCard iid =<< getSetAsideCard Treacheries.fire
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      fireCount <- selectCount $ treacheryIs Treacheries.fire
      when (fireCount >= 5) $ advanceVia #other attrs (attrs.ability 3)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R4
      pure a
    _ -> FateOfTheValeV3 <$> liftRunMessage msg attrs
