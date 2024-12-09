module Arkham.Location.Cards.CanalsOfTenochtitlan_181 (
  canalsOfTenochtitlan_181,
  CanalsOfTenochtitlan_181 (..),
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CanalsOfTenochtitlan_181 = CanalsOfTenochtitlan_181 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

canalsOfTenochtitlan_181 :: LocationCard CanalsOfTenochtitlan_181
canalsOfTenochtitlan_181 =
  symbolLabel $ location CanalsOfTenochtitlan_181 Cards.canalsOfTenochtitlan_181 2 (PerPlayer 1)

instance HasModifiersFor CanalsOfTenochtitlan_181 where
  getModifiersFor (CanalsOfTenochtitlan_181 a) =
    whenRevealed a $ modifySelfWhen a (a.token #resource > 0) [ShroudModifier (a.token #resource)]

instance HasAbilities CanalsOfTenochtitlan_181 where
  getAbilities (CanalsOfTenochtitlan_181 a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ PutLocationIntoPlay #after Anyone (be a)
      , restricted a 2 (ResourcesOnThis $ atLeast 1) $ forced $ RoundEnds #when
      ]

instance RunMessage CanalsOfTenochtitlan_181 where
  runMessage msg l@(CanalsOfTenochtitlan_181 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs #resource 4
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeTokens (attrs.ability 2) attrs #resource 1
      pure l
    _ -> CanalsOfTenochtitlan_181 <$> liftRunMessage msg attrs
