module Arkham.Act.Cards.PursuitOfTheUnknownV3 (pursuitOfTheUnknownV3) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (ElderThing, Elite, Shoggoth))

newtype PursuitOfTheUnknownV3 = PursuitOfTheUnknownV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pursuitOfTheUnknownV3 :: ActCard PursuitOfTheUnknownV3
pursuitOfTheUnknownV3 = act (2, A) PursuitOfTheUnknownV3 Cards.pursuitOfTheUnknownV3 Nothing

instance HasAbilities PursuitOfTheUnknownV3 where
  getAbilities (PursuitOfTheUnknownV3 x) =
    [ mkAbility x 1
        $ forced
        $ EnemyEnters #when (LocationWithEnemy $ EnemyWithTrait ElderThing) (hasAnyTrait [Shoggoth, Elite])
    , restricted x 2 (exists $ OnlyInBag #elderthing)
        $ actionAbilityWithCost (SpendTokenKeyCost 2 #elderthing)
    , restricted
        x
        3
        (EachUndefeatedInvestigator $ at_ $ "Hidden Tunnel" <> LocationWithoutClues)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage PursuitOfTheUnknownV3 where
  runMessage msg a@(PursuitOfTheUnknownV3 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      push $ RemoveAllChaosTokens #elderthing
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    _ -> PursuitOfTheUnknownV3 <$> liftRunMessage msg attrs
