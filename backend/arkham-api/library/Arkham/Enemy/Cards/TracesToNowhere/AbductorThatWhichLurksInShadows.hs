module Arkham.Enemy.Cards.TracesToNowhere.AbductorThatWhichLurksInShadows (
  abductorThatWhichLurksInShadows,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Trait (Trait (Ally, Witness))

newtype AbductorThatWhichLurksInShadows = AbductorThatWhichLurksInShadows EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abductorThatWhichLurksInShadows :: EnemyCard AbductorThatWhichLurksInShadows
abductorThatWhichLurksInShadows =
  enemy AbductorThatWhichLurksInShadows Cards.abductorThatWhichLurksInShadows

-- | "Each investigator at Abductor's location cannot parley."
instance HasModifiersFor AbductorThatWhichLurksInShadows where
  getModifiersFor (AbductorThatWhichLurksInShadows a) =
    modifySelect a (InvestigatorAt $ locationWithEnemy a) [CannotParleyWith AnyEnemy]

instance HasAbilities AbductorThatWhichLurksInShadows where
  getAbilities (AbductorThatWhichLurksInShadows a) =
    extend
      a
      [ mkAbility a 1
          $ forced
          $ oneOf
            [ EnemyAttackedSuccessfully #after You AnySource (be a)
            , EnemyEvadedSuccessfully #after You AnySource (be a)
            ]
      , mkAbility a 2
          $ forced
          $ oneOf [PhaseEnds #when #enemy, EnemyAttacks #after You AnyEnemyAttack (be a)]
      ]

instance RunMessage AbductorThatWhichLurksInShadows where
  runMessage msg e@(AbductorThatWhichLurksInShadows attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      setCardAside (toCard attrs)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      victims <- select $ at_ (locationWithEnemy attrs) <> mapOneOf AssetWithTrait [Ally, Witness]
      chooseTargetM iid victims abductAsset
      setCardAside (toCard attrs)
      pure e
    _ -> AbductorThatWhichLurksInShadows <$> liftRunMessage msg attrs
