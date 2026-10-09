module Arkham.Homebrew.TheSymphonyOfErichZann.Enemies.EarsOfTheVoid (earsOfTheVoid) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (PlayCard)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype EarsOfTheVoid = EarsOfTheVoid EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- "Prey - Largest hand size."
earsOfTheVoid :: EnemyCard EarsOfTheVoid
earsOfTheVoid = enemy EarsOfTheVoid Cards.earsOfTheVoid & setPrey MostCardsInHand

instance HasAbilities EarsOfTheVoid where
  -- While ready, playing or committing a card at its location provokes an attack.
  getAbilities (EarsOfTheVoid a) =
    extend1 a
      $ restricted a 1 (youExist (at_ (locationWithEnemy a.id)) <> thisExists a ReadyEnemy)
      $ forced
      $ oneOf
        [ PlayCard #after (at_ $ locationWithEnemy a.id) #any
        , CommittedCard #after (at_ $ locationWithEnemy a.id) #any
        ]

instance RunMessage EarsOfTheVoid where
  runMessage msg e@(EarsOfTheVoid attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      unless attrs.exhausted do
        initiateEnemyAttack attrs (attrs.ability 1) iid
        exhaustThis attrs
      pure e
    _ -> EarsOfTheVoid <$> liftRunMessage msg attrs
