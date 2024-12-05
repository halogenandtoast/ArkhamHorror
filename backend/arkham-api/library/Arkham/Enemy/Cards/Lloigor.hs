module Arkham.Enemy.Cards.Lloigor (lloigor, Lloigor (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher

newtype Lloigor = Lloigor EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lloigor :: EnemyCard Lloigor
lloigor =
  enemyWith Lloigor Cards.lloigor (3, Static 5, 4) (1, 1)
    $ preyL
    .~ Prey FewestCardsInHand

instance HasModifiersFor Lloigor where
  getModifiersFor (Lloigor a) = do
    valid <-
      selectAny $ InvestigatorAt (locationWithEnemy a.id) <> HandWith (LengthIs $ EqualTo $ Static 0)
    modifySelfWhen a valid [RemoveKeyword Aloof]

instance HasAbilities Lloigor where
  getAbilities (Lloigor a) =
    extend1 a
      $ restricted a 1 (exists $ at_ (orConnected $ locationWithEnemy a) <> HandWith AnyCards)
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage Lloigor where
  runMessage msg e@(Lloigor attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (at_ (orConnected $ locationWithEnemy attrs) <> HandWith AnyCards) \iid -> do
        randomDiscard iid (attrs.ability 1)
      pure e
    _ -> Lloigor <$> liftRunMessage msg attrs
