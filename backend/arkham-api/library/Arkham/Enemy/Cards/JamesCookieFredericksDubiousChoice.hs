module Arkham.Enemy.Cards.JamesCookieFredericksDubiousChoice
  ( jamesCookieFredericksDubiousChoice
  , JamesCookieFredericksDubiousChoice(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype JamesCookieFredericksDubiousChoice = JamesCookieFredericksDubiousChoice EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

jamesCookieFredericksDubiousChoice :: EnemyCard JamesCookieFredericksDubiousChoice
jamesCookieFredericksDubiousChoice = enemy JamesCookieFredericksDubiousChoice Cards.jamesCookieFredericksDubiousChoice (0, Static 1, 0) (0, 0)

instance RunMessage JamesCookieFredericksDubiousChoice where
  runMessage msg (JamesCookieFredericksDubiousChoice attrs) = runQueueT $ case msg of
    _ -> JamesCookieFredericksDubiousChoice <$> liftRunMessage msg attrs
