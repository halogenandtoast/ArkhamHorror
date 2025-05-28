module Arkham.Location.Cards.TheOnyxCastle (theOnyxCastle) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Placement qualified as Placement

newtype TheOnyxCastle = TheOnyxCastle LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOnyxCastle :: LocationCard TheOnyxCastle
theOnyxCastle = location TheOnyxCastle Cards.theOnyxCastle 3 (Static 0)

instance HasModifiersFor TheOnyxCastle where
  getModifiersFor (TheOnyxCastle a) = whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities TheOnyxCastle where
  getAbilities (TheOnyxCastle a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( exists (NotYou <> at_ (be a))
            <> oneOf
              [ exists (TreacheryInHandOf You <> not_ SignatureTreachery)
              , exists (EnemyInHandOf You <> not_ SignatureEnemy)
              ]
        )
        actionAbility

instance RunMessage TheOnyxCastle where
  runMessage msg l@(TheOnyxCastle attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ not_ (InvestigatorWithId iid) <> investigatorAt attrs
      treacheries <- select $ treacheryInHandOf iid <> not_ SignatureTreachery
      enemies <- select $ enemyInHandOf iid <> not_ SignatureEnemy
      chooseOrRunOneM iid do
        targets treacheries \treachery ->
          chooseOneM iid do
            targets investigators $ place treachery . Placement.HiddenInHand
        targets enemies \enemy ->
          chooseOneM iid do
            targets investigators $ place enemy . Placement.HiddenInHand
      pure l
    _ -> TheOnyxCastle <$> liftRunMessage msg attrs
