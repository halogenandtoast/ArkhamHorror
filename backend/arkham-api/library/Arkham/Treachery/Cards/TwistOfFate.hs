module Arkham.Treachery.Cards.TwistOfFate (twistOfFate) where

import Arkham.ChaosToken
import Arkham.Game.Helpers (getModifiedChaosTokenFaces)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TwistOfFate = TwistOfFate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistOfFate :: TreacheryCard TwistOfFate
twistOfFate = treachery TwistOfFate Cards.twistOfFate

instance RunMessage TwistOfFate where
  runMessage msg t@(TwistOfFate attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      requestChaosTokens iid attrs 1
      pure t
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      faces <- getModifiedChaosTokenFaces tokens
      let
        (damage, horror) = bimap getSum getSum $ flip foldMap faces \case
          ElderSign -> (Sum 0, Sum 0)
          PlusOne -> (Sum 0, Sum 0)
          Zero -> (Sum 1, Sum 0)
          MinusOne -> (Sum 1, Sum 0)
          MinusTwo -> (Sum 1, Sum 0)
          MinusThree -> (Sum 1, Sum 0)
          MinusFour -> (Sum 1, Sum 0)
          MinusFive -> (Sum 1, Sum 0)
          MinusSix -> (Sum 1, Sum 0)
          MinusSeven -> (Sum 1, Sum 0)
          MinusEight -> (Sum 1, Sum 0)
          Skull -> (Sum 0, Sum 2)
          Cultist -> (Sum 0, Sum 2)
          Tablet -> (Sum 0, Sum 2)
          ElderThing -> (Sum 0, Sum 2)
          AutoFail -> (Sum 0, Sum 2)
          CurseToken -> (Sum 0, Sum 0)
          BlessToken -> (Sum 0, Sum 0)
          FrostToken -> (Sum 0, Sum 0)

      continue iid $ assignDamageAndHorror iid attrs damage horror
      pure t
    _ -> TwistOfFate <$> liftRunMessage msg attrs
