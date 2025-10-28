module Arkham.Treachery.Cards.CallingCard (callingCard) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token qualified as Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CallingCard = CallingCard TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callingCard :: TreacheryCard CallingCard
callingCard = treachery CallingCard Cards.callingCard

instance HasModifiersFor CallingCard where
  getModifiersFor (CallingCard a) = do
    modifySelect a (InvestigatorAt Anywhere) [CannotSpendClues]

instance HasAbilities CallingCard where
  getAbilities (CallingCard a) =
    [ restricted a 1 OnSameLocation
        $ freeReaction
        $ SkillTestResult
          #after
          You
          (WhileInvestigating $ locationWithTreachery a)
          (SuccessResult $ atLeast 2)
    ]

instance RunMessage CallingCard where
  runMessage msg t@(CallingCard attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      locations <-
        select
          $ not_ (LocationWithToken Token.Target)
          <> LocationWithoutTreachery (treacheryIs Cards.callingCard)
      chooseTargetM iid locations $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      chooseRevealConcealedAt iid (attrs.ability 1) Anywhere
      pure t
    _ -> CallingCard <$> liftRunMessage msg attrs
